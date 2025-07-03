module UI.Bricks exposing (
    Model,
    init, advanceTime, updateTree, view
    )

import Html exposing (Html)
import Dict
import Set
import Array
-- Ours
import Algo.Math as Math
import Algo.Matcher as Matcher
import UI.Animation as Animation
import UI.BrickSvg as BrickSvg
import UI.HtmlEvent as HtmlEvent


type alias Rect =
    {   text: String
    ,   prevID: Int
    ,   visible: Bool  -- False means it existed before but needs to fade away
    ,   bottomLeft: Animation.EaseState Animation.Vector2
    ,   topRight: Animation.EaseState Animation.Vector2
    ,   opacity: Animation.EaseState Float
    }

type alias Model =
    {   rects: Dict.Dict Int Rect
    ,   viewBox: Animation.EaseState Animation.Vector2
    }

smoothTime_: Float
smoothTime_ = 300

init: Math.Tree (Matcher.State Animation.State) -> Model
init root =
    let
        movingTree = updateTree root (Model Dict.empty (Animation.EaseState (0, 0) (0, 0) (0, 0)))
    in
        -- move time forward by a lot to force init as static
        advanceTime (smoothTime_ * 1e6) movingTree

advanceTime: Float -> Model -> Model
advanceTime millis model =
    let
        newRects = model.rects |> Dict.map (\_ rect ->
            let
                newBottomLeft = rect.bottomLeft |> Animation.smoothDampVector2 smoothTime_ millis
                newTopRight = rect.topRight |> Animation.smoothDampVector2 smoothTime_ millis
                newOpacity = rect.opacity |> Animation.smoothDampFloat (smoothTime_ / 2) millis
            in
                {   rect
                |   bottomLeft = newBottomLeft
                ,   topRight = newTopRight
                ,   opacity = newOpacity
                }
            )
        newViewBox = model.viewBox |> Animation.smoothDampVector2 smoothTime_ millis
    in
        { model | rects = newRects, viewBox = newViewBox }

view: Int -> Set.Set Int -> (Int -> Int -> Bool -> e) -> Model -> Html e
view eq highlight onShiftClick model =
    let
        -- we want ids that exist to be drawn on top, so prepend visible bricks to the resulting list first
        visBricks = model.rects |> Dict.foldl (foldRectToBrick eq highlight onShiftClick True) []
        bricks = model.rects |> Dict.foldl (foldRectToBrick eq highlight onShiftClick False) visBricks
        (maxX, maxY) = model.viewBox.current
    in
        BrickSvg.bricks maxX maxY bricks

foldRectToBrick: Int -> Set.Set Int -> (Int -> Int -> Bool -> e) -> Bool -> Int -> Rect -> List (Html e) -> List (Html e)
foldRectToBrick eq highlight onShiftClick includeVisible id rect foldBricks =
    if rect.visible /= includeVisible
    then foldBricks
    else
        let
            onClick = HtmlEvent.onShiftClick (onShiftClick eq id)
            selected = highlight |> Set.member id
            (blX, blY) = rect.bottomLeft.current
            (trX, trY) = rect.topRight.current
            brick = BrickSvg.brick blX trX blY trY rect.opacity.current rect.visible selected onClick rect.text
        in
            brick :: foldBricks

-- TODO: make falling bricks drop like gravity, and rising bricks pushed by new ones below
updateTree: Math.Tree (Matcher.State Animation.State) -> Model -> Model
updateTree root model =
    let
        emptyGrid = (Grid Dict.empty (Array.initialize 1 (\_ -> 0)))
        grid = stackRecursive_ 0 root emptyGrid

        -- don't tween if they aren't visible
        oldRects = model.rects |> Dict.filter (\_ rect -> rect.visible)

        -- if we are doing an undo then we can try to reverse-engineer where it came from
        nextIDs = oldRects |> Dict.foldl (\id rect foldMap -> foldMap |> Dict.insert rect.prevID id) Dict.empty

        visibleRects = grid.items |> Dict.foldl (\id item foldRects ->
            let
                blTarget = (grid.lines |> getLine item.colStart, toFloat item.rowStart)
                trTarget = (grid.lines |> getLine item.colEnd, toFloat item.rowEnd)

                -- TODO: use prevID more correctly
                --   e.g. the new rect may jerkily appear outside the current frame if there is nothing to tween from
                --   I also wonder if we can allow blocks to share a border to make it look like it's splitting better
                thisOld = oldRects |> Dict.get id
                prevOld = oldRects |> Dict.get item.prevID
                nextOld = oldRects |> Dict.get (nextIDs |> Dict.get id |> Maybe.withDefault -1)
                noOld = Rect item.text id True (Animation.EaseState blTarget blTarget (0, 0)) (Animation.EaseState trTarget trTarget (0, 0)) (Animation.EaseState 0 1 0)
                old = thisOld |> Maybe.withDefault (prevOld |> Maybe.withDefault (nextOld |> Maybe.withDefault noOld))

                blOld = old.bottomLeft
                trOld = old.topRight
                opOld = old.opacity
                new = (
                    {   old
                    |   text = item.text
                    ,   prevID = item.prevID
                    ,   visible = True
                    ,   bottomLeft = { blOld | target = blTarget }
                    ,   topRight = { trOld | target = trTarget }
                    ,   opacity = { opOld | target = 1 }
                    })
            in
                foldRects |> Dict.insert id new
            ) Dict.empty

        -- need to keep deleted nodes in order to animate them away
        leavingRects = oldRects |> Dict.foldl (\id rect foldRects ->
            if Dict.member id foldRects
            then foldRects
            else
                let
                    opOld = rect.opacity
                    leavingRect = { rect | visible = False, opacity = { opOld | target = 0 } }
                in
                    foldRects |> Dict.insert id leavingRect
            ) visibleRects

        (maxX, maxY) = visibleRects |> Dict.foldl (\_ rect (foldX, foldY) ->
            let
                (trX, trY) = rect.topRight.target
            in
                (max foldX trX, max foldY trY)
            ) (0, 0)

        vbOld = model.viewBox
        newViewBox = { vbOld | target = (maxX, maxY) }
    in
        { model | rects = leavingRects, viewBox = newViewBox }


type alias GridItem =
    {   prevID: Int
    ,   text: String
    ,   colStart: Int
    ,   colEnd: Int
    ,   rowStart: Int
    ,   rowEnd: Int
    }

type alias Grid =
    {   items: Dict.Dict Int GridItem
    ,   lines: Array.Array Float  -- only need column lines because children only nest in the x-axis
    }

getLine: Int -> Array.Array Float -> Float
getLine col lines =
    -- I would assert col < length lines here if I could
    lines |> Array.get col |> Maybe.withDefault -1

-- We are using 0.5pt font so the width should match 0.5 units
textWidth_: Float
textWidth_ = 0.5

stackRecursive_: Int  -> Math.Tree (Matcher.State Animation.State) -> Grid -> Grid
stackRecursive_ depth node grid =
    let
        id = Math.getState node |> Matcher.getID
        prevID = Math.getState node |> Matcher.getState |> .prevID
        text = Math.getName node
        colStart = (Array.length grid.lines) - 1
        lineStart = grid.lines |> getLine colStart
        width = (toFloat (String.length text)) * textWidth_ + (1 - textWidth_)
        children = Math.getChildren node
    in
        if List.isEmpty children
        then
            {   grid
            |   items = grid.items |> Dict.insert id (GridItem prevID text colStart (colStart+1) depth (depth+1))
            ,   lines = grid.lines |> Array.push (lineStart + width)
            }
        else
            let
                childrenGrid = children |>
                    List.foldl (stackRecursive_ (depth+1)) grid
                colEnd = (Array.length childrenGrid.lines) - 1
                lineEndChildren = childrenGrid.lines |> getLine colEnd

                -- if the parent is wider than all its children, then expand all contained columns to fit inside parent
                lineEndNode = lineStart + width
                expandedGrid =
                    if lineEndNode > lineEndChildren
                    then
                        {   childrenGrid
                        |   lines = childrenGrid.lines |> Array.indexedMap (\col line ->
                                if col >= colStart
                                then line * (lineEndNode / lineEndChildren)
                                else line
                            )
                        }
                    else childrenGrid
            in
                {   expandedGrid
                |   items = expandedGrid.items |> Dict.insert id (GridItem prevID text colStart colEnd depth (depth+1))
                }
