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

init: Animation.Tracker -> Math.Tree (Matcher.State Animation.State) -> (Model, Animation.Tracker)
init tracker root =
    let
        (movingTree, viewBox, newT) = calculateTree_ tracker root Dict.empty
    in
        ({rects = movingTree, viewBox = Animation.newEase smoothTime_ (0,0) viewBox}, newT)

advanceTime: Float -> Model -> Model
advanceTime millis model =
    let
        newRects = model.rects |> Dict.map (\_ rect ->
            let
                newBottomLeft = rect.bottomLeft |> Animation.smoothDampVector2 millis
                newTopRight = rect.topRight |> Animation.smoothDampVector2 millis
                newOpacity = rect.opacity |> Animation.smoothDampFloat millis
            in
                {   rect
                |   bottomLeft = newBottomLeft
                ,   topRight = newTopRight
                ,   opacity = newOpacity
                }
            )
        newViewBox = model.viewBox |> Animation.smoothDampVector2 millis
    in
        { model | rects = newRects, viewBox = newViewBox }

view: Int -> Set.Set Int -> (Int -> Int -> Bool -> e) -> Model -> Html e
view eq highlight onShiftClick model =
    let
        -- we want ids that exist to be drawn on top, so prepend visible bricks to the resulting list first
        visBricks = model.rects |> Dict.foldl (foldRectToBrick eq highlight onShiftClick True) []
        bricks = model.rects |> Dict.foldl (foldRectToBrick eq highlight onShiftClick False) visBricks
        (maxX, maxY) = Animation.current model.viewBox
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
            (blX, blY) = Animation.current rect.bottomLeft
            (trX, trY) = Animation.current rect.topRight
            brick = BrickSvg.brick blX trX blY trY (Animation.current rect.opacity) rect.visible selected onClick rect.text
        in
            brick :: foldBricks

-- TODO: make falling bricks drop like gravity, and rising bricks pushed by new ones below
updateTree: Animation.Tracker -> Math.Tree (Matcher.State Animation.State) -> Model -> (Model, Animation.Tracker)
updateTree tracker root model =
    let
        (newRect, newViewBox, newT) = calculateTree_ tracker root model.rects
        (finalViewBox, t1) = Animation.setEase newT newViewBox model.viewBox
    in

        ({model | rects = newRect, viewBox = finalViewBox}, t1)

calculateTree_: Animation.Tracker -> Math.Tree (Matcher.State Animation.State) -> Dict.Dict Int Rect -> (Dict.Dict Int Rect, Animation.Vector2, Animation.Tracker)
calculateTree_ animation root rects =
    let
        emptyGrid = (Grid Dict.empty (Array.initialize 1 (\_ -> 0)))
        grid = stackRecursive_ 0 root emptyGrid

        -- don't tween if they aren't visible
        oldRects = rects |> Dict.filter (\_ rect -> rect.visible)

        -- if we are doing an undo then we can try to reverse-engineer where it came from
        nextIDs = oldRects |> Dict.foldl (\id rect foldMap -> foldMap |> Dict.insert rect.prevID id) Dict.empty

        (visibleRects, newAnimaiton) = grid.items |> Dict.foldl (\id item (foldRects, a0) ->
            let
                blTarget = (grid.lines |> getLine item.colStart, toFloat item.rowStart)
                trTarget = (grid.lines |> getLine item.colEnd, toFloat item.rowEnd)

                -- TODO: use prevID more correctly
                --   e.g. the new rect may jerkily appear outside the current frame if there is nothing to tween from
                --   I also wonder if we can allow blocks to share a border to make it look like it's splitting better
                thisOld = oldRects |> Dict.get id
                prevOld = oldRects |> Dict.get item.prevID
                nextOld = oldRects |> Dict.get (nextIDs |> Dict.get id |> Maybe.withDefault -1)
                noOld = Rect item.text id True (Animation.newEase smoothTime_ (0,0) blTarget) (Animation.newEase smoothTime_ (0, 0) trTarget) (Animation.newEase (smoothTime_/2) 0 0)
                old = thisOld |> Maybe.withDefault (prevOld |> Maybe.withDefault (nextOld |> Maybe.withDefault noOld))

                (bl, a1) = Animation.setEase a0 blTarget old.bottomLeft
                (tr, a2) = Animation.setEase a1 trTarget old.topRight
                (op, a3) = Animation.setEase a2 1 old.opacity
                new = (
                    {   old
                    |   text = item.text
                    ,   prevID = item.prevID
                    ,   visible = True
                    ,   bottomLeft = bl
                    ,   topRight = tr
                    ,   opacity = op
                    })
            in
                (Dict.insert id new foldRects, a3)
            ) (Dict.empty, animation)

        -- need to keep deleted nodes in order to animate them away
        (leavingRects, finalA) = oldRects |> Dict.foldl (\id rect (foldRects, a) ->
            if Dict.member id foldRects
            then (foldRects, a)
            else
                let
                    (op, a1) = Animation.setEase a 0 rect.opacity
                    leavingRect = { rect | visible = False, opacity = op }
                in
                    (Dict.insert id leavingRect foldRects, a1)
            ) (visibleRects, newAnimaiton)

        viewBox = visibleRects |> Dict.foldl (\_ rect (foldX, foldY) ->
            let
                (trX, trY) = Animation.target rect.topRight
            in
                (max foldX trX, max foldY trY)
            ) (0, 0)
    in
        (leavingRects, viewBox, finalA)


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
