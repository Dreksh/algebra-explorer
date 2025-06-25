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
    ,   visible: Bool  -- False means it existed before but needs to fade away
    ,   bottomLeft: Animation.EaseState Animation.Vector2
    ,   topRight: Animation.EaseState Animation.Vector2
    ,   opacity: Animation.EaseState Float
    }

type alias Model =
    {   rects: Dict.Dict Int Rect
    }

init: Math.Tree (Matcher.State Animation.State) -> Model
init root = updateTree root (Model Dict.empty)

advanceTime: Float -> Model -> Model
advanceTime millis model =
    let
        newRects = model.rects |> Dict.map (\_ rect ->
            let
                newBottomLeft = rect.bottomLeft |> Animation.smoothDampVector2 300 millis
                newTopRight = rect.topRight |> Animation.smoothDampVector2 300 millis
                newOpacity = rect.opacity |> Animation.smoothDampFloat 150 millis
            in
                {   rect
                |   bottomLeft = newBottomLeft
                ,   topRight = newTopRight
                ,   opacity = newOpacity
                }
            )
    in
        { model | rects = newRects }

view: Int -> Set.Set Int -> (Int -> Int -> Bool -> e) -> Model -> Html e
view eq highlight onShiftClick model =
    let
        -- we want ids that exist to be drawn on top, so prepend visible bricks to the resulting list first
        (visBricks, maxX, maxY) = model.rects |> Dict.foldl (foldRectToBrick eq highlight onShiftClick True) ([], 0, 0)
        (bricks, _, _) = model.rects |> Dict.foldl (foldRectToBrick eq highlight onShiftClick False) (visBricks, maxX, maxY)
    in
        BrickSvg.bricks maxX maxY bricks

foldRectToBrick: Int -> Set.Set Int -> (Int -> Int -> Bool -> e) -> Bool -> Int -> Rect -> ((List (Html e)), Float, Float) -> ((List (Html e)), Float, Float)
foldRectToBrick eq highlight onShiftClick includeVisible id rect (foldBricks, foldX, foldY) =
    if rect.visible /= includeVisible
    then (foldBricks, foldX, foldY)
    else
        let
            onClick = HtmlEvent.onShiftClick (onShiftClick eq id)
            selected = highlight |> Set.member id
            (blX, blY) = rect.bottomLeft.current
            (trX, trY) = rect.topRight.current
            brick = BrickSvg.brick blX trX blY trY rect.opacity.current rect.visible selected onClick rect.text
        in
            (brick :: foldBricks, max foldX trX, max foldY trY)

-- TODO: make falling bricks drop like gravity, and rising bricks pushed by new ones below
updateTree: Math.Tree (Matcher.State Animation.State) -> Model -> Model
updateTree root model =
    let
        emptyGrid = (Grid Dict.empty (Array.initialize 1 (\_ -> 0)))
        grid = stackRecursive_ 0 root emptyGrid

        visibleRects = grid.items |> Dict.foldl (\id item foldRects ->
            let
                blTarget = (grid.lines |> getLine item.colStart, toFloat item.rowStart)
                trTarget = (grid.lines |> getLine item.colEnd, toFloat item.rowEnd)

                -- TODO: use prevID more correctly
                --   e.g. if neither id nor prevID exist then the new rect may jerkily appear outside the current frame
                thisOld = model.rects |> Dict.get id
                prevOld = model.rects |> Dict.get item.prevID
                noOld = Rect item.text True (Animation.EaseState blTarget blTarget (0, 0)) (Animation.EaseState trTarget trTarget (0, 0)) (Animation.EaseState 0 1 0)
                old = prevOld |> Maybe.withDefault (thisOld |> Maybe.withDefault noOld)

                blOld = old.bottomLeft
                trOld = old.topRight
                opOld = old.opacity
                new = (
                    {   old
                    |   text = item.text
                    ,   visible = True
                    ,   bottomLeft = { blOld | target = blTarget }
                    ,   topRight = { trOld | target = trTarget }
                    ,   opacity = { opOld | target = 1 }
                    })
            in
                foldRects |> Dict.insert id new
            ) Dict.empty

        -- need to keep deleted nodes in order to animate them away
        leavingRects = model.rects |> Dict.foldl (\id rect foldRects ->
            if Dict.member id visibleRects || not rect.visible  -- don't include ones that were already not visible
            then foldRects
            else
                let
                    opOld = rect.opacity
                    goneRect = { rect | visible = False, opacity = { opOld | target = 0 } }
                in
                    foldRects |> Dict.insert id goneRect
            ) visibleRects
    in
        { model | rects = leavingRects }


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
