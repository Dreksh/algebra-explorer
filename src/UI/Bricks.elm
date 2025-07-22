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


type alias Rect =
    {   text: String
    ,   prevID: Int
    ,   visible: Bool  -- False means it existed before but needs to fade away
    ,   bottomLeft: Animation.EaseState Animation.Vector2
    ,   topRight: Animation.EaseState Animation.Vector2
    ,   opacity: Animation.EaseState Float
    ,   draggable: Maybe (Int, List Float)
    }

type alias Model =
    {   rects: Dict.Dict Int Rect
    ,   viewBox: Animation.EaseState Animation.Vector2
    }

smoothTime_: Float
smoothTime_ = 750

init: Animation.Tracker -> Math.Tree (Matcher.State Animation.State) -> (Model, Animation.Tracker)
init tracker root =
    let
        (movingTree, viewBox, newT) = calculateTree_ tracker root Dict.empty
    in
        ({rects = movingTree, viewBox = Animation.newEaseVector2 smoothTime_ viewBox}, newT)

advanceTime: Float -> Model -> Model
advanceTime millis model =
    let
        newRects = model.rects |> Dict.map (\_ rect ->
            let
                newBottomLeft = rect.bottomLeft |> Animation.advance millis
                newTopRight = rect.topRight |> Animation.advance millis
                newOpacity = rect.opacity |> Animation.advance millis
            in
                {   rect
                |   bottomLeft = newBottomLeft
                ,   topRight = newTopRight
                ,   opacity = newOpacity
                }
            )
        newViewBox = model.viewBox |> Animation.advance millis
    in
        { model | rects = newRects, viewBox = newViewBox }

view: (Int -> Maybe (Int, List Float) -> List (Html.Attribute e)) -> Model -> Html e
view attrs model =
    let
        -- we want ids that exist to be drawn on top, so prepend visible bricks to the resulting list first
        visBricks = model.rects |> Dict.foldl (foldRectToBrick attrs True) []
        bricks = model.rects |> Dict.foldl (foldRectToBrick attrs False) visBricks
        (maxX, maxY) = Animation.current model.viewBox
    in
        BrickSvg.bricks maxX maxY bricks

foldRectToBrick: (Int -> Maybe (Int, List Float) -> List (Html.Attribute e)) -> Bool -> Int -> Rect -> List (Html e) -> List (Html e)
foldRectToBrick createAttrs includeVisible id rect foldBricks =
    if rect.visible /= includeVisible
    then foldBricks
    else
        let
            attrs = createAttrs id rect.draggable
            (blX, blY) = Animation.current rect.bottomLeft
            (trX, trY) = Animation.current rect.topRight
            brick = BrickSvg.brick blX trX blY trY (Animation.current rect.opacity) rect.visible attrs rect.text
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
        (grid, _) = stackRecursive_ 0 root ({items = Dict.empty, lines = Array.empty}, -1)

        -- don't tween if they aren't visible
        oldRects = rects |> Dict.filter (\_ rect -> rect.visible)

        -- if we are doing an undo then we can try to reverse-engineer where it came from
        nextIDs = oldRects |> Dict.foldl (\id rect foldMap -> foldMap |> Dict.insert rect.prevID id) Dict.empty

        (visibleRects, newAnimaiton) = grid.items |> Dict.foldl (\id item (foldRects, a0) ->
            let
                blTarget = (getColX item.colStart grid.lines, toFloat item.rowStart)
                trTarget = (getColX item.colEnd grid.lines, toFloat item.rowEnd)

                -- TODO: use prevID more correctly
                --   e.g. the new rect may jerkily appear outside the current frame if there is nothing to tween from
                --   I also wonder if we can allow blocks to share a border to make it look like it's splitting better
                thisOld = oldRects |> Dict.get id
                prevOld = oldRects |> Dict.get item.prevID
                nextOld = oldRects |> Dict.get (nextIDs |> Dict.get id |> Maybe.withDefault -1)
                noOld = Rect item.text id True (Animation.newEaseVector2 smoothTime_ blTarget) (Animation.newEaseVector2 smoothTime_ trTarget) (Animation.newEaseFloat (smoothTime_/2) 0) Nothing
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
                    ,   draggable = item.draggable
                            |> Maybe.map (\(index, list) ->
                                (index, List.map (\(sCol,eCol) -> (getColX sCol grid.lines + getColX eCol grid.lines)/2 ) list)
                            )
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
    ,   draggable: Maybe (Int, List (Int, Int))
    }

type alias Grid =
    {   items: Dict.Dict Int GridItem
    ,   lines: Array.Array Float  -- only need column lines because children only nest in the x-axis
    }

-- getColX returns the x-component of the column's right-most point
-- the -1 input represents the left-most point (the left of the 0th column)
getColX: Int -> Array.Array Float -> Float
getColX col = Array.get col >> Maybe.withDefault 0

-- We are using 0.5pt font so the width should match 0.5 units
textWidth_: Float
textWidth_ = 0.5

stackRecursive_: Int -> Math.Tree (Matcher.State Animation.State) -> (Grid, Int) -> (Grid, Int)
stackRecursive_ depth node (grid, colStart) =
    let
        text = Math.getName node
        width = (toFloat (String.length text)) * textWidth_ + (1 - textWidth_)
        insertItem colEnd dict =
            GridItem
            (Math.getState node |> Matcher.getState |> .prevID)
            text colStart colEnd depth (depth + 1) Nothing
            |> \item -> Dict.insert (Math.getState node |> Matcher.getID) item dict
    in
        case Math.getChildren node of
            [] ->
                let
                    prevWidth = getColX colStart grid.lines
                    colEnd = colStart + 1
                in
                (   { grid
                    | items = insertItem colEnd grid.items
                    , lines = Array.push (width + prevWidth) grid.lines
                    }
                ,   colEnd
                )
            children -> List.foldl (\elem ((input, prevIndex), list) -> let (cGrid, cEnd) = stackRecursive_ (depth+1) elem (input, prevIndex) in
                        ((cGrid, cEnd), (prevIndex, cEnd) :: list)
                    ) ((grid, colStart), []) children
                |> \((childrenGrid, colEnd), revRange) -> let childrenWidth = getColX colEnd childrenGrid.lines in
                    (   {   childrenGrid
                        |   items =
                                let
                                    added = insertItem colEnd childrenGrid.items
                                    updateChildren allChildren = let childRanges = List.reverse revRange in
                                        List.foldl (\child (dict, index) -> let num = Math.getState child |> Matcher.getID in
                                            (   Dict.update num (Maybe.map (\entry -> {entry | draggable = Just (index, childRanges)})) dict
                                            ,   index + 1
                                            )
                                        ) (added, 0) allChildren
                                        |> Tuple.first
                                in
                                case node of
                                    Math.BinaryNode n -> if n.commutative then updateChildren n.children else added
                                    Math.DeclarativeNode n -> updateChildren n.children -- Assume it's commutative for now (until we introduce inequalities)
                                    _ -> added
                        ,   lines = if width <= childrenWidth then childrenGrid.lines
                                -- if the parent is wider than all its children, then expand all contained columns to fit inside parent
                                else Array.indexedMap (\col line ->
                                        if col >= colStart
                                        then line * (width / childrenWidth)
                                        else line
                                    ) childrenGrid.lines
                        }
                    ,   colEnd
                    )
