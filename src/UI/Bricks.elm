module UI.Bricks exposing (
    Model,
    init, advanceTime, updateTree, view
    )

import Array
import Dict
import Html exposing (Html)
import Set
-- Ours
import Algo.Math as Math
import Algo.Matcher as Matcher
import Components.Latex as Latex
import Components.Rules as Rules
import UI.Animation as Animation
import UI.BrickSvg as BrickSvg
import UI.MathIcon as MathIcon

type alias Rect =
    {   text: MathIcon.Model
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
                {   rect
                |   bottomLeft = Animation.advance millis rect.bottomLeft
                ,   topRight = Animation.advance millis rect.topRight
                ,   opacity = Animation.advance millis rect.opacity
                ,   text = MathIcon.advanceTime millis rect.text
                }
            )
        newViewBox = model.viewBox |> Animation.advance millis
    in
        { model | rects = newRects, viewBox = newViewBox }

view: (Int -> Maybe (Int, List Float) -> List (Html.Attribute e)) -> Model -> Html e
view createAttrs model =
    -- TODO: allow blocks to share a border to make it look like mitosis
    -- TODO: can pass in some extra params here to allow dragging to move the node with cursor
    let
        -- we want ids that exist to be drawn on top, so prepend visible bricks to the resulting list first
        visBricks = model.rects |> Dict.foldl (foldRectToBrick createAttrs True) []
        bricks = model.rects |> Dict.foldl (foldRectToBrick createAttrs False) visBricks
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
        -- note that this can be a one-to-many mapping and this current logic just chooses a random last one
        nextIDs = oldRects |> Dict.foldl (\id rect foldMap -> foldMap |> Dict.insert rect.prevID id) Dict.empty

        (visibleRects, newAnimation, easedIds) = grid.items |> Dict.foldl (\id item (foldRects, a0, foldEased) ->
            let
                blTarget = (getColX item.colStart grid.lines, toFloat item.rowStart)
                trTarget = (getColX item.colEnd grid.lines, toFloat item.rowEnd)

                nextID = nextIDs |> Dict.get id |> Maybe.withDefault -1
                thisOld = oldRects |> Dict.get id
                prevOld = oldRects |> Dict.get item.prevID
                nextOld = oldRects |> Dict.get nextID

                -- TODO: handle ctrl+z better because currently prevID always has priority over nextID
                (old, easedId, a1) = case (thisOld, prevOld, nextOld) of
                    (Just o, _, _) ->  let (text, newA) = MathIcon.set a0 (Just item.text.scale) item.text.frame o.text in
                        ({o | text = text}, id, newA)
                    (Nothing, Just o, _) -> let (text, newA) = MathIcon.set a0 (Just item.text.scale) item.text.frame o.text in
                        ({o | text = text}, item.prevID, newA)
                    (Nothing, Nothing, Just o) -> let (text, newA) = MathIcon.set a0 (Just item.text.scale) item.text.frame o.text in
                        ({o | text = text}, nextID, newA)
                    (Nothing, Nothing, Nothing) -> let (text, newA) = MathIcon.init a0 (Just item.text.scale) item.text.frame in
                        (   Rect text id True
                            (Animation.newEaseVector2 smoothTime_ blTarget)
                            (Animation.newEaseVector2 smoothTime_ trTarget)
                            (Animation.newEaseFloat (smoothTime_/2) 0)
                            Nothing
                        ,   -1
                        ,   newA
                        )

                (bl, a2) = Animation.setEase a1 blTarget old.bottomLeft
                (tr, a3) = Animation.setEase a2 trTarget old.topRight
                (op, a4) = Animation.setEase a3 1 old.opacity
                new = (
                    {   old
                    |   prevID = item.prevID
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
                (Dict.insert id new foldRects, a4, Set.insert easedId foldEased)
            ) (Dict.empty, animation, Set.empty)

        -- need to keep deleted nodes in order to animate them away
        (leavingRects, finalA) = oldRects |> Dict.foldl (\id rect (foldRects, a) ->
            if Dict.member id foldRects || Set.member id easedIds
            then (foldRects, a)
            else
                let
                    (op, a1) = Animation.setEase a 0 rect.opacity
                    leavingRect = { rect | visible = False, opacity = op }
                in
                    (Dict.insert id leavingRect foldRects, a1)
            ) (visibleRects, newAnimation)

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
    ,   text: {frame: Latex.Model (Matcher.State Animation.State), scale: Float}
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

textWidth_: Float
textWidth_ = 0.5

stackRecursive_: Int -> Math.Tree (Matcher.State Animation.State) -> (Grid, Int) -> (Grid, Int)
stackRecursive_ depth node (grid, colStart) =
    let
        latex = Rules.toSymbol .function node
        textFrame = MathIcon.latexToFrames latex
        textHeight = (Tuple.second textFrame.botRight) - (Tuple.second textFrame.topLeft)
        textScale = textWidth_ / textHeight
        width = (Tuple.first textFrame.botRight)*textScale + 2*textWidth_ -- Add a char's width on either side
        insertItem colEnd dict =
            GridItem
            (Math.getState node |> Matcher.getState |> .prevID)
            {frame = latex, scale = textScale} colStart colEnd depth (depth + 1) Nothing
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
