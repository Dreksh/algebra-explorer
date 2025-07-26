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
    ,   commutable: Maybe (Int, List Float)  -- sibling order, siblings midpoints
    }

type alias Model =
    {   rects: Dict.Dict (Int, Int) Rect  -- the key here needs to be two Ints because DeclarativeNodes map to multiple rects
    ,   viewBox: Animation.EaseState Animation.Vector2
    }

smoothTime_: Float
smoothTime_ = 750

splitDeclarativeNodesFlag_: Bool
splitDeclarativeNodesFlag_ = True

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
        -- we want ids that exist to be drawn on top, so make sure not visible ones are drawn first
        sortedRects = model.rects |> Dict.toList |> List.sortWith (\((id, id2), irect) ((jd, jd2), jrect) ->
            case (irect.visible, jrect.visible) of
                (False, False) -> EQ
                (True, False) -> GT
                (False, True) -> LT
                (True, True) -> compare (id, id2) (jd, jd2)
                -- DeclarativeNodes should have low ids so this should place them behind
            )

        bricks = sortedRects |> List.map (\((id, _), rect) ->
            let
                attrs = createAttrs id rect.commutable
                (blX, blY) = Animation.current rect.bottomLeft
                (trX, trY) = Animation.current rect.topRight
            in
                BrickSvg.brick blX trX blY trY (Animation.current rect.opacity) rect.visible attrs rect.text
            )
        (maxX, maxY) = Animation.current model.viewBox
    in
        BrickSvg.bricks maxX maxY bricks

-- TODO: make falling bricks drop like gravity, and rising bricks pushed by new ones below
updateTree: Animation.Tracker -> Math.Tree (Matcher.State Animation.State) -> Model -> (Model, Animation.Tracker)
updateTree tracker root model =
    let
        (newRect, newViewBox, newT) = calculateTree_ tracker root model.rects
        (finalViewBox, t1) = Animation.setEase newT newViewBox model.viewBox
    in

        ({model | rects = newRect, viewBox = finalViewBox}, t1)

calculateTree_: Animation.Tracker -> Math.Tree (Matcher.State Animation.State) -> Dict.Dict (Int, Int) Rect -> (Dict.Dict (Int, Int) Rect, Animation.Vector2, Animation.Tracker)
calculateTree_ animation root rects =
    let
        (grid, _) = stackRecursive_ 0 root ({items = Dict.empty, lines = Array.empty}, -1)
        items = Dict.foldl expandDeclarativeItems Dict.empty grid.items

        -- don't tween if they aren't visible
        oldRects = rects |> Dict.filter (\_ rect -> rect.visible)

        -- if we are doing an undo then we can try to reverse-engineer where it came from
        -- note that this can be a one-to-many mapping and this current logic just chooses a random last one
        nextIDs = oldRects |> Dict.foldl (\(id, id2) rect foldMap -> foldMap |> Dict.insert (rect.prevID, id2) (id, id2)) Dict.empty

        (visibleRects, newAnimation, easedIds) = items |> Dict.foldl (\(id, id2) item (foldRects, a0, foldEased) ->
            let
                (offset, opTarget) = case (splitDeclarativeNodesFlag_, item.declarative) of
                    (True, Just _) -> ((0, 0.5), 0.4)  -- move DeclarativeNodes up a bit to differentiate them from children
                    _ -> ((0, 0), 1)

                blTarget = Animation.addVector2 (getColX item.colStart grid.lines, toFloat item.rowStart) offset
                trTarget = Animation.addVector2 (getColX item.colEnd grid.lines, toFloat item.rowEnd) offset

                nextID = nextIDs |> Dict.get (id, id2) |> Maybe.withDefault (-1, -1)
                thisOld = oldRects |> Dict.get (id, id2)
                prevOld = oldRects |> Dict.get (item.prevID, id2)
                nextOld = oldRects |> Dict.get nextID

                -- TODO: handle ctrl+z better because currently prevID always has priority over nextID
                (old, easedId, a1) = case (thisOld, prevOld, nextOld) of
                    (Just o, _, _) ->  let (text, newA) = MathIcon.set a0 (Just item.text.scale) item.text.frame o.text in
                        ({o | text = text}, (id, id2), newA)
                    (Nothing, Just o, _) -> let (text, newA) = MathIcon.set a0 (Just item.text.scale) item.text.frame o.text in
                        ({o | text = text}, (item.prevID, id2), newA)
                    (Nothing, Nothing, Just o) -> let (text, newA) = MathIcon.set a0 (Just item.text.scale) item.text.frame o.text in
                        ({o | text = text}, nextID, newA)
                    (Nothing, Nothing, Nothing) -> let (text, newA) = MathIcon.init a0 (Just item.text.scale) item.text.frame in
                        (   Rect text id True
                            (Animation.newEaseVector2 smoothTime_ blTarget)
                            (Animation.newEaseVector2 smoothTime_ trTarget)
                            (Animation.newEaseFloat (smoothTime_/2) 0)
                            Nothing
                        ,   (-1, -1)
                        ,   newA
                        )

                (bl, a2) = Animation.setEase a1 blTarget old.bottomLeft
                (tr, a3) = Animation.setEase a2 trTarget old.topRight
                (op, a4) = Animation.setEase a3 opTarget old.opacity

                commutable = item.commutable
                    |> Maybe.map (\(index, list) ->
                        (index, List.map (\(sCol,eCol) -> (getColX sCol grid.lines + getColX eCol grid.lines)/2 ) list)
                    )
                new = (
                    {   old
                    |   prevID = item.prevID
                    ,   visible = True
                    ,   bottomLeft = bl
                    ,   topRight = tr
                    ,   opacity = op
                    ,   commutable = commutable
                    })
            in
                (Dict.insert (id, id2) new foldRects, a4, Set.insert easedId foldEased)
            ) (Dict.empty, animation, Set.empty)

        -- need to keep deleted nodes in order to animate them away
        (leavingRects, finalA) = oldRects |> Dict.foldl (\(id, id2) rect (foldRects, a) ->
            if Dict.member (id, id2) foldRects || Set.member (id, id2) easedIds
            then (foldRects, a)
            else
                let
                    (op, a1) = Animation.setEase a 0 rect.opacity
                    leavingRect = { rect | visible = False, opacity = op }
                in
                    (Dict.insert (id, id2) leavingRect foldRects, a1)
            ) (visibleRects, newAnimation)

        viewBox = visibleRects |> Dict.foldl (\_ rect (foldX, foldY) ->
            let
                (trX, trY) = Animation.target rect.topRight
            in
                (max foldX trX, max foldY trY)
            ) (0, 0)
    in
        (leavingRects, viewBox, finalA)


-- this takes a GridItem from a DeclarativeNode and splits it into multiple GridItems to place between each child
-- meant to be used with Dict.foldl
expandDeclarativeItems: Int -> GridItem -> Dict.Dict (Int, Int) GridItem -> Dict.Dict (Int, Int) GridItem
expandDeclarativeItems id item foldDict =
    case (splitDeclarativeNodesFlag_, item.declarative) of
        (True, Just children) -> case List.head children of
            Nothing -> foldDict  -- this should never happen since DeclarativeNode should have a child
            Just head -> (List.foldl
                (\(colStart, colEnd) ((prevColStart, prevColEnd), id2, foldDict2) ->
                    (   (colStart, colEnd)
                    ,   id2 + 1
                    ,   Dict.insert (id, id2) { item | colStart = prevColEnd, colEnd = colStart } foldDict2
                    )
                )
                (head, 0, foldDict)
                (List.tail children |> Maybe.withDefault [])
                )
                |> (\(_, _, expanded) -> expanded)

        _ -> Dict.insert (id, 0) item foldDict


type alias GridItem =
    {   prevID: Int
    ,   text: {frame: Latex.Model (Matcher.State Animation.State), scale: Float}
    ,   colStart: Int
    ,   colEnd: Int
    ,   rowStart: Int
    ,   rowEnd: Int
    ,   commutable: Maybe (Int, List (Int, Int))  -- nth-child index, siblings' (colStart, colEnd)
    ,   declarative: Maybe (List (Int, Int))  -- children's (colStart, colEnd)
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
        insertItem colEnd declarative dict =
            GridItem
            (Math.getState node |> Matcher.getState |> .prevID)
            {frame = latex, scale = textScale} colStart colEnd depth (depth + 1) Nothing declarative
            |> \item -> Dict.insert (Math.getState node |> Matcher.getID) item dict

        appendLine array =
            let
                colEnd = (Array.length array) - 1
                prevWidth = getColX colEnd array
            in Array.push (prevWidth + width) array
    in
        case Math.getChildren node of
            [] ->
                let colEnd = colStart + 1
                in
                (   { grid
                    | items = insertItem colEnd Nothing grid.items
                    , lines = appendLine grid.lines
                    }
                ,   colEnd
                )
            children -> List.foldl (\child ((foldGrid, foldCol), nthChild, list) ->
                let
                    -- declarative nodes are between children so
                    depthOffset = case (splitDeclarativeNodesFlag_, node) of
                        (True, Math.DeclarativeNode _)  -> 0
                        _ -> 1

                    (newGrid, newCol) = stackRecursive_ (depth+depthOffset) child (foldGrid, foldCol)

                    -- declarative nodes need a column of space between each child
                    (newNewGrid, newNewCol) = case (splitDeclarativeNodesFlag_, node, nthChild>0) of
                        (True, Math.DeclarativeNode _, True)  -> ({ newGrid | lines = appendLine newGrid.lines }, newCol+1)
                        _ -> (newGrid, newCol)
                in
                    ((newNewGrid, newNewCol), nthChild-1, (foldCol, newCol) :: list)
                ) ((grid, colStart), (List.length children)-1, []) children
                |> \((childrenGrid, colEnd), _, revRange) ->
                    let
                        lineStart = (getColX colStart childrenGrid.lines)
                        lineEnd = (getColX colEnd childrenGrid.lines)
                        childrenWidth = lineEnd - lineStart
                    in
                    (   {   childrenGrid
                        |   items =
                                let
                                    declarative = case node of
                                        Math.DeclarativeNode _ -> Just (List.reverse revRange)
                                        _ -> Nothing

                                    added = insertItem colEnd declarative childrenGrid.items

                                    updateChildren allChildren = let childRanges = List.reverse revRange in
                                        List.foldl (\child (dict, index) -> let num = Math.getState child |> Matcher.getID in
                                            (   Dict.update num (Maybe.map (\entry -> {entry | commutable = Just (index, childRanges)})) dict
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
                                        if col > colStart
                                        then lineStart + (line - lineStart) * (width / childrenWidth)
                                        else line
                                    ) childrenGrid.lines
                        }
                    ,   colEnd
                    )
