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

splitDeclarativeNodeFlag_: Bool
splitDeclarativeNodeFlag_ = True

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
        -- Dicts are already sorted on the keys by default, and DeclarativeNodes should have low ids
        -- so this should place them earlier in the SVG children list, appearing behind other visible elements
        (visible, invisible) = model.rects |> Dict.toList |> List.partition (\(_, rect) -> rect.visible)

        bricks = invisible ++ visible |> List.map (\((id, _), rect) ->
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
        grid = stack root
        (declID, items) = distributeDeclarative grid

        -- don't tween if they aren't visible
        oldRects = rects |> Dict.filter (\_ rect -> rect.visible)

        -- if we are doing an undo then we can try to reverse-engineer where it came from
        -- note that this can be a one-to-many mapping and this current logic just chooses a random last one
        nextIDs = oldRects |> Dict.foldl (\(id, id2) rect foldMap -> foldMap |> Dict.insert (rect.prevID, id2) (id, id2)) Dict.empty

        (visibleRects, newAnimation, easedIds) = items |> Dict.foldl (\(id, id2) item (foldRects, a0, foldEased) ->
            let
                (offset, opTarget) = if id == declID
                    then ((0, 0.5), 0.4)  -- move DeclarativeNodes up a bit to differentiate them from children
                    else ((0, 0), 1)

                blTarget = Animation.addVector2 (getColX item.colStart grid.lines, toFloat item.rowStart) offset
                trTarget = Animation.addVector2 (getColX item.colEnd grid.lines, toFloat item.rowEnd) offset

                nextID = nextIDs |> Dict.get (id, id2) |> Maybe.withDefault (-1, -1)
                thisOld = oldRects |> Dict.get (id, id2)
                prevOld = oldRects |> Dict.get (item.prevID, id2)
                nextOld = oldRects |> Dict.get nextID

                -- TODO: handle ctrl+z better because currently prevID always has priority over nextID
                --   maybe we should just not ease anything when swapping between non-adjacent equations
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


-- this takes a GridItem from a DeclarativeNode and splits it into multiple GridItems to place between each child
-- returns the index of the DeclarativeNode and a new Dict of items with compound keys
distributeDeclarative: Grid -> (Int, Dict.Dict (Int, Int) GridItem)
distributeDeclarative grid =
    let
        nonDeclItems = grid.items |> Dict.foldl (\id item foldDict -> foldDict |> Dict.insert (id, 0) item) Dict.empty
    in case grid.declarative of
        Nothing -> (-1, nonDeclItems)
        Just declItem -> case declItem.commutable of
            Nothing -> (-1, nonDeclItems)  -- this should never happen since we should always set the commutable field
            Just (id, ranges) -> case List.head ranges of
                Nothing -> (-1, nonDeclItems)  -- this should never happen since DeclarativeNode always needs children
                Just head -> (List.tail ranges |> Maybe.withDefault []) |> List.foldl
                    (\(colStart, colEnd) ((prevColStart, prevColEnd), id2, foldItems) ->
                        (   (colStart, colEnd)
                        ,   id2 + 1
                        ,   foldItems |> Dict.insert (id, id2) { declItem | colStart = prevColEnd, colEnd = colStart, commutable = Nothing }
                        )
                    )
                    (head, 0, nonDeclItems)
                    |> (\(_, _, items) -> (id, items))

type alias GridText = {frame: Latex.Model (Matcher.State Animation.State), scale: Float}

type alias GridItem =
    {   prevID: Int
    ,   text: GridText
    ,   colStart: Int
    ,   colEnd: Int
    ,   rowStart: Int
    ,   rowEnd: Int
    ,   commutable: Maybe (Int, List (Int, Int))  -- nth-child index, siblings' (colStart, colEnd)
    }

type alias Grid =
    {   items: Dict.Dict Int GridItem
    ,   lines: Array.Array Float  -- only need column lines because children only nest in the x-axis
    ,   declarative: Maybe GridItem  -- for DeclarativeNode, reuses .commutable as child ranges
    }

-- getColX returns the x-component of the column's right-most point
-- the -1 input represents the left-most point (the left of the 0th column)
getColX: Int -> Array.Array Float -> Float
getColX col = Array.get col >> Maybe.withDefault 0

appendLine: Float -> Array.Array Float -> Array.Array Float
appendLine width array =
    let
        colEnd = (Array.length array) - 1
        prevWidth = getColX colEnd array
    in Array.push (prevWidth + width) array

textWidth_: Float
textWidth_ = 0.5

extractText_: Math.Tree (Matcher.State Animation.State) -> (GridText, Float)
extractText_ node =
    let
        latex = Rules.toSymbol .function node
        textFrame = MathIcon.latexToFrames latex
        textHeight = (Tuple.second textFrame.botRight) - (Tuple.second textFrame.topLeft)
        textScale = textWidth_ / textHeight
        width = (Tuple.first textFrame.botRight)*textScale + 2*textWidth_ -- Add a char's width on either side
    in
        ({frame = latex, scale = textScale}, width)

stack: Math.Tree (Matcher.State Animation.State) -> Grid
stack root = let initialGrid = (Grid Dict.empty Array.empty Nothing, -1) in
    case (splitDeclarativeNodeFlag_, root) of
        (True, Math.DeclarativeNode n) -> let (text, width) = extractText_ root in
            n.children |> List.foldl (\child ((foldGrid, foldColEnd), nthChild, ranges) ->
                let
                    -- DeclarativeNode should only ever be the root
                    (childGrid, childColEnd) = stackRecursive_ 0 child (foldGrid, foldColEnd)

                    -- declarative nodes need a column of space between each child
                    -- if condition prevents one dangling off the right side
                    filledGrid = if nthChild < (List.length n.children) - 1
                        then { childGrid | lines = appendLine width childGrid.lines }
                        else childGrid

                    -- need ranges of children to determine where to place in-between nodes
                    range = (foldColEnd, childColEnd)
                in
                    ((filledGrid, childColEnd+1), nthChild+1, range :: ranges)
                ) (initialGrid, 0, [])
            |> (\((grid, colEnd), _, revRange) ->
                let
                    childRanges = List.reverse revRange
                    newItems = updateCommutativeChildren grid.items childRanges n.children
                    declarative = Just
                        (   GridItem
                            (n.state |> Matcher.getState |> .prevID)
                            text 0 (colEnd-1) 0 1
                            (Just (n.state |> Matcher.getID, childRanges))
                            -- reuse the commutable field because it fits our use case perfectly
                        )
                in
                    { grid | items = newItems, declarative = declarative }
                )
        _ -> stackRecursive_ 0 root initialGrid |> Tuple.first

updateCommutativeChildren: Dict.Dict Int GridItem -> List (Int, Int) -> List (Math.Tree (Matcher.State Animation.State)) -> Dict.Dict Int GridItem
updateCommutativeChildren items siblingRanges children =
    children |> List.foldl (\child (foldGrid, nthChild) ->
        let childId = Math.getState child |> Matcher.getID
        in  (   Dict.update childId (Maybe.map (\entry -> {entry | commutable = Just (nthChild, siblingRanges)})) foldGrid
            ,   nthChild + 1
            )
    ) (items, 0)
    |> Tuple.first

stackRecursive_: Int -> Math.Tree (Matcher.State Animation.State) -> (Grid, Int) -> (Grid, Int)
stackRecursive_ depth node (grid, colStart) =
    let
        (text, width) = extractText_ node
        insertItem colEnd dict =
            GridItem
            (Math.getState node |> Matcher.getState |> .prevID)
            text colStart colEnd depth (depth + 1) Nothing
            |> \item -> Dict.insert (Math.getState node |> Matcher.getID) item dict
    in
        case Math.getChildren node of
            [] ->
                let colEnd = colStart + 1
                in
                (   { grid
                    | items = insertItem colEnd grid.items
                    , lines = appendLine width grid.lines
                    }
                ,   colEnd
                )
            children -> List.foldl (\elem ((input, prevIndex), list) ->
                    let (cGrid, cEnd) = stackRecursive_ (depth+1) elem (input, prevIndex)
                    in ((cGrid, cEnd), (prevIndex, cEnd) :: list)
                ) ((grid, colStart), []) children
                |> \((childrenGrid, colEnd), revRange) ->
                    (   {   childrenGrid
                        |   items =
                                let
                                    added = insertItem colEnd childrenGrid.items
                                    updateChildren = updateCommutativeChildren added (List.reverse revRange)
                                in case node of
                                    Math.BinaryNode n -> if n.commutative then updateChildren n.children else added
                                    Math.DeclarativeNode n -> updateChildren n.children -- Assume it's commutative for now (until we introduce inequalities)
                                    _ -> added
                        ,   lines =
                                let
                                    childrenXMin = (getColX colStart childrenGrid.lines)
                                    childrenXMax = (getColX colEnd childrenGrid.lines)
                                    childrenWidth = childrenXMax - childrenXMin
                                in
                                    -- if the parent is wider than all its children, then expand all contained columns to fit inside parent
                                    if width <= childrenWidth then childrenGrid.lines
                                    else Array.indexedMap (\col line ->
                                        if col > colStart
                                        then childrenXMin + (line - childrenXMin) * (width / childrenWidth)
                                        else line
                                        ) childrenGrid.lines
                        }
                    ,   colEnd
                    )
