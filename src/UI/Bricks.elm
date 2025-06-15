module UI.Bricks exposing (
    Model,
    init, advanceTime, updateTree, view
    )

import Html exposing (Html)
import Dict
import Set
import Array

import Algo.Math as Math
import UI.BrickSvg as BrickSvg
import UI.HtmlEvent as HtmlEvent

-- taken from Unity
-- https://github.com/Unity-Technologies/UnityCsReference/blob/4b463aa72c78ec7490b7f03176bd012399881768/Runtime/Export/Math/Vector2.cs#L289
type alias Vector2 =
    {   x: Float
    ,   y: Float
    }

-- note that this doesn't have maxSpeed unlike the Unity original
smoothDamp2: Vector2 -> Vector2 -> Vector2 -> Float -> Float -> (Vector2, Vector2)
smoothDamp2 current target velocity smoothTime deltaTime =
    let
        omega = 2 / smoothTime

        x = omega * deltaTime
        exp = 1 / (1 + x + 0.48 * x * x + 0.235 * x * x * x)

        change_x = current.x - target.x
        change_y = current.y - target.y

        newTarget = Vector2 (current.x - change_x) (current.y - change_y)

        temp_x = (velocity.x + omega * change_x) * deltaTime
        temp_y = (velocity.y + omega * change_y) * deltaTime

        output_x = newTarget.x + (change_x + temp_x) * exp
        output_y = newTarget.y + (change_y + temp_y) * exp

        origMinusCurrent_x = target.x - current.x
        origMinusCurrent_y = target.y - current.y
        outMinusOrig_x = output_x - target.x
        outMinusOrig_y = output_y - target.y

        stop = origMinusCurrent_x * outMinusOrig_x + origMinusCurrent_y * outMinusOrig_y > 0
        newPosition = if stop
            then target
            else Vector2 output_x output_y
        newVelocity = if stop
            then Vector2 0 0
            else Vector2 ((velocity.x - omega * temp_x) * exp) ((velocity.y - omega * temp_y) * exp)
    in
        (newPosition, newVelocity)

smoothDamp: Float -> Float -> Float -> Float -> Float -> (Float, Float)
smoothDamp current target velocity smoothTime deltaTime =
    let
        omega = 2 / smoothTime

        x = omega * deltaTime
        exp = 1 / (1 + x + 0.48 * x * x + 0.235 * x * x * x)

        change = current - target

        newTarget = current - change

        temp = (velocity + omega * change) * deltaTime

        output = newTarget + (change + temp) * exp

        origMinusCurrent = target - current
        outMinusOrig = output - target

        stop = origMinusCurrent * outMinusOrig > 0
        newPosition = if stop
            then target
            else output
        newVelocity = if stop
            then 0
            else ((velocity - omega * temp) * exp)
    in
        (newPosition, newVelocity)

type alias Rect =
    {   text: String
    ,   canHover: Bool
    ,   currentPos0: Vector2
    ,   targetPos0: Vector2
    ,   velocity0: Vector2
    ,   currentPos1: Vector2
    ,   targetPos1: Vector2
    ,   velocity1: Vector2
    ,   currentOpacity: Float
    ,   targetOpacity: Float
    ,   velocipacity: Float
    }

zeroRect: Rect
zeroRect = let zero = Vector2 0 0 in
    Rect "" False zero zero zero zero zero zero 0 0 0

type alias Model s =
    {   rects: Dict.Dict Int Rect
    ,   getID: (Math.Tree s) -> Int
    ,   getPrevID: (Math.Tree s) -> Int
    }

init: Math.Tree s -> (Math.Tree s -> Int) -> (Math.Tree s -> Int) -> Model s
init root getID getPrevID =
    let
        emptyModel = Model Dict.empty getID getPrevID
    in
        updateTree root emptyModel

advanceTime: Float -> Model s -> Model s
advanceTime millis model =
    let
        newRects = model.rects |> Dict.map (\_ rect ->
            let
                (newPos0, newVelocity0) = smoothDamp2 rect.currentPos0 rect.targetPos0 rect.velocity0 200 millis
                (newPos1, newVelocity1) = smoothDamp2 rect.currentPos1 rect.targetPos1 rect.velocity1 200 millis
                (newOpacity, newVelocipacity) = smoothDamp rect.currentOpacity rect.targetOpacity rect.velocipacity 200 millis
            in
                {   rect
                |   currentPos0 = newPos0
                ,   velocity0 = newVelocity0
                ,   currentPos1 = newPos1
                ,   velocity1 = newVelocity1
                ,   currentOpacity = newOpacity
                ,   velocipacity = newVelocipacity
                }
            )
    in
        { model | rects = newRects }

view: Int -> Set.Set Int -> (Int -> Int -> Bool -> e) -> Model s -> Html e
view eq highlight onShiftClick model =
    let
        (bricks, maxX, maxY) = model.rects |> Dict.foldl (\id rect (foldSvgs, foldX, foldY) ->
            let
                onClick = HtmlEvent.onShiftClick (onShiftClick eq id)
                selected = highlight |> Set.member id
                brick = BrickSvg.brick
                    rect.currentPos0.x
                    rect.currentPos1.x
                    rect.currentPos0.y
                    rect.currentPos1.y
                    rect.currentOpacity
                    rect.canHover
                    selected
                    onClick
                    rect.text
            in
                (brick :: foldSvgs, max foldX rect.currentPos1.x, max foldY rect.currentPos1.y)
            ) ([], 0, 0)
    in
        BrickSvg.bricks maxX maxY bricks


-- TODO: make falling bricks drop like gravity, and rising bricks pushed by new ones below
updateTree: Math.Tree s -> Model s -> Model s
updateTree root model =
    let
        emptyGrid = (Grid Dict.empty (Array.initialize 1 (\_ -> 0)))
        grid = stackRecursive_ 0 emptyGrid model.getID model.getPrevID root

        newRects = grid.items |> Dict.foldl (\id item foldRects ->
            let
                pos0 = Vector2 (grid.lines |> getLine item.colStart) (toFloat item.rowStart)
                pos1 = Vector2 (grid.lines |> getLine item.colEnd) (toFloat item.rowEnd)
                --  TODO: use prevID more correctly
                --    also simply fade in instead of defaulting to zeroRect
                thisOld = foldRects |> Dict.get id
                prevOld = foldRects |> Dict.get item.prevID
                old = prevOld |> Maybe.withDefault (thisOld |> Maybe.withDefault zeroRect)
                new = (
                    {   old
                    |   text = item.text
                    ,   canHover = True
                    ,   targetPos0 = pos0
                    ,   targetPos1 = pos1
                    ,   targetOpacity = 1
                    })
            in
                foldRects |> Dict.insert id new
            ) model.rects

        -- need to keep deleted nodes in order to animate them away
        goneRects = newRects |> Dict.map (\id rect ->
            if grid.items |> Dict.member id
            then rect
            else { rect | canHover = False, targetOpacity = 0 }
            )
    in
        { model | rects = goneRects }
        -- TODO: fade away rects that no longer appear (probably use opacity for this)


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
    lines |> Array.get col |> Maybe.withDefault 0

-- in relation to block width
-- TODO: measure exactly how wide it is
textWidth_: Float
textWidth_ = 0.5

stackRecursive_: Int -> Grid -> (Math.Tree s -> Int) -> (Math.Tree s -> Int) -> Math.Tree s -> Grid
stackRecursive_ depth grid getID getPrevID node =
    let
        id = getID node
        prevID = getPrevID node
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
                    List.foldl (\child foldGrid ->
                        child |> stackRecursive_ (depth+1) foldGrid getID getPrevID
                    ) grid
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
