module UI.Bricks exposing (
    Model,
    init, advanceTime, updateTree, view
    )

import Html exposing (Html, div, text)
import Dict
import Set

import Algo.Math as Math
import Algo.Matcher as Matcher

-- taken from Unity
-- https://github.com/Unity-Technologies/UnityCsReference/blob/4b463aa72c78ec7490b7f03176bd012399881768/Runtime/Export/Math/Vector2.cs#L289
type alias Vector2 =
    {   x: Float
    ,   y: Float
    }

-- note that this doesn't have maxSpeed unlike the Unity original
smoothDamp: Vector2 -> Vector2 -> Vector2 -> Float -> Float -> (Vector2, Vector2)
smoothDamp current target velocity smoothTime deltaTime =
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

type alias Rect =
    {   currentPos0: Vector2
    ,   targetPos0: Vector2
    ,   velocity0: Vector2
    ,   currentPos1: Vector2
    ,   targetPos1: Vector2
    ,   velocity1: Vector2
    }

-- TODO: make falling blocks drop like gravity, and rising blocks pushed by new ones below
--   This actually probably requires not using Animator anymore

type alias Model =
    {   bricks: Dict.Dict Int Rect
    }

init: Math.Tree (Matcher.State s) -> Model
init eq =
    let
        (maxWidth, maxDepth, coords) = eq |> stackRecursive 0 1
    in
        Model Dict.empty

advanceTime: Float -> Model -> Model
advanceTime millis model =
    let
        newBricks = model.bricks |> Dict.map (\_ rect ->
            let
                (newPos0, newVelocity0) = smoothDamp rect.currentPos0 rect.targetPos0 rect.velocity0 1 millis
                (newPos1, newVelocity1) = smoothDamp rect.currentPos1 rect.targetPos1 rect.velocity1 1 millis
            in
                {   rect
                |   currentPos0 = newPos0
                ,   velocity0 = newVelocity0
                ,   currentPos1 = newPos1
                ,   velocity1 = newVelocity1
                }
            )
    in
        { model | bricks = newBricks }

updateTree: Math.Tree (Matcher.State s) -> Model -> Model
updateTree root model = model  -- need to keep deleted nodes in order to animate them away

view: Int -> Set.Set Int -> (Int -> Int -> Bool -> e) -> Model -> Html e
view eqNum highlight onShiftClick bricks = div [] [ text "aloha" ]
        -- onClick = HtmlEvent.onShiftClick (Select eq id)
        -- selected = (Set.member id highlight)

-- TODO: fix the width+height of the <svg> itself (maybe to 100%) and then just tween the viewbox to make it zoom in + out
--   maybe don't let the number 1 fill the entire thing lol
-- stackedView_: Int -> Set.Set Int -> Math.Tree (Matcher.State State) -> Html Event
-- stackedView_ eq highlight root =
--     let
--         (maxWidth, maxDepth, blocks) = stackRecursive eq highlight 0 1 root
--     in
--         div [] [ Block.blocks maxWidth maxDepth blocks ]

stackRecursive: Int -> Int -> Math.Tree (Matcher.State s) -> (Int, Int, List (Int, ((Int, Int), (Int, Int))))
stackRecursive minWidth minDepth node =
    let
        children = Math.getChildren node
        (maxWidth, maxDepth, childBlocks) =
            if List.isEmpty children
            then (minWidth+1, minDepth, [])
            else
                children
                |>  List.foldl (\child (foldWidth, foldDepth, foldDivs) ->
                    let (w, d, divs) = child |> stackRecursive foldWidth (minDepth+1)
                    in (w, max foldDepth d, foldDivs ++ divs)
                ) (minWidth, minDepth, [])
    in
        (   maxWidth
        ,   maxDepth
        ,   ((node |> Math.getState |> Matcher.getID), ((minWidth, maxWidth), (minDepth, maxDepth))) :: childBlocks
        )
