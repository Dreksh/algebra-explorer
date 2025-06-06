module UI.Bricks exposing (..)

import Html exposing (Html, div, text)
import Dict
import Set
import Time
import Animator

import Algo.Matcher as Matcher

type alias Rect =
    {   x: Float
    ,   y: Float
    ,   width: Float
    ,   height: Float
    }

-- TODO: make falling blocks drop like gravity, and rising blocks pushed by new ones below
--   This actually probably requires not using Animator anymore

type alias Model =
    {   bricks: Animator.Timeline (Dict.Dict Int Rect, Dict.Dict Int Rect)
    }

-- TODO: either pass this Tick into Animator.update
type Event s
    = Tick Time.Posix
    | Equation (Matcher.Equation s)

-- TODO: need to take in a Matcher.Equation Display.State
init : Matcher.Equation s -> Model
init eq = Model (Animator.init (Dict.empty, Dict.empty))

-- -- TODO: Display.State has prevID and Matcher.State has id, use these to update prev and current positions
-- update

-- -- TODO: tween between bricks and prevBricks for two position keyframes
-- --         but this is strange, need to work out how to tween from prev
view : Model -> Int -> Set.Set Int -> (Int -> Int -> Bool -> e) -> Html e
view bricks eqNum highlight onClick = div [] [ text "aloha" ]

-- the library code says "You likely only need one animator for a given project."
animator : Animator.Animator Model
animator =
    Animator.animator  -- this is an 'empty' animator that exists only to chain with |>
        |> Animator.watching .bricks (\newBricks model -> { model | bricks = newBricks })


-- stackedView_: Int -> Set.Set Int -> Math.Tree (Matcher.State State) -> Html Event
-- stackedView_ eq highlight root =
--     let
--         (maxWidth, maxDepth, blocks) = stackRecursive eq highlight 0 1 root
--     in
--         div [] [ Block.blocks maxWidth maxDepth blocks ]

-- stackRecursive: Int -> Set.Set Int -> Int -> Int -> Math.Tree (Matcher.State State) -> (Int, Int, List (Html Event))
-- stackRecursive eq highlight minWidth minDepth node =
--     let
--         children = Math.getChildren node
--         id = Math.getState node |> Matcher.getID
--         (maxWidth, maxDepth, childBlocks) =
--             if List.isEmpty children
--             then (minWidth+1, minDepth, [])
--             else
--                 children
--                 |>  List.foldl (\child (foldWidth, foldDepth, foldDivs) ->
--                     let (w, d, divs) = stackRecursive eq highlight foldWidth (minDepth+1) child
--                     in (w, max foldDepth d, foldDivs ++ divs)
--                 ) (minWidth, minDepth, [])
--         onClick = HtmlEvent.onShiftClick (Select eq id)
--         selected = (Set.member id highlight)
--     in
--         (   maxWidth
--         ,   maxDepth
--         ,   ( Block.block minWidth maxWidth minDepth maxDepth selected onClick (Math.getName node)) :: childBlocks
--         )
