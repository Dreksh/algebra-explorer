module Algo.BFS exposing (Change(..), minDiff)

import Set

-- Like an iterator for the inputs
type alias Next group individual = group -> (group, Maybe individual)
type alias Equal individual = individual -> individual -> Bool

minDiff: Equal individual -> Next group individual -> group -> group -> List (Change individual)
minDiff equal next newIt oldIt =
    let
        for seen queue = case queue of
            [] -> []
            (x::nextQueue) -> case minDiff_ equal next seen x of
                Nothing -> x.changes
                Just (newSeen,nextCheck) -> for newSeen (nextQueue ++ nextCheck)
    in
        for Set.empty [{newIt = newIt, oldIt = oldIt, new = 0, old = 0, changes = []}]
        |> List.reverse -- changes are prepended, so the ordered is flipped

type alias DiffState group elem =
    {   newIt: group
    ,   oldIt: group
    ,   new: Int
    ,   old: Int
    ,   changes: List (Change elem)
    }
type Change elem =
    Add elem
    | Delete elem
    | None elem elem -- new then old

-- Based off of Myer's diff algorithm
minDiff_: Equal individual -> Next group individual -> Set.Set (Int, Int) -> DiffState group individual -> Maybe (Set.Set (Int, Int), List (DiffState group individual))
minDiff_ equal next seen current =
    let
        (newIt, newVal) = next current.newIt
        (oldIt, oldVal) = next current.oldIt
    in
    (
        case (newVal, oldVal) of
            (Nothing, Nothing) -> Nothing
            (Just val, Nothing) -> Just [{current | newIt = newIt, new = current.new + 1, changes = Add val :: current.changes}]
            (Nothing, Just val) -> Just [{current | oldIt = oldIt, old = current.old + 1, changes = Delete val :: current.changes}]
            (Just left, Just right) ->
                let
                    -- prefer add over delete
                    list =  [    {current | newIt = newIt, new = current.new + 1, changes = Add left :: current.changes}
                            ,   {current | oldIt = oldIt, old = current.old + 1, changes = Delete right :: current.changes}
                            ]
                in
                    if equal left right
                    -- Diagonal is prefixed, as we want to explore these first
                    then Just ({current | newIt = newIt, new = current.new + 1, oldIt = oldIt, old = current.old + 1, changes = None left right :: current.changes } :: list)
                    else Just list
    )
    |> Maybe.map (List.foldr (\elem (set, list) ->
            if Set.member (elem.new, elem.old) seen then (set, list)
            else (Set.insert (elem.new, elem.old) set, elem :: list)
        ) (seen, [])
    )

