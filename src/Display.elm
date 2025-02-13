module Display exposing (
    Model, Event(..), init, update, view,
    addEquation, updateEquation, listEquations
    )

import Dict
import Math
import Html exposing (Html, div, p, text)

type alias TraversalMap_ = Dict.Dict Int (List Int) -- Each NodeID will have a "child-index path"
type alias Equation_ = (Math.Tree State, TraversalMap_)

type alias Model =
    {   equations: Dict.Dict Int Equation_
    ,   nextEquationNum: Int
    -- runtime, don't need to store into query
    ,   selected: Maybe (Int, List Int)
    ,   createModeForEquation: Maybe Int
    }

type Event =
    Select Int Int
    | Unselect
    | Reselect Int Int

type alias State =
    {   position: (Float, Float)
    ,   id: Int
    }

init: List (Math.Tree ()) -> Model
init =
    List.foldl addEquation
    {   equations = Dict.empty
    ,   selected = Nothing
    ,   nextEquationNum = 0
    ,   createModeForEquation = Nothing
    }

addEquation: Math.Tree () -> Model -> Model
addEquation tree model = let (_, equation) = processNode_ 0 0 Dict.empty tree in
    {   model
    |   nextEquationNum = model.nextEquationNum + 1
    ,   equations = Dict.insert model.nextEquationNum equation model.equations
    }

updateEquation: Math.Tree () -> Model -> Model
updateEquation _ model = model -- TODO: Not gotten to this yet

listEquations: Model -> Dict.Dict Int (Math.Tree State)
listEquations model = Dict.map (\_ (tree, _) -> tree) model.equations

update: Event -> Model -> (Model, Cmd Event)
update event model = (model, Cmd.none)

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view _ attr model = div (attr ++ [])
    (   Dict.foldl (\_ (elem, _) result -> (p [] [text (Math.toString elem)]) :: result) [] model.equations
    )

{-
TREE PROCESSING! Surprising harder than I thought
-}

type alias ParentNode_ child = State -> child -> Math.Tree State
type alias NodeProcessor_ child = Int -> Int -> TraversalMap_ -> child -> (Int, (child, TraversalMap_))

-- Return is the exclusive upper range of the children ID + new node
processNode_: Int -> Int -> TraversalMap_ -> Math.Tree () -> (Int, Equation_)
processNode_ parentIndex start map root = case root of
    Math.Add _ children -> processChildren_ (0,1) start map children
        |> finalizeNode_ Math.Add parentIndex start
    Math.Multiply _ children -> processChildren_ (0,1) start map children
        |> finalizeNode_ Math.Multiply parentIndex start
    Math.Equal _ children -> processChildren_ (0,1) start map children
        |> finalizeNode_ Math.Equal parentIndex start
    Math.Function _ properties -> processFunctionProperties_ start map properties
        |>  finalizeNode_ Math.Function parentIndex start
    Math.Negative _ child -> processNode_ 0 start map child
        |> finalizeNode_ Math.Negative parentIndex start
    Math.Reciprocal _ child -> processNode_ 0 start map child
        |> finalizeNode_ Math.Negative parentIndex start
    Math.Collapsed _ child -> processNode_ 0 start map child
        |> finalizeNode_ Math.Negative parentIndex start 
    Math.Variable _ name ->
        finalizeNode_ Math.Variable parentIndex start (start, (name, map))
    Math.Real _ val ->
        finalizeNode_ Math.Real parentIndex start (start, (val, map))

insertParentIndex_: Int -> Int -> Int -> TraversalMap_ -> TraversalMap_
insertParentIndex_ start end index map = if start == end then map
    else map
        |> Dict.update start 
            (   \value -> case value of
                    Just a -> Just (index :: a)
                    Nothing -> Just [index]
            )
        |> insertParentIndex_ (start+1) end index

finalizeNode_: ParentNode_ child -> Int -> Int -> (Int, (child, TraversalMap_)) -> (Int, Equation_)
finalizeNode_ nodeGen parentIndex start (end, (c, map)) =
    (end + 1, (nodeGen {position = (0,0), id = end} c, insertParentIndex_ start (end+1) parentIndex map))

processChildren_: (Int, Int) -> Int -> TraversalMap_ -> List (Math.Tree ()) -> (Int, (List (Math.Tree State), TraversalMap_))
processChildren_ (indexStart, indexIncr) start map children = children
    |> List.foldl (\input ((newStart, index), (list, newMap)) ->
        let
            (end, (newChild, finalMap)) = processNode_ index newStart newMap input
        in
            ((end, index + indexIncr), (list ++ [newChild], finalMap))
    )
    ((start, indexStart), ([], map))
    |> (\((end,_),result) -> (end, result))

-- Function Parameters (rather than args) are listed as negative indexes starting at -1
processFunctionProperties_: Int -> TraversalMap_ -> Math.Properties () -> (Int, (Math.Properties State, TraversalMap_))
processFunctionProperties_ start map properties =
    let
        (newStart, (newArgs, newMap)) = processChildren_ (0,1) start map properties.args
        (end, (newParams, finalMap)) = processChildren_ (-1,-1) newStart newMap properties.parameters
    in
        (   newStart
        ,   (
                {   name = properties.name
                ,   args = newArgs
                ,   parameters = newParams
                ,   unary = properties.unary
                ,   associative = properties.associative
                ,   commutative = properties.commutative
                ,   linear = properties.linear
                }
            , finalMap
            )
        )