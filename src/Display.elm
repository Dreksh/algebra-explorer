module Display exposing (
    Model, Event(..), init, update, view,
    addEquation, updateEquation, listEquations
    )

import Dict
import Math
import Html exposing (Html, div, p, text)

type alias ParentMap_ = (Int, Dict.Dict Int Int) -- Next available ID + mapping of child to parent node, allowing for faster traversal
type alias Equation_ = (Math.Tree State, ParentMap_)

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
addEquation tree model = let equation = processNode_ 0 (1, Dict.empty) tree in
    {   model
    |   nextEquationNum = model.nextEquationNum + 1
    ,   equations = Dict.insert model.nextEquationNum equation model.equations
    }

updateEquation: Int -> Math.Tree () -> Model -> Model
updateEquation _ _ model = model -- TODO: Not gotten to this yet

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

processNode_: Int -> ParentMap_ -> Math.Tree () -> Equation_
processNode_ parentId (id, map) root =
  case root of
    Math.Add _ children -> processChildren_ id (id+1,map) children
        |> finalizeNode_ Math.Add parentId id
    Math.Multiply _ children -> processChildren_ id (id+1,map) children
        |> finalizeNode_ Math.Multiply parentId id
    Math.Equal _ children -> processChildren_ id (id+1,map) children
        |> finalizeNode_ Math.Equal parentId id
    Math.Function _ properties -> processFunctionProperties_ id (id+1,map) properties
        |>  finalizeNode_ Math.Function parentId id
    Math.Negative _ child -> processNode_ id (id+1,map) child
        |> finalizeNode_ Math.Negative parentId id
    Math.Reciprocal _ child -> processNode_ id (id+1,map) child
        |> finalizeNode_ Math.Reciprocal parentId id
    Math.Collapsed _ child -> processNode_ id (id+1,map) child
        |> finalizeNode_ Math.Collapsed parentId id
    Math.Variable _ name ->
        finalizeNode_ Math.Variable parentId id (name, (id+1, map))
    Math.Real _ val ->
        finalizeNode_ Math.Real parentId id (val, (id+1, map))

finalizeNode_: ParentNode_ child -> Int -> Int -> (child, ParentMap_) -> Equation_
finalizeNode_ nodeGen parentId id (c, (nextId, map)) =
    (nodeGen {position = (0,0), id = id} c, (nextId, Dict.insert id parentId map))

processChildren_: Int -> ParentMap_ -> List (Math.Tree ()) -> (List (Math.Tree State), ParentMap_)
processChildren_ id map children = 
    children
    |> List.foldl (\input (list, newMap) ->
        let
            (node, finalMap) = processNode_ id newMap input
        in
            (list ++ [node], finalMap)
    )
    ([], map)

processFunctionProperties_: Int -> ParentMap_ -> Math.Properties () -> (Math.Properties State, ParentMap_)
processFunctionProperties_ id map properties =
    let
        (newArgs, newMap) = processChildren_ id map properties.args
        (newParams, finalMap) = processChildren_ id newMap properties.parameters
    in
        (   {   name = properties.name
            ,   args = newArgs
            ,   parameters = newParams
            ,   unary = properties.unary
            ,   associative = properties.associative
            ,   commutative = properties.commutative
            ,   linear = properties.linear
            }
        , finalMap
        )
