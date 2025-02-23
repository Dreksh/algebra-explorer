module Display exposing (
    Model, Event(..), init, update, view,
    addEquation, updateEquation, listEquations
    )

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Set
-- Ours
import Math
import HtmlEvent

type alias ParentMap_ = (Int, Dict.Dict Int Int) -- Next available ID + mapping of child to parent node, allowing for faster traversal
type alias Equation_ = (ParentMap_, Math.Tree State)

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
    | DeleteTree Int Int -- Eq Num + Node Num

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
addEquation tree model = let ((_, map), root) = Math.process idProcessor_ (-1, (0, Dict.empty)) tree in
    case root of
        Nothing -> model
        Just r ->
            {   model
            |   nextEquationNum = model.nextEquationNum + 1
            ,   equations = Dict.insert model.nextEquationNum (map, r) model.equations
            }

updateEquation: Int -> Math.Tree () -> Model -> Model
updateEquation _ _ model = model -- TODO: Not gotten to this yet

listEquations: Model -> Dict.Dict Int (Math.Tree State)
listEquations model = Dict.map (\_ -> Tuple.second) model.equations

deleteNode_: Int -> Equation_ -> Maybe Equation_
deleteNode_ id (dict, root) = processSearch_ id deleteSubTree_ (dict, root) |> Tuple.first

deleteSubTree_: Equation_ -> (ParentMap_, Maybe (Math.Tree State), ())
deleteSubTree_ ((maxNum, map), node) = let id = Math.processState (\s -> s.id) node in
    Math.getChildren node
    |> List.foldl (\elem ((_, newMap),_, _) -> deleteSubTree_ ((maxNum, newMap), elem)) ((maxNum, Dict.remove id map), Nothing, ())

update: Event -> Model -> (Model, Cmd Event)
update event model = case event of
    DeleteTree eq node -> if node == 0
        then ({model | equations = Dict.remove eq model.equations}, Cmd.none)
        else ({model | equations = Dict.update eq (Maybe.andThen (deleteNode_ node)) model.equations}, Cmd.none)
    _ -> (model, Cmd.none)

{-
# View-related functions
-}

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attr model = div (attr ++ [])
    (   Dict.foldl
        (\eqNum (_, root) result ->
            Math.symbolicate root
            |> collapsedView_ eqNum Set.empty
            |> (\child -> (div [] [child] |> Html.map converter) :: result)
        )
        []
        model.equations
    )

collapsedView_: Int -> Set.Set Int -> Math.Symbol State -> Html Event
collapsedView_ eq highlight node = case node of
    Math.Text val -> text val
    Math.Node s children ->
        children
        |> List.map (collapsedView_ eq highlight)
        |> div [class "node", HtmlEvent.onClick (DeleteTree eq s.id)]

-- Parent's ID, Maximum ID num, Dict
type alias IDGlobal_ = (Int, ParentMap_)
idProcessor_: Math.Processor () State IDGlobal_
idProcessor_ =
    {   children = (\recurse (_, (maxNum, dict)) children -> idChildren_ recurse maxNum (maxNum+1,dict) children |> (\(g, list) -> (g, Just list)))
    ,   function = (\recurse (_, (maxNum, dict)) prop ->
            let
                ((_, newDict),args) = idChildren_ recurse maxNum (maxNum+1,dict) prop.args
                (g, parameters) = idChildren_ recurse maxNum newDict prop.parameters
            in
                (g, Just {name = prop.name, args = args, parameters = parameters, unary = prop.unary, associative = prop.associative, commutative = prop.commutative, linear = prop.linear})
        )
    ,   child = (\recurse (_, (maxNum, dict)) child -> recurse (maxNum, (maxNum+1,dict)) child )
    ,   var = (\(p, (maxNum, dict)) str -> ((p, (maxNum+1, dict)), Just str) )
    ,   real = (\(p, (maxNum, dict)) val -> ((p, (maxNum+1, dict)), Just val) )
    ,   finalize = (\(parent, (id, _)) (_, (maxNum, dict)) _ -> ((parent, (maxNum, Dict.insert id parent dict)), {position = (0,0), id = id}))
    }

-- Do not use id*_ directly, use Math.process idProcessor_
idChildren_: (IDGlobal_ -> Math.Tree () -> (IDGlobal_, Maybe (Math.Tree State))) -> Int -> ParentMap_ -> List (Math.Tree ()) -> (IDGlobal_, List (Math.Tree State))
idChildren_ recurse parent dict children = List.foldl
    (\elem (s, list) -> let (g, e) = recurse s elem in
        case e of
            Nothing -> (g, list)
            Just newChild -> (g, list ++ [newChild])
    )
    ((parent, dict), [])
    children

{-
# Search-based functions
-}
type alias SearchGlobal_ output = (List Int, ParentMap_, Maybe output)

processSearch_: Int -> (Equation_ -> (ParentMap_, Maybe (Math.Tree State), output)) -> Equation_ -> (Maybe Equation_, Maybe output)
processSearch_ id process (map, root) =
    if id == 0 then process (map, root) |> (\(newMap, newTree, o) -> case newTree of
        Nothing -> (Nothing, Just o)
        Just newRoot -> (Just (newMap,newRoot), Just o)
    )
    else
        let
            path = searchPath_ map id |> List.reverse
            p = searchProcessor_ process
        in
            Math.process p (path, map, Nothing) root
            |> (\((_, dict, o), newRoot) -> case newRoot of
                Nothing -> (Nothing, o)
                Just r -> (Just (dict, r), o)
            )

-- Do not use the search*_ methods directly, use processSearch_
searchPath_: ParentMap_ -> Int -> List Int
searchPath_ (num, map) id = case Dict.get id map of
    Nothing -> []
    -- No matching on negative numbers: https://github.com/elm/compiler/issues/1773
    Just a -> if a < 0 then [] else searchPath_ (num, map) a |> (\list -> id::list)

searchProcessor_: (Equation_ -> (ParentMap_, Maybe (Math.Tree State), output)) -> Math.Processor State State (SearchGlobal_ output)
searchProcessor_ process =
    {   children = (\recurse g children ->
            searchChildren_ process recurse g children
            |> (\(newG, list) -> if List.isEmpty list then (newG,Nothing) else (newG, Just list))
        )
    ,   function = searchFunction_ process
    ,   child = searchChild_ process
    -- Cannot search leaf nodes
    ,   var = (\g str -> (g, Just str) )
    ,   real = (\g val -> (g, Just val) )
    ,   finalize = (\_ g s -> (g,s)) -- Keep the new global, as children might update the result
    }

searchChild_: (Equation_ -> (ParentMap_, Maybe (Math.Tree State), output)) -> (SearchGlobal_ output -> Math.Tree State -> (SearchGlobal_ output, Maybe (Math.Tree State))) -> SearchGlobal_ output -> Math.Tree State -> (SearchGlobal_ output, Maybe (Math.Tree State))
searchChild_ process recurse (path, map, _) node = case path of
    [] -> ((path, map, Nothing), Just node) -- Reached EOF for some reason
    [id] -> if (Math.processState (\s -> s.id) node) /= id then ((path,map,Nothing), Just node) -- Wrong ID
        else
            process (map,node)
            |> (\(newMap, newNode, o) -> ((path, newMap, Just o), newNode))
    (id::next) ->
        let
            currentId = Math.processState (\s -> s.id) node
        in
        if currentId /= id then ((path,map,Nothing), Just node) -- Wrong ID
        else
            recurse (next, map, Nothing) node

searchChildren_: (Equation_ -> (ParentMap_, Maybe (Math.Tree State), output)) -> (SearchGlobal_ output -> Math.Tree State -> (SearchGlobal_ output, Maybe (Math.Tree State))) -> SearchGlobal_ output -> List (Math.Tree State) -> (SearchGlobal_ output, List (Math.Tree State))
searchChildren_ process recurse (path, map, _) children = case children of
    [] -> ((path, map, Nothing), []) -- No more children to process
    (child::next) -> let ((_, newDict, o), newChild) = searchChild_ process recurse (path, map, Nothing) child in
        case o of
            Nothing -> searchChildren_ process recurse (path, map, Nothing) next |> (\(g, list) -> (g, child::list))
            Just _ -> case newChild of
                Nothing -> ((path, newDict, o), next)
                Just c -> ((path, newDict, o), c::next)

searchFunction_: (Equation_ -> (ParentMap_, Maybe (Math.Tree State), output)) -> (SearchGlobal_ output -> Math.Tree State -> (SearchGlobal_ output, Maybe (Math.Tree State))) -> SearchGlobal_ output -> Math.Properties State -> (SearchGlobal_ output, Maybe (Math.Properties State))
searchFunction_ process recurse (path, map, _) props = let ((_, firstMap, firstO),args) = searchChildren_ process recurse (path, map, Nothing) props.args in
    case firstO of
        Just _ -> if List.isEmpty args then ((path,firstMap,firstO),Nothing) else ((path, firstMap, firstO), Just {props | args = args})
        Nothing -> let ((_, secondMap, secondO),parameters) = searchChildren_ process recurse (path, map, Nothing) props.parameters in
            ((path, secondMap, secondO), Just {props | parameters = parameters})