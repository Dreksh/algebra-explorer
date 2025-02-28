module Display exposing (
    Model, Event(..), init, update, view,
    addEquation, updateEquation, listEquations,
    selectedNode
    )

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
-- Ours
import Math
import HtmlEvent

type alias ParentMap_ = (Int, Dict.Dict Int Int) -- Next available ID + mapping of child to parent node, allowing for faster traversal
type alias Equation_ = (ParentMap_, Math.Tree State)

type alias Model =
    {   equations: Dict.Dict Int Equation_
    ,   nextEquationNum: Int
    ,   selected: Maybe (Int, Int)
    ,   createModeForEquation: Maybe Int
    }

type Event =
    Select Int Int
    | Unselect
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
deleteNode_ id eq = processSearch_ id deleteSubTree_ eq |> Tuple.first

deleteSubTree_: Equation_ -> (ParentMap_, Maybe (Math.Tree State), ())
deleteSubTree_ ((maxNum, map), node) = let id = Math.getState node |> (\s -> s.id) in
    Math.getChildren node
    |> List.foldl (\elem ((_, newMap),_, _) -> deleteSubTree_ ((maxNum, newMap), elem)) ((maxNum, Dict.remove id map), Nothing, ())

selectedNode: Model -> Maybe (Math.Tree State)
selectedNode model = model.selected
    |> Maybe.andThen (\(eq, num) -> Dict.get eq model.equations
        |> Maybe.andThen (\equation -> equation
            |> processSearch_ num (\(map, node) -> (map, Just node, node)) 
            |> Tuple.second
        )
    )

update: Event -> Model -> (Model, Cmd Event)
update event model = case event of
    Select eq node -> case model.selected of
        Nothing -> ({model | selected = Just (eq, node)}, Cmd.none)
        Just (e, n) -> if e == eq && n == node 
            then ({model | selected = Nothing}, Cmd.none)
            else ({model | selected = Just (eq, node)}, Cmd.none)
    Unselect -> ({model | selected = Nothing}, Cmd.none)
    DeleteTree eq node -> if node == 0
        then ({model | equations = Dict.remove eq model.equations}, Cmd.none)
        else ({model | equations = Dict.update eq (Maybe.andThen (deleteNode_ node)) model.equations}, Cmd.none)

{-
# View-related functions
-}

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attr model = div (attr ++ [])
    (   Dict.foldl
        (\eqNum (_, root) result -> let highlight = Maybe.andThen (\(eq, num) -> if eq == eqNum then Just num else Nothing) model.selected in
            Math.symbolicate root
            |> collapsedView_ eqNum highlight
            |> (\child -> (div [] [child] |> Html.map converter) :: result)
        )
        []
        model.equations
    )

collapsedView_: Int -> Maybe Int -> Math.Symbol State -> Html Event
collapsedView_ eq highlight node = case node of
    Math.Text val -> text val
    Math.Node s children ->
        children
        |> List.map (collapsedView_ eq highlight)
        |> div
            (   [class "node", HtmlEvent.onClick (Select eq s.id)]
            ++  if s.id == Maybe.withDefault -1 highlight then [class "selected"] else []
            )

-- Parent's ID, Maximum ID num, Dict
type alias IDGlobal_ = (Int, ParentMap_)
idProcessor_: Math.Processor () State IDGlobal_
idProcessor_ =
    {   function = (\recurse (p, (id, dict)) (_, children) ->
            let
                ((_, g),args) = idChildren_ recurse id (id+1,dict) children.args
                ((_,(next, finalDict)), parameters) = idChildren_ recurse id g children.parameters
            in
                ((p, (next, Dict.insert id p finalDict)), Just (Math.FunctionNode {position=(0,0), id=id} {name = children.name, args = args, parameters = parameters}))
        )
    ,   var = (\(p, (id, dict)) (_, name) -> ((p, (id+1, Dict.insert id p dict)), Just (Math.VariableNode {position=(0,0), id = id} name) ))
    ,   real = (\(p, (id, dict)) (_, val) -> ((p, (id+1, Dict.insert id p dict)), Just (Math.RealNode {position=(0,0), id = id} val) ))
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
    {   function = searchFunction_ process
    -- Cannot search leaf nodes
    ,   var = (\g (s, name) -> (g, Just (Math.VariableNode s name) ))
    ,   real = (\g (s, val) -> (g, Just (Math.RealNode s val) ))
    }

searchChildren_: (Equation_ -> (ParentMap_, Maybe (Math.Tree State), output)) -> (SearchGlobal_ output -> Math.Tree State -> (SearchGlobal_ output, Maybe (Math.Tree State))) -> SearchGlobal_ output -> List (Math.Tree State) -> (SearchGlobal_ output, List (Math.Tree State))
searchChildren_ process recurse (path, map, _) children = case children of
    -- No more children to process, with recursion, the children will be added back
    [] -> ((path, map, Nothing), [])
    (child::nextChildren) -> case path of
        -- Reached end of path for some reason
        [] -> ((path, map, Nothing), children)
        (id::next) ->
            -- Search next child
            if (Math.getState child |> (\s -> s.id)) /= id then searchChildren_ process recurse (path, map, Nothing) nextChildren
                |> (\(o, newChildren) -> (o, child::newChildren)) -- Add back the child
            else if List.isEmpty next |> not then
                -- Step to the next child
                recurse (next, map, Nothing) child
                |>  (\(o, newNode) -> case newNode of
                    Nothing -> (o, nextChildren)
                    Just n -> (o, n::nextChildren)
                )
            else
                -- Process child node
                process (map,child)
                |> (\(newMap, newNode, o) -> case newNode of
                    Nothing -> ((path, newMap, Just o), nextChildren)
                    Just n -> ((path, newMap, Just o), n::nextChildren)
                )

searchFunction_: (Equation_ -> (ParentMap_, Maybe (Math.Tree State), output)) -> (SearchGlobal_ output -> Math.Tree State -> (SearchGlobal_ output, Maybe (Math.Tree State))) -> SearchGlobal_ output -> (State, Math.Function State) -> (SearchGlobal_ output, Maybe (Math.Tree State))
searchFunction_ process recurse (path, map, _) (s, props) =
    searchChildren_ process recurse (path, map, Nothing) props.args
    |> (\((_, firstMap, firstO), args) -> case firstO of
        -- Child node found
        Just _ -> ((path, firstMap, firstO), {props | args = args})
        -- Continue searching
        Nothing ->
            searchChildren_ process recurse (path, map, Nothing) props.parameters
            |> (\(secondO,parameters) -> (secondO, {props | parameters = parameters}))
    )
    -- Remove the node if no children are left
    |> (\(o, func) -> if List.isEmpty func.args && List.isEmpty func.parameters then (o, Nothing) else (o, Just (Math.FunctionNode s func) ))