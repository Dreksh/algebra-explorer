module Display exposing (
    Model, Event(..), init, update, view,
    addEquation, updateEquation, listEquations,
    selectedNode
    )

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
-- Ours
import Math
import HtmlEvent
import Html exposing (button)

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
addEquation tree model = let eq = processID_ -1 (0, Dict.empty) tree in
    {   model
    |   nextEquationNum = model.nextEquationNum + 1
    ,   equations = Dict.insert model.nextEquationNum eq model.equations
    }

updateEquation: Int -> Math.Tree () -> Model -> Model
updateEquation _ _ model = model -- TODO: Not gotten to this yet

listEquations: Model -> Dict.Dict Int (Math.Tree State)
listEquations model = Dict.map (\_ -> Tuple.second) model.equations

selectedNode: Model -> Maybe (Math.Tree State)
selectedNode model = model.selected
    |> Maybe.andThen (\(eq, num) -> Dict.get eq model.equations
        |> Maybe.andThen (processSearch_ num)
    )

update: Event -> Model -> (Model, Cmd Event)
update event model = case event of
    Select eq node -> case model.selected of
        Nothing -> ({model | selected = Just (eq, node)}, Cmd.none)
        Just (e, n) -> if e == eq && n == node
            then ({model | selected = Nothing}, Cmd.none)
            else ({model | selected = Just (eq, node)}, Cmd.none)
    Unselect -> ({model | selected = Nothing}, Cmd.none)
    DeleteTree _ _ -> (model, Cmd.none) -- TODO: Implement this when we need it

{-
# View-related functions
-}

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attr model = div (attr ++ [])
    (   Dict.foldl
        (\eqNum (_, root) result -> let highlight = Maybe.andThen (\(eq, num) -> if eq == eqNum then Just num else Nothing) model.selected in
            [   root
                |>  Math.symbolicate
                |>  collapsedView_ eqNum highlight
            ,   root
                |>  stackedView_ eqNum highlight
            ]
            |> (\children -> (div [] children |> Html.map converter) :: result)
        )
        []
        model.equations
    )

collapsedView_: Int -> Maybe Int -> Math.Symbol State -> Html Event
collapsedView_ eq highlight node = case node of
    Math.Text val -> text val
    Math.Node s ->
        s.children
        |> List.map (collapsedView_ eq highlight)
        |> div
            (   [class "node", HtmlEvent.onClick (Select eq s.state.id)]
            ++  if s.state.id == Maybe.withDefault -1 highlight then [class "selected"] else []
            )

stackedView_: Int -> Maybe Int -> Math.Tree State -> Html Event
stackedView_ eq highlight node =
    let
        (width, depth, divs) = stackRecursive eq highlight 1 1 node
    in
        div
        [   style "display" "grid"
        ,   style "column-gap" ".3rem"
        ,   style "grid-template-columns" ("repeat(" ++ String.fromInt (width - 1) ++ ", 1fr)")
        ,   style "grid-template-rows" ("repeat(" ++ String.fromInt depth ++ ", 1fr)")
        ] divs

stackRecursive: Int -> Maybe Int -> Int -> Int -> Math.Tree State -> (Int, Int, List (Html Event))
stackRecursive eq highlight width depth node =
    let
        children = Math.getChildren node
        state = Math.getState node
        (maxWidth, maxDepth, childDivs) =
            if List.isEmpty children
            then (width+1, depth, [])
            else
                children
                |>  List.foldl (\child (foldWidth, foldDepth, foldDivs) ->
                    let (w, d, divs) = stackRecursive eq highlight foldWidth (depth+1) child
                    in (w, max foldDepth d, foldDivs ++ divs)
                ) (width, depth, [])
    in
        (   maxWidth
        ,   maxDepth
        ,   (   button
                [   style "text-align" "center"
                ,   style "grid-column" (String.fromInt width ++ "/" ++ String.fromInt maxWidth)
                ,   style "grid-row" (String.fromInt -depth ++ "/" ++ String.fromInt (-depth - 1))  -- might want to allow shorter height unary tiles in the future
                ,   HtmlEvent.onClick (Select eq state.id)
                ]
                [   text ( case node of
                        Math.RealNode n -> String.fromFloat n.value
                        Math.VariableNode n -> n.name
                        Math.UnaryNode n -> n.name
                        Math.BinaryNode n -> n.name
                        Math.GenericNode n -> n.name
                    )
                ]
            )::childDivs
        )

-- Parent's ID, Maximum ID num, Dict
processID_: Int -> ParentMap_ -> Math.Tree () -> Equation_
processID_ parent (next, map) oldRoot = case oldRoot of
    Math.RealNode s -> ((next+1, Dict.insert next parent map), Math.RealNode {state = {id = next, position = (0,0)}, value = s.value})
    Math.VariableNode s -> ((next+1, Dict.insert next parent map), Math.VariableNode {state = {id = next, position = (0,0)}, name = s.name})
    Math.UnaryNode s -> let ((newNext, newDict), newChild) = processID_ next (next+1, map) s.child in
        ((newNext, Dict.insert next parent newDict), Math.UnaryNode {state = {id = next, position = (0,0)}, name = s.name, child = newChild})
    Math.BinaryNode s -> let ((finalNext, finalMap), newChildren) = List.foldl (processIDChild_ next) ((next+1, map),[]) s.children in
        ((finalNext, Dict.insert next parent finalMap), Math.BinaryNode {state = {id = next, position = (0,0)}, name = s.name, associative = s.associative, commutative = s.commutative, children = newChildren})
    Math.GenericNode s -> let ((finalNext, finalMap), newChildren) = List.foldl (processIDChild_ next) ((next+1,map), []) s.children in
        ((finalNext, Dict.insert next parent finalMap), Math.GenericNode {state = {id = next, position = (0,0)}, name = s.name, children = newChildren})

processIDChild_: Int -> Math.Tree () -> (ParentMap_, List (Math.Tree State)) -> (ParentMap_, List (Math.Tree State))
processIDChild_ parent oldRoot (parentMap, result) = let (newMap, newChild) = processID_ parent parentMap oldRoot in
    (newMap, result ++ [newChild])

-- Searching for a specific node
processSearch_: Int -> Equation_ -> Maybe (Math.Tree State)
processSearch_ id (map, root) = if id == 0 then Just root
    else let path = searchPath_ map id in
        Math.getChildren root
        |> processSearchRecursive_ path


searchPath_: ParentMap_ -> Int -> List Int
searchPath_ (num, map) id = case Dict.get id map of
    Nothing -> []
    -- No matching on negative numbers: https://github.com/elm/compiler/issues/1773
    Just a -> if a < 0 then [] else searchPath_ (num, map) a |> (\list -> id::list)

processSearchRecursive_: List Int -> List (Math.Tree State) -> Maybe (Math.Tree State)
processSearchRecursive_ path children = case children of
    [] -> Nothing
    (root::other) -> let s = Math.getState root in
        case path of
            [] -> Nothing
            [x] -> if x /= s.id then Nothing else Just root
            (x::next) -> if x /= s.id
                then processSearchRecursive_ path other
                else Math.getChildren root
                    |> processSearchRecursive_ next