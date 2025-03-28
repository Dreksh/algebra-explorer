module Display exposing (
    Model, Event(..), State, init, update, view,
    addEquation, updateEquation, listEquations,
    selectedNode
    )

import Dict
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Set
-- Ours
import Helper
import Math
import Matcher
import UI.HtmlEvent

type alias Model =
    {   equations: Dict.Dict Int (Matcher.Equation State)
    ,   nextEquationNum: Int
    ,   selected: Maybe (Int, Set.Set Int)
    ,   createModeForEquation: Maybe Int
    }

type Event =
    Select Int Int
    | Unselect
    | DeleteTree Int Int -- Eq Num + Node Num

type alias State =
    {   position: (Float, Float)
    }

init: List (Matcher.Equation State) -> Model
init eqs =
    {   equations = List.indexedMap Tuple.pair eqs |> Dict.fromList
    ,   selected = Nothing
    ,   nextEquationNum = List.length eqs
    ,   createModeForEquation = Nothing
    }

addEquation: Matcher.Equation State -> Model -> Model
addEquation eq model =
    {   model
    |   nextEquationNum = model.nextEquationNum + 1
    ,   equations = Dict.insert model.nextEquationNum eq model.equations
    }

updateEquation: Int -> Matcher.Equation State -> Model -> Model
updateEquation id eq model = {model | equations = Dict.insert id eq model.equations}

listEquations: Model -> Dict.Dict Int (Matcher.Equation State)
listEquations model = model.equations

selectedNode: Model -> Maybe (Math.Tree (Matcher.State State))
selectedNode model = model.selected
    |> Maybe.andThen (\(eq, ids) -> Dict.get eq model.equations
        |> Maybe.andThen (Matcher.selectedSubtree ids)
    )

update: Event -> Model -> (Model, Cmd Event)
update event model = case event of
    Select eq node -> case model.selected of
        Nothing -> ({model | selected = Just (eq, Set.singleton node)}, Cmd.none)
        Just (e, current) -> if e /= eq
            then ({model | selected = Just (eq, Set.singleton node)}, Cmd.none)
            else if Set.member node current
            then let newSet = Set.remove node current in
                if Set.isEmpty newSet then ({model | selected = Nothing}, Cmd.none)
                else ({model | selected = Just (eq, newSet)}, Cmd.none)
            else ({model | selected = Just (eq, Set.insert node current)}, Cmd.none)
    Unselect -> ({model | selected = Nothing}, Cmd.none)
    DeleteTree _ _ -> (model, Cmd.none) -- TODO: Implement this when we need it

{-
# View-related functions
-}

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attr model = div attr
    (   Dict.foldl
        (\eqNum eq result ->
            let
                highlight = model.selected
                    |> Maybe.andThen (\(selEq, set) -> if selEq == eqNum then Just set else Nothing)
                    |> Maybe.withDefault Set.empty
            in
                (   div []
                    [   eq.root
                        |>  Math.symbolicate
                        |>  collapsedView_ eqNum highlight
                    ,   eq.root
                        |>  stackedView_ eqNum highlight
                    ]
                    |> Html.map converter
                ) :: result
        )
        []
        model.equations
    )

collapsedView_: Int -> Set.Set Int -> Math.Symbol (Matcher.State State) -> Html Event
collapsedView_ eq highlight node = case node of
    Math.Text val -> text val
    Math.Node s -> let id = Matcher.getID s.state in
        s.children
        |> List.map (collapsedView_ eq highlight)
        |> div
            (   [class "node", UI.HtmlEvent.onClick (Select eq id)]
            ++  if Set.member id highlight then [class "selected"] else []
            )

stackedView_: Int -> Set.Set Int -> Math.Tree (Matcher.State State) -> Html Event
stackedView_ eq highlight node =
    let
        (width, depth, divs) = stackRecursive eq highlight 1 1 node
    in
        div
        [   class "blocks"
        ,   style "grid-template-columns" ("repeat(" ++ String.fromInt (width - 1) ++ ", 1fr)")
        ,   style "grid-template-rows" ("repeat(" ++ String.fromInt depth ++ ", 1fr)")
        ] divs

stackRecursive: Int -> Set.Set Int -> Int -> Int -> Math.Tree (Matcher.State State) -> (Int, Int, List (Html Event))
stackRecursive eq highlight width depth node =
    let
        children = Math.getChildren node
        id = Math.getState node |> Matcher.getID
        (childDivs, (maxWidth, maxDepth)) =
            if List.isEmpty children
            then ([], (width+1, depth))
            else
                children
                |>  Helper.listMapWithState (\(foldWidth, foldDepth) child ->
                    let (w, d, divs) = stackRecursive eq highlight foldWidth (depth+1) child
                    in (divs, (w, max foldDepth d))
                ) (width, depth)
    in
        (   maxWidth
        ,   maxDepth
        ,   (   button
                [   class "block"
                ,   style "grid-column" (String.fromInt width ++ "/" ++ String.fromInt maxWidth)
                ,   style "grid-row" (String.fromInt -depth ++ "/" ++ String.fromInt (-depth - 1))  -- might want to allow shorter height unary tiles in the future
                ,   UI.HtmlEvent.onClick (Select eq id)
                ]
                [   text (Math.getName node)
                ]
            ) :: childDivs
        )
