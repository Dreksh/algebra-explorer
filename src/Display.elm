module Display exposing (
    Model, Event(..), State, init, update, view,
    createState, updateState,
    addEquation, updateEquation, transformEquation, listEquations,
    groupChildren, ungroupChildren, replaceNumber,
    selectedNode,
    encode, decoder, encodeState, stateDecoder
    )

import Dict
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Json.Encode as Encode
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

type alias State =
    {   prevID: Int -- To track where it originated from. If it's the same ID as itself, it's new
    }

createState: Int -> State
createState num = { prevID = num }

updateState: Matcher.State State -> Int -> State
updateState s _ = {prevID = Matcher.getID s}

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

selectedNode: Model -> Maybe (Math.Tree (Matcher.State State), Set.Set Int, Int)
selectedNode model = model.selected
    |> Maybe.andThen (\(eq, ids) -> Dict.get eq model.equations
        |> Maybe.andThen (Matcher.selectedSubtree ids >> Result.toMaybe)
    )

groupChildren: Model -> Result String Model
groupChildren model = case model.selected of
    Nothing -> Err "Nothing was selected"
    Just (eqNum, ids) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation not found"
        Just eq -> Matcher.groupSubtree ids eq
            |> Result.map (\newEq -> {model | equations = Dict.insert eqNum newEq model.equations, selected = Nothing})

ungroupChildren: Model -> Result String Model -- only ungroups one, and can be an unselected node
ungroupChildren model = case model.selected of
    Nothing -> Err "Nothing was selected"
    Just (eqNum, ids) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation not found"
        Just eq -> Matcher.ungroupSubtree ids eq
            |> Result.map (\newEq -> {model | equations = Dict.insert eqNum newEq model.equations, selected = Nothing})

replaceNumber: Model -> Float -> Math.Tree () -> Result String Model
replaceNumber model target subtree = case model.selected of
    Nothing -> Err "Nothing was selected"
    Just (eqNum, ids) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation not found"
        Just eq -> Matcher.replaceRealNode ids target subtree eq
            |> Result.map (\newEq -> {model | equations = Dict.insert eqNum newEq model.equations, selected = Nothing})

transformEquation: Matcher.Matcher -> Matcher.MatchResult State -> Model -> Result String Model
transformEquation matcher result model = case model.selected of
    Nothing -> Err "No nodes were selected"
    Just (eqNum, ids) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation is not found"
        Just eq -> Matcher.replaceSubtree ids matcher result eq
            |> Result.map (\newEq -> {model | selected = Nothing, equations = Dict.insert eqNum newEq model.equations})

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
                (
                    [   class "block"
                    ,   style "grid-column" (String.fromInt width ++ "/" ++ String.fromInt maxWidth)
                    ,   style "grid-row" (String.fromInt -depth ++ "/" ++ String.fromInt (-depth - 1))  -- might want to allow shorter height unary tiles in the future
                    ,   UI.HtmlEvent.onClick (Select eq id)
                    ]
                    ++  ( if (Set.member id highlight) then [class "selected"] else [] )
                )
                [   text (Math.getName node)
                ]
            ) :: childDivs
        )

encode: Model -> Encode.Value
encode model = Encode.object
    [   ("equations", Encode.dict String.fromInt (Matcher.encodeEquation (\s -> Encode.object [("prevID", Encode.int s.prevID)])) model.equations)
    ,   ("nextEquationNum", Encode.int model.nextEquationNum)
    ,   (   "selected"
        ,   case model.selected of
            Nothing -> Encode.null
            Just (eq, nodes) -> Encode.object
                [   ("eq", Encode.int eq)
                ,   ("nodes", Encode.set Encode.int nodes)
                ]
        )
    ,   (   "createModeForEquation"
        ,   case model.createModeForEquation of
            Nothing -> Encode.null
            Just n -> Encode.int n
        )
    ]


decoder: Decode.Decoder Model
decoder = Decode.map4 (\eq next sel create -> {equations = eq, nextEquationNum = next, selected = sel, createModeForEquation = create})
    (   Decode.field "equations"
        <| Helper.intDictDecoder (Matcher.equationDecoder createState updateState (Decode.map (\id -> {prevID = id}) <| Decode.field "prevID" Decode.int) )
    )
    (Decode.field "nextEquationNum" Decode.int)
    (   Decode.field "selected"
        <| Decode.maybe <| Decode.map2 Tuple.pair (Decode.field "eq" Decode.int) (Decode.field "nodes" <| Decode.map Set.fromList <| Decode.list Decode.int)
    )
    (   Decode.field "createModeForEquation" <| Decode.maybe Decode.int)

stateDecoder: Decode.Decoder State
stateDecoder = Decode.map (\n -> {prevID = n})
    (Decode.field "prevID" Decode.int)

encodeState: State -> Encode.Value
encodeState s = Encode.object
    [("prevID", Encode.int s.prevID)]