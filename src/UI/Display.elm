module UI.Display exposing (
    Model, Event(..), SelectedNode, init, update, views, menu,
    undo, redo, updateQueryCmd,
    add, advanceTime, transform, substitute, getSelected,
    groupChildren, ungroupChildren, replaceNumber, replaceNodeWithNumber,
    encode, decoder
    )

import Dict
import Html exposing (Html, a, div, p, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Encode as Encode
import Set
-- Ours
import Helper
import Algo.History as History
import Algo.Math as Math
import Algo.Matcher as Matcher
import Components.Latex as Latex
import Components.Rules as Rules
import UI.Animation as Animation
import UI.Bricks as Bricks
import UI.Draggable as Draggable
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.MathIcon as MathIcon
import UI.Menu as Menu

type alias State = Matcher.State Animation.State
type alias LatexConverter = Matcher.Equation Animation.State -> Result String (Latex.Model State)

type alias Model =
    {   equations: Dict.Dict Int Entry
    ,   nextEquationNum: Int
    ,   selected: Maybe (Int, Set.Set Int)
    ,   createModeForEquation: Maybe Int
    -- Command creators
    ,   setCapture: Bool -> String -> Encode.Value -> Cmd Event
    ,   updateQuery: List (Matcher.Equation Animation.State) -> Cmd Event
    }

type UIModel =
    Blocks Bricks.Model
    | Written MathIcon.Model

type alias Entry =
    {   history: History.Model (Matcher.Equation Animation.State, Latex.Model State)
    ,   view: (Bool, Draggable.Model)
    ,   showHistory: Bool
    ,   show: Bool
    ,   ui: UIModel
    }

type Event =
    Select Int Int Bool
    | ToggleHide Int
    | ToggleHistory Int
    | HistoryEvent Int History.Event
    | DraggableEvent Int Draggable.Event

type alias SelectedNode =
    {   eq: Int
    ,   root: Int
    ,   tree: Math.Tree State
    ,   selected: Set.Set Int
    ,   nodes: Set.Set Int
    }

newEntry_: Int -> Int -> (Matcher.Equation Animation.State, Latex.Model State) -> Entry
newEntry_ size index (eq, latex) =
    {   history = History.init (eq, latex)
    ,   view = (False, createDraggable_ size index index)
    ,   showHistory = False
    ,   show = True
    ,   ui = Bricks.init eq.root |> Blocks
    }

init: (Bool -> String -> Encode.Value -> Cmd Event) -> (List (Matcher.Equation Animation.State) -> Cmd Event) -> List (Matcher.Equation Animation.State, Latex.Model State) -> Model
init setCapture updateQuery l = let size = List.length l in
    {   equations =
        List.foldl (\eq list -> let index = List.length list in (index, newEntry_ size index eq) :: list ) [] l
        |> Dict.fromList
    ,   selected = Nothing
    ,   nextEquationNum = size
    ,   createModeForEquation = Nothing
    ,   setCapture = setCapture
    ,   updateQuery = updateQuery
    }

createDraggable_: Int -> Int -> Int -> Draggable.Model
createDraggable_ numVisible index eqNum = let indHeight = 100.0 / toFloat numVisible in
    Draggable.init ("Equation-" ++ String.fromInt eqNum) (25,indHeight * (toFloat index) + 1.0) (50,indHeight - 2.0)

add: (Matcher.Equation Animation.State, Latex.Model State) -> Model -> Model
add eq model =
    {   model
    |   nextEquationNum = model.nextEquationNum + 1
    ,   equations = newEntry_ (model.nextEquationNum + 1) model.nextEquationNum eq
            |> \e -> Dict.insert model.nextEquationNum e model.equations
    }
    |> updatePositions_

updatePositions_: Model -> Model
updatePositions_ model =
    let
        filtered = Dict.toList model.equations |> List.filter (\(_, entry) -> entry.show)
        size = List.length filtered
    in
        filtered
        |> List.indexedMap (\index (eqNum, entry) ->
            (eqNum, {entry | view = if entry.view |> Tuple.first then entry.view else (False, createDraggable_ size index eqNum)})
        )
        |> List.foldl (\(eqNum, entry) m -> {m | equations = Dict.insert eqNum entry m.equations}) model

getSelected: Model -> Maybe SelectedNode
getSelected model = model.selected
    |> Maybe.andThen (\(eqNum, ids) -> Dict.get eqNum model.equations
        |> Maybe.andThen (.history >> History.current >> Tuple.first >> Matcher.selectedSubtree ids >> Result.toMaybe)
        |> Maybe.map (\(root, nodes, tree) -> {eq = eqNum, root = root, nodes = nodes, selected = ids, tree = tree})
    )

updateBricks: Entry -> Entry
updateBricks entry = let (eq, latex) = History.current entry.history in
    {   entry
    |   ui = case entry.ui of
        Blocks b -> Bricks.updateTree eq.root b |> Blocks
        Written w -> MathIcon.set latex w |> Written
    }

advanceTime: Float -> Model -> Model
advanceTime millis model =
    {   model
    |   equations = Dict.map (\_ entry -> case entry.ui of
                Blocks b -> {entry | ui = Bricks.advanceTime millis b |> Blocks }
                Written w -> {entry | ui = MathIcon.advanceTime millis w |> Written }
            ) model.equations
    }

undo: Model -> Result String Model
undo model = case model.selected of
    Nothing -> Err "Equation not found to undo"
    Just (eqNum, selected) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation not found to undo"
        Just entry -> let newHis = History.undo entry.history in
            Ok
            {   model
            |   equations = Dict.insert eqNum (updateBricks {entry | history = newHis}) model.equations
            ,   selected = Just (eqNum, newSelectedNodes_ selected (History.current newHis |> Tuple.first))
            }

redo: Model -> Result String Model
redo model = case model.selected of
    Nothing -> Err "Equation not found to redo"
    Just (eqNum, selected) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation not found to redo"
        Just entry -> let newHis = History.redo entry.history in
            Ok
            {   model
            |   equations = Dict.insert eqNum (updateBricks {entry | history = newHis}) model.equations
            ,   selected = Just (eqNum, newSelectedNodes_ selected (History.current newHis |> Tuple.first))
            }

updateQueryCmd: Model -> (Model, Cmd Event)
updateQueryCmd model =
    (   model
    ,   Dict.values model.equations
        |> List.filter (\entry -> entry.show)
        |> List.map (\entry -> History.current entry.history |> Tuple.first)
        |> model.updateQuery
    )

groupChildren: LatexConverter -> Int -> Int -> Set.Set Int -> Model -> Result String Model
groupChildren convert eqNum root children model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry -> History.current entry.history
        |> Tuple.first
        |> Matcher.groupSubtree root children
        |> Result.andThen (\(newSelect, newEq) -> convert newEq
            |> Result.map (\l ->
                {   model
                |   equations = Dict.insert eqNum (updateBricks {entry | history = History.add (newEq, l) entry.history}) model.equations
                ,   selected = Just (eqNum, Set.singleton newSelect)
                }
            )
        )

ungroupChildren: LatexConverter -> Int -> Int -> Set.Set Int -> Model -> Result String Model
ungroupChildren convert eqNum id selected model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry -> History.current entry.history
        |> Tuple.first
        |> Matcher.ungroupSubtree id selected
        |> Result.andThen (\newEq -> convert newEq
            |> Result.map (\l ->
                {   model
                |   equations = Dict.insert eqNum (updateBricks {entry | history = History.add (newEq, l) entry.history}) model.equations
                ,   selected = Just (eqNum, Set.singleton id)
                }
            )
        )

replaceNumber: LatexConverter -> Int -> Int -> Float -> Matcher.Replacement -> Model -> Result String Model
replaceNumber convert eqNum root target replacement model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry -> History.current entry.history
        |> Tuple.first
        |> Matcher.replaceRealNode root target replacement
        |> Result.andThen (\(newSelect, newEq) -> convert newEq
            |> Result.map (\l ->
                {   model
                |   equations = Dict.insert eqNum (updateBricks {entry | history = History.add (newEq, l) entry.history}) model.equations
                ,   selected = Just (eqNum, Set.singleton newSelect)
                }
            )
        )

replaceNodeWithNumber: LatexConverter -> Int -> Int -> Float -> Model -> Result String Model
replaceNodeWithNumber convert eqNum id number model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry ->
        let
            replacement = if number < 0
                then Math.UnaryNode {state = Nothing, name = "-", child = Math.RealNode {state = Nothing, value = -number}}
                else Math.RealNode {state = Nothing, value = number}
        in
            History.current entry.history
            |> Tuple.first
            |> Matcher.replaceSubtree (Set.singleton id) replacement Matcher.newResult
            |> Result.andThen (\(newSelect, newEq) -> convert newEq
                |> Result.map (\l ->
                    {   model
                    |   equations = Dict.insert eqNum (updateBricks {entry | history = History.add (newEq, l) entry.history}) model.equations
                    ,   selected = Just (eqNum, Set.singleton newSelect)
                    }
                )
            )

transform: LatexConverter -> List {a | root: Matcher.Replacement} -> Matcher.MatchResult Animation.State -> Model -> Result String Model
transform convert replacement result model = case model.selected of
    Nothing -> Err "No nodes were selected"
    Just (eqNum, ids) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation is not found"
        Just entry -> History.current entry.history
            |> Tuple.first
            |> \current -> Helper.resultList (\r (_, others) -> Matcher.replaceSubtree ids r.root result current
                |> Result.andThen (\(num, newEq) -> convert newEq |> Result.map (\l -> (num, (newEq, l) :: others)))
                ) (0, []) replacement
            |> Result.map (\(newSelect, newEq) ->
                {   model
                |   equations = Dict.insert eqNum (updateBricks {entry | history = History.addAll (List.reverse newEq) entry.history}) model.equations
                ,   selected = Just (eqNum, Set.singleton newSelect)
                })

substitute: LatexConverter -> Math.FunctionProperties -> Int -> Set.Set Int -> Int -> Model -> Result String Model
substitute convert funcs origNum selected eqNum model = case Dict.get eqNum model.equations of
    Nothing -> Err "Substitution equation not found"
    Just subEntry -> case Dict.get origNum model.equations of
        Nothing -> Err "Target equation not found"
        Just origEntry -> let origEq = History.current origEntry.history |> Tuple.first in
            Matcher.replaceAllOccurrences funcs selected (History.current subEntry.history |> Tuple.first) origEq
            |> Result.andThen (\(newSelected, newEq) -> convert newEq
                |> Result.map (\l ->
                    {   model
                    |   selected = Just (origNum, newSelected)
                    ,   equations = Dict.insert origNum (updateBricks {origEntry | history = History.add (newEq,l) origEntry.history}) model.equations
                    }
                )
            )

update: Draggable.Size -> Event -> Model -> (Model, Cmd Event)
update size event model = case event of
    Select eq node shift -> case (shift, model.selected) of
        (True, Just (e, current)) -> if e /= eq
            then ({model | selected = Just (eq, Set.singleton node)}, Cmd.none)
            else if Set.member node current
            then let newSet = Set.remove node current in
                if Set.isEmpty newSet then ({model | selected = Nothing}, Cmd.none)
                else ({model | selected = Just (eq, newSet)}, Cmd.none)
            else ({model | selected = Just (eq, Set.insert node current)}, Cmd.none)
        _ -> ({model | selected = Just (eq, Set.singleton node)}, Cmd.none)
    ToggleHide eq -> case Dict.get eq model.equations of
        Nothing -> (model, Cmd.none)
        Just entry -> updatePositions_ {model | equations = Dict.insert eq {entry | show = not entry.show} model.equations} |> updateQueryCmd
    ToggleHistory eq -> case Dict.get eq model.equations of
        Nothing -> (model, Cmd.none)
        Just entry -> ({model | equations = Dict.insert eq {entry | showHistory = not entry.showHistory} model.equations}, Cmd.none)
    HistoryEvent eq he -> case Dict.get eq model.equations of
        Nothing ->(model, Cmd.none)
        Just entry -> let newHis = History.update he entry.history in
            let newModel = {model | equations = Dict.insert eq (updateBricks {entry | history = newHis}) model.equations} in
            case model.selected of
                Nothing -> updateQueryCmd newModel
                Just (eqNum, selected) -> if eqNum /= eq
                    then updateQueryCmd newModel
                    else updateQueryCmd {newModel | selected = Just (eqNum, newSelectedNodes_ selected (History.current newHis |> Tuple.first))}
    DraggableEvent eq dEvent -> case Dict.get eq model.equations of
        Nothing -> (model, Cmd.none)
        Just entry -> Draggable.update size dEvent (entry.view |> Tuple.second)
            |> \(dModel, action) ->
                (   {model | equations = Dict.insert eq {entry | view = (True, dModel)} model.equations}
                ,   Maybe.map (\a -> case a of
                        Draggable.SetCapture s v -> model.setCapture True s v
                        Draggable.ReleaseCapture s v -> model.setCapture False s v
                    ) action
                    |> Maybe.withDefault Cmd.none
                )

newSelectedNodes_: Set.Set Int -> Matcher.Equation Animation.State -> Set.Set Int
newSelectedNodes_ selected eq = let intersection = Set.filter (\n -> Dict.member n eq.tracker.parent) selected in
    if Set.isEmpty intersection then Set.singleton (Math.getState eq.root |> Matcher.getID) else intersection

{-
# View-related functions
-}

menu: (Event -> msg) -> Model -> List (Menu.Part msg)
menu convert model = Dict.toList model.equations
    |> List.map (\(num, entry) -> Menu.Content [a [class "clickable", HtmlEvent.onClick (convert (ToggleHide num))]
        [   if entry.show then Icon.shown [] else Icon.hidden []
        ,   span [class "space"] []
        ,   p [] [text (History.current entry.history |> Tuple.first |> .root |> treeToString_)]
        ]]
    )

treeToString_: Math.Tree s -> String
treeToString_ = Rules.process (\_ -> String.join "") identity

views: (Event -> msg) -> Model -> List (String, Html msg)
views converter model = Dict.toList model.equations
    |> List.filter (\(_,entry) -> entry.show)
    |> List.map (\(eqNum, entry) ->
        let
            (_, dModel) = entry.view
            (eq, _) = History.current entry.history
            highlight = model.selected
                |> Maybe.andThen (\(selEq, set) -> if selEq == eqNum then Just set else Nothing)
                |> Maybe.withDefault Set.empty
        in
            (   dModel.id
            ,   Draggable.div (DraggableEvent eqNum >> converter) dModel [class "equationHolder"]
                [   div []
                    (   case entry.ui of
                        Blocks b ->
                            [   Rules.process (\s -> let id = Matcher.getID s in
                                    Html.span
                                    ( HtmlEvent.onShiftClick (Select eqNum id)
                                    :: (class "node" :: if Set.member id highlight then [class "selected"] else [])
                                    )
                                )
                                Html.text eq.root
                            ,   Bricks.view eqNum highlight Select b
                            ]
                        Written w -> [MathIcon.view (\_ -> []) [] w]
                    )
                    |>  Html.map converter
                ,   Icon.verticalLine []
                ,   if entry.showHistory
                    then div []
                        [   a [class "clickable", HtmlEvent.onClick (ToggleHistory eqNum |> converter)] [Html.text "Close"]
                        ,   div [class "history"]
                            (   History.serialize (\current index (c,_) children -> let middle = max 0 (List.length children - 1) in
                                case List.drop middle children |> List.head of
                                    Nothing ->[historyEntry_ current (History.SelectPast index |> HistoryEvent eqNum |> converter) (c.root |> treeToString_)]
                                    Just after -> historyEntry_ current (History.SelectPast index |> HistoryEvent eqNum |> converter) (c.root |> treeToString_)
                                        ::  (
                                            List.map (Html.div []) (List.take middle children)
                                            ++ after
                                        )
                                ) entry.history
                            )
                        ]
                    else a [class "historyButton", class "clickable", HtmlEvent.onClick (ToggleHistory eqNum |> converter)] [Html.text "Show History"]
                ]
            )
    )

historyEntry_: Bool -> msg -> String -> Html.Html msg
historyEntry_ current event t = Html.a
    (   if current then [class "selected"] else [ class "clickable", HtmlEvent.onClick event])
    [Html.text t]

encode: Model -> Encode.Value
encode model = Encode.object
    [   (   "equations",   Encode.dict String.fromInt encodeEntry_ model.equations)
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

encodeEntry_: Entry -> Encode.Value
encodeEntry_ entry = Encode.object
    [   ("history", History.encode encodeHistoryState_ entry.history)
    ,   ("show", Encode.bool entry.show)
    ,   ("showHistory", Encode.bool entry.showHistory)
    ,   ("view", entry.view
            |> \(overwritten, dModel) -> if overwritten then Draggable.encode dModel else Encode.null
        )
    ]

encodeHistoryState_: (Matcher.Equation Animation.State, Latex.Model State) -> Encode.Value
encodeHistoryState_ (eq, l) = Encode.object
    [   ("equation", Matcher.encodeEquation Animation.encodeState eq)
    ,   ("latex", Latex.encode (Matcher.encodeState Animation.encodeState) l)
    ]

decoder: (Bool -> String -> Encode.Value -> Cmd Event) -> (List (Matcher.Equation Animation.State) -> Cmd Event) -> Decode.Decoder Model
decoder setCapture updateQuery = Decode.map4 (\eq next sel create -> Model eq next sel create setCapture updateQuery)
    (   Decode.field "equations" <| Decode.map addDefaultPositions_ <| Helper.intDictDecoder entryDecoder_)
    (Decode.field "nextEquationNum" Decode.int)
    (   Decode.field "selected"
        <| Decode.maybe <| Decode.map2 Tuple.pair (Decode.field "eq" Decode.int) (Decode.field "nodes" <| Decode.map Set.fromList <| Decode.list Decode.int)
    )
    (   Decode.field "createModeForEquation" <| Decode.maybe Decode.int)

type alias TmpEntry_ =
    {   history: History.Model (Matcher.Equation Animation.State, Latex.Model State)
    ,   view: Maybe Draggable.Model
    ,   showHistory: Bool
    ,   show: Bool
    }

addDefaultPositions_: Dict.Dict Int TmpEntry_ -> Dict.Dict Int Entry
addDefaultPositions_ orig =
    let
        (shown, hidden) = Dict.toList orig |> List.partition (\(_, entry) -> entry.show)
        size = List.length shown
        create tEntry newView =
            {   history = tEntry.history
            ,   view = case tEntry.view of
                    Just view -> (True, view)
                    Nothing -> (False, newView)
            ,   ui = (History.current tEntry.history |> Tuple.first |> .root) |> Bricks.init |> Blocks
            ,   showHistory = tEntry.showHistory
            ,   show = tEntry.show
            }
    in
        shown
        |> List.indexedMap (\index (eqNum, entry) -> (eqNum, create entry (createDraggable_ size index eqNum)))
        |> List.foldl (\(eqNum, entry) -> Dict.insert eqNum entry)
            (List.map (\(eq, entry) -> (eq, create entry (createDraggable_ 1 0 eq))) hidden |> Dict.fromList)

entryDecoder_: Decode.Decoder TmpEntry_
entryDecoder_ = Decode.map4 TmpEntry_
    (Decode.field "history" <| History.decoder <| historyStateDecoder_ )
    (Decode.maybe <| Decode.field "view" <| Draggable.decoder)
    (Decode.field "show" <| Decode.bool)
    (Decode.field "showHistory" Decode.bool)

historyStateDecoder_: Decode.Decoder (Matcher.Equation Animation.State, Latex.Model State)
historyStateDecoder_ = Decode.map2 Tuple.pair
    (Decode.field "equation" <| Matcher.equationDecoder Animation.createState Animation.updateState Animation.stateDecoder)
    (Decode.field "latex" <| Latex.decoder (Matcher.stateDecoder Animation.stateDecoder))
