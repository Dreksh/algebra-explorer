module UI.Display exposing (
    Model, Event(..), State, SelectedNode, init, update, views, menu,
    createState, updateState, undo, redo, list,
    add, transform, substitute, getSelected,
    groupChildren, ungroupChildren, replaceNumber, replaceNodeWithNumber,
    encode, decoder, encodeState, stateDecoder
    )

import Dict
import Html exposing (Html, a, button, div, p, span, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Json.Encode as Encode
import Set
-- Ours
import Helper
import Algo.History as History
import Algo.Math as Math
import Algo.Matcher as Matcher
import UI.Block as Block
import UI.Draggable as Draggable
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.Menu as Menu

type alias Model =
    {   equations: Dict.Dict Int Entry
    ,   nextEquationNum: Int
    ,   selected: Maybe (Int, Set.Set Int)
    ,   createModeForEquation: Maybe Int
    ,   setCapture: Bool -> String -> Encode.Value -> Cmd Event
    }

type alias Entry =
    {   history: History.Model (Matcher.Equation State)
    ,   view: (Bool, Draggable.Model)
    ,   showHistory: Bool
    ,   show: Bool
    }

type Event =
    Select Int Int
    | Unselect
    | ToggleHide Int
    | ToggleHistory Int
    | HistoryEvent Int History.Event
    | DraggableEvent Int Draggable.Event

type alias State =
    {   prevID: Int -- To track where it originated from. If it's the same ID as itself, it's new
    }

createState: Int -> State
createState num = { prevID = num }

updateState: Matcher.State State -> Int -> State
updateState s _ = {prevID = Matcher.getID s}

type alias SelectedNode =
    {   eq: Int
    ,   root: Int
    ,   tree: Math.Tree (Matcher.State State)
    ,   selected: Set.Set Int
    ,   nodes: Set.Set Int
    }

init: (Bool -> String -> Encode.Value -> Cmd Event) -> List (Matcher.Equation State) -> Model
init setCapture l = let size = List.length l in
    l
    |> List.indexedMap (\index eq -> (index, {history = History.init eq, view = (False, createDraggable_ size index index), showHistory = False, show = True}))
    |> \result -> {equations = Dict.fromList result, selected = Nothing, nextEquationNum = size, createModeForEquation = Nothing, setCapture = setCapture}

createDraggable_: Int -> Int -> Int -> Draggable.Model
createDraggable_ numVisible index eqNum = let indHeight = 100.0 / toFloat numVisible in
    Draggable.init ("Equation-" ++ String.fromInt eqNum) (25,indHeight * (toFloat index) + 1.0) (50,indHeight - 2.0)

add: Matcher.Equation State -> Model -> Model
add eq model =
    {   model
    |   nextEquationNum = model.nextEquationNum + 1
    ,   equations = Dict.insert
            model.nextEquationNum
            {history = History.init eq, view = (False, createDraggable_ 1 0 model.nextEquationNum), showHistory = False, show = True}
            model.equations
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
        |> Maybe.andThen (.history >> History.current >> Matcher.selectedSubtree ids >> Result.toMaybe)
        |> Maybe.map (\(root, nodes, tree) -> {eq = eqNum, root = root, nodes = nodes, selected = ids, tree = tree})
    )

undo: Model -> Result String Model
undo model = case model.selected of
    Nothing -> Err "Equation not found to undo"
    Just (eqNum, selected) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation not found to undo"
        Just entry -> let newHis = History.undo entry.history in
            Ok
            {   model
            |   equations = Dict.insert eqNum {entry | history = newHis} model.equations
            ,   selected = Just (eqNum, newSelectedNodes_ selected (History.current newHis))
            }

redo: Model -> Result String Model
redo model = case model.selected of
    Nothing -> Err "Equation not found to redo"
    Just (eqNum, selected) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation not found to redo"
        Just entry -> let newHis = History.redo entry.history in
            Ok
            {   model
            |   equations = Dict.insert eqNum {entry | history = newHis} model.equations
            ,   selected = Just (eqNum, newSelectedNodes_ selected (History.current newHis))
            }

list: Model -> Dict.Dict Int (Matcher.Equation State)
list model = model.equations |> Dict.map (\_ entry -> History.current entry.history)

groupChildren: Int -> Int -> Set.Set Int -> Model -> Result String Model
groupChildren eqNum root children model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry -> History.current entry.history
        |> Matcher.groupSubtree root children
        |> Result.map (\(newSelect, newEq) ->
            {   model
            |   equations = Dict.insert eqNum {entry | history = History.add newEq entry.history} model.equations
            ,   selected = Just (eqNum, Set.singleton newSelect)
            })

ungroupChildren: Int -> Int -> Set.Set Int -> Model -> Result String Model
ungroupChildren eqNum id selected model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry -> History.current entry.history
        |> Matcher.ungroupSubtree id selected
        |> Result.map (\newEq ->
            {   model
            |   equations = Dict.insert eqNum {entry | history = History.add newEq entry.history} model.equations
            ,   selected = Just (eqNum, Set.singleton id)
            })

replaceNumber: Int -> Int -> Float -> Matcher.Replacement -> Model -> Result String Model
replaceNumber eqNum root target replacement model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry -> History.current entry.history
        |> Matcher.replaceRealNode root target replacement
        |> Result.map (\(newSelect, newEq) ->
            {   model
            |   equations = Dict.insert eqNum {entry | history = History.add newEq entry.history} model.equations
            ,   selected = Just (eqNum, Set.singleton newSelect)
            })

replaceNodeWithNumber: Int -> Int -> Float -> Model -> Result String Model
replaceNodeWithNumber eqNum id number model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry ->
        let
            replacement = if number < 0
                then Math.UnaryNode {state = Nothing, name = "-", child = Math.RealNode {state = Nothing, value = -number}}
                else Math.RealNode {state = Nothing, value = number}
        in
            History.current entry.history
            |> Matcher.replaceSubtree (Set.singleton id) replacement Matcher.newResult
            |> Result.map (\(newSelect, newEq) ->
                {   model
                |   equations = Dict.insert eqNum {entry | history = History.add newEq entry.history} model.equations
                ,   selected = Just (eqNum, Set.singleton newSelect)
                })

transform: List {a | root: Matcher.Replacement} -> Matcher.MatchResult State -> Model -> Result String Model
transform replacement result model = case model.selected of
    Nothing -> Err "No nodes were selected"
    Just (eqNum, ids) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation is not found"
        Just entry -> History.current entry.history
            |> \current -> Helper.resultList (\r (_, others) -> Matcher.replaceSubtree ids r.root result current
                |> Result.map (\(num, newEq) -> (num, newEq :: others))
                ) (0, []) replacement
            |> Result.map (\(newSelect, newEq) ->
                {   model
                |   equations = Dict.insert eqNum {entry | history = History.addAll (List.reverse newEq) entry.history} model.equations
                ,   selected = Just (eqNum, Set.singleton newSelect)
                })

substitute: Math.FunctionProperties -> Int -> Set.Set Int -> Int -> Model -> Result String Model
substitute funcs origNum selected eqNum model = case Dict.get eqNum model.equations of
    Nothing -> Err "Substitution equation not found"
    Just subEntry -> case Dict.get origNum model.equations of
        Nothing -> Err "Target equation not found"
        Just origEntry -> let origEq = History.current origEntry.history in
            Matcher.replaceAllOccurrences funcs selected (History.current subEntry.history) origEq
            |> Result.map (\(newSelected, newEq) ->
                {   model
                |   selected = Just (origNum, newSelected)
                ,   equations = Dict.insert origNum {origEntry | history = History.add newEq origEntry.history} model.equations
                }
            )

update: Draggable.Size -> Event -> Model -> (Model, Cmd Event)
update size event model = case event of
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
    ToggleHide eq -> case Dict.get eq model.equations of
        Nothing -> (model, Cmd.none)
        Just entry -> (updatePositions_ {model | equations = Dict.insert eq {entry | show = not entry.show} model.equations}, Cmd.none)
    ToggleHistory eq -> case Dict.get eq model.equations of
        Nothing -> (model, Cmd.none)
        Just entry -> ({model | equations = Dict.insert eq {entry | showHistory = not entry.showHistory} model.equations}, Cmd.none)
    HistoryEvent eq he -> case Dict.get eq model.equations of
        Nothing ->(model, Cmd.none)
        Just entry -> let newHis = History.update he entry.history in
            let newModel = {model | equations = Dict.insert eq {entry | history = newHis} model.equations} in
            case model.selected of
                Nothing -> (newModel, Cmd.none)
                Just (eqNum, selected) -> if eqNum /= eq
                    then (newModel, Cmd.none)
                    else ({newModel | selected = Just (eqNum, newSelectedNodes_ selected (History.current newHis))}, Cmd.none)
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

newSelectedNodes_: Set.Set Int -> Matcher.Equation State -> Set.Set Int
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
        ,   p [] [text (History.current entry.history |> .root |> Math.toString)]
        ]]
    )

views: (Event -> msg) -> Model -> List (String, Html msg)
views converter model = Dict.toList model.equations
    |> List.filter (\(_,entry) -> entry.show)
    |> List.map (\(eqNum, entry) ->
        let
            (_, dModel) = entry.view
            eq = History.current entry.history
            highlight = model.selected
                |> Maybe.andThen (\(selEq, set) -> if selEq == eqNum then Just set else Nothing)
                |> Maybe.withDefault Set.empty
        in
            (   dModel.id
            ,   Draggable.div (DraggableEvent eqNum >> converter) dModel [class "equationHolder"]
                [   div [] [   eq.root
                        |>  Math.symbolicate
                        |>  collapsedView_ eqNum highlight
                        |> Html.map converter
                    ,   eq.root
                        |>  stackedView_ eqNum highlight
                        |> Html.map converter
                    ]
                ,   Icon.verticalLine []
                ,   if entry.showHistory
                    then div []
                        [   a [class "clickable", HtmlEvent.onClick (ToggleHistory eqNum |> converter)] [Html.text "Close"]
                        ,   div [class "history"]
                            (   History.serialize (\current index c children -> let middle = max 0 (List.length children - 1) in
                                case List.drop middle children |> List.head of
                                    Nothing ->[historyEntry_ current (History.SelectPast index |> HistoryEvent eqNum |> converter) (c.root |> Math.toString)]
                                    Just after -> historyEntry_ current (History.SelectPast index |> HistoryEvent eqNum |> converter) (c.root |> Math.toString)
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

collapsedView_: Int -> Set.Set Int -> Math.Symbol (Matcher.State State) -> Html Event
collapsedView_ eq highlight node = case node of
    Math.Text val -> text val
    Math.Node s -> let id = Matcher.getID s.state in
        s.children
        |> List.map (collapsedView_ eq highlight)
        |> div
            (   [class "node", HtmlEvent.onClick (Select eq id)]
            ++  if Set.member id highlight then [class "selected"] else []
            )

stackedView_: Int -> Set.Set Int -> Math.Tree (Matcher.State State) -> Html Event
stackedView_ eq highlight root =
    let
        (maxWidth, maxDepth, blocks) = stackRecursive eq highlight 0 1 root
    in
        div [] [ Block.blocks maxWidth maxDepth blocks ]

stackRecursive: Int -> Set.Set Int -> Int -> Int -> Math.Tree (Matcher.State State) -> (Int, Int, List (Html Event))
stackRecursive eq highlight minWidth minDepth node =
    let
        children = Math.getChildren node
        id = Math.getState node |> Matcher.getID
        (maxWidth, maxDepth, childBlocks) =
            if List.isEmpty children
            then (minWidth+1, minDepth, [])
            else
                children
                |>  List.foldl (\child (foldWidth, foldDepth, foldDivs) ->
                    let (w, d, divs) = stackRecursive eq highlight foldWidth (minDepth+1) child
                    in (w, max foldDepth d, foldDivs ++ divs)
                ) (minWidth, minDepth, [])
        onClick = HtmlEvent.onClick (Select eq id)
        selected = (Set.member id highlight)
    in
        (   maxWidth
        ,   maxDepth
        ,   ( Block.block minWidth maxWidth minDepth maxDepth selected onClick (Math.getName node)) :: childBlocks
        )

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
    [   ("history", History.encode (Matcher.encodeEquation <| \s -> Encode.object [("prevID", Encode.int s.prevID)]) entry.history)
    ,   ("show", Encode.bool entry.show)
    ,   ("showHistory", Encode.bool entry.showHistory)
    ,   ("view", entry.view
            |> \(overwritten, dModel) -> if overwritten then Draggable.encode dModel else Encode.null
        )
    ]

decoder: (Bool -> String -> Encode.Value -> Cmd Event) -> Decode.Decoder Model
decoder setCapture = Decode.map4 (\eq next sel create -> Model eq next sel create setCapture)
    (   Decode.field "equations" <| Decode.map addDefaultPositions_ <| Helper.intDictDecoder entryDecoder_)
    (Decode.field "nextEquationNum" Decode.int)
    (   Decode.field "selected"
        <| Decode.maybe <| Decode.map2 Tuple.pair (Decode.field "eq" Decode.int) (Decode.field "nodes" <| Decode.map Set.fromList <| Decode.list Decode.int)
    )
    (   Decode.field "createModeForEquation" <| Decode.maybe Decode.int)

type alias TmpEntry_ =
    {   history: History.Model (Matcher.Equation State)
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
    (Decode.field "history" <| History.decoder <| Matcher.equationDecoder createState updateState (Decode.map (\id -> {prevID = id}) <| Decode.field "prevID" Decode.int) )
    (Decode.maybe <| Decode.field "view" <| Draggable.decoder)
    (Decode.field "show" <| Decode.bool)
    (Decode.field "showHistory" Decode.bool)

stateDecoder: Decode.Decoder State
stateDecoder = Decode.map (\n -> {prevID = n})
    (Decode.field "prevID" Decode.int)

encodeState: State -> Encode.Value
encodeState s = Encode.object
    [("prevID", Encode.int s.prevID)]