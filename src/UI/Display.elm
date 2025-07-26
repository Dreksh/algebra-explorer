module UI.Display exposing (
    Model, Event(..), FullEquation, SelectedNode, init, update, views, menu,
    anyVisible, undo, redo, updateQueryCmd, svgDragEvent, refresh,
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
import Svg.Attributes
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
type alias FullEquation = Matcher.Equation Rules.FunctionProp Animation.State

type alias Model =
    {   equations: Dict.Dict Int Entry
    ,   nextEquationNum: Int
    ,   selected: Maybe (Int, Set.Set Int)
    ,   createModeForEquation: Maybe Int
    -- Command creators
    ,   setCapture: Bool -> String -> Encode.Value -> Cmd Event
    ,   svgMouseCmd: Int -> Cmd Event -- start/stop eqNum
    ,   updateQuery: List FullEquation -> Cmd Event
    }

longClickThreshold: Float
longClickThreshold = 300 -- in ms

svgDragEvent: {final: Bool, id: Int, x: Float, y: Float, time: Float} -> Event
svgDragEvent n = if n.final then CommuteEnd n.id n.time (n.x,n.y)
    else CommuteContinue n.id (n.x,n.y)

type alias UIModel = (Bricks.Model, MathIcon.Model)

type alias Entry =
    {   history: History.Model (FullEquation, Latex.Model State) -- latex model a cache for displaying history
    ,   view: (Bool, Draggable.Model)
    ,   showHistory: Bool
    ,   show: Bool
    ,   ui: UIModel
    ,   commuting: Maybe {id: Int, currentIndex: Int, originalIndex: Int, moved: Bool, midpoints: List Float}
    }

type Event =
    Select Int Int
    | Delete Int
    | ToggleHide Int
    | ToggleHistory Int
    | HistoryEvent Int History.Event
    | DraggableEvent Int Draggable.Event
    | CommuteStart Int Int Int (List Float) (Float, Float) -- eqNum root currentIndex midpoints position
    | CommuteContinue Int (Float, Float)
    | CommuteEnd Int Float (Float, Float)

type alias SelectedNode =
    {   eq: Int
    ,   root: Int
    ,   tree: FullEquation
    ,   selected: Set.Set Int
    ,   nodes: Set.Set Int
    }

newEntry_: Animation.Tracker -> Int -> Int -> FullEquation -> (Entry, Animation.Tracker)
newEntry_ tracker size index eq =
    let
        latex = Rules.toLatex eq
        (b, t0) = Bricks.init tracker eq.root
        (m, newT) = MathIcon.init t0 Nothing latex
    in
    (   {   history = History.init (eq, latex)
        ,   view = (False, createDraggable_ size index index)
        ,   showHistory = False
        ,   show = True
        ,   ui = (b, m)
        ,   commuting = Nothing
        }
    ,   newT
    )

anyVisible: Model -> Bool
anyVisible model = Dict.foldl (\_ value -> (||) value.show) False model.equations

init: (Bool -> String -> Encode.Value -> Cmd Event) -> (List FullEquation -> Cmd Event) -> (Int -> Cmd Event) ->
    Animation.Tracker -> List FullEquation -> (Model, Animation.Tracker)
init setCapture updateQuery svgMouseCmd tracker l =
    let
        size = List.length l
        (eqList, newTracker) = List.foldl (\eq (list, t) ->
                let
                    index = List.length list
                    (newEntry, newT) = newEntry_ t size index eq
                in
                    ((index, newEntry) :: list, newT)
            ) ([], tracker) l
    in
        (   {   equations = Dict.fromList eqList
            ,   selected = Nothing
            ,   nextEquationNum = size
            ,   createModeForEquation = Nothing
            ,   setCapture = setCapture
            ,   updateQuery = updateQuery
            ,   svgMouseCmd = svgMouseCmd
            }
        ,   newTracker
        )

createDraggable_: Int -> Int -> Int -> Draggable.Model
createDraggable_ numVisible index eqNum = let indHeight = 100.0 / toFloat numVisible in
    Draggable.init ("Equation-" ++ String.fromInt eqNum) (25,indHeight * (toFloat index) + 1.0) (50,indHeight - 2.0)

add: Animation.Tracker -> FullEquation -> Model -> (Model, Animation.Tracker)
add tracker eq model = let (newEntry, newTracker) = newEntry_ tracker (model.nextEquationNum + 1) model.nextEquationNum eq in
    (   {   model
        |   nextEquationNum = model.nextEquationNum + 1
        ,   equations = Dict.insert model.nextEquationNum newEntry model.equations
        }
        |> updatePositions_
    ,   newTracker
    )

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
        |> Maybe.map (.history >> History.current >> Tuple.first)
        |> Maybe.andThen (\eq -> Matcher.selectedSubtree ids eq |> Result.toMaybe
            |> Maybe.map (\(root, nodes) -> {eq = eqNum, root = root, nodes = nodes, selected = ids, tree = eq})
        )
    )

updateBricks: Animation.Tracker -> Entry -> (Entry, Animation.Tracker)
updateBricks tracker entry =
    let
        (eq, latex) = History.current entry.history
        (b, m) = entry.ui
        (newB, t0) = Bricks.updateTree tracker eq.root b
        (newW, newT) = MathIcon.set t0 Nothing latex m
    in
        ({entry | ui = (newB, newW)}, newT)

advanceTime: Float -> Model -> Model
advanceTime millis model =
    {   model
    |   equations = Dict.map (\_ entry -> let (b, m) = entry.ui in
            {entry | ui = (Bricks.advanceTime millis b, MathIcon.advanceTime millis m)}
        ) model.equations
    }

undo: Animation.Tracker -> Model -> Result String (Model, Animation.Tracker)
undo tracker model = case model.selected of
    Nothing -> Err "Equation not found to undo"
    Just (eqNum, selected) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation not found to undo"
        Just entry -> let newHis = History.undo entry.history in
            let (newEntry, newT) = updateBricks tracker {entry | history = newHis} in
            Ok
            (   {   model
                |   equations = Dict.insert eqNum newEntry model.equations
                ,   selected = Just (eqNum, newSelectedNodes_ selected (History.current newHis |> Tuple.first))
                }
            ,   newT
            )

redo: Animation.Tracker -> Model -> Result String (Model, Animation.Tracker)
redo tracker model = case model.selected of
    Nothing -> Err "Equation not found to redo"
    Just (eqNum, selected) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation not found to redo"
        Just entry -> let newHis = History.redo entry.history in
            let (newEntry, newT) = updateBricks tracker {entry | history = newHis} in
            Ok
            (   {   model
                |   equations = Dict.insert eqNum newEntry model.equations
                ,   selected = Just (eqNum, newSelectedNodes_ selected (History.current newHis |> Tuple.first))
                }
            ,   newT
            )

updateQueryCmd: Animation.Tracker -> Model -> (Model, Animation.Tracker, Cmd Event)
updateQueryCmd t model =
    (   model
    ,   t
    ,   Dict.values model.equations
        |> List.filter (\entry -> entry.show)
        |> List.map (\entry -> History.current entry.history |> Tuple.first)
        |> model.updateQuery
    )

groupChildren: Animation.Tracker -> Int -> Int -> Set.Set Int -> Model -> Result String (Model, Animation.Tracker)
groupChildren tracker eqNum root children model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry -> History.current entry.history |> Tuple.first
        |> Matcher.groupSubtree root children
        |> Result.map (\(newSelect, newEq) ->
            let
                (newEntry, newTracker) = updateBricks tracker {entry | history = History.add (newEq, Rules.toLatex newEq) entry.history}
            in
            (   {   model
                |   equations = Dict.insert eqNum newEntry model.equations
                ,   selected = Just (eqNum, Set.singleton newSelect)
                }
            ,   newTracker
            )
        )

ungroupChildren: Animation.Tracker -> Int -> Int -> Model -> Result String (Model, Animation.Tracker)
ungroupChildren tracker eqNum id model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry -> History.current entry.history
        |> Tuple.first
        |> Matcher.ungroupSubtree id
        |> Result.map (\(newID,newEq) ->
            let
                (newEntry, newTracker) = updateBricks tracker {entry | history = History.add (newEq, Rules.toLatex newEq) entry.history}
            in
                (   {   model
                    |   equations = Dict.insert eqNum newEntry model.equations
                    ,   selected = Just (eqNum, Set.singleton newID)
                    }
                ,   newTracker
                )
            )

replaceNumber: Animation.Tracker -> Int -> Int -> Float -> Math.Tree (Maybe Rules.FunctionProp) -> Model -> Result String (Model, Animation.Tracker)
replaceNumber tracker eqNum root target replacement model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry -> History.current entry.history
        |> Tuple.first
        |> Matcher.replaceRealNode root target replacement
        |> Result.map (\(newSelect, newEq) ->
            let
                (newEntry, newTracker) = updateBricks tracker {entry | history = History.add (newEq, Rules.toLatex newEq) entry.history}
            in
                (   {   model
                    |   equations = Dict.insert eqNum newEntry model.equations
                    ,   selected = Just (eqNum, Set.singleton newSelect)
                    }
                ,   newTracker
                )
            )

replaceNodeWithNumber: Animation.Tracker -> Int -> Int -> Float -> Model -> Result String (Model, Animation.Tracker)
replaceNodeWithNumber tracker eqNum id number model = case Dict.get eqNum model.equations of
    Nothing -> Err "Equation not found"
    Just entry ->
        let
            replacement = if number < 0
                then Math.UnaryNode {state = (Just Rules.negateProp,Nothing), name = "-", child = Math.RealNode {state = (Nothing, Nothing), value = -number}}
                else Math.RealNode {state = (Nothing, Nothing), value = number}
        in
            History.current entry.history
            |> Tuple.first
            |> Matcher.replaceSubtree (Set.singleton id) replacement Matcher.newResult
            |> Result.map (\(newSelect, newEq) ->
                let
                    (newEntry, newTracker) = updateBricks tracker {entry | history = History.add (newEq, Rules.toLatex newEq) entry.history}
                in
                    (   {   model
                        |   equations = Dict.insert eqNum newEntry model.equations
                        ,   selected = Just (eqNum, Set.singleton newSelect)
                        }
                    ,   newTracker
                    )
                )

transform: Animation.Tracker -> List {a | root: Matcher.Replacement Rules.FunctionProp} -> Matcher.MatchResult Rules.FunctionProp Animation.State -> Model -> Result String (Model, Animation.Tracker)
transform tracker replacement result model = case model.selected of
    Nothing -> Err "No nodes were selected"
    Just (eqNum, ids) -> case Dict.get eqNum model.equations of
        Nothing -> Err "Equation is not found"
        Just entry -> History.current entry.history
            |> Tuple.first
            |> \current -> Helper.resultList (\r (_, others) -> Matcher.replaceSubtree ids r.root result current
                |> Result.map (\(num, newEq) -> (num, (newEq, Rules.toLatex newEq) :: others))
                ) (0, []) replacement
            |> Result.map (\(newSelect, newEq) -> let (newEntry, newTracker) = updateBricks tracker {entry | history = History.addAll (List.reverse newEq) entry.history} in
                (   {   model
                    |   equations = Dict.insert eqNum newEntry model.equations
                    ,   selected = Just (eqNum, Set.singleton newSelect)
                    }
                ,   newTracker
                )
            )

substitute: Animation.Tracker -> Int -> Set.Set Int -> Int -> Model -> Result String (Model, Animation.Tracker)
substitute tracker origNum selected eqNum model = case Dict.get eqNum model.equations of
    Nothing -> Err "Substitution equation not found"
    Just subEntry -> case Dict.get origNum model.equations of
        Nothing -> Err "Target equation not found"
        Just origEntry -> let origEq = History.current origEntry.history |> Tuple.first in
            Matcher.replaceAllOccurrences selected (History.current subEntry.history |> Tuple.first) origEq
            |> Result.map (\(newSelected, newEq) ->
                let
                    (newEntry, newTracker) = updateBricks tracker {origEntry | history = History.add (newEq,Rules.toLatex newEq) origEntry.history}
                in
                    (   {   model
                        |   selected = Just (origNum, newSelected)
                        ,   equations = Dict.insert origNum newEntry model.equations
                        }
                    ,   newTracker
                    )
                )

updateSelected_: Int -> Int -> Bool -> Model -> Model
updateSelected_ eq node combine model = case (combine, model.selected) of
    (True, Just (e, current)) -> if e /= eq
        then {model | selected = Just (eq, Set.singleton node)}
        else if Set.member node current
        then let newSet = Set.remove node current in
            if Set.isEmpty newSet then {model | selected = Nothing}
            else {model | selected = Just (eq, newSet)}
        else {model | selected = Just (eq, Set.insert node current)}
    _ -> {model | selected = Just (eq, Set.singleton node)}

refresh: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Animation.Tracker -> Model -> (Model, Animation.Tracker)
refresh dict tracker model = Dict.foldl (\key entry (nextDict, t) -> let eq = History.current entry.history |> Tuple.first in
        (   case Matcher.refreshFuncProp dict eq of
                Nothing -> (entry, t)
                Just newEq -> updateBricks t {entry | history = History.add (newEq, Rules.toLatex newEq) entry.history}
        )
        |> \(newEntry, newT) -> (Dict.insert key newEntry nextDict, newT)
    )
    (model.equations, tracker)
    model.equations
    |> \(eqs, newT) -> ({model | equations = eqs}, newT)

update: Draggable.Size -> Animation.Tracker -> Event -> Model -> (Model, Animation.Tracker, Cmd Event)
update size tracker event model = case event of
    Select eq node -> updateSelected_ eq node False model
        |> \newModel -> (newModel, tracker, Cmd.none)
    Delete eq -> updatePositions_ {model | equations = Dict.remove eq model.equations} |> updateQueryCmd tracker
    ToggleHide eq -> case Dict.get eq model.equations of
        Nothing -> (model, tracker, Cmd.none)
        Just entry -> updatePositions_ {model | equations = Dict.insert eq {entry | show = not entry.show, commuting = Nothing} model.equations} |> updateQueryCmd tracker
    ToggleHistory eq -> case Dict.get eq model.equations of
        Nothing -> (model, tracker, Cmd.none)
        Just entry -> ({model | equations = Dict.insert eq {entry | showHistory = not entry.showHistory} model.equations}, tracker, Cmd.none)
    HistoryEvent eq he -> case Dict.get eq model.equations of
        Nothing ->(model, tracker, Cmd.none)
        Just entry ->
            let
                newHis = History.update he entry.history
                (newEntry, newTracker) = updateBricks tracker {entry | history = newHis}
                newModel = {model | equations = Dict.insert eq newEntry model.equations}
            in
            case model.selected of
                Nothing -> updateQueryCmd newTracker newModel
                Just (eqNum, selected) -> if eqNum /= eq
                    then updateQueryCmd newTracker newModel
                    else updateQueryCmd newTracker {newModel | selected = Just (eqNum, newSelectedNodes_ selected (History.current newHis |> Tuple.first))}
    DraggableEvent eq dEvent -> case Dict.get eq model.equations of
        Nothing -> (model, tracker, Cmd.none)
        Just entry -> Draggable.update size dEvent (entry.view |> Tuple.second)
            |> \(dModel, action) ->
                (   {model | equations = Dict.insert eq {entry | view = (True, dModel)} model.equations}
                ,   tracker
                ,   Maybe.map (\a -> case a of
                        Draggable.SetCapture s v -> model.setCapture True s v
                        Draggable.ReleaseCapture s v -> model.setCapture False s v
                    ) action
                    |> Maybe.withDefault Cmd.none
                )
    CommuteStart eq root index midpoints _ -> case Dict.get eq model.equations of
        Nothing -> (model, tracker, Cmd.none)
        Just entry ->
            (   {   model
                |   equations = Dict.insert eq
                    {   entry
                    |   commuting = Just {id = root, currentIndex = index, originalIndex = index, midpoints = midpoints, moved = False}
                    }
                    model.equations
                }
            ,   tracker
            ,   model.svgMouseCmd eq
            )
    CommuteContinue eq (x, _) -> modifyEntry_ model tracker eq
        (\entry t -> entry.commuting
            -- Maybe {id: Int, currentIndex: Int, originalIndex: Int, moved: Bool, midpoints: List Float}
            |> Maybe.andThen (\n ->
                if n.originalIndex == -1 then Just (entry, t)-- It is not draggable, and we've detected a drag
                else let newIndex = indexFromMidpoints_ n.originalIndex n.midpoints x in
                    if newIndex == n.currentIndex then Nothing
                    else let (b, m) = entry.ui in
                        History.current entry.history
                        |> (\(currentEq, currentLatex) -> case Matcher.setChildIndex n.id newIndex currentEq of
                            Err _ -> (currentEq, currentLatex)
                            Ok newEq -> (newEq, Rules.toLatex newEq)
                        )
                        |> \(newEq, newL) ->
                            let
                                (newB, t0) = Bricks.updateTree t newEq.root b
                                (newM, newT) = MathIcon.set t0 Nothing newL m
                            in
                            Just (   {   entry
                                    |   ui = (newB, newM)
                                    ,   commuting = Just {n | moved = True, currentIndex = newIndex}
                                    }
                                ,   newT
                                )
            )
        )
    CommuteEnd eqNum time (x, _) -> case Dict.get eqNum model.equations of
        Nothing -> (model, tracker, Cmd.none)
        Just entry -> case entry.commuting of
            Nothing -> (model, tracker, Cmd.none)
            Just n -> if n.moved
                then let newIndex = indexFromMidpoints_ n.originalIndex n.midpoints x in
                    (   if newIndex == n.originalIndex
                        then {entry | commuting = Nothing}
                        else History.current entry.history |> Tuple.first
                            |> Matcher.setChildIndex n.id newIndex
                            |> \res -> case res of
                                Err _ -> {entry | commuting = Nothing}
                                Ok newEq -> {entry | commuting = Nothing, history = History.add (newEq, Rules.toLatex newEq) entry.history}
                    )
                    |>\newEntry -> let (finalEntry, finalT) = updateBricks tracker newEntry in
                        updateQueryCmd finalT {model | equations = Dict.insert eqNum finalEntry model.equations}
                else updateSelected_ eqNum n.id (time > longClickThreshold) model
                    |> \newModel -> (newModel, tracker, Cmd.none)

newSelectedNodes_: Set.Set Int -> FullEquation -> Set.Set Int
newSelectedNodes_ selected eq = let intersection = Set.filter (\n -> Dict.member n eq.tracker.parent) selected in
    if Set.isEmpty intersection then Set.singleton (Math.getState eq.root |> Matcher.getID) else intersection

indexFromMidpoints_: Int -> List Float -> Float -> Int
indexFromMidpoints_ original midpoints value =
    let
        segmentIndex m v = case m of
            [] -> 0
            (x::next) -> if v >= x
                then 1 + segmentIndex next v
                else 0
        sIndex = segmentIndex midpoints value
    in
        if sIndex <= original then sIndex else sIndex - 1

modifyEntry_: Model -> Animation.Tracker -> Int -> (Entry -> Animation.Tracker -> Maybe (Entry, Animation.Tracker)) -> (Model, Animation.Tracker, Cmd.Cmd msg)
modifyEntry_ model tracker eqNum process = case Dict.get eqNum model.equations of
    Nothing -> (model, tracker, Cmd.none)
    Just entry -> case process entry tracker of
        Nothing -> (model, tracker, Cmd.none)
        Just (newEntry, t) ->
            ({model | equations = Dict.insert eqNum newEntry model.equations}, t, Cmd.none)

{-
# View-related functions
-}

menu: (Event -> msg) -> Model -> List (Menu.Part msg)
menu convert model = Dict.toList model.equations
    |> List.map (\(num, entry) -> Menu.Content [class "equationMenu"]
        [   a [class "clickable", HtmlEvent.onClick (convert (Delete num))]
            [   Icon.bin []]
        ,   a [class "clickable", HtmlEvent.onClick (convert (ToggleHide num))]
            [   if entry.show then Icon.shown [] else Icon.hidden []
            ,   span [class "space"] []
            ,   History.current entry.history |> Tuple.second
                |> MathIcon.static []
            ]
        ]
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
            (b,m) = entry.ui
        in
            (   dModel.id
            ,   Draggable.div (DraggableEvent eqNum >> converter) dModel [class "equationHolder"]
                [   div [class "equation"]
                    [   MathIcon.view (\id -> List.filterMap identity
                            [   HtmlEvent.onClick (Select eqNum id) |> Just
                            ,   Svg.Attributes.class "selected" |> Helper.maybeGuard (Set.member id highlight)
                            ]
                        ) [Svg.Attributes.class "mathIcons"] m
                        |> Html.map converter
                    ,   Bricks.view (brickAttr_ highlight eqNum) b
                        |> Html.map converter
                    ]
                ,   div [class "historyHolder"]
                    [   Icon.verticalLine []
                    ,   if entry.showHistory
                        then div []
                            [   a [class "clickable", HtmlEvent.onClick (ToggleHistory eqNum |> converter)] [Html.text "Close"]
                            ,   div [class "history"]
                                (   History.serialize (\current index (_,latex) children -> let middle = max 0 (List.length children - 1) in
                                    case List.drop middle children |> List.head of
                                        Nothing ->[historyEntry_ current (History.SelectPast index |> HistoryEvent eqNum |> converter) (MathIcon.static [] latex)]
                                        Just after -> historyEntry_ current (History.SelectPast index |> HistoryEvent eqNum |> converter) (MathIcon.static [] latex)
                                            ::  (
                                                List.map (Html.div []) (List.take middle children)
                                                ++ after
                                            )
                                    ) entry.history
                                )
                            ]
                        else a [class "historyButton", class "clickable", HtmlEvent.onClick (ToggleHistory eqNum |> converter)] [Html.text "Show History"]
                    ]
                -- ,   div [class "contextualToolbar"] [text "undo+redo"]  -- TODO: connect ActionView up here
                ]
            )
    )

historyEntry_: Bool -> msg -> Html.Html msg -> Html.Html msg
historyEntry_ current event inner = Html.a
    (   if current then [class "selected"] else [ class "clickable", HtmlEvent.onClick event])
    [inner]

brickAttr_: Set.Set Int -> Int -> Int -> Maybe (Int, List Float) -> List (Html.Attribute Event)
brickAttr_ highlight eqNum id draggable =
    (   case draggable of
            Nothing ->
                [   HtmlEvent.onMouseDown (CommuteStart eqNum id -1 [])
                ]
            Just (originalIndex, midpoints) ->
                [   Svg.Attributes.class "draggable"
                ,   HtmlEvent.onMouseDown (CommuteStart eqNum id originalIndex midpoints)
                ]
    )
    |> \list -> if Set.member id highlight then (Svg.Attributes.class "selected" :: list) else list

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

encodeHistoryState_: (FullEquation, Latex.Model State) -> Encode.Value
encodeHistoryState_ (eq, l) = Encode.object
    [   ("equation", Matcher.encodeEquation Animation.encodeState eq)
    ,   ("latex", Latex.encode (Matcher.encodeState Animation.encodeState) l)
    ]

decoder: (Bool -> String -> Encode.Value -> Cmd Event) -> (List FullEquation -> Cmd Event) -> (Int -> Cmd Event) ->
    Decode.Decoder (Model, Animation.Tracker)
decoder setCapture updateQuery svgMouseCmd = Decode.map4 (\(eq, t) next sel create -> (Model eq next sel create setCapture svgMouseCmd updateQuery, t))
    (   Decode.field "equations" <| Decode.map addDefaultPositions_ <| Helper.intDictDecoder entryDecoder_)
    (Decode.field "nextEquationNum" Decode.int)
    (   Decode.field "selected"
        <| Decode.maybe <| Decode.map2 Tuple.pair (Decode.field "eq" Decode.int) (Decode.field "nodes" <| Decode.map Set.fromList <| Decode.list Decode.int)
    )
    (   Decode.field "createModeForEquation" <| Decode.maybe Decode.int)

type alias TmpEntry_ =
    {   history: History.Model (FullEquation, Latex.Model State)
    ,   view: Maybe Draggable.Model
    ,   showHistory: Bool
    ,   show: Bool
    }

addDefaultPositions_: Dict.Dict Int TmpEntry_ -> (Dict.Dict Int Entry, Animation.Tracker)
addDefaultPositions_ orig =
    let
        (shown, hidden) = Dict.toList orig |> List.partition (\(_, entry) -> entry.show)
        size = List.length shown
        create t tEntry newView =
            let
                (eq, l) = History.current tEntry.history
                (b, t0) = Bricks.init t eq.root
                (m, newT) = MathIcon.init t0 Nothing l
            in
            (   {   history = tEntry.history
                ,   view = case tEntry.view of
                        Just view -> (True, view)
                        Nothing -> (False, newView)
                ,   ui = (b, m)
                ,   showHistory = tEntry.showHistory
                ,   show = tEntry.show
                ,   commuting = Nothing
                }
            ,   newT
            )
        (hiddenEntries, nextT) = List.foldl (\(eq, entry) (dict, tracker) -> let (newEntry, newTracker) = create tracker entry (createDraggable_ 1 0 eq) in
                (Dict.insert eq newEntry dict, newTracker)
            ) (Dict.empty, -1) hidden
    in
        List.foldl
        (\(eqNum, entry) (dict, index, tracker)-> let (newEntry, newTracker) = create tracker entry (createDraggable_ size index eqNum) in
            ( Dict.insert eqNum newEntry dict , index + 1 , newTracker)
        )
        (hiddenEntries, 0,nextT)
        shown
        |> \(finalEntries, _, finalT) -> (finalEntries, finalT)

entryDecoder_: Decode.Decoder TmpEntry_
entryDecoder_ = Decode.map4 TmpEntry_
    (Decode.field "history" <| History.decoder <| historyStateDecoder_ )
    (Decode.maybe <| Decode.field "view" <| Draggable.decoder)
    (Decode.field "show" <| Decode.bool)
    (Decode.field "showHistory" Decode.bool)

historyStateDecoder_: Decode.Decoder (FullEquation, Latex.Model State)
historyStateDecoder_ = Decode.map2 Tuple.pair
    (Decode.field "equation" <| Matcher.equationDecoder Animation.stateOps Animation.stateDecoder)
    (Decode.field "latex" <| Latex.decoder (Matcher.stateDecoder Animation.stateDecoder))
