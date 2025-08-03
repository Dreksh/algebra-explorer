module UI.Display exposing (
    Model, Event(..), FullEquation, init, update, views, menu,
    anyVisible, undo, redo, updateQueryCmd, refresh,
    add, advanceTime, transform, substitute,
    groupChildren, ungroupChildren, replaceNumber, replaceNodeWithNumber,
    historyCommit, historyReset,
    encode, decoder
    )

import Dict
import Html exposing (Html, a, div, span, text)
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
import Components.Actions as Actions
import UI.Animation as Animation
import UI.Bricks as Bricks
import UI.Draggable as Draggable
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.MathIcon as MathIcon
import UI.Menu as Menu
import UI.SvgDrag as SvgDrag
import UI.ActionView as ActionView

type alias State = Matcher.State Animation.State
type alias FullEquation = Matcher.Equation Rules.FunctionProp Animation.State

type alias Model =
    {   equations: Dict.Dict Int Entry
    ,   nextEquationNum: Int
    ,   selected: Maybe (Int, Set.Set Int)
    ,   actions: Dict.Dict String (List Actions.Action)  -- actions a cache for displaying matched rules
    ,   createModeForEquation: Maybe Int
    -- Command creators
    ,   setCapture: Bool -> String -> Encode.Value -> Cmd Event
    ,   svgMouseCmd: Int -> (Float, Float) -> Cmd Event
    ,   updateQuery: List FullEquation -> Cmd Event
    }

longClickThreshold: Float
longClickThreshold = 300 -- in ms

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
    | HistoryEvent Int (History.Event (FullEquation, Latex.Model State))
    | DraggableEvent Int Draggable.Event
    | MouseDown Int Int Int (List Float) (Float, Float) -- eqNum root currentIndex midpoints position
    | Commute Int SvgDrag.Event

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

init: (Bool -> String -> Encode.Value -> Cmd Event) -> (List FullEquation -> Cmd Event) -> (Int -> (Float, Float) -> Cmd Event) ->
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
            ,   nextEquationNum = size
            ,   selected = Nothing
            ,   actions = Dict.empty
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

updateBricks: Animation.Tracker -> Entry -> (Entry, Animation.Tracker)
updateBricks tracker entry =
    let
        (eq, latex) = History.next entry.history
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
undo tracker model = selectedEquation_ model
    |> Result.andThen (\(eq, selected, entry) ->
        let
            newHis = History.update (History.Stage History.Undo) entry.history |> History.commit
            (newEntry, newT) = updateBricks tracker {entry | history = newHis}
        in
            Ok
            (   {   model
                |   equations = Dict.insert eq newEntry model.equations
                ,   selected = Just (eq, newSelectedNodes_ selected (History.current newHis |> Tuple.first))
                }
            ,   newT
            )
        )

redo: Animation.Tracker -> Model -> Result String (Model, Animation.Tracker)
redo tracker model = selectedEquation_ model
    |> Result.andThen (\(eq, selected, entry) ->
        let
            newHis = History.update (History.Stage History.Redo) entry.history |> History.commit
            (newEntry, newT) = updateBricks tracker {entry | history = newHis}
        in
            Ok
            (   {   model
                |   equations = Dict.insert eq newEntry model.equations
                ,   selected = Just (eq, newSelectedNodes_ selected (History.current newHis |> Tuple.first))
                }
            ,   newT
            )
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

selectedEquation_: Model -> Result String (Int, Set.Set Int, Entry)
selectedEquation_ model = case model.selected of
    Nothing -> Err "No nodes were selected"
    Just (eq, ids) -> case Dict.get eq model.equations of
        Nothing -> Err "Equation is not found"
        Just entry -> Ok (eq, ids, entry)

groupChildren: Animation.Tracker -> Int -> Set.Set Int -> Model -> Result String (Model, Animation.Tracker)
groupChildren tracker root children model = selectedEquation_ model
    |> Result.andThen (\(eq, _, entry) ->
        History.current entry.history
        |> Tuple.first
        |> Matcher.groupSubtree root children
        |> Result.map (\(newSelect, newEq) ->
            let
                (newEntry, newTracker) = updateBricks tracker {entry | history = History.stage (newEq, Rules.toLatex newEq) entry.history}
            in
                (   {   model
                    |   equations = Dict.insert eq newEntry model.equations
                    }
                ,   newTracker
                )
            )
        )

ungroupChildren: Animation.Tracker -> Int -> Model -> Result String (Model, Animation.Tracker)
ungroupChildren tracker root model = selectedEquation_ model
    |> Result.andThen (\(eq, _, entry) ->
        History.current entry.history
        |> Tuple.first
        |> Matcher.ungroupSubtree root
        |> Result.map (\(newID,newEq) ->
            let
                (newEntry, newTracker) = updateBricks tracker {entry | history = History.stage (newEq, Rules.toLatex newEq) entry.history}
            in
                (   {   model
                    |   equations = Dict.insert eq newEntry model.equations
                    }
                ,   newTracker
                )
            )
        )

replaceNumber: Animation.Tracker -> Int -> Float -> Math.Tree (Maybe Rules.FunctionProp) -> Model -> Result String (Model, Animation.Tracker)
replaceNumber tracker root target replacement model = selectedEquation_ model
    |> Result.andThen (\(eq, _, entry) ->
        History.current entry.history
        |> Tuple.first
        |> Matcher.replaceRealNode root target replacement
        |> Result.map (\(newSelect, newEq) ->
            let
                (newEntry, newTracker) = updateBricks tracker {entry | history = History.stage (newEq, Rules.toLatex newEq) entry.history |> History.commit}
            in
                (   {   model
                    |   equations = Dict.insert eq newEntry model.equations
                    ,   selected = Just (eq, Set.singleton newSelect)
                    }
                ,   newTracker
                )
            )
        )

replaceNodeWithNumber: Animation.Tracker -> Int -> Float -> Model -> Result String (Model, Animation.Tracker)
replaceNodeWithNumber tracker root number model = selectedEquation_ model
    |> Result.andThen (\(eq, _, entry) ->
        let
            replacement = if number < 0
                then Math.UnaryNode {state = (Just Rules.negateProp,Nothing), name = "-", child = Math.RealNode {state = (Nothing, Nothing), value = -number}}
                else Math.RealNode {state = (Nothing, Nothing), value = number}
        in
            History.current entry.history
            |> Tuple.first
            |> Matcher.replaceSubtree (Set.singleton root) replacement Matcher.newResult
            |> Result.map (\(newSelect, newEq) ->
                let
                    (newEntry, newTracker) = updateBricks tracker {entry | history = History.stage (newEq, Rules.toLatex newEq) entry.history}
                in
                    (   {   model
                        |   equations = Dict.insert eq newEntry model.equations
                        }
                    ,   newTracker
                    )
                )
        )

transform: Animation.Tracker -> List {a | root: Matcher.Replacement Rules.FunctionProp} -> Matcher.MatchResult Rules.FunctionProp Animation.State -> Model -> Result String (Model, Animation.Tracker)
transform tracker replacement result model = selectedEquation_ model
    |> Result.andThen (\(eq, ids, entry) ->
        History.current entry.history
        |> Tuple.first
        |> \current -> Helper.resultList (\r (_, others) -> Matcher.replaceSubtree ids r.root result current
            |> Result.map (\(num, newEq) -> (num, (newEq, Rules.toLatex newEq) :: others))
            ) (0, []) replacement
        |> Result.map (\(newSelect, newEq) ->
            let
                (newEntry, newTracker) = updateBricks tracker {entry | history = History.stageMany (List.reverse newEq) entry.history}
            in
                (   {   model
                    |   equations = Dict.insert eq newEntry model.equations
                    -- ,   selected = Just (eq, Set.singleton newSelect)
                    }
                ,   newTracker
                )
            )
        )

substitute: Animation.Tracker -> Int -> Model -> Result String (Model, Animation.Tracker)
substitute tracker eqSub model = case Dict.get eqSub model.equations of
    Nothing -> Err "Substitution equation not found"
    Just subEntry -> selectedEquation_ model
        |> Result.andThen (\(origNum, selected, origEntry) ->
            let origEq = History.current origEntry.history |> Tuple.first
            in Matcher.replaceAllOccurrences selected (History.current subEntry.history |> Tuple.first) origEq
                |> Result.map (\(newSelected, newEq) ->
                    let
                        (newEntry, newTracker) = updateBricks tracker {origEntry | history = History.stage (newEq, Rules.toLatex newEq) origEntry.history |> History.commit}
                    in
                        (   {   model
                            |   equations = Dict.insert origNum newEntry model.equations
                            ,   selected = Just (origNum, newSelected)
                            }
                        ,   newTracker
                        )
                    )
            )

historyCommit: Model -> Result String Event
historyCommit model = selectedEquation_ model
    |> Result.andThen (\(eq, _, entry) -> case entry.history.staged of
        Nothing -> Err "Nothing staged to commit"
        _ -> Ok (HistoryEvent eq History.Commit)
        )

historyReset: Model -> Result String Event
historyReset model = selectedEquation_ model
    |> Result.andThen (\(eq, _, entry) -> case entry.history.staged of
        Nothing -> Err "Nothing staged to reset"
        _ -> Ok (HistoryEvent eq History.Reset)
        )

updateSelected_: Int -> Int -> Bool -> Rules.Model -> Model -> Model
updateSelected_ eq node combine rules model =
    let
        selected = case (combine, model.selected) of
            (True, Just (e, current)) -> if e /= eq
                then Just (eq, Set.singleton node)
                else if Set.member node current
                then let newSet = Set.remove node current in
                    if Set.isEmpty newSet then Nothing
                    else Just (eq, newSet)
                else Just (eq, Set.insert node current)
            _ -> Just (eq, Set.singleton node)

        actions = updateActions_ selected rules model.equations
    in
        { model | selected = selected, actions = actions }

updateActions_: Maybe (Int, Set.Set Int) -> Rules.Model -> Dict.Dict Int Entry -> Dict.Dict String (List Actions.Action)
updateActions_ selected rules equations =
    let
        selection = selected
            |> Maybe.andThen (\(eq, ids) -> Dict.get eq equations
                |> Maybe.map (.history >> History.current >> Tuple.first)
                |> Maybe.andThen (\fullEq -> Matcher.selectedSubtree ids fullEq |> Result.toMaybe
                    |> Maybe.map (\(root, nodes) -> {tree = fullEq, root = root, nodes = nodes})
                )
            )
    in
        Actions.matchRules rules (Dict.size equations) selection

retainSelected: Maybe (Int, Set.Set Int) -> Entry -> Maybe (Int, Set.Set Int)
retainSelected prevSelected newEntry = case prevSelected of
    Nothing -> Nothing
    Just (eq, ids) -> newEntry.history
        |> History.current
        |> Tuple.first
        |> .root
        |> matchPrevIDs ids Set.empty
        |> \matched -> Just (eq, matched)

matchPrevIDs: Set.Set Int -> Set.Set Int -> Math.Tree (Matcher.State Animation.State) -> Set.Set Int
matchPrevIDs prevIDs matchedIDs node =
    let
        state = Math.getState node
        id = Matcher.getID state
        prevID = Matcher.getState state |> .prevID
        newMatchedIDs = if Set.member prevID prevIDs || Set.member id prevIDs
            then Set.insert id matchedIDs
            else matchedIDs
    in
        Math.getChildren node |> List.foldl (\child foldIDs ->
            matchPrevIDs prevIDs foldIDs child
        ) newMatchedIDs


refresh: Rules.Model -> Animation.Tracker -> Model -> (Model, Animation.Tracker)
refresh rules tracker model =
    let
        dict = Rules.functionProperties rules
        actions = Actions.matchRules rules (Dict.size model.equations) Nothing
    in
        Dict.foldl (\key entry (nextDict, t) ->
            let eq = History.current entry.history |> Tuple.first in
            (   case Matcher.refreshFuncProp dict eq of
                    Nothing -> (entry, t)
                    Just newEq -> updateBricks t {entry | history = History.stage (newEq, Rules.toLatex newEq) entry.history}
            )
            |> \(newEntry, newT) -> (Dict.insert key newEntry nextDict, newT)
        )
        (model.equations, tracker)
        model.equations
        |> \(eqs, newT) -> ({model | equations = eqs, actions = actions}, newT)

update: Draggable.Size -> Animation.Tracker -> Rules.Model -> Event -> Model -> (Model, Animation.Tracker, Cmd Event)
update size tracker rules event model = let default = (model, tracker, Cmd.none) in case event of
    Select eq node -> updateSelected_ eq node False rules model
        |> \newModel -> (newModel, tracker, Cmd.none)
    Delete eq -> updatePositions_ {model | equations = Dict.remove eq model.equations} |> updateQueryCmd tracker
    ToggleHide eq -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry -> updatePositions_ {model | equations = Dict.insert eq {entry | show = not entry.show, commuting = Nothing} model.equations} |> updateQueryCmd tracker
    ToggleHistory eq -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry -> ({model | equations = Dict.insert eq {entry | showHistory = not entry.showHistory} model.equations}, tracker, Cmd.none)
    HistoryEvent eq he -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry ->
            let
                newHis = History.update he entry.history
                (newEntry, newTracker) = updateBricks tracker {entry | history = newHis}
                newEquations = Dict.insert eq newEntry model.equations
                newSelected = case he of
                    History.Commit -> retainSelected model.selected newEntry
                    _ -> model.selected
                newActions = updateActions_ newSelected rules newEquations
            in
                updateQueryCmd newTracker {model | equations = newEquations, selected = newSelected, actions = newActions}
    DraggableEvent eq dEvent -> case Dict.get eq model.equations of
        Nothing -> default
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
    MouseDown eq root index midpoints point -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry ->
            (   {   model
                |   equations = Dict.insert eq
                    {   entry
                    |   commuting = Just {id = root, currentIndex = index, originalIndex = index, midpoints = midpoints, moved = False}
                    }
                    model.equations
                }
            ,   tracker
            ,   model.svgMouseCmd eq point
            )
    Commute eqNum dragEvent -> case dragEvent of
        SvgDrag.Start _ -> default -- Handled in MouseDown
        SvgDrag.Move _ (x, _) -> case Dict.get eqNum model.equations of
            Nothing -> default
            Just entry -> case entry.commuting of
                Nothing -> default
                -- Maybe {id: Int, currentIndex: Int, originalIndex: Int, moved: Bool, midpoints: List Float}
                Just n -> if n.originalIndex == -1
                    then default  -- It is not draggable, and we've detected a drag
                    else let newIndex = indexFromMidpoints_ n.originalIndex n.midpoints x in
                        if newIndex == n.currentIndex
                        then default
                        else let (b, m) = entry.ui in
                            History.next entry.history
                            |> \(currentEq, currentLatex) -> (Matcher.setChildIndex n.id newIndex currentEq, currentLatex)
                            |> \(newEq, newL) -> case newEq of
                                Err _ -> default
                                Ok commutedEq ->
                                    let
                                        commutedL = Rules.toLatex commutedEq
                                        (newEntry, newTracker) = updateBricks tracker {entry | history = History.stage (commutedEq, commutedL) entry.history}
                                        commutingEntry = { newEntry | commuting = Just {n | moved = True, currentIndex = newIndex} }
                                    in
                                        (   {   model
                                            |   equations = Dict.insert eqNum commutingEntry model.equations
                                            -- ,   selected = Just (eqNum, Set.singleton newSelect)
                                            }
                                        ,   newTracker
                                        ,   Cmd.none
                                        )

        SvgDrag.End time (x, _) -> case Dict.get eqNum model.equations of
            Nothing -> default
            Just entry -> case entry.commuting of
                Nothing -> default
                Just n -> if n.moved
                    then update size tracker rules (HistoryEvent eqNum History.Commit) model
                    -- TODO: either check distance mouse moved before or if mouse is still over block before selecting
                    else updateSelected_ eqNum n.id (time > longClickThreshold) rules model
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

views: (Event -> msg) -> (Actions.Event -> msg) -> Model -> List (String, Html msg)
views converter actionConvert model = Dict.toList model.equations
    |> List.filter (\(_,entry) -> entry.show)
    |> List.map (\(eqNum, entry) ->
        let
            (_, dModel) = entry.view
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
                                        Nothing ->[historyEntry_ converter current eqNum index (MathIcon.static [] latex)]
                                        Just after -> historyEntry_ converter current eqNum index (MathIcon.static [] latex)
                                            ::  (
                                                List.map (Html.div []) (List.take middle children)
                                                ++ after
                                            )
                                    ) entry.history
                                )
                            ]
                        else a [class "historyButton", class "clickable", HtmlEvent.onClick (ToggleHistory eqNum |> converter)] [Html.text "Show History"]
                    ]
                ,   div [class "contextualToolbar"]
                    ([  div [class "contextualTopic"]
                        [   div
                            ([ class "contextualAction" ] ++ (if History.canUndo entry.history then
                                [   class "clickable"
                                ,   HtmlEvent.onMouseEnter (HistoryEvent eqNum (History.Stage History.Undo) |> converter)
                                ,   HtmlEvent.onMouseLeave (HistoryEvent eqNum History.Reset |> converter)
                                ,   HtmlEvent.onClick (HistoryEvent eqNum History.Commit |> converter)
                                -- TODO: allow user to undo a bunch of times without another mouseEnter
                                --   maybe just skip the preview and allow direct commit in that case
                                -- TODO: make the blocks look see-through or something to show it being committed
                                ]
                                else [class "contextualDisabled"]
                            ))
                            [text "Undo"]  -- TODO: make these icons instead
                        ,   div
                            ([ class "contextualAction" ] ++ (if History.canRedo entry.history then
                                [   class "clickable"
                                ,   HtmlEvent.onMouseEnter (HistoryEvent eqNum (History.Stage History.Redo) |> converter)
                                ,   HtmlEvent.onMouseLeave (HistoryEvent eqNum History.Reset |> converter)
                                ,   HtmlEvent.onClick (HistoryEvent eqNum History.Commit |> converter)
                                ]
                                else [class "contextualDisabled"]
                            ))
                            [text "Redo"]
                        ]
                    ] ++ if Set.isEmpty highlight then [] else ActionView.contextualActions actionConvert model.actions)
                ]
            )
    )

historyEntry_: (Event -> msg) -> Bool -> Int -> Int -> Html.Html msg -> Html.Html msg
historyEntry_ converter current eqNum index inner = Html.a
    (   if current
        then [class "selected"]
        else
            [   class "clickable"
            ,   HtmlEvent.onMouseEnter (HistoryEvent eqNum (History.Stage (History.Revert index)) |> converter)
            ,   HtmlEvent.onMouseLeave (HistoryEvent eqNum History.Reset |> converter)
            ,   HtmlEvent.onClick (HistoryEvent eqNum History.Commit |> converter)
            ]
    )
    [inner]

brickAttr_: Set.Set Int -> Int -> Int -> Maybe (Int, List Float) -> List (Html.Attribute Event)
brickAttr_ highlight eqNum id draggable =
    (   case draggable of
            Nothing ->
                [   HtmlEvent.onMouseDown (MouseDown eqNum id -1 [])
                ]
            Just (originalIndex, midpoints) ->
                [   Svg.Attributes.class "commutable"
                ,   HtmlEvent.onMouseDown (MouseDown eqNum id originalIndex midpoints)
                ]
    )
    |> \list -> if Set.member id highlight then (Svg.Attributes.class "selected" :: list) else list

encode: Model -> Encode.Value
encode model = Encode.object
    [   (   "equations", Encode.dict String.fromInt encodeEntry_ model.equations)
    ,   (   "nextEquationNum", Encode.int model.nextEquationNum)
    -- TODO: add back .selected along with .actions
    -- ,   (   "selected"
    --     ,   case model.selected of
    --         Nothing -> Encode.null
    --         Just (eq, nodes) -> Encode.object
    --             [   ("eq", Encode.int eq)
    --             ,   ("nodes", Encode.set Encode.int nodes)
    --             ]
    --     )
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

decoder: (Bool -> String -> Encode.Value -> Cmd Event) -> (List FullEquation -> Cmd Event) -> (Int -> (Float, Float) -> Cmd Event) ->
    Decode.Decoder (Model, Animation.Tracker)
decoder setCapture updateQuery svgMouseCmd = Decode.map3 (\(eq, t) next create -> (Model eq next Nothing Dict.empty create setCapture svgMouseCmd updateQuery, t))
    (   Decode.field "equations" <| Decode.map addDefaultPositions_ <| Helper.intDictDecoder entryDecoder_)
    (   Decode.field "nextEquationNum" Decode.int)
    -- TODO: add back .selected along with .actions
    -- (   Decode.field "selected"
    --     <| Decode.maybe <| Decode.map2 Tuple.pair (Decode.field "eq" Decode.int) (Decode.field "nodes" <| Decode.map Set.fromList <| Decode.list Decode.int)
    -- )
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
