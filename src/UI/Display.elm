module UI.Display exposing (
    Model, Event(..), FullEquation, init, update, views, menu, updateSubactions,
    anyVisible, undo, redo, updateQueryCmd, previewOnHover, suspendHoverCmd, refresh,
    add, advanceTime, transform, substitute, commit, reset,
    partitionNumber, evaluateToNumber,
    encode, decoder
    )

import Dict
import Html exposing (Html, a, div, span)
import Html.Attributes exposing (class, style)
import Html.Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Set
import Svg.Attributes
import Task
import Process
-- Ours
import Helper
import Algo.History as History
import Algo.Math as Math
import Algo.Matcher as Matcher
import Components.Latex as Latex
import Components.Rules as Rules
import UI.Actions as Actions
import UI.Animation as Animation
import UI.Bricks as Bricks
import UI.Draggable as Draggable
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.MathIcon as MathIcon
import UI.Menu as Menu
import UI.SvgDrag as SvgDrag

type alias State = Matcher.State Animation.State
type alias FullEquation = Matcher.Equation Rules.FunctionProp Animation.State

type alias Model =
    {   equations: Dict.Dict Int Entry
    ,   nextEquationNum: Int
    ,   selected: Set.Set Int
    ,   staged: Set.Set Int
    ,   actions: List (String, (List Actions.Action))  -- actions a cache for displaying matched rules
    ,   subactions: List Actions.MatchedRule  -- for displaying multiple matches with same name
    ,   hoverSuspensions: Int  -- whether or not to preview on hover
    ,   enterSuspended: Bool  -- whether or not to unsuspend preview on enter
    ,   recencyList: List Int
    -- Command creators
    ,   setCapture: String -> Encode.Value -> Cmd Event
    ,   svgMouseCmd: Int -> Encode.Value -> (Float, Float) -> Cmd Event
    ,   updateQuery: List FullEquation -> Cmd Event
    }

longClickThreshold: Float
longClickThreshold = 300 -- in ms

type alias UIModel = (Bricks.Model, MathIcon.Model State)

type alias Entry =
    {   history: History.Model (FullEquation, Latex.Model State) -- latex model a cache for displaying history
    ,   view: (Bool, Draggable.Model)
    ,   showHistory: Bool
    ,   show: Bool
    ,   ui: UIModel
    ,   grabbing: Maybe Grab
    ,   toolbarHeight: Animation.EaseState Float
    ,   subtoolbarHeight: Animation.EaseState Float
    }

type alias Grab =
    {   id: Int
    ,   commutable: Maybe (Int, List Float)
    ,   groupable: Maybe (Float, Float)
    ,   ungroupable: Maybe Float
    ,   moved: GrabState
    }

type GrabState =
    Initial
    | NoOp
    | Commuting Int  -- new nthChild index
    | Grouping Bool  --  left or Right
    | Ungrouping

type Event =
    Select Int Int
    | Delete Int
    | ToggleHide Int
    | ToggleHistory Int
    | HistoryEvent Int (History.Event (FullEquation, Latex.Model State))
    | DraggableEvent Int Draggable.Event
    | PointerDown Int Grab Encode.Value (Float, Float) -- eqNum root currentIndex midpoints position
    | PointerDrag Int SvgDrag.Event
    | UnsuspendHover Bool -- force or not
    | UnsuspendEnter

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
        ,   grabbing = Nothing
        ,   toolbarHeight = Animation.newEaseFloat toolbarSmoothTime_ 0
        ,   subtoolbarHeight = Animation.newEaseFloat toolbarSmoothTime_ 0
        }
    ,   newT
    )

anyVisible: Model -> Bool
anyVisible model = Dict.foldl (\_ value -> (||) value.show) False model.equations

init: (String -> Encode.Value -> Cmd Event) -> (List FullEquation -> Cmd Event)
    -> (Int -> Encode.Value -> (Float, Float) -> Cmd Event)
    -> Animation.Tracker -> List FullEquation -> (Model, Animation.Tracker)
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
            ,   selected = Set.empty
            ,   staged = Set.empty
            ,   actions = []
            ,   subactions = []
            ,   hoverSuspensions = 0
            ,   enterSuspended = False
            ,   recencyList = []
            ,   setCapture = setCapture
            ,   updateQuery = updateQuery
            ,   svgMouseCmd = svgMouseCmd
            }
        ,   newTracker
        )
        |> (\(m, t) -> case eqList |> List.reverse |> List.head |> Maybe.map Tuple.first of
            Nothing -> (m, t)
            Just idx -> updateEqOrder_ t idx m
            )

createDraggable_: Int -> Int -> Int -> Draggable.Model
createDraggable_ numVisible index eqNum = let indHeight = 100.0 / toFloat numVisible in
    Draggable.init ("Equation-" ++ String.fromInt eqNum) (10,indHeight * (toFloat index) + 1.0) (80,indHeight - 2.0)

add: Animation.Tracker -> FullEquation -> Model -> (Model, Animation.Tracker)
add tracker eq model = let (newEntry, newTracker) = newEntry_ tracker (model.nextEquationNum + 1) model.nextEquationNum eq in
    {   model
    |   nextEquationNum = model.nextEquationNum + 1
    ,   equations = Dict.insert model.nextEquationNum newEntry model.equations
    }
    |> updateEqOrder_ tracker model.nextEquationNum
    |> \(m, t) -> (updatePositions_ m, tracker)

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
        (newM, newT) = MathIcon.set t0 Nothing latex m
    in
        ({entry | ui = (newB, newM)}, newT)

advanceTime: Float -> Model -> Model
advanceTime millis model =
    {   model
    |   equations = Dict.map (\_ entry -> let (b, m) = entry.ui in
            {   entry
            |   ui = (Bricks.advanceTime millis b, MathIcon.advanceTime millis m)
            ,   toolbarHeight = Animation.advance millis entry.toolbarHeight
            ,   subtoolbarHeight = Animation.advance millis entry.subtoolbarHeight
            }
        ) model.equations
    }

selectedEquation_: Model -> Result String (Int, Set.Set Int, Entry)
selectedEquation_ model = case List.head model.recencyList of
    Nothing -> Err "no equations to select"
    Just eq -> case Dict.get eq model.equations of
        Nothing -> Err "Equation is not found, invariant violated!"
        Just entry -> Ok (eq, model.selected, entry)

stageChangeNewIds: Animation.Tracker -> Int -> Set.Set Int -> Set.Set Int -> History.Model (FullEquation, Latex.Model State) -> Entry -> Model -> (Model, Animation.Tracker)
stageChangeNewIds tracker eq ids newIds newHistory entry model =
    let
        (newEntry, newT) = updateBricks tracker {entry | history = newHistory}
    in
        (   {   model
            |   equations = Dict.insert eq newEntry model.equations
            ,   staged = newIds
            }
        ,   newT
        )

stageChange: Animation.Tracker -> Int -> Set.Set Int -> History.Model (FullEquation, Latex.Model State) -> Entry -> Model -> (Model, Animation.Tracker)
stageChange tracker eq ids newHistory entry model =
    let newIds = (matchPrevIDs ids (History.next newHistory |> Tuple.first))
    in stageChangeNewIds tracker eq ids newIds newHistory entry model

undoOrRedo_: Bool -> Animation.Tracker -> Model -> Result String (Model, Animation.Tracker)
undoOrRedo_ isUndo tracker model = List.head model.recencyList
    |> Maybe.andThen (\eq ->
        Dict.get eq model.equations |> Maybe.map (\entry -> (eq, entry))
        )
    |> Result.fromMaybe ("No equation selected to " ++ (if isUndo then "undo" else "redo"))
    |> Result.andThen (\(eq, entry) ->
        if (if isUndo then History.canUndo else History.canRedo) entry.history
        then Ok (eq, entry)
        else Err ("Nothing to " ++ (if isUndo then "undo" else "redo"))
        )
    |> Result.map (\(eq, entry) ->
        let
            newHis = History.update (History.Stage (if isUndo then History.Undo else History.Redo)) entry.history
            (newEntry, newT) = updateBricks tracker {entry | history = newHis}
        in
            ({model | equations = Dict.insert eq newEntry model.equations}, newT)
        )

undo = undoOrRedo_ True
redo = undoOrRedo_ False

updateQueryCmd: Model -> Cmd Event
updateQueryCmd model =
    Dict.values model.equations
    |> List.filter (\entry -> entry.show)
    |> List.map (\entry -> History.current entry.history |> Tuple.first)
    |> model.updateQuery

previewOnHover: Model -> Bool
previewOnHover model = model.hoverSuspensions == 0

suspendHoverCooldown: Float
suspendHoverCooldown = 1500

toolbarSmoothTime_: Float
toolbarSmoothTime_ = 500

suspendHoverCmd: Model -> (Model, Cmd Event)
suspendHoverCmd model =
    (   {model | hoverSuspensions = model.hoverSuspensions + 1, enterSuspended = True}
    ,   Cmd.batch
        [   Task.perform (always (UnsuspendHover False)) (Process.sleep suspendHoverCooldown)
        ,   Task.perform (always UnsuspendEnter) (Process.sleep 100)  -- we only need to suspend onPointerEnter instantaneously to prevent an immediate pointerenter event when the DOM shifts underneath
        ]
    )

partitionNumber: Animation.Tracker -> Int -> Float -> Math.Tree (Maybe Rules.FunctionProp) -> Model -> Result String (Model, Animation.Tracker)
partitionNumber tracker root target replacement model = selectedEquation_ model
    |> Result.andThen (\(eq, ids, entry) ->
        History.current entry.history
        |> Tuple.first
        |> Matcher.replaceRealNode root target replacement
        |> Result.map (\(newSelect, newEq) ->
            let newHis = History.update (History.Stage (History.Change (newEq, Rules.toLatex newEq))) entry.history
            in stageChangeNewIds tracker eq ids (Set.singleton newSelect) newHis entry model
            )
        )

evaluateToNumber: Animation.Tracker -> Int -> Float -> Model -> Result String (Model, Animation.Tracker)
evaluateToNumber tracker root number model = selectedEquation_ model
    |> Result.andThen (\(eq, ids, entry) ->
        let
            replacement = if number < 0
                then Math.UnaryNode {state = (Just Rules.negateProp, Nothing), name = "-", child = Math.RealNode {state = (Nothing, Nothing), value = -number}}
                else Math.RealNode {state = (Nothing, Nothing), value = number}
        in
            History.current entry.history
            |> Tuple.first
            |> Matcher.replaceSubtree (Set.singleton root) replacement Matcher.newResult
            |> Result.map (\(newSelect, newEq) ->
                let newHis = History.update (History.Stage (History.Change (newEq, Rules.toLatex newEq))) entry.history
                in stageChangeNewIds tracker eq ids (Set.singleton newSelect) newHis entry model
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
            let newHis = History.update (History.Stage (History.Changes (List.reverse newEq))) entry.history
            in stageChange tracker eq ids newHis entry model
            )
        )

substitute: Animation.Tracker -> FullEquation -> Model -> Result String (Model, Animation.Tracker)
substitute tracker otherEq model = selectedEquation_ model
    |> Result.andThen (\(eqNum, ids, entry) ->
        let origEq = History.current entry.history |> Tuple.first
        in Matcher.replaceAllOccurrences ids otherEq origEq
            |> Result.map (\(newSelect, newEq) ->
                let newHis = History.update (History.Stage (History.Change (newEq, Rules.toLatex newEq))) entry.history
                in stageChangeNewIds tracker eqNum ids newSelect newHis entry model
                )
        )

commit: Animation.Tracker -> Rules.Model -> Model -> Result String (Model, Animation.Tracker)
commit tracker rules model = selectedEquation_ model
    |> Result.map (\(eq, ids, entry) ->
        let
            newHis = History.update History.Commit entry.history
            newEquations = Dict.insert eq {entry | history = newHis} model.equations
            newActions = updateActions_ eq model.staged rules newEquations
        in
            ({model | equations = newEquations, selected = model.staged, staged = Set.empty, actions = newActions, subactions = []}, tracker)
        )

reset: Animation.Tracker -> Model -> Result String (Model, Animation.Tracker)
reset tracker model = selectedEquation_ model
    |> Result.andThen (\(eq, ids, entry) -> case entry.history.staged of
        Nothing -> Err "Nothing staged to reset"
        _ -> Ok (
            let
                newHis = History.update History.Reset entry.history
                (newEntry, newTracker) = updateBricks tracker {entry | history = newHis}
                newEquations = Dict.insert eq newEntry model.equations
            in
                ({model | equations = newEquations, staged = Set.empty}, newTracker)
            )
        )

revert_: Int -> Int -> Rules.Model -> Animation.Tracker -> Model -> Result String (Model, Animation.Tracker)
revert_ eq idx rules tracker model = Dict.get eq model.equations
    |> Maybe.map (\entry ->
        let
            newHis = entry.history |> History.update (History.Stage (History.Revert idx))
            (newEntry, newTracker) = updateBricks tracker {entry | history = newHis}
            newEquations = Dict.insert eq newEntry model.equations
        in
            ({model | equations = newEquations, selected = Set.empty, actions = []}, newTracker)
        )
    |> Result.fromMaybe "Entry being reverted does not exist, invariant violated"

updateSelected_: Int -> Int -> Bool -> Rules.Model -> Model -> Model
updateSelected_ eq node combine rules model =
    let
        newSelected = if combine
            then if Set.member node model.selected
                then Set.remove node model.selected
                else Set.insert node model.selected
            else Set.singleton node

        actions = updateActions_ eq newSelected rules model.equations
    in
        { model | selected = newSelected, actions = actions, subactions = [] }

updateActions_: Int -> Set.Set Int -> Rules.Model -> Dict.Dict Int Entry -> List (String, (List Actions.Action))
updateActions_ eq ids rules entries =
    let
        (selected, others) = entries |> Dict.foldr (\eqNum entry (foldSelect, foldList) ->
            let root = entry.history |> History.current |> Tuple.first
            in if eq == eqNum
                then (Just root, foldList)
                else (foldSelect, root::foldList)
            ) (Nothing, [])
    in
        Actions.matchRules rules ids selected others

updateSubactions: Animation.Tracker -> List Actions.MatchedRule -> Model -> Result String (Model, Animation.Tracker)
updateSubactions tracker matchedRules model = selectedEquation_ model
    |> Result.map (\(eq, ids, entry) ->
        {model | subactions = matchedRules}
        |> easeSubtoolbar_ tracker eq (if List.isEmpty matchedRules then 0 else 1)
        )

matchPrevIDs: Set.Set Int -> FullEquation -> Set.Set Int
matchPrevIDs prevSelected nextEquation =
    let ids = matchPrevIDs_ prevSelected Set.empty nextEquation.root in
    if Set.isEmpty ids then Set.singleton (Math.getState nextEquation.root |> Matcher.getID) else ids

matchPrevIDs_: Set.Set Int -> Set.Set Int -> Math.Tree (Matcher.State Animation.State) -> Set.Set Int
matchPrevIDs_ prevIDs matchedIDs node =
    let
        state = Math.getState node
        id = Matcher.getID state
        prevID = Matcher.getState state |> .prevID
        newMatchedIDs = if Set.member prevID prevIDs || Set.member id prevIDs
            then Set.insert id matchedIDs
            else matchedIDs
    in
        Math.getChildren node |> List.foldl (\child foldIDs ->
            matchPrevIDs_ prevIDs foldIDs child
        ) newMatchedIDs

refresh: Rules.Model -> Animation.Tracker -> Model -> (Model, Animation.Tracker)
refresh rules tracker model =
    let
        dict = Rules.functionProperties rules
        actions = Actions.matchRules rules Set.empty Nothing []
    in
        Dict.foldl (\key entry (nextDict, t) ->
            let eq = History.current entry.history |> Tuple.first in
            (   case Matcher.refreshFuncProp dict eq of
                    Nothing -> (entry, t)
                    Just newEq -> updateBricks t {entry | history = History.init (newEq, Rules.toLatex newEq)}
            )
            |> \(newEntry, newT) -> (Dict.insert key newEntry nextDict, newT)
        )
        (model.equations, tracker)
        model.equations
        |> \(eqs, newT) -> ({model | equations = eqs, actions = actions, selected = Set.empty}, newT)

easeToolbar_: Animation.Tracker -> Int -> Float -> Model -> (Model, Animation.Tracker)
easeToolbar_ tracker num height model = Dict.get num model.equations
    |> Maybe.map (\entry ->
        let (newHeight, newTracker) = Animation.setEase tracker height entry.toolbarHeight
        in ({model | equations = Dict.insert num {entry | toolbarHeight = newHeight} model.equations}, newTracker)
        )
    |> Maybe.withDefault (model, tracker)

easeSubtoolbar_: Animation.Tracker -> Int -> Float -> Model -> (Model, Animation.Tracker)
easeSubtoolbar_ tracker num height model = Dict.get num model.equations
    |> Maybe.map (\entry ->
        let (newHeight, newTracker) = Animation.setEase tracker height entry.subtoolbarHeight
        in ({model | equations = Dict.insert num {entry | subtoolbarHeight = newHeight} model.equations}, newTracker)
        )
    |> Maybe.withDefault (model, tracker)

updateEqOrder_: Animation.Tracker -> Int -> Model -> (Model, Animation.Tracker)
updateEqOrder_ tracker num model = let selEq = List.head model.recencyList in
    if selEq == Just num
    then (model, tracker)
    else { model
        |   recencyList = num :: (List.filter (\n -> n /= num) model.recencyList)
        ,   selected = Set.empty
        ,   staged = Set.empty
        ,   actions = []
        }
        |> easeToolbar_ tracker num 1
        |> (\(m, t) -> case selEq of
            Nothing -> (m, t)
            Just prevFocus ->
                easeToolbar_ t prevFocus 0 m
                |> (\(m2, t2) -> easeSubtoolbar_ t2 prevFocus 0 m2)
            )

deleteEqOrder_: Animation.Tracker -> Int -> Model -> (Model, Animation.Tracker)
deleteEqOrder_ tracker num model =
    let filtered = {model | recencyList = List.filter (\n -> n /= num) model.recencyList} in
    if List.head model.recencyList /= Just num
    then (filtered, tracker)
    else {filtered | selected = Set.empty, staged = Set.empty, actions = []}
        |> (\m -> case List.head filtered.recencyList of
            Nothing -> (m, tracker)
            Just newSelect -> easeToolbar_ tracker newSelect 1 m
            )

update: Draggable.Size -> Animation.Tracker -> Rules.Model -> Event -> Model -> (Model, Animation.Tracker, Cmd Event)
update size tracker rules event model = let default = (model, tracker, Cmd.none) in case event of
    Select eq node -> updateSelected_ eq node False rules model
        |> updateEqOrder_ tracker eq
        |> (\(m, t) -> (m, t, Cmd.none))
    Delete eq -> {model | equations = Dict.remove eq model.equations}
        |> deleteEqOrder_ tracker eq
        |> (\(m, t) -> (updatePositions_ m, t, updateQueryCmd m))
    ToggleHide eq -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry ->
            {   model
            |   equations = Dict.insert eq {entry | show = not entry.show, grabbing = Nothing} model.equations
            }
            |> deleteEqOrder_ tracker eq
            |> (\(m, t) -> (updatePositions_ m, t, updateQueryCmd m))
    ToggleHistory eq -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry ->
            {model | equations = Dict.insert eq {entry | showHistory = not entry.showHistory} model.equations}
            |> updateEqOrder_ tracker eq
            |> (\(m, t) -> (m, t, Cmd.none))
    HistoryEvent eq he ->
        let
            (newModel, newTracker) = updateEqOrder_ tracker eq model
            stage staged = case staged of
                History.Undo -> undo newTracker newModel
                History.Redo -> redo newTracker newModel
                History.Revert idx -> revert_ eq idx rules newTracker newModel
                _ -> Err "Change should only be staged via Actions.Event, invariant violated"
            doCommit (m, t) = commit t rules m

            resModelTracker = case he of
                History.Stage staged -> stage staged
                History.StageAndCommit staged -> stage staged |> Result.andThen doCommit
                History.Reset -> reset newTracker newModel
                History.Commit -> doCommit (newModel, newTracker)

            commitCmd m t = updateQueryCmd m
                |> (\updCmd -> suspendHoverCmd m |> (\(susMod, susCmd) -> (susMod, t, Cmd.batch [updCmd, susCmd])))
        in
            case resModelTracker of
                Err _ -> default
                Ok (m, t) -> case he of
                    History.StageAndCommit _ -> commitCmd m t
                    History.Commit -> commitCmd m t
                    _ -> (m, t, Cmd.none)

    DraggableEvent eq dEvent -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry -> Draggable.update size dEvent (entry.view |> Tuple.second)
            |> \(dModel, action) ->
                {model | equations = Dict.insert eq {entry | view = (True, dModel)} model.equations}
                |> updateEqOrder_ tracker eq
                |> \(m, t) -> (m, t, Maybe.map (\v -> model.setCapture v.id v.pID) action |> Maybe.withDefault Cmd.none)

    PointerDown eq grab pid point -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry ->
            {model | equations = Dict.insert eq { entry | grabbing = Just grab } model.equations}
            |> updateEqOrder_ tracker eq
            |> \(m, t) -> (m, t, model.svgMouseCmd eq pid point)

    PointerDrag eqNum dragEvent -> case dragEvent of
        SvgDrag.Start _ -> default -- Handled in MouseDown
        SvgDrag.Move _ (x, y) -> case Dict.get eqNum model.equations of
            Nothing -> default
            Just entry -> case commuteOrAssociate x y entry of
                Err e -> default
                Ok (newGrabbing, grabbedEq) ->
                    let
                        grabbingEntry = { entry | grabbing = Just newGrabbing }
                        grabbedL = Rules.toLatex grabbedEq
                        newHis = History.update (History.Stage (History.Change (grabbedEq, grabbedL))) grabbingEntry.history
                        newSel = Set.singleton newGrabbing.id
                        -- don't use stageChange because it updates the selected nodes
                        (newEntry, newT) = updateBricks tracker {grabbingEntry | history = newHis}
                        newEquations = Dict.insert eqNum newEntry model.equations
                    in
                        ({model | equations = newEquations, selected = newSel, staged = newSel}, newT, Cmd.none)

        SvgDrag.End _ _ -> case Dict.get eqNum model.equations of
            Nothing -> default
            Just entry -> case entry.grabbing of
                Nothing -> default
                Just grab ->
                    let
                        noGrabModel = {model | equations = Dict.insert eqNum {entry | grabbing = Nothing} model.equations}
                    in case grab.moved of
                        Initial -> updateSelected_ eqNum grab.id False rules noGrabModel
                            |> \newModel -> (newModel, tracker, Cmd.none)
                        NoOp -> reset tracker noGrabModel
                            |> Result.map (\(newModel, newTracker) -> (updateSelected_ eqNum grab.id False rules newModel, newTracker))
                            |> Result.map (\(newModel, newTracker) -> (newModel, newTracker, Cmd.none))
                            |> Result.withDefault default
                        _ -> commit tracker rules noGrabModel
                            |> Result.map (\(newModel, newTracker) -> (newModel, newTracker, updateQueryCmd newModel))
                            |> Result.withDefault default
    UnsuspendHover force -> ({model | hoverSuspensions = if force then 0 else max 0 (model.hoverSuspensions - 1)}, tracker, Cmd.none)
    UnsuspendEnter -> ({model | enterSuspended = False}, tracker, Cmd.none)

commuteOrAssociate: Float -> Float -> Entry -> Result String (Grab, FullEquation)
commuteOrAssociate x y entry = case entry.grabbing of
    Nothing -> Err "node is not grabbing, invariant violated!"
    Just grab -> (case grab.commutable of
        Nothing -> Err "node is not commutable"
        Just (nthChildIndex, midpoints) -> let newIndex = indexFromMidpoints_ nthChildIndex midpoints x in
            if newIndex == nthChildIndex
            then Err "node is not commuting"
            else Ok (Commuting newIndex)
        )
        |> (\commuted -> case commuted of
            Ok c -> Ok c
            Err _ -> case grab.groupable of
                Nothing -> Err "node is not groupable"
                Just (midpoint, top) -> if -y < top  -- need negative because svg y-axis goes downwards
                    then Err "not grabbing above selected block"
                    else Ok (Grouping (x<midpoint))
            )
        |> (\grouped -> case grouped of
            Ok g -> g
            Err _ -> case grab.ungroupable of
                Nothing -> NoOp
                Just bottom -> if -y > bottom  -- need negative because svg y-axis goes downwards
                    then NoOp
                    else Ungrouping
            )
        |> (\newState ->
            let
                newMoved = if grab.moved == Initial && newState == NoOp then Initial else newState
            in (if grab.moved == newMoved
                then Err "grab state has not changed"
                else let (currentEq, _) = History.current entry.history in case newMoved of
                    Initial -> Err "state returned to Initial, invariant violated!"
                    NoOp -> Ok currentEq
                    Commuting newIndex -> Matcher.setChildIndex grab.id newIndex currentEq
                    Grouping left -> Matcher.groupSibling grab.id left currentEq |> Result.map Tuple.second
                    Ungrouping -> Matcher.ungroupSubtree grab.id currentEq |> Result.map Tuple.second
                )
                |> Result.map (\newEq -> ({grab | moved = newMoved}, newEq))
            )

indexFromMidpoints_: Int -> List Float -> Float -> Int
indexFromMidpoints_ original midpoints val =
    let
        segmentIndex m v = case m of
            [] -> 0
            (x::next) -> if v >= x
                then 1 + segmentIndex next v
                else 0
        sIndex = segmentIndex midpoints val
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
views converter actionConvert model =
    let
        orderDict = List.indexedMap (\i num -> (num, i)) model.recencyList
            |> Dict.fromList
        getRelativeIndex i = case Dict.get i orderDict of
            Nothing -> 0
            Just n -> (Dict.size orderDict) - n
    in
    Dict.filter (\_ entry -> entry.show) model.equations
    |> Dict.foldl (\k v -> Dict.insert (getRelativeIndex k, k) (k,v)) Dict.empty -- Reorder
    |> Dict.values
    |> List.map (\(eqNum, entry) ->
        let
            (_, dModel) = entry.view
            eqSelected = List.head model.recencyList |> Maybe.map (\selEq -> selEq == eqNum) |> Maybe.withDefault False
            highlight = if eqSelected then Set.union model.selected model.staged else Set.empty
            staging = entry.history.staged /= Nothing
            grab = entry.grabbing |> Maybe.map .id

            (b,m) = entry.ui
        in
            (   dModel.id
            ,   let dragConvert = DraggableEvent eqNum >> converter in
                Draggable.div dragConvert dModel [class "equationHolder"]
                [   div [class "equation", Svg.Attributes.id ("equation-view-" ++ String.fromInt eqNum)]
                    [   MathIcon.view (\id -> List.filterMap identity
                            [   HtmlEvent.onClick (Select eqNum id) |> Just
                            ,   Svg.Attributes.class "selected" |> Helper.maybeGuard (Set.member id highlight)
                            ]
                        ) [Svg.Attributes.class "mathIcons"] m
                        |> Html.map converter
                    ,   Bricks.view (brickAttr_ highlight staging grab eqNum) b
                        |> Html.map converter
                    ]
                ,   div [class "controls"]
                    [   Draggable.dragElement dragConvert
                    ,   Icon.history [Icon.class "clickable", HtmlEvent.onClick (ToggleHistory eqNum |> converter)]
                    ]
                ,   div ([class "historyHolder", class "hideScrollbar"] ++ if entry.showHistory then [] else [class "closed"])
                    [   Icon.verticalLine []
                    ,   div [class "history", class "hideScrollbar"]
                        (   History.serialize (\current index (_,latex) children -> let middle = max 0 (List.length children - 1) in
                            case List.drop middle children |> List.head of
                                Nothing -> [historyEntry_ converter current eqNum index (MathIcon.static [] latex)]
                                Just after -> historyEntry_ converter current eqNum index (MathIcon.static [] latex)
                                    ::  (
                                        List.map (div []) (List.take middle children)
                                        ++ after
                                    )
                            ) entry.history
                        )
                    ]
                ,   Html.div [class "actionSubtoolbar"]
                    [   Html.div
                        [   class "actions"
                        ,   class "hideScrollbar"
                        ,   style "height" ((String.fromFloat ((Animation.current entry.subtoolbarHeight) * 3)) ++ "rem")
                        ,   HtmlEvent.onPointerEnter (Actions.ShowSubactions model.subactions |> actionConvert)
                        ,   HtmlEvent.onPointerLeave (Actions.HideSubactions |> actionConvert)
                        ]
                        (   Actions.viewSubactions actionConvert (previewOnHover model) (not model.enterSuspended) (UnsuspendHover True |> converter) model.subactions
                        ++ [Html.div [class "space"] []]
                        )
                    ,   Html.div [class "scrollMask"] []
                    ]
                ,   Html.div [class "actionToolbar"]
                    [   Html.Keyed.node "div"
                        [   class "actions"
                        ,   class "hideScrollbar"
                        ,   style "height" ((String.fromFloat ((Animation.current entry.toolbarHeight) * 3)) ++ "rem")
                        ]
                        (if not eqSelected then [] else
                        [   undoOrRedoAction converter eqNum (History.canUndo entry.history) (previewOnHover model) (not model.enterSuspended) True
                        ,   undoOrRedoAction converter eqNum (History.canRedo entry.history) (previewOnHover model) (not model.enterSuspended) False
                        ]
                        ++ Actions.view actionConvert (previewOnHover model) (not model.enterSuspended) (UnsuspendHover True |> converter) model.actions
                        ++ [("space_", Html.div [class "space"] [])]
                        )
                    ,   Html.div [class "scrollMask"] []
                    ]
                ]
            )
        )

undoOrRedoAction: (Event -> msg) -> Int -> Bool -> Bool -> Bool -> Bool -> (String, Html.Html msg)
undoOrRedoAction converter eqNum canUndo previewHover suspendEnter isUndo =
    (   (if isUndo then "undo" else "redo") ++ if previewHover then "-hover" else ""
    ,   div
        (   class "action" :: if canUndo
            then
            (   if previewHover
                then
                [   HtmlEvent.onPointerEnter (HistoryEvent eqNum (History.Stage (if isUndo then History.Undo else History.Redo)) |> converter)
                ,   HtmlEvent.onPointerLeave (HistoryEvent eqNum History.Reset |> converter)
                ,   HtmlEvent.onClick (HistoryEvent eqNum History.Commit |> converter)
                ,   class "clickable"
                ]
                else
                [   HtmlEvent.onClick (HistoryEvent eqNum (History.StageAndCommit (if isUndo then History.Undo else History.Redo)) |> converter)
                ,   class "clickableNoHover"
                ]
                ++  if suspendEnter
                    then [HtmlEvent.onPointerEnter (UnsuspendHover True |> converter)]
                    else []
            )
            else [class "actionDisabled"]
        )
        [div [class "actionButton", class "actionIcon"] [(if isUndo then Icon.undo else Icon.redo) []]]
    )

historyEntry_: (Event -> msg) -> Bool -> Int -> Int -> Html.Html msg -> Html.Html msg
historyEntry_ converter current eqNum index inner = Html.a
    (   if current
        then [class "selected"]
        else
            [   class "clickable"
            ,   HtmlEvent.onClick (HistoryEvent eqNum (History.StageAndCommit (History.Revert index)) |> converter)
            ]
    )
    [inner]

brickAttr_: Set.Set Int -> Bool -> Maybe Int -> Int -> Int -> Maybe (Int, List Float) -> Maybe (Float, Float) -> Maybe Float -> List (Html.Attribute Event)
brickAttr_ highlight staged grabbed eqNum id commutable groupable ungroupable =
    let
        grab = Grab id commutable groupable ungroupable Initial
    in
        List.filterMap identity
        [   HtmlEvent.onPointerCapture identity (PointerDown eqNum grab) |> Just
        ,   Svg.Attributes.class "commutable" |> Helper.maybeGuard (commutable /= Nothing || groupable /= Nothing || ungroupable /= Nothing)
        ,   Svg.Attributes.class "selected" |> Helper.maybeGuard (Set.member id highlight)
        ,   Svg.Attributes.class "staged" |> Helper.maybeGuard staged
        ,   Svg.Attributes.class "grabbed" |> Helper.maybeGuard (grabbed == Just id)
        ]

encode: Model -> Encode.Value
encode model = Encode.object
    [   (   "equations", Encode.dict String.fromInt encodeEntry_ model.equations)
    ,   (   "nextEquationNum", Encode.int model.nextEquationNum)
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

decoder: (String -> Encode.Value -> Cmd Event) -> (List FullEquation -> Cmd Event) -> (Int -> Encode.Value -> (Float, Float) -> Cmd Event) ->
    Decode.Decoder (Model, Animation.Tracker)
decoder setCapture updateQuery svgMouseCmd = Decode.map2 (\(eq, t) next -> (Model eq next Set.empty Set.empty [] [] 0 False [] setCapture svgMouseCmd updateQuery, t))
    (   Decode.field "equations" <| Decode.map addDefaultPositions_ <| Helper.intDictDecoder entryDecoder_)
    (   Decode.field "nextEquationNum" Decode.int)

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
                ,   grabbing = Nothing
                ,   toolbarHeight = Animation.newEaseFloat toolbarSmoothTime_ 0
                ,   subtoolbarHeight = Animation.newEaseFloat toolbarSmoothTime_ 0
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
    (Decode.field "showHistory" Decode.bool)
    (Decode.field "show" <| Decode.bool)

historyStateDecoder_: Decode.Decoder (FullEquation, Latex.Model State)
historyStateDecoder_ = Decode.map2 Tuple.pair
    (Decode.field "equation" <| Matcher.equationDecoder Animation.stateOps Animation.stateDecoder)
    (Decode.field "latex" <| Latex.decoder (Matcher.stateDecoder Animation.stateDecoder))
