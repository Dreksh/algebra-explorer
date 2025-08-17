module UI.Display exposing (
    Model, Event(..), FullEquation, init, update, views, menu,
    anyVisible, undo, redo, updateQueryCmd, refresh,
    add, advanceTime, transform, substitute, commit, reset,
    partitionNumber, evaluateToNumber,
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
import UI.Animation as Animation
import UI.Bricks as Bricks
import UI.Draggable as Draggable
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.MathIcon as MathIcon
import UI.Menu as Menu
import UI.SvgDrag as SvgDrag
import UI.Actions as Actions

type alias State = Matcher.State Animation.State
type alias FullEquation = Matcher.Equation Rules.FunctionProp Animation.State

type alias Model =
    {   equations: Dict.Dict Int Entry
    ,   nextEquationNum: Int
    ,   selected: Maybe (Int, Set.Set Int, Set.Set Int)  -- eq, selected, staged
    ,   actions: List (String, (List Actions.Action))  -- actions a cache for displaying matched rules
    ,   createModeForEquation: Maybe Int
    ,   recencyList: List Int
    -- Command creators
    ,   setCapture: String -> Encode.Value -> Cmd Event
    ,   svgMouseCmd: Int -> Encode.Value -> (Float, Float) -> Cmd Event
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
    ,   grabbing: Maybe Grab
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
            ,   selected = Nothing
            ,   actions = []
            ,   createModeForEquation = Nothing
            ,   recencyList = []
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
        ,   recencyList = model.nextEquationNum :: model.recencyList
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

selectedEquation_: Model -> Result String (Int, Set.Set Int, Entry)
selectedEquation_ model = case model.selected of
    Nothing -> Err "No nodes were selected"
    Just (eq, ids, _) -> case Dict.get eq model.equations of
        Nothing -> Err "Equation is not found"
        Just entry -> Ok (eq, ids, entry)

stageChange: Animation.Tracker -> Int -> Set.Set Int -> History.Model (FullEquation, Latex.Model State) -> Entry -> Model -> (Model, Animation.Tracker)
stageChange tracker eq ids newHistory entry model =
    let
        (newEntry, newT) = updateBricks tracker {entry | history = newHistory}
        newSelected = Just (eq, ids, matchPrevIDs ids (History.next newHistory |> Tuple.first))
    in
        (   {   model
            |   equations = Dict.insert eq newEntry model.equations
            ,   selected = newSelected
            }
        ,   newT
        )

undo: Animation.Tracker -> Model -> Result String (Model, Animation.Tracker)
undo tracker model = selectedEquation_ model
    |> Result.map (\(eq, ids, entry) ->
        let newHis = History.update (History.Stage History.Undo) entry.history
        in stageChange tracker eq ids newHis entry model
        )

redo: Animation.Tracker -> Model -> Result String (Model, Animation.Tracker)
redo tracker model = selectedEquation_ model
    |> Result.map (\(eq, ids, entry) ->
        let newHis = History.update (History.Stage History.Redo) entry.history
        in stageChange tracker eq ids newHis entry model
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

partitionNumber: Animation.Tracker -> Int -> Float -> Math.Tree (Maybe Rules.FunctionProp) -> Model -> Result String (Model, Animation.Tracker)
partitionNumber tracker root target replacement model = selectedEquation_ model
    |> Result.andThen (\(eq, ids, entry) ->
        History.current entry.history
        |> Tuple.first
        |> Matcher.replaceRealNode root target replacement
        |> Result.map (\(newSelect, newEq) ->
            let newHis = History.update (History.Stage (History.Change (newEq, Rules.toLatex newEq))) entry.history
            in stageChange tracker eq ids newHis entry model
            )
        )

evaluateToNumber: Animation.Tracker -> Int -> Float -> Model -> Result String (Model, Animation.Tracker)
evaluateToNumber tracker root number model = selectedEquation_ model
    |> Result.andThen (\(eq, ids, entry) ->
        let
            replacement = if number < 0
                then Math.UnaryNode {state = (Just Rules.negateProp,Nothing), name = "-", child = Math.RealNode {state = (Nothing, Nothing), value = -number}}
                else Math.RealNode {state = (Nothing, Nothing), value = number}
        in
            History.current entry.history
            |> Tuple.first
            |> Matcher.replaceSubtree (Set.singleton root) replacement Matcher.newResult
            |> Result.map (\(newSelect, newEq) ->
                let newHis = History.update (History.Stage (History.Change (newEq, Rules.toLatex newEq))) entry.history
                in stageChange tracker eq ids newHis entry model
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

substitute: Animation.Tracker -> Int -> Model -> Result String (Model, Animation.Tracker)
substitute tracker eqSub model = case Dict.get eqSub model.equations of
    Nothing -> Err "Substitution equation not found"
    Just subEntry -> selectedEquation_ model
        |> Result.andThen (\(eqOrig, ids, origEntry) ->
            let origEq = History.current origEntry.history |> Tuple.first
            in if eqOrig == eqSub
                then Err "Cannot substitute equation with itself"
                else Matcher.replaceAllOccurrences ids (History.current subEntry.history |> Tuple.first) origEq
                    |> Result.map (\(newSelected, newEq) ->
                        let newHis = History.update (History.Stage (History.Change (newEq, Rules.toLatex newEq))) origEntry.history
                        in stageChange tracker eqOrig ids newHis origEntry model
                        )
            )

commit: Rules.Model -> Model -> Result String Model
commit rules model = selectedEquation_ model
    |> Result.map (\(eq, ids, entry) ->
        let
            newHis = History.update History.Commit entry.history
            -- do not need to call updateBricks because it should already be 'previewed'
            newEquations = Dict.insert eq {entry | history = newHis} model.equations
            newSelected = let staged = model.selected |> Maybe.map (\(_, _, s) -> s) |> Maybe.withDefault Set.empty
                in Just (eq, staged, Set.empty)
            newActions = updateActions_ newSelected rules newEquations
        in
            -- TODO: make the blocks look see-through or something to show it being committed
            --   if we want to animate it then this function likely needs to return a Tracker
            {model | equations = newEquations, selected = newSelected, actions = newActions}
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
                newSelected = Just (eq, ids, Set.empty)
                -- do not need to call updateActions_ because it has not been committed yet
            in
                ({model | equations = newEquations, selected = newSelected}, newTracker)
            )
        )

-- a fallback selection is needed in case another or no equation is selected
-- TODO: clean up since this is a bit ugly
swapSelectedIfNecessary_: Int -> Rules.Model -> Model -> Model
swapSelectedIfNecessary_ eq rules model =
    let
        selEq = model.selected |> Maybe.map (\(sel, _, _) -> sel)
        newSelected = if selEq == Just eq
            then model.selected
            else (
                model.equations
                |> Dict.get eq
                |> Maybe.map (\entry ->
                    entry.history
                    |> History.current
                    |> Tuple.first
                    |> .root
                    |> Math.getState
                    |> Matcher.getID
                    |> \id -> (eq, Set.singleton id, Set.empty)
                    )
                )
        newActions = if selEq == Just eq
            then model.actions
            else updateActions_ newSelected rules model.equations
    in
        { model | selected = newSelected, actions = newActions }

revert_: Int -> Int -> Rules.Model -> Animation.Tracker -> Model -> Result String (Model, Animation.Tracker)
revert_ eq idx rules tracker model = Dict.get eq model.equations
    |> Maybe.map (\entry ->
        let
            newHis = History.update (History.Revert idx) entry.history
            (newEntry, newTracker) = updateBricks tracker {entry | history = newHis}
            newEquations = Dict.insert eq newEntry model.equations
            newRootId = newHis |> History.current |> Tuple.first |> .root |> Math.getState |> Matcher.getID
            newSelected = Just (eq, Set.singleton newRootId, Set.empty)
            newActions = updateActions_ newSelected rules newEquations
        in
            ({model | equations = newEquations, selected = newSelected, actions = newActions}, newTracker)
        )
    |> Result.fromMaybe "Entry being reverted does not exist, invariant violated"

updateSelected_: Int -> Int -> Bool -> Rules.Model -> Model -> Model
updateSelected_ eq node combine rules model =
    let
        selected = case (combine, model.selected) of
            (True, Just (e, current, _)) -> if e /= eq
                then Just (eq, Set.singleton node, Set.empty)
                else if Set.member node current
                then let newSet = Set.remove node current in
                    if Set.isEmpty newSet then Nothing
                    else Just (eq, newSet, Set.empty)
                else Just (eq, Set.insert node current, Set.empty)
            _ -> Just (eq, Set.singleton node, Set.empty)

        actions = updateActions_ selected rules model.equations
    in
        { model | selected = selected, actions = actions }

updateActions_: Maybe (Int, Set.Set Int, Set.Set Int) -> Rules.Model -> Dict.Dict Int Entry -> List (String, (List Actions.Action))
updateActions_ selected rules equations =
    let
        selection = selected
            |> Maybe.andThen (\(eq, ids, _) -> Dict.get eq equations
                |> Maybe.map (.history >> History.current >> Tuple.first)
                |> Maybe.andThen (\fullEq -> Matcher.selectedSubtree ids fullEq |> Result.toMaybe
                    |> Maybe.map (\(root, nodes) -> {tree = fullEq, root = root, nodes = nodes})
                )
            )
    in
        Actions.matchRules rules (Dict.size equations) selection


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
        actions = Actions.matchRules rules (Dict.size model.equations) Nothing
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
        |> \(eqs, newT) -> ({model | equations = eqs, actions = actions, selected = Nothing}, newT)

updateEqOrder_: Int -> List Int -> List Int
updateEqOrder_ num original = num :: (List.filter (\n -> n /= num) original)

update: Draggable.Size -> Animation.Tracker -> Rules.Model -> Event -> Model -> (Model, Animation.Tracker, Cmd Event)
update size tracker rules event model = let default = (model, tracker, Cmd.none) in case event of
    Select eq node -> updateSelected_ eq node False rules model
        |> \newModel -> ({newModel | recencyList = updateEqOrder_ eq newModel.recencyList}, tracker, Cmd.none)
    Delete eq -> updatePositions_
        {model | equations = Dict.remove eq model.equations, recencyList = List.filter (\n -> n /= eq) model.recencyList}
        |> updateQueryCmd tracker
    ToggleHide eq -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry -> updatePositions_
            {   model
            |   equations = Dict.insert eq {entry | show = not entry.show, grabbing = Nothing} model.equations
            ,   recencyList = updateEqOrder_ eq model.recencyList
            }
            |> updateQueryCmd tracker
    ToggleHistory eq -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry ->
            (   {   model
                |   equations = Dict.insert eq {entry | showHistory = not entry.showHistory} model.equations
                ,   recencyList = updateEqOrder_ eq model.recencyList
                }
            , tracker, Cmd.none)
    HistoryEvent eq he ->
        let
            newModel = {model | recencyList = updateEqOrder_ eq model.recencyList}
            resModelTracker = case he of
                History.Stage staged -> case staged of
                    History.Undo -> undo tracker (swapSelectedIfNecessary_ eq rules newModel)
                    History.Redo -> redo tracker (swapSelectedIfNecessary_ eq rules newModel)
                    _ -> Err "Change should only be staged via Actions.Event, invariant violated"
                History.Reset -> reset tracker newModel
                History.Revert idx -> revert_ eq idx rules tracker newModel
                History.Commit -> commit rules newModel |> Result.map (\m -> (m, tracker))
        in
            case resModelTracker of
                Err _ -> default
                Ok (m, t) -> updateQueryCmd t m
    DraggableEvent eq dEvent -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry -> Draggable.update size dEvent (entry.view |> Tuple.second)
            |> \(dModel, action) ->
                (   {   model
                    |   equations = Dict.insert eq {entry | view = (True, dModel)} model.equations
                    ,   recencyList = updateEqOrder_ eq model.recencyList
                    }
                ,   tracker
                ,   Maybe.map (\v -> model.setCapture v.id v.pID) action
                    |> Maybe.withDefault Cmd.none
                )
    PointerDown eq grab pid point -> case Dict.get eq model.equations of
        Nothing -> default
        Just entry ->
            (   {   model
                |   equations = Dict.insert eq { entry | grabbing = Just grab } model.equations
                ,   recencyList = updateEqOrder_ eq model.recencyList
                }
            ,   tracker
            ,   model.svgMouseCmd eq pid point
            )
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
                        newSel = Just (eqNum, Set.singleton newGrabbing.id, Set.singleton newGrabbing.id)
                        -- don't use stageChange because it updates the selected nodes
                        (newEntry, newT) = updateBricks tracker {grabbingEntry | history = newHis}
                    in
                        ({model | equations = Dict.insert eqNum newEntry model.equations, selected = newSel}, newT, Cmd.none)

        SvgDrag.End time _ -> case Dict.get eqNum model.equations of
            Nothing -> default
            Just entry -> case entry.grabbing of
                Nothing -> default
                Just grab -> let noGrabModel = {model | equations = Dict.insert eqNum {entry | grabbing = Nothing} model.equations}
                    in case grab.moved of
                        Initial -> updateSelected_ eqNum grab.id False rules noGrabModel
                                |> \newModel -> (newModel, tracker, Cmd.none)
                        NoOp -> (noGrabModel, tracker, Cmd.none)
                        _ -> update size tracker rules (HistoryEvent eqNum History.Commit) noGrabModel

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
            highlight = model.selected
                |> Maybe.andThen (\(selEq, set, stage) -> if selEq == eqNum
                    then Just (if Set.isEmpty stage then set else stage)
                    else Nothing
                    )
                |> Maybe.withDefault Set.empty
            grab = entry.grabbing |> Maybe.map .id

            (b,m) = entry.ui
        in
            (   dModel.id
            ,   Draggable.div (DraggableEvent eqNum >> converter) dModel [class "equationHolder"]
                [   div [class "equation", Svg.Attributes.id ("equation-view-" ++ String.fromInt eqNum)]
                    [   MathIcon.view (\id -> List.filterMap identity
                            [   HtmlEvent.onClick (Select eqNum id) |> Just
                            ,   Svg.Attributes.class "selected" |> Helper.maybeGuard (Set.member id highlight)
                            ]
                        ) [Svg.Attributes.class "mathIcons"] m
                        |> Html.map converter
                    ,   Bricks.view (brickAttr_ highlight grab eqNum) b
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
                                                List.map (div []) (List.take middle children)
                                                ++ after
                                            )
                                    ) entry.history
                                )
                            ]
                        else a [class "historyButton", class "clickable", HtmlEvent.onClick (ToggleHistory eqNum |> converter)] [Html.text "Show History"]
                    ]
                ,   div [class "contextualToolbar"]
                    (   div [class "contextualTopic"]
                        [   div
                            (   class "contextualAction" :: if History.canUndo entry.history then
                                [   class "clickable"
                                ,   HtmlEvent.onPointerEnter (HistoryEvent eqNum (History.Stage History.Undo) |> converter)
                                ,   HtmlEvent.onPointerLeave (HistoryEvent eqNum History.Reset |> converter)
                                ,   HtmlEvent.onClick (HistoryEvent eqNum History.Commit |> converter)
                                -- TODO: allow user to undo a bunch of times without another pointerenter
                                --   maybe just skip the preview and allow direct commit in that case
                                ]
                                else [class "contextualDisabled"]
                            )
                            [div [class "contextualActionLabel"] [text "Undo"]]  -- TODO: make these icons instead
                        ,   div
                            (   class "contextualAction" :: if History.canRedo entry.history then
                                [   class "clickable"
                                ,   HtmlEvent.onPointerEnter (HistoryEvent eqNum (History.Stage History.Redo) |> converter)
                                ,   HtmlEvent.onPointerLeave (HistoryEvent eqNum History.Reset |> converter)
                                ,   HtmlEvent.onClick (HistoryEvent eqNum History.Commit |> converter)
                                ]
                                else [class "contextualDisabled"]
                            )
                            [div [class "contextualActionLabel"] [text "Redo"]]
                        ]
                        :: if Set.isEmpty highlight then [] else Actions.viewContextual actionConvert model.actions
                    )
                ]
            )
    )

historyEntry_: (Event -> msg) -> Bool -> Int -> Int -> Html.Html msg -> Html.Html msg
historyEntry_ converter current eqNum index inner = Html.a
    (   if current
        then [class "selected"]
        else
            [   class "clickable"
            ,   HtmlEvent.onClick (HistoryEvent eqNum (History.Revert index) |> converter)
            ]
    )
    [inner]

brickAttr_: Set.Set Int -> Maybe Int -> Int -> Int -> Maybe (Int, List Float) -> Maybe (Float, Float) -> Maybe Float -> List (Html.Attribute Event)
brickAttr_ highlight grabbed eqNum id commutable groupable ungroupable =
    let
        grab = Grab id commutable groupable ungroupable Initial
    in
        List.filterMap identity
        [   HtmlEvent.onPointerCapture identity (PointerDown eqNum grab) |> Just
        ,   Svg.Attributes.class "commutable" |> Helper.maybeGuard (commutable /= Nothing || groupable /= Nothing || ungroupable /= Nothing)
        ,   Svg.Attributes.class "selected" |> Helper.maybeGuard (Set.member id highlight)
        ,   Svg.Attributes.class "grabbed" |> Helper.maybeGuard (grabbed == Just id)
        ]

encode: Model -> Encode.Value
encode model = Encode.object
    [   (   "equations", Encode.dict String.fromInt encodeEntry_ model.equations)
    ,   (   "nextEquationNum", Encode.int model.nextEquationNum)
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

decoder: (String -> Encode.Value -> Cmd Event) -> (List FullEquation -> Cmd Event) -> (Int -> Encode.Value -> (Float, Float) -> Cmd Event) ->
    Decode.Decoder (Model, Animation.Tracker)
decoder setCapture updateQuery svgMouseCmd = Decode.map3 (\(eq, t) next create -> (Model eq next Nothing [] create [] setCapture svgMouseCmd updateQuery, t))
    (   Decode.field "equations" <| Decode.map addDefaultPositions_ <| Helper.intDictDecoder entryDecoder_)
    (   Decode.field "nextEquationNum" Decode.int)
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
                ,   grabbing = Nothing
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
