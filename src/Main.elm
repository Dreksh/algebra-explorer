port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvent
import Browser.Navigation as Nav
import Dict
import File
import File.Download as FDownload
import File.Select as FSelect
import Html exposing (a, div, input, text)
import Html.Attributes exposing (class, href, id, name, target)
import Html.Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Set
import Task
import Url
-- Our imports
import Algo.History as History
import Algo.Matcher as Matcher
import Algo.Math as Math
import Components.Evaluate as Evaluate
import Components.Query as Query
import Components.Rules as Rules
import Helper
import UI.Actions as Actions
import UI.Animation as Animation
import UI.Dialog as Dialog
import UI.Display as Display
import UI.Draggable as Draggable
import UI.FocusEvent as FocusEvent
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.Input as Input
import UI.InputWithHistory as InputWithHistory
import UI.MathIcon as MathIcon
import UI.Menu as Menu
import UI.Notification as Notification
import UI.SvgDrag as SvgDrag
import Components.Rules as Rules
import UI.Actions as Actions
import UI.Dialog as Dialog

-- Overall Structure of the app: it's a document

main : Program Decode.Value Model Event
main = Browser.application
    {    init = init
    ,    view = view
    ,    update = update
    ,    subscriptions = subscriptions
    ,    onUrlRequest = EventUrlRequest
    ,    onUrlChange = EventUrlChange
    }

port evaluateString: {id: Int, str: String} -> Cmd msg
port evaluateResult: ({id: Int, value: Float} -> msg) -> Sub msg
port onKeyDown: ({ctrl: Bool, shift: Bool, key: String} -> msg) -> Sub msg
port svgMouseBegin: {id: String, x: Float, y: Float, pointerID: Encode.Value, svg: Bool} -> Cmd msg
port svgMouseEvent: (SvgDrag.Raw -> msg) -> Sub msg
port focusOutCmd: {id: String} -> Cmd msg
port focusOutSub: ({id: String} -> msg) -> Sub msg

setCapture: String -> Encode.Value -> Cmd msg
setCapture e p = svgMouseBegin {id = e, x = 0 , y = 0, pointerID = p, svg = False}

displayMouseCmd: Int -> Encode.Value -> (Float, Float) -> Cmd msg
displayMouseCmd id pId (x, y) = svgMouseBegin {id = "equation-view-" ++ String.fromInt id, x = x, y = y, pointerID = pId, svg = True}

inputMouseCmd: String -> Encode.Value -> (Float, Float) -> Cmd msg
inputMouseCmd name pId (x, y) = svgMouseBegin {id = name, x=x, y=y, pointerID = pId, svg = True}

-- Types

type alias Model =
    {   swappable: Swappable
    ,   query: Query.Model
    ,   size: Draggable.Size
    ,   dialog: Maybe (Dialog.Model Event, Maybe Actions.MatchedRule)
    ,   animation: Animation.Tracker
    ,   input: InputWithHistory.Model Event
    ,   svgDragMap: SvgDrag.Model Event
    ,   focusEvent: FocusEvent.Model Event
    }

type alias Swappable =
    {   display: Display.Model
    ,   rules: Rules.Model
    ,   notification: Notification.Model
    ,   menu: Menu.Model
    ,   evaluator: Evaluate.Model EvalType Event
    -- UI fields
    ,   showMenu: Bool
    }

type Event =
    EventUrlRequest Browser.UrlRequest
    | EventUrlChange Url.Url
    | DisplayEvent Display.Event
    | RuleEvent Rules.Event
    | ActionEvent Actions.Event
    | NotificationEvent Notification.Event
    | MenuEvent Menu.Event
    | InputEvent InputWithHistory.Event
    | SvgDragEvent SvgDrag.Raw
    | DialogEvent Dialog.Event
    -- Event from the UI
    | NoOp -- For setting focus on textbox
    | RedirectTo String
    | PressedKey {ctrl: Bool, shift: Bool, key: String}
    | EnterCreateMode
    | ToggleMenu
    | Save
    | OpenDialog (Dialog.Model Event)
    | CloseDialog
    | ProcessTopic Bool String (Result Http.Error Rules.Topic)
    | ProcessSource String (Result Http.Error Source)
    | FileSelect LoadableFile
    | FileSelected LoadableFile File.File
    | FileLoaded LoadableFile String
    | WindowResize Int Int
    | AnimationDelta Float
    -- Rules
    | ApplyParameters (Dict.Dict String Dialog.Extracted)
    | ApplySubstitution Bool Display.FullEquation
    | ConvertSubString Int Float String -- root target subExpr
    | EvalComplete {id: Int, value: Float}

type LoadableFile =
    TopicFile
    | SaveFile

type EvalType =
    NumSubType_ Int Float (Math.Tree (Maybe Rules.FunctionProp))
    | EvalType_ Int

type alias Source =
    {   topics: Dict.Dict String Rules.Source
    }

-- Events

init: Decode.Value -> Url.Url -> Nav.Key -> (Model, Cmd Event)
init flags url key =
    let
        query = Query.parseInit url key
        (eqs, errs) = List.foldl (parseEquations_ Rules.init) ([], []) query.equations
        newScreen = List.isEmpty eqs
        (newDisplay, tracker) = Display.init setCapture (Query.pushEquations query) displayMouseCmd -1 eqs
        (nModel, finalT, nCmd) = List.foldl Notification.displayError (Notification.init, tracker, Cmd.none) errs
    in
    (   {   swappable =
            { display = newDisplay
            , rules = Rules.init
            , notification = nModel
            , menu = Menu.init (Set.fromList ["Settings", "Equations", "Topics"])
            , evaluator = Evaluate.init evaluateString
            , showMenu = False
            }
        , query = query
        , dialog = Nothing
        , size = Decode.decodeValue
            (Decode.map2 Tuple.pair (Decode.field "width" Decode.float) (Decode.field "height" Decode.float))
            flags
            |> Result.toMaybe
            |> Maybe.withDefault (0, 0)
        , animation = finalT
        , input = InputWithHistory.init newScreen (inputMouseCmd "mainInput") focusTextBar_
        , svgDragMap = Dict.fromList
            [   ("equation-view-", (\str event -> String.toInt str |> Maybe.map (\eqNum -> Display.PointerDrag eqNum event |> DisplayEvent)))
            ,   ("Equation-", (\str event -> String.toInt str |> Maybe.map (\eqNum -> Draggable.Drag event |> Display.DraggableEvent eqNum |> DisplayEvent)))
            ,   ("mainInput", (\_ -> Input.Shift >> InputWithHistory.InputEvent >> InputEvent >> Just ))
            ,   ("dialog_", (\str -> Input.Shift >> Dialog.InputEvent str >> DialogEvent >> Just))
            ]
            |> SvgDrag.init
        , focusEvent = Dict.singleton "dialog" CloseDialog
            |> FocusEvent.init
        }
    ,   Cmd.batch
        [   loadSources query.sources
        ,   Cmd.map NotificationEvent nCmd
        ]
    )

parseEquations_: Rules.Model -> String -> (List Display.FullEquation, List String) -> (List Display.FullEquation, List String)
parseEquations_ model elem (result, errs) = case parseEquation_ model elem of
    Result.Ok root -> (root :: result, errs)
    Result.Err err -> (result, err :: errs )

parseEquation_: Rules.Model -> String -> Result String Display.FullEquation
parseEquation_ model str = Matcher.parseEquation (Rules.functionProperties model) Animation.stateOps str

subscriptions: Model -> Sub Event
subscriptions model = Sub.batch
    [   onKeyDown PressedKey
    ,   evaluateResult EvalComplete
    ,   BrowserEvent.onResize WindowResize
    ,   svgMouseEvent SvgDragEvent
    ,   focusOutSub (FocusEvent.resolve model.focusEvent NoOp)
    ,   if model.animation >= 0 then BrowserEvent.onAnimationFrameDelta AnimationDelta else Sub.none
    ]

{-
## State changes
-}

update: Event -> Model -> ( Model, Cmd Event )
update event core = let model = core.swappable in
    let updateCore newModel = {core | swappable = newModel} in
    case event of
        EventUrlRequest req ->
            ( case req of
                Browser.Internal inReq -> Url.toString inReq
                Browser.External exReq -> exReq
            )
            |> \str -> if str == "" then (core, Cmd.none)
                else ({core | dialog = Just (leaveDialog_ str, Nothing)}, Cmd.batch [focusTextBar_ "dialog", focusOutCmd {id= "dialog"}])
        EventUrlChange _ -> (core, Cmd.none)
        DisplayEvent e -> let (dModel, newAnimation, dCmd) = Display.update core.size core.animation model.rules e model.display in
            ({core | swappable = {model | display = dModel}, animation = newAnimation}, Cmd.map DisplayEvent dCmd)
        NotificationEvent e -> let (nModel, newT) = Notification.update core.animation e model.notification in
            ({core | animation = newT, swappable = {model | notification = nModel}}, Cmd.none)
        MenuEvent e -> (updateCore {model | menu = Menu.update e model.menu}, Cmd.none)
        InputEvent e -> let ((newIn, newT), submitted, cmd) = InputWithHistory.update InputEvent core.animation (Rules.functionProperties model.rules) e core.input in
            case submitted of
                Err err -> submitNotification_ core err
                Ok Nothing -> case newIn.current of
                    Nothing -> if Display.anyVisible model.display
                        then ({core | animation = newT, input = newIn}, cmd)
                        else ({core | animation = newT, input = newIn, swappable = {model | showMenu = True}}, cmd)
                    Just _ -> ({core | animation = newT, input = newIn}, cmd)
                Ok (Just root) -> Display.add newT root model.display
                    |> (\(dModel, animation) ->
                        (   { core | swappable = {model | display = dModel}, animation = animation, input = newIn }
                        ,   Cmd.batch [ updateQuery_ dModel, cmd]
                        )
                    )
        SvgDragEvent e -> case SvgDrag.resolve e core.svgDragMap of
            Nothing -> (core, Cmd.none)
            Just newE -> update newE core
        DialogEvent e -> case core.dialog of
            Nothing -> (core, Cmd.none)
            Just (dialog, matched) -> Dialog.update (Rules.functionProperties model.rules) core.animation e dialog
                |> \((newDialog, newA), errStr, cmd) -> if errStr /= "" then submitNotification_ core errStr
                    else ({core  | dialog = Just (newDialog, matched), animation = newA}, cmd)
        NoOp -> (core, Cmd.none)
        RedirectTo url -> (core, Nav.load url)
        PressedKey input -> case (input.ctrl, input.shift, input.key) of
            (_, _, "Escape") -> case core.dialog of
                Just _ -> ({core | dialog = Nothing}, Cmd.none)
                Nothing -> case (Display.anyVisible model.display, model.showMenu, core.input.current) of
                    (True, True, _) -> ({core | swappable = {model | showMenu = False}}, Cmd.none)
                    (True, False, Just _) -> let (newIn, newT) = InputWithHistory.close core.animation core.input in
                        ({core | animation = newT, input = newIn}, Cmd.none)
                    (True, False, Nothing) -> ({core | swappable = {model | showMenu = True}}, Cmd.none)
                    (False, True, Just _) -> ({core | swappable = {model | showMenu = False}}, Cmd.none)
                    (False, _, Nothing) -> let (newIn, newT) = InputWithHistory.open core.animation core.input in
                        ({core | animation = newT, input = newIn, swappable = {model | showMenu = False}}, Cmd.none)
                    (False, False, Just _) -> let (newIn, newT) = InputWithHistory.close core.animation core.input in
                        ({core | animation = newT, input = newIn, swappable = {model | showMenu = True}}, Cmd.none)
            (True, False, "z") -> case Display.undo core.animation model.display of
                Err errStr -> submitNotification_ core errStr
                Ok (display, animation) -> commitChange_ {core | swappable = {model | display = display}, animation = animation}
            (True, True, "z") -> case Display.redo core.animation model.display of
                Err errStr -> submitNotification_ core errStr
                Ok (display, animation) -> commitChange_ {core | swappable = {model | display = display}, animation = animation}
            (_, _, " ") -> update EnterCreateMode core
            _ -> (core, Cmd.none)
        EnterCreateMode -> let (inputModel, newT) = InputWithHistory.open core.animation core.input in
            (   {   core
                |   swappable = {model | showMenu = False}
                ,   input = inputModel
                ,   animation = newT
                }
            ,   focusTextBar_ "mainInput-input"
            )
        ToggleMenu -> if not model.showMenu
            then (updateCore {model | showMenu = True}, Cmd.none)
            else if Display.anyVisible model.display
            then (updateCore {model | showMenu = False}, Cmd.none)
            else let (newIn, newT) = InputWithHistory.open core.animation core.input in
                ({core | animation = newT, input = newIn, swappable = {model | showMenu = False}}, Cmd.none)
        Save -> (core, saveFile model)
        OpenDialog d ->
            (   {core | dialog = Just (d, Nothing)}
            ,   Cmd.batch
                [   case d.focus of
                    Nothing -> focusTextBar_ "dialog"
                    Just name -> Dialog.fieldID name |> focusTextBar_
                ,   focusOutCmd {id = "dialog"}
                ]
            )
        CloseDialog -> ({core | dialog = Nothing}, Cmd.none)
        ProcessTopic preinstall url result -> case result of
            Err err -> httpErrorToString_ url err |> submitNotification_ core
            Ok topic -> case Rules.addTopic (Just url) topic model.rules of
                Err errStr -> submitNotification_ core errStr
                Ok rModel -> let (dModel, t) = Display.refresh rModel core.animation model.display in
                    if preinstall then
                        (   {core | swappable = {model | rules = rModel, display = dModel}, animation = t}
                        ,   Cmd.none
                        )
                    else let (nModel, t1, nCmd) = Notification.displayInfo ("Loaded topic: " ++ topic.name) (model.notification, t) in
                    (   {   core
                        |   swappable = { model | rules = rModel, display = dModel, notification = nModel }
                        ,   dialog = Nothing -- Most likely triggered from a dialog
                        ,   animation = t1
                        }
                    ,   Cmd.map NotificationEvent nCmd
                    )
        ProcessSource url result -> case result of
            Err err -> httpErrorToString_ url err |> submitNotification_ core
            Ok source ->
                (   updateCore {model | rules = Rules.addSources source.topics model.rules}
                ,   Dict.toList source.topics
                    |> List.filterMap (\(_, s) -> if s.preinstall then Just (downloadTopicCmd_ True s.url) else Nothing)
                    |> Cmd.batch
                )
        FileSelect fileType -> (core, FSelect.file ["application/json"] (FileSelected fileType))
        FileSelected fileType file -> ({core | dialog = Nothing}, Task.perform (FileLoaded fileType) (File.toString file))
        FileLoaded fileType str -> case fileType of
            TopicFile -> Decode.decodeString Rules.topicDecoder str
                |> Result.mapError Decode.errorToString
                |> Result.andThen (\topic -> Rules.addTopic Nothing topic model.rules)
                |> (\result -> case result of
                    Err errStr -> submitNotification_ core errStr
                    Ok rModel -> (updateCore {model | rules = rModel}, Cmd.none)
                )
            SaveFile -> Decode.decodeString (swappableDecoder (Query.pushEquations core.query)) str
                |> Result.mapError Decode.errorToString
                |> (\result -> case result of
                    Err errStr -> submitNotification_ core errStr
                    Ok (s, animation) -> ({core | swappable = s, animation = animation}, updateQuery_ s.display)
                )
        WindowResize width height -> ({core | size = (toFloat width, toFloat height)}, Cmd.none)
        AnimationDelta millis ->
            (   {   core
                |   swappable =
                    {   model
                    |   display = Display.advanceTime millis model.display
                    ,   notification = Notification.advance millis model.notification
                    }
                ,   animation = Animation.updateTracker millis core.animation
                ,   input = InputWithHistory.advance millis core.input
                ,   dialog = Maybe.map (\(a,b) -> (Dialog.advanceTime millis a, b)) core.dialog
                }
            , Cmd.none
            )
        RuleEvent e -> case e of
            Rules.Download url -> (core, downloadTopicCmd_ False url)
            Rules.Delete topicName -> ({core | dialog = Nothing, swappable = { model | rules = Rules.deleteTopic topicName model.rules}}, Cmd.none)
        ActionEvent e -> case e of
            Actions.Commit -> commitChange_ core
            Actions.Reset -> resetChange_ core
            Actions.Apply p -> if List.length p.matches == 1 && Dict.isEmpty p.parameters
                -- don't spawn dialog on hover if multiple matches
                then case Helper.listIndex 0 p.matches of
                    Nothing -> submitNotification_ core "Unable to extract the match"
                    Just m -> applyChange_ m (Display.previewOnHover model.display |> not) core
                else ({ core | dialog = Just (parameterDialog_ model.rules p, Just p)}, Cmd.batch [focusTextBar_ "dialog", focusOutCmd {id = "dialog"}])
            Actions.Substitute eqs -> case eqs of
                [] -> submitNotification_ core "no equations to substitute"
                [eq] -> update (ApplySubstitution False eq) core
                _ -> ({core | dialog = Just (substitutionDialog_ eqs, Nothing)} , Cmd.batch [focusTextBar_ "dialog", focusOutCmd {id = "dialog"}])
            Actions.NumericalSubstitution root target ->
                (   { core | dialog = Just (numSubDialog_ model.rules root target, Nothing)}
                ,   Cmd.batch [focusTextBar_ (Dialog.fieldID "expr-input"), focusOutCmd {id = "dialog"}]
                )
            Actions.Evaluate id evalStr -> let (eModel, cmd) = Evaluate.send (EvalType_ id) evalStr model.evaluator in
                (updateCore {model | evaluator = eModel}, cmd)
            Actions.ShowSubactions matchedRules -> case Display.updateSubactions core.animation matchedRules model.display of
                Ok (newDisplay, newAnimation) -> ({core | swappable = {model | display=newDisplay}, animation=newAnimation}, Cmd.none)
                Err errStr -> submitNotification_ core errStr
            Actions.HideSubactions -> (core, Task.perform identity (Task.succeed (Actions.ShowSubactions [] |> ActionEvent)))  -- need to delay by one frame to allow leaving toolbar and entering subtoolbar
        ApplyParameters params -> case core.dialog of
            Just (_, Just existing) -> ( case Dict.get "_method" params of
                    Just (Dialog.IntValue n) -> Helper.listIndex n existing.matches
                    _ -> Helper.listIndex 0 existing.matches
                )
                |> Result.fromMaybe "Unable to find the match"
                |>  Result.andThen (\prev -> Helper.resultDict (\k v r -> if k == "_method" then Ok r
                        else case v of
                            Dialog.TextValue val -> Math.parse (Rules.functionProperties model.rules) val
                                |> Result.andThen (Matcher.toReplacement identity False Dict.empty)
                                |> Result.map (\tree -> {r | from = Matcher.addMatch k tree r.from})
                            Dialog.MathValue str -> Matcher.toSubstitution (Rules.functionProperties model.rules) str
                                |> Result.map (\(key, replacement) -> {r | from = Matcher.addMatch key replacement r.from})
                            _ -> Ok r
                        )
                    prev params
                )
                |> (\result -> case result of
                    Err errStr -> submitNotification_ core errStr
                    Ok newParams -> applyChange_ newParams True core
                )
            _ -> ({ core | dialog = Nothing}, Cmd.none)
        ApplySubstitution fromDialog eq -> case Display.substitute core.animation eq model.display of
            Err errStr -> submitNotification_ core errStr
            Ok (dModel, animation) -> {core | swappable = {model | display = dModel}, dialog = Nothing, animation = animation}
                |> if Display.previewOnHover dModel && not fromDialog then \m -> (m, Cmd.none) else commitChange_
        ConvertSubString root target str -> case Math.parse (Rules.functionProperties model.rules) str of
            Err errStr -> submitNotification_ core errStr
            Ok replacement -> case Rules.evaluateStr model.rules replacement of
                Err errStr -> submitNotification_ core errStr
                Ok evalStr -> let (eModel, cmd) = Evaluate.send (NumSubType_ root target replacement) evalStr model.evaluator in
                    (updateCore {model | evaluator = eModel}, cmd)
        EvalComplete reply -> let (eModel, c) = Evaluate.finish reply.id model.evaluator in
            let newCore = updateCore {model | evaluator = eModel } in
            case c of
                Nothing -> submitNotification_ newCore "Unable to evaluate a string"
                Just (NumSubType_ root target replacement) -> if target /= reply.value
                    then submitNotification_ newCore ("Expression evaluates to: " ++ String.fromFloat reply.value ++ ", but expecting: " ++ String.fromFloat target)
                    else case Display.partitionNumber core.animation root target replacement model.display of
                        Err errStr -> submitNotification_ newCore errStr
                        Ok (dModel, animation) -> commitChange_ {core | dialog = Nothing, swappable = {model | evaluator = eModel, display = dModel}, animation = animation}
                Just (EvalType_ id) -> case Display.evaluateToNumber core.animation id reply.value model.display of
                    Err errStr -> submitNotification_ newCore errStr
                    Ok (dModel, animation) -> {core | swappable = {model | evaluator = eModel, display = dModel}, animation = animation}
                        |> if Display.previewOnHover model.display then (\m -> (m, Cmd.none)) else commitChange_


applyChange_: Actions.SingleMatch -> Bool -> Model -> (Model, Cmd Event)
applyChange_ params commit model = let swappable = model.swappable in
    case Display.transform model.animation params.replacements params.from swappable.display of
        Err errStr -> submitNotification_ model errStr
        Ok (newDisplay, newAnim) -> {model | dialog = Nothing, swappable = {swappable | display = newDisplay}, animation = newAnim}
            |> if commit then commitChange_ else \m -> (m, Cmd.none)

commitChange_: Model -> (Model, Cmd Event)
commitChange_ model = let swappable = model.swappable in
    case Display.commit model.animation swappable.rules swappable.display of
        Err errStr -> submitNotification_ model errStr
        Ok (newDisplay, newTracker) -> let (susDisplay, susCmd) = Display.suspendHoverCmd newDisplay in
            (   {model | swappable = {swappable | display = susDisplay}, animation = newTracker}
            ,   Cmd.batch [updateQuery_ newDisplay, Cmd.map DisplayEvent susCmd]
            )

resetChange_: Model -> (Model, Cmd Event)
resetChange_ model = let swappable = model.swappable in
    case Display.reset model.animation swappable.display of
        Err _ -> (model, Cmd.none)
        Ok (newDisplay, newAnim) -> ({model | swappable = {swappable | display = newDisplay}, animation = newAnim}, Cmd.none)

submitNotification_: Model -> String -> (Model, Cmd Event)
submitNotification_ model str = let swappable = model.swappable in
    let (nModel, newT, cmd) = Notification.displayError str (swappable.notification, model.animation, Cmd.none) in
    ({model | swappable = {swappable | notification = nModel}, animation = newT}, Cmd.map NotificationEvent cmd)

httpErrorToString_: String -> Http.Error -> String
httpErrorToString_ url err = case err of
    Http.BadUrl _ -> "Invalid URL provided: " ++ url
    Http.Timeout -> "Timed out waiting for: " ++ url
    Http.NetworkError -> "Unable to reach: " ++ url
    Http.BadStatus code -> "The url returned an error code [" ++ String.fromInt code ++ "]: " ++ url
    Http.BadBody str -> "The file is malformed:\n" ++ str

updateQuery_: Display.Model -> Cmd Event
updateQuery_ = Display.updateQueryCmd >> \c -> Cmd.map DisplayEvent c

focusTextBar_: String -> Cmd Event
focusTextBar_ id = Dom.focus id |> Task.attempt (\_ -> NoOp)

downloadTopicCmd_: Bool -> String -> Cmd Event
downloadTopicCmd_ preinstall url = Http.get
    {   url = url
    ,   expect = Http.expectJson (ProcessTopic preinstall url) Rules.topicDecoder
    }

{-
## UI
-}

view: Model -> Browser.Document Event
view core = let model = core.swappable in
    { title = "Algebra Explorer"
    , body =
        Html.Keyed.node "div" [id "body"]
        (   [   ("draggableListener", div [id "draggableListener"] [])
            ,   ("menuIcon", Icon.menu
                    [ id "menuToggle"
                    , HtmlEvent.onClick ToggleMenu
                    , Icon.class "clickable"
                    ]
                )
            ,   ("logo", Icon.fullLogo [id "logo", HtmlEvent.onClick EnterCreateMode, Icon.class "clickable"])
            ]
        ++  Display.views DisplayEvent ActionEvent model.display
        ++  List.filterMap identity
            [   ("inputPane", div [id "inputPane"]
                [   Html.Keyed.node "div"
                    (id "leftPane" :: if model.showMenu then [HtmlEvent.onClick ToggleMenu] else [class "closed"])
                    (InputWithHistory.view InputEvent core.input)
                ,   div (id "rightPane" :: (if model.showMenu then [] else [class "closed"]))
                    [   Icon.close [id "closeMenu", HtmlEvent.onClick ToggleMenu, Icon.class "clickable"]
                    ,   Menu.view MenuEvent model.menu
                        [   Menu.Section {name = "Settings", icon = Nothing}
                            [   Menu.Content [] [a [HtmlEvent.onClick (FileSelect SaveFile), class "clickable"] [text "Open"]]
                            ,   Menu.Content [] [a [HtmlEvent.onClick Save, class "clickable"] [text "Save"]]
                            ,   Menu.Content [] [a [class "clickable", href "https://github.com/Dreksh/algebra-explorer", target "_blank"] [text "Github Source"]]
                            ]
                        ,   Menu.Section {name = "Equations", icon = Just (\c -> a [HtmlEvent.onClick EnterCreateMode, class "clickable", class c] [text "+"])}
                            (Display.menu DisplayEvent model.display)
                        ,   Menu.Section {name = "Topics", icon = Just (\c -> a [HtmlEvent.onClick (OpenDialog addTopicDialog_), class "clickable", class c] [text "+"])}
                            (Menu.rules RuleEvent model.rules)
                        ]
                    ]
                ]) |> Just
            ,   core.dialog |> Maybe.map (\(d, _) -> ("dialog", Dialog.view DialogEvent d))
            ,   ("notification", Notification.view NotificationEvent [id "notification"] model.notification) |> Just
            ]
        )
        |> List.singleton
    }

addTopicDialog_: Dialog.Model Event
addTopicDialog_ =
    {   title = "Add a new Topic"
    ,   sections =
        [   {   subtitle = "Load from a URL"
            ,   lines = [[Dialog.Text {id = "url"}]]
            }
        ,   {   subtitle = "Load from a file"
            ,   lines = [[Dialog.Button {text = "Select a file", event =(FileSelect TopicFile)}]]
            }
        ]
    ,   success = (\val -> case Dict.get "url" val of
            Just (Dialog.TextValue a) -> RuleEvent (Rules.Download a)
            _ -> NoOp
        )
    ,   cancel = CloseDialog
    ,   focus = Just "url"
    ,   inputFields = Dict.empty
    }

parameterDialog_: Rules.Model -> Actions.MatchedRule -> Dialog.Model Event
parameterDialog_ rules params = Dialog.processMathInput inputMouseCmd focusTextBar_ (Rules.functionProperties rules)
    {   title = "Set parameters for " ++ params.title
    ,   sections =
        [   case params.matches of
                [] -> {subtitle = "", lines = []}
                [m] -> {subtitle = "", lines = [[Dialog.FormattedInfo (Actions.matchToLatex [class "centered"] m)]]}
                _ -> { subtitle = ""
                    , lines =
                        [   [Dialog.Info {text = "Select the pattern"}]
                        ,   [   Dialog.Radio
                                {   name = "_method"
                                ,   options = List.indexedMap
                                    (\k m ->
                                        (   k
                                        ,   Actions.matchToLatex [] m
                                        )
                                    )
                                    params.matches
                                    |> Dict.fromList
                                }
                            ]
                        ]
                    }
        ,   {   subtitle = "Fill in the parameters"
            ,   lines = Dict.toList params.parameters
                    |> List.map (\(key, param) ->
                        [Dialog.ParameterInput {id = key, args = param.arguments, example = param.example}, Dialog.Info {text = param.description}]
                    )
            }
        ]
    ,   success = ApplyParameters
    ,   cancel = CloseDialog
    ,   focus = Nothing
    ,   inputFields = Dict.empty
    }

substitutionDialog_: List Display.FullEquation -> Dialog.Model Event
substitutionDialog_ eqs =
    let
        eqDict = eqs
            |> List.indexedMap Tuple.pair
            |> Dict.fromList
    in
    {   title = "Substitute a variable for a formula"
    ,   sections =
        [{  subtitle = "Select the equation to use for substitution"
        ,   lines = [[
                Dialog.Radio
                {   name = "eqNum"
                ,   options = eqDict |> Dict.map (\_ eq -> eq
                        |> Rules.toLatex
                        |> MathIcon.static []
                        |> List.singleton
                        |> Html.div [class "dialogLatex"]
                        )
                }
            ]]
        }]
    ,   success = (\dict -> case Dict.get "eqNum" dict of
            Just (Dialog.IntValue a) -> eqDict |> Dict.get a |> Maybe.map (ApplySubstitution True) |> Maybe.withDefault NoOp
            _ -> NoOp
        )
    ,   cancel = CloseDialog
    ,   focus = Just "eqNum"
    ,   inputFields = Dict.empty
    }

numSubDialog_: Rules.Model -> Int -> Float -> Dialog.Model Event
numSubDialog_ rules root target = Dialog.processMathInput inputMouseCmd focusTextBar_ (Rules.functionProperties rules)
    {   title = "Expand a number into an expression"
    ,   sections =
        [{  subtitle = "Choose the expression to replace " ++ String.fromFloat target
        ,   lines = [[Dialog.MathInput {id="expr"}]]
        }]
    ,   success = (\dict -> case Dict.get "expr" dict of
            Just (Dialog.MathValue val) -> ConvertSubString root target val
            _ -> NoOp
        )
    ,   cancel = CloseDialog
    ,   focus = Just "expr-input"
    ,   inputFields = Dict.empty
    }

leaveDialog_: String -> Dialog.Model Event
leaveDialog_ url =
    {   title = "Are you sure you want to leave?"
    ,   sections =
        [{  subtitle = "You are being redirected to:"
        ,   lines = [[Dialog.Link {url = url}]]
        }]
    ,   success = (\_ -> RedirectTo url)
    ,   cancel = CloseDialog
    ,   focus = Just "expr"
    ,   inputFields = Dict.empty
    }

{-
## State
-}

loadSources: List String -> Cmd Event
loadSources sources = List.map
    (\url -> Http.get { url = url, expect = Http.expectJson (ProcessSource url) sourceDecoder})
    (if List.isEmpty sources then ["source.json"] else sources)
    |> Cmd.batch

sourceDecoder: Decode.Decoder Source
sourceDecoder = Decode.map Source
    (Decode.field "topics" <| Decode.dict Rules.sourceDecoder)

triplet: a -> b -> c -> (a,b,c)
triplet x y z = (x,y,z)

swappableDecoder: (List Display.FullEquation -> Cmd Display.Event) -> Decode.Decoder (Swappable, Animation.Tracker)
swappableDecoder updateQuery = Decode.map2 Tuple.pair
    (   Decode.map2 Tuple.pair
        (Decode.field "display" (Display.decoder setCapture updateQuery displayMouseCmd))
        (Decode.field "rules" Rules.decoder)
    )
    (   Decode.map3 triplet
        (Decode.field "menu" Menu.decoder)
        (Decode.field "evaluator" (Evaluate.decoder evaluateString evalTypeDecoder_))
        (Decode.field "showMenu" Decode.bool)
    )
    |> Decode.map (\(((display, tracker), rules),(menu,evaluator,showMenu)) ->
       (Swappable display rules Notification.init menu evaluator showMenu, tracker)
    )

evalTypeDecoder_: Decode.Decoder EvalType
evalTypeDecoder_ = Decode.field "type" Decode.string
    |> Decode.andThen (\t -> case t of
        "numSub" -> Decode.map3 NumSubType_
            (Decode.field "node" Decode.int)
            (Decode.field "target" Decode.float)
            (Decode.field "replacement" (Math.decoder (Decode.maybe Rules.functionPropDecoder)))
        "eval" -> Decode.map EvalType_ (Decode.field "node" Decode.int)
        _ -> Decode.fail "unknown type"
    )

-- All internal state information should be encoded. This is mainly useful for debugging / bug-reports
-- Except for query, since it's only a reflection of the state
saveFile: Swappable -> Cmd Event
saveFile model = Encode.encode 0
    (   Encode.object
        [   ("display", Display.encode model.display)
        ,   ("rules", Rules.encode model.rules)
        ,   ("notification", Notification.encode model.notification)
        ,   ("menu", Menu.encode model.menu)
        ,   ("evaluator", Evaluate.encode (\t -> case t of
                NumSubType_ node f replacement -> Encode.object
                    [   ("node",Encode.int node)
                    ,   ("target",Encode.float f)
                    ,   (   "replacement"
                        ,   Math.encode (\s -> case s of
                                Nothing -> Encode.null
                                Just prop -> Rules.encodeFunctionProp prop |> Encode.object
                            )
                            replacement
                        )
                    ,   ("type",Encode.string "numSub")
                    ]
                EvalType_ node -> Encode.object [("node",Encode.int node),("type",Encode.string "eval")]
                ) model.evaluator
            )
        ,   ("showMenu", Encode.bool model.showMenu)
        ]
    )
    |> FDownload.string "math.json" "application/json"
