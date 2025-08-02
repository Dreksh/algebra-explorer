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
import Components.Actions as Actions
import Components.Tutorial as Tutorial
import Helper
import UI.ActionView as ActionView
import UI.Animation as Animation
import UI.Dialog as Dialog
import UI.Display as Display
import UI.Draggable as Draggable
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.Input as Input
import UI.Menu as Menu
import UI.Notification as Notification
import UI.SvgDrag as SvgDrag
import Components.Actions as Actions
import UI.Display as Display
import Components.Actions as Actions

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
port capture: {set: Bool, eId: String, pId: Encode.Value} -> Cmd msg
port onKeyDown: ({ctrl: Bool, shift: Bool, key: String} -> msg) -> Sub msg
port svgMouseBegin: {id: String, x: Float, y: Float} -> Cmd msg
port svgMouseEvent: (SvgDrag.Raw -> msg) -> Sub msg

setCapture: Bool -> String -> Encode.Value -> Cmd msg
setCapture s e p = capture {set = s, eId = e, pId = p}

displayMouseCmd: Int -> (Float, Float) -> Cmd msg
displayMouseCmd id (x, y) = svgMouseBegin {id = "Equation-" ++ String.fromInt id, x = x, y = y}

-- Types

type alias Model =
    {   swappable: Swappable
    ,   query: Query.Model
    ,   size: Draggable.Size
    ,   dialog: Maybe (Dialog.Model Event, Maybe Actions.Application)
    ,   animation: Animation.Tracker
    ,   svgDragMap: SvgDrag.Model Event
    }

type alias Swappable =
    {   display: Display.Model
    ,   rules: Rules.Model
    ,   tutorial: Tutorial.Model
    ,   notification: Notification.Model
    ,   menu: Menu.Model
    ,   evaluator: Evaluate.Model EvalType Event
    -- UI fields
    ,   showMenu: Bool
    ,   actionView: ActionView.Model
    ,   input: Input.Model
    }

type Event =
    EventUrlRequest Browser.UrlRequest
    | EventUrlChange Url.Url
    | DisplayEvent Display.Event
    | RuleEvent Rules.Event
    | ActionEvent Actions.Event
    | TutorialEvent Tutorial.Event
    | NotificationEvent Notification.Event
    | MenuEvent Menu.Event
    | ActionViewEvent ActionView.Event
    | InputEvent Input.Event
    | SvgDragEvent SvgDrag.Raw
    -- Event from the UI
    | NoOp -- For setting focus on textbox
    | RedirectTo String
    | PressedKey {ctrl: Bool, shift: Bool, key: String}
    | EnterCreateMode
    | ToggleMenu
    | Save
    | OpenDialog (Dialog.Model Event)
    | CloseDialog
    | ProcessTopic String (Result Http.Error Rules.Topic)
    | ProcessSource String (Result Http.Error Source)
    | FileSelect LoadableFile
    | FileSelected LoadableFile File.File
    | FileLoaded LoadableFile String
    | WindowResize Int Int
    | AnimationDelta Float
    -- Rules
    | ApplyParameters (Dict.Dict String Dialog.Extracted)
    | ApplySubstitution Int -- otherEqNum
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
            , tutorial = Tutorial.init
            , notification = nModel
            , menu = Menu.init (Set.fromList ["Settings", "Equations", "Tutorials"])
            , evaluator = Evaluate.init evaluateString
            , showMenu = False
            , actionView = ActionView.init
            , input = Input.init newScreen
            }
        , query = query
        , dialog = Nothing
        , size = Decode.decodeValue
            (Decode.map2 Tuple.pair (Decode.field "width" Decode.float) (Decode.field "height" Decode.float))
            flags
            |> Result.toMaybe
            |> Maybe.withDefault (0, 0)
        , animation = finalT
        , svgDragMap = SvgDrag.init
            (   Dict.singleton "Equation-" (\str event -> String.toInt str
                    |> Maybe.map (\eqNum -> Display.Commute eqNum event |> DisplayEvent)
                )
            )
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
                else ({core | dialog = Just (leaveDialog_ str, Nothing)}, Cmd.none)
        EventUrlChange _ -> (core, Cmd.none)
        DisplayEvent e -> let (dModel, newAnimation, dCmd) = Display.update core.size core.animation e model.display in
            ({core | swappable = {model | display = dModel}, animation = newAnimation}, Cmd.map DisplayEvent dCmd)
        TutorialEvent e -> let (tModel, tCmd) = Tutorial.update e model.tutorial in
            (updateCore {model | tutorial = tModel}, Cmd.map TutorialEvent tCmd)
        NotificationEvent e -> let (nModel, newT) = Notification.update core.animation e model.notification in
            ({core | animation = newT, swappable = {model | notification = nModel}}, Cmd.none)
        MenuEvent e -> (updateCore {model | menu = Menu.update e model.menu}, Cmd.none)
        ActionViewEvent e -> let (newIn, newT) = Input.close core.animation model.input in
            ({core | animation = newT, swappable = {model | actionView = ActionView.update e model.actionView, input = newIn}}, Cmd.none)
        InputEvent e -> let (newIn, submitted, newT) = Input.update core.animation e model.input in
            case submitted of
                Nothing -> ({core | animation = newT, swappable = {model | input = newIn}}, focusTextBar_ "textInput")
                Just "" -> if Display.anyVisible model.display
                    then ({core | animation = newT, swappable = {model | input = newIn}}, Cmd.none)
                    else ({core | animation = newT, swappable = {model | input = newIn, showMenu = True}}, Cmd.none)
                Just str -> case parseEquation_ model.rules str of
                    Result.Ok root -> Display.add newT root model.display
                        |> (\(dModel, animation) ->
                            (   { core | swappable = {model | display = dModel, input = newIn}, animation = animation }
                            ,   updateQuery_ dModel
                            )
                        )
                    Result.Err err -> submitNotification_ core err
        SvgDragEvent e -> case SvgDrag.resolve e core.svgDragMap of
            Nothing -> (core, Cmd.none)
            Just newE -> update newE core
        NoOp -> (core, Cmd.none)
        RedirectTo url -> (core, Nav.load url)
        PressedKey input -> case (input.ctrl, input.shift, input.key) of
            (_, _, "Escape") -> case core.dialog of
                Just _ -> ({core | dialog = Nothing}, Cmd.none)
                Nothing -> if ActionView.isOpen model.actionView
                    then (updateCore {model | actionView = ActionView.hide model.actionView}, Cmd.none)
                    else case (Display.anyVisible model.display, model.showMenu, model.input.current) of
                        (True, True, _) -> ({core | swappable = {model | showMenu = False}}, Cmd.none)
                        (True, False, Just _) -> let (newIn, newT) = Input.close core.animation model.input in
                            ({core | animation = newT, swappable = {model | input = newIn}}, Cmd.none)
                        (True, False, Nothing) -> ({core | swappable = {model | showMenu = True}}, Cmd.none)
                        (False, True, Just _) -> ({core | swappable = {model | showMenu = False}}, Cmd.none)
                        (False, _, Nothing) -> let (newIn, newT) = Input.open core.animation model.input in
                            ({core | animation = newT, swappable = {model | input = newIn, showMenu = False}}, Cmd.none)
                        (False, False, Just _) -> let (newIn, newT) = Input.close core.animation model.input in
                            ({core | animation = newT, swappable = {model | input = newIn, showMenu = True}}, Cmd.none)
            (True, False, "z") -> case Display.undo core.animation model.display of
                Err errStr -> submitNotification_ core errStr
                Ok (display, animation) -> ({core | swappable = {model | display = display}, animation = animation}, Cmd.none)
            (True, True, "z") -> case Display.redo core.animation model.display of
                Err errStr -> submitNotification_ core errStr
                Ok (display, animation) -> ({core | swappable = {model | display = display}, animation = animation}, Cmd.none)
            _ -> (core, Cmd.none)
        EnterCreateMode -> let (inputModel, newT) = Input.open core.animation model.input in
            (   {   core
                |   swappable = {   model
                    |   input = inputModel
                    ,   actionView = ActionView.hide model.actionView
                    ,   showMenu = False
                    }
                ,   animation = newT
                }
            ,   focusTextBar_ "textInput"
            )
        ToggleMenu -> if not model.showMenu
            then (updateCore {model | showMenu = True}, Cmd.none)
            else if Display.anyVisible model.display
            then (updateCore {model | showMenu = False}, Cmd.none)
            else let (newIn, newT) = Input.open core.animation model.input in
                ({core | animation = newT, swappable = {model | showMenu = False, input = newIn}}, Cmd.none)
        Save -> (core, saveFile model)
        OpenDialog d ->
            (   {core | dialog = Just (d, Nothing)}
            ,   case d.focus of
                Nothing -> Cmd.none
                Just name -> Dialog.fieldID name |> focusTextBar_
            )
        CloseDialog -> ({core | dialog = Nothing}, Cmd.none)
        ProcessTopic url result -> case result of
            Err err -> httpErrorToString_ url err |> submitNotification_ core
            Ok topic -> case Rules.addTopic (Just url) topic model.rules of
                Err errStr -> submitNotification_ core errStr
                Ok rModel -> let (dModel, t) = Display.refresh (Rules.functionProperties rModel) core.animation model.display in
                    (   {   core
                        |   swappable = { model | rules = rModel, display = dModel }
                        ,   animation = t
                        }
                    , Cmd.none
                    )
        ProcessSource url result -> case result of
            Err err -> httpErrorToString_ url err |> submitNotification_ core
            Ok source ->
                (   updateCore {model | rules = Rules.addSources source.topics model.rules}
                ,   Dict.toList source.topics
                    |> List.filterMap (\(_, s) -> if s.preinstall then Just (downloadTopicCmd_ s.url) else Nothing)
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
                    ,   input = Input.advance millis model.input
                    ,   notification = Notification.advance millis model.notification
                    }
                ,   animation = Animation.updateTracker millis core.animation
                }
            , Cmd.none
            )
        ActionEvent e -> case e of
            Actions.Commit -> case Display.historyCommit model.display of
                Err errStr -> submitNotification_ core errStr
                Ok dEvent -> update (DisplayEvent dEvent) core
            Actions.Reset -> case Display.historyReset model.display of
                Err errStr -> submitNotification_ core errStr
                Ok dEvent -> update (DisplayEvent dEvent) core
            Actions.Apply p -> if List.length p.matches == 1 && Dict.isEmpty p.parameters
                then case Helper.listIndex 0 p.matches of
                    Nothing -> submitNotification_ core "Unable to extract the match"
                    Just m -> applyChange_ m core
                else ({ core | dialog = Just (parameterDialog_ p, Just p)}, Cmd.none)
            Actions.Group root children -> case Display.groupChildren core.animation root children model.display of
                Err errStr -> submitNotification_ core errStr
                Ok (dModel, animation) -> ({core | swappable = {model | display = dModel}, animation = animation}, updateQuery_ dModel)
            Actions.Ungroup root -> case Display.ungroupChildren core.animation root model.display of
                Err errStr -> submitNotification_ core errStr
                Ok (dModel, animation) -> ({core | swappable = {model | display = dModel}, animation = animation}, updateQuery_ dModel)
            Actions.Substitute -> ({core | dialog = Just (substitutionDialog_ model, Nothing)} , Cmd.none)
            Actions.NumericalSubstitution root target -> ({ core | dialog = Just (numSubDialog_ root target, Nothing)}, Cmd.none)
            Actions.Evaluate id evalStr -> let (eModel, cmd) = Evaluate.send (EvalType_ id) evalStr model.evaluator in
                (updateCore {model | evaluator = eModel}, cmd)
        RuleEvent e -> case e of
            Rules.Download url -> (core, downloadTopicCmd_ url)
            Rules.Delete topicName -> ({core | dialog = Nothing, swappable = { model | rules = Rules.deleteTopic topicName model.rules}}, Cmd.none)
        ApplyParameters params -> case core.dialog of
            Just (_, Just existing) -> ( case Dict.get "_method" params of
                    Just (Dialog.IntValue n) -> Helper.listIndex n existing.matches
                    _ -> Helper.listIndex 0 existing.matches
                )
                |> Result.fromMaybe "Unable to find the match"
                |>  Result.andThen (\prev -> Helper.resultDict (\k v r -> if k == "_method" then Ok r
                        else case v of
                            Dialog.TextValue val -> Matcher.toReplacement (Rules.functionProperties model.rules) False Dict.empty val
                                |> Result.map (\tree -> {r | from = Matcher.addMatch k tree r.from})
                            Dialog.FunctionValue args val -> List.indexedMap Tuple.pair args
                                |> Helper.resultList (\(i, name) dict -> if Dict.member name dict
                                        then Err "Function arguments need to be unique in the function definition"
                                        else Math.validVariable name |> Result.map (\n -> Dict.insert n (0, i) dict)
                                    ) Dict.empty
                                |> Result.andThen (\argDict -> Matcher.toReplacement (Rules.functionProperties model.rules) False argDict val)
                                |> Result.map (\tree -> {r | from = Matcher.addMatch k tree r.from})
                            _ -> Ok r
                        )
                    prev params
                )
                |> (\result -> case result of
                    Err errStr -> submitNotification_ core errStr
                    Ok newParams -> applyChange_ newParams core
                )
            _ -> ({ core | dialog = Nothing}, Cmd.none)
        ApplySubstitution eqNum -> case Display.substitute core.animation eqNum model.display of
            Err errStr -> submitNotification_ core errStr
            Ok (dModel, animation) -> ({core | swappable = {model | display = dModel}, dialog = Nothing, animation = animation}, updateQuery_ dModel)
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
                    else case Display.replaceNumber core.animation root target replacement model.display of
                        Err errStr -> submitNotification_ newCore errStr
                        Ok (dModel, animation) -> ({core | dialog = Nothing, swappable = {model | evaluator = eModel, display = dModel}, animation = animation}, updateQuery_ dModel)
                Just (EvalType_ id) -> case Display.replaceNodeWithNumber core.animation id reply.value model.display of
                    Err errStr -> submitNotification_ newCore errStr
                    Ok (dModel, animation) -> ({core | swappable = {model | evaluator = eModel, display = dModel}, animation = animation}, updateQuery_ dModel)


applyChange_: {from: Matcher.MatchResult Rules.FunctionProp Animation.State, replacements: List {name: String, root: Matcher.Replacement Rules.FunctionProp}} -> Model -> (Model, Cmd Event)
applyChange_ params model = let swappable = model.swappable in
    case Display.transform model.animation params.replacements params.from swappable.display of
        Err errStr -> submitNotification_ model errStr
        Ok (newDisplay, animation) -> ({model | dialog = Nothing, swappable = {swappable | display = newDisplay}, animation = animation}, updateQuery_ newDisplay)

submitNotification_: Model -> String -> (Model, Cmd Event)
submitNotification_ model str = let swappable = model.swappable in
    let (nModel, newT, cmd) = Notification.displayError str (swappable.notification, model.animation, Cmd.none) in
    ({model | swappable = {swappable | notification = nModel}, animation = newT}, Cmd.map NotificationEvent cmd)

httpErrorToString_: String -> Http.Error -> String
httpErrorToString_ url err = case err of
    Http.BadUrl _ -> "Invalid URL provided: " ++ url
    Http.Timeout -> "Timed out waitiing for: " ++ url
    Http.NetworkError -> "Unable to reach: " ++ url
    Http.BadStatus code -> "The url returned an error code [" ++ String.fromInt code ++ "]: " ++ url
    Http.BadBody str -> "The file is malformed:\n" ++ str

updateQuery_: Display.Model -> Cmd Event
updateQuery_ = Display.updateQueryCmd 0
    >> \(_, _, c) -> Cmd.map DisplayEvent c

focusTextBar_: String -> Cmd Event
focusTextBar_ id = Dom.focus id |> Task.attempt (\_ -> NoOp)

downloadTopicCmd_: String -> Cmd Event
downloadTopicCmd_ url = Http.get
    {   url = url
    ,   expect = Http.expectJson (ProcessTopic url) Rules.topicDecoder
    }

{-
## UI
-}

view: Model -> Browser.Document Event
view core = let model = core.swappable in
    { title = "Maths"
    , body = let actions = Actions.rulesToActions model.rules (Dict.size model.display.equations) (Display.getSelected model.display) in
        Html.Keyed.node "div" [id "body"]
        (   Display.views DisplayEvent (ActionView.contextualActions ActionEvent actions) model.display
        ++  List.filterMap identity
            [   ("actions", ActionView.view ActionViewEvent ActionEvent actions model.actionView) |> Just
            ,   ("inputPane", div [id "inputPane"]
                [   Html.Keyed.node "div"
                    (id "leftPane" :: if model.showMenu then [HtmlEvent.onClick ToggleMenu] else [class "closed"])
                    (Input.view InputEvent model.input)
                ,   div (id "rightPane" :: (if model.showMenu then [] else [class "closed"]))
                    [   Menu.view MenuEvent model.menu
                        [   Menu.Section {name = "Settings", icon = Nothing}
                            [   Menu.Content [] [a [HtmlEvent.onClick (FileSelect SaveFile), class "clickable"] [text "Open"]]
                            ,   Menu.Content [] [a [HtmlEvent.onClick Save, class "clickable"] [text "Save"]]
                            ,   Menu.Content [] [a [class "clickable", href "https://github.com/jxz12/math", target "_blank"] [text "Github Source"]]
                            ]
                        ,   Menu.Section {name = "Equations", icon = Just (\c -> a [HtmlEvent.onClick EnterCreateMode, class "clickable", class c] [text "+"])}
                            (Display.menu DisplayEvent model.display)
                        ,   Tutorial.menu TutorialEvent model.tutorial
                        ,   Menu.Section {name = "Topics", icon = Just (\c -> a [HtmlEvent.onClick (OpenDialog addTopicDialog_), class "clickable", class c] [text "+"])}
                            (Rules.menuTopics RuleEvent model.rules)
                        ]
                    ,   Icon.menu (List.filterMap identity
                            [ id "menuToggle" |> Just
                            , HtmlEvent.onClick ToggleMenu |> Just
                            , Icon.class "clickable" |> Just
                            , Icon.class "closed" |> Helper.maybeGuard (not model.showMenu)
                            ]
                        )
                    ]
                ]) |> Just
            --,   ("tutorial", Tutorial.view TutorialEvent [] model.tutorial) |> Just
            ,   core.dialog |> Maybe.map (\(d, _) -> ("dialog", Dialog.view d))
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
    }

parameterDialog_: Actions.Application -> Dialog.Model Event
parameterDialog_ params =
    {   title = "Set parameters for " ++ params.title
    ,   sections =
            [{   subtitle = "Fill in the parameters"
            ,   lines = Dict.toList params.parameters
                    |> List.map (\(key, param) -> if param.arguments == 0
                        then [Dialog.Info {text = param.name ++ "= "}, Dialog.Text {id = param.name}, Dialog.Info {text = param.description}]
                        else [Dialog.Function {name = key, arguments = param.arguments}, Dialog.Info {text = param.description}]
                    )
            }]
            |> (\sections -> if List.length params.matches <= 1 then sections
                else { subtitle = ""
                    , lines =
                        [   [Dialog.Info {text = "Select the pattern"}]
                        ,   [Dialog.Radio {name = "_method", options = List.indexedMap (\k m -> (k, List.map (.name) m.replacements |> String.join ", ")) params.matches |> Dict.fromList}]
                        ]
                    }
                    :: sections
            )
    ,   success = ApplyParameters
    ,   cancel = CloseDialog
    ,   focus = Nothing
    }

substitutionDialog_: Swappable -> Dialog.Model Event
substitutionDialog_ model = let eqNum = model.display.selected |> Maybe.map (\(eq, _) -> eq) |> Maybe.withDefault -1 in
    {   title = "Substitute a variable for a formula"
    ,   sections =
        [{  subtitle = "Select the equation to use for substitution"
        ,   lines = [[
                Dialog.Radio
                {   name = "eqNum"
                ,   options = Dict.filter (\k _ -> k /= eqNum) model.display.equations
                        |> Dict.map (\_ -> .history >> History.current >> Tuple.first >> .root >> Rules.process (\_ -> String.join "") identity)
                }
            ]]
        }]
    ,   success = (\dict -> case Dict.get "eqNum" dict of
            Just (Dialog.IntValue a) -> ApplySubstitution a
            _ -> NoOp
        )
    ,   cancel = CloseDialog
    ,   focus = Just "eqNum"
    }

numSubDialog_: Int -> Float -> Dialog.Model Event
numSubDialog_ root target =
    {   title = "Expand a number into an expression"
    ,   sections =
        [{  subtitle = "The expression to replace " ++ String.fromFloat target
        ,   lines = [[Dialog.Text {id="expr"}]]
        }]
    ,   success = (\dict -> case Dict.get "expr" dict of
            Just (Dialog.TextValue val) -> ConvertSubString root target val
            _ -> NoOp
        )
    ,   cancel = CloseDialog
    ,   focus = Just "expr"
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

quarter: a -> b -> c -> d -> ((a,b),(c,d))
quarter w x y z = ((w,x),(y,z))

swappableDecoder: (List Display.FullEquation -> Cmd Display.Event) -> Decode.Decoder (Swappable, Animation.Tracker)
swappableDecoder updateQuery = Decode.map3 triplet
    (   Decode.map3 triplet
        (Decode.field "display" (Display.decoder setCapture updateQuery displayMouseCmd))
        (Decode.field "rules" Rules.decoder)
        (Decode.field "tutorial" Tutorial.decoder)
    )
    (   Decode.map3 triplet
        (Decode.field "notification" Notification.decoder)
        (Decode.field "menu" Menu.decoder)
        (Decode.field "evaluator" (Evaluate.decoder evaluateString evalTypeDecoder_))
    )
    (   Decode.map3 triplet
        (Decode.field "showMenu" Decode.bool)
        (Decode.field "actionView" ActionView.decoder)
        (Decode.field "input" Input.decoder)
    )
    |> Decode.map (\(((display, tracker), rules, tutorial),(notification,menu,evaluator),(showMenu,actionView, input)) ->
       (Swappable display rules tutorial notification menu evaluator showMenu actionView input, tracker)
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
        ,   ("tutorial", Tutorial.encode model.tutorial)
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
        ,   ("actionView", ActionView.encode model.actionView)
        ,   ("input", Input.encode model.input)
        ]
    )
    |> FDownload.string "math.json" "application/json"
