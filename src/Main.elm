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
import Components.Latex as Latex
import Components.Query as Query
import Components.Rules as Rules
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

port updateMathJax: () -> Cmd msg
port evaluateString: {id: Int, str: String} -> Cmd msg
port evaluateResult: ({id: Int, value: Float} -> msg) -> Sub msg
port capture: {set: Bool, eId: String, pId: Encode.Value} -> Cmd msg
port onKeyDown: ({ctrl: Bool, shift: Bool, key: String} -> msg) -> Sub msg

setCapture: Bool -> String -> Encode.Value -> Cmd msg
setCapture s e p = capture {set = s, eId = e, pId = p}

-- Types

type alias Model =
    {   swappable: Swappable
    ,   query: Query.Model
    ,   size: Draggable.Size
    ,   dialog: Maybe (Dialog.Model Event, Maybe (Rules.Parameters Animation.State))
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
    | RuleEvent (Rules.Event Animation.State)
    | TutorialEvent Tutorial.Event
    | NotificationEvent Notification.Event
    | MenuEvent Menu.Event
    | ActionEvent ActionView.Event
    | InputEvent Input.Event
    -- Event from the UI
    | NoOp -- For setting focus on textbox
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
    | ApplySubstitution Int (Set.Set Int) Int -- eqNum root otherEqNum
    | ConvertSubString Int Int Float String -- eqNum root target subExpr
    | EvalComplete {id: Int, value: Float}

type LoadableFile =
    TopicFile
    | SaveFile

type EvalType =
    NumSubType_ Int Int Float Matcher.Replacement
    | EvalType_ Int Int

type alias Source =
    {   topics: Dict.Dict String Rules.Source
    }

-- Events

init: Decode.Value -> Url.Url -> Nav.Key -> (Model, Cmd Event)
init flags url key =
    let
        query = Query.parseInit url key
        (eqs, errs) = List.foldl (parseEquations_ Rules.init) ([], []) query.equations
        (nModel, nCmd) = List.foldl Notification.displayError (Notification.init, Cmd.none) errs
        newScreen = List.isEmpty eqs
    in
    (   {   swappable = { display = Display.init setCapture (Query.pushEquations query) eqs
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
        }
    ,   Cmd.batch
        [   Cmd.map NotificationEvent nCmd
        ,   loadSources query.sources
        ]
    )

parseEquations_: Rules.Model -> String -> (List (Matcher.Equation Animation.State, Latex.Model (Matcher.State Animation.State)), List String) -> (List (Matcher.Equation Animation.State, Latex.Model (Matcher.State Animation.State)), List String)
parseEquations_ model elem (result, errs) = case parseEquation_ model elem of
    Result.Ok root -> (root :: result, errs)
    Result.Err err -> (result, err :: errs )

parseEquation_: Rules.Model -> String -> Result String (Matcher.Equation Animation.State, Latex.Model (Matcher.State Animation.State))
parseEquation_ model str = Matcher.parseEquation (Rules.functionProperties model) Animation.createState Animation.updateState str
    |> Result.andThen (\root -> Rules.toLatex model root |> Result.map (\l -> (root, l)) )

subscriptions: Model -> Sub Event
subscriptions _ = Sub.batch
    [   onKeyDown PressedKey
    ,   evaluateResult EvalComplete
    ,   BrowserEvent.onResize WindowResize
    ,   BrowserEvent.onAnimationFrameDelta AnimationDelta
    ]

{-
## State changes
-}

update: Event -> Model -> ( Model, Cmd Event )
update event core = let model = core.swappable in
    let updateCore newModel = {core | swappable = newModel} in
    case event of
        EventUrlRequest _ -> (core, Cmd.none)
        EventUrlChange _ -> (core, Cmd.none)
        DisplayEvent e -> let (dModel, dCmd) = Display.update core.size e model.display in
            (updateCore {model | display = dModel}, Cmd.batch [ Cmd.map DisplayEvent dCmd, updateMathJax ()])
        TutorialEvent e -> let (tModel, tCmd) = Tutorial.update e model.tutorial in
            (updateCore {model | tutorial = tModel}, Cmd.map TutorialEvent tCmd)
        NotificationEvent e -> let (nModel, nCmd) = Notification.update e model.notification in
            (updateCore {model | notification = nModel}, Cmd.map NotificationEvent nCmd)
        MenuEvent e -> (updateCore {model | menu = Menu.update e model.menu}, Cmd.none)
        ActionEvent e -> let (newIn, inCmd) = Input.close model.input in
            (updateCore {model | actionView = ActionView.update e model.actionView, input = newIn}, Cmd.map InputEvent inCmd)
        InputEvent e -> let (newIn, submitted, inCmd) = Input.update e model.input in
            if submitted == "" then (updateCore {model | input = newIn}, Cmd.batch [Cmd.map InputEvent inCmd, focusTextBar_ "textInput"])
            else case parseEquation_ model.rules submitted of
                Result.Ok root -> Display.add root model.display
                    |> (\dModel ->
                        (   updateCore {model | display = dModel, input = newIn}
                        ,   Cmd.batch [updateQuery_ dModel, Cmd.map InputEvent inCmd, updateMathJax ()]
                        )
                    )
                Result.Err err -> submitNotification_ core err
        NoOp -> (core, Cmd.none)
        PressedKey input -> case (input.ctrl, input.shift, input.key) of
            (_, _, "Escape") -> case core.dialog of
                Just _ -> ({core | dialog = Nothing}, Cmd.none)
                Nothing -> case model.input.existing of
                    Just _ -> let (newIn, inCmd) = Input.close model.input in
                        (updateCore {model | input = newIn}, Cmd.map InputEvent inCmd)
                    Nothing -> if ActionView.isOpen model.actionView
                        then (updateCore {model | actionView = ActionView.hide model.actionView}, Cmd.none)
                        else (updateCore {model | showMenu = not model.showMenu}, Cmd.none)
            (True, False, "z") -> case Display.undo model.display of
                Err errStr -> submitNotification_ core errStr
                Ok display -> (updateCore {model | display = display}, Cmd.none)
            (True, True, "z") -> case Display.redo model.display of
                Err errStr -> submitNotification_ core errStr
                Ok display -> (updateCore {model | display = display}, Cmd.none)
            _ -> (core, Cmd.none)
        EnterCreateMode ->
            (   updateCore {model | input = Input.open model.input, actionView = ActionView.hide model.actionView, showMenu = False}
            ,   focusTextBar_ "textInput"
            )
        ToggleMenu -> (updateCore {model | showMenu = not model.showMenu}, Cmd.none)
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
            Ok topic -> case Rules.addTopic topic model.rules of
                Err errStr -> submitNotification_ core errStr
                Ok rModel -> (updateCore {model | rules = rModel}, Cmd.none)
        ProcessSource url result -> case result of
            Err err -> httpErrorToString_ url err |> submitNotification_ core
            Ok source -> (updateCore {model | rules = Rules.addSources source.topics model.rules}, Cmd.none)
        FileSelect fileType -> (core, FSelect.file ["application/json"] (FileSelected fileType))
        FileSelected fileType file -> ({core | dialog = Nothing}, Task.perform (FileLoaded fileType) (File.toString file))
        FileLoaded fileType str -> case fileType of
            TopicFile -> Decode.decodeString Rules.topicDecoder str
                |> Result.mapError Decode.errorToString
                |> Result.andThen (\topic -> Rules.addTopic topic model.rules)
                |> (\result -> case result of
                    Err errStr -> submitNotification_ core errStr
                    Ok rModel -> (updateCore {model | rules = rModel}, Cmd.none)
                )
            SaveFile -> Decode.decodeString (swappableDecoder (Query.pushEquations core.query)) str
                |> Result.mapError Decode.errorToString
                |> (\result -> case result of
                    Err errStr -> submitNotification_ core errStr
                    Ok s -> ({core | swappable = s}, updateQuery_ s.display)
                )
        WindowResize width height -> ({core | size = (toFloat width, toFloat height)}, Cmd.none)
        AnimationDelta millis -> (updateCore {model | display = Display.advanceTime millis model.display}, Cmd.none)
        RuleEvent e -> case e of
            Rules.Apply p -> if List.length p.matches == 1 && Dict.isEmpty p.parameters
                then case Helper.listIndex 0 p.matches of
                    Nothing -> submitNotification_ core "Unable to extract the match"
                    Just m -> applyChange_ m core
                else ({ core | dialog = Just (parameterDialog_ p, Just p)}, Cmd.none)
            Rules.Group eqNum root children -> case Display.groupChildren (Rules.toLatex model.rules) eqNum root children model.display of
                Err errStr -> submitNotification_ core errStr
                Ok dModel -> (updateCore {model | display = dModel}, updateQuery_ dModel)
            Rules.Ungroup eqNum root selected -> case Display.ungroupChildren (Rules.toLatex model.rules) eqNum root selected model.display of
                Err errStr -> submitNotification_ core errStr
                Ok dModel -> (updateCore {model | display = dModel}, updateQuery_ dModel)
            Rules.Substitute eqNum selected -> if Dict.size model.display.equations < 2 then submitNotification_ core "There are no equations to use for substitution"
                else ({core | dialog = Just (substitutionDialog_ eqNum selected model, Nothing)} , Cmd.none)
            Rules.NumericalSubstitution eqNum root target -> ({ core | dialog = Just (numSubDialog_ eqNum root target, Nothing)}, Cmd.none)
            Rules.Download url -> (core, Http.get { url = url, expect = Http.expectJson (ProcessTopic url) Rules.topicDecoder})
            Rules.Evaluate eq id evalStr -> let (eModel, cmd) = Evaluate.send (EvalType_ eq id) evalStr model.evaluator in
                (updateCore {model | evaluator = eModel}, cmd)
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
        ApplySubstitution origNum selected eqNum -> case Display.substitute (Rules.toLatex model.rules) (Rules.functionProperties model.rules) origNum selected eqNum model.display of
            Err errStr -> submitNotification_ core errStr
            Ok dModel -> ({core | swappable = {model | display = dModel}, dialog = Nothing}, updateQuery_ dModel)
        ConvertSubString eqNum root target str -> case Matcher.toReplacement (Rules.functionProperties model.rules) False Dict.empty str of
            Err errStr -> submitNotification_ core errStr
            Ok replacement -> case Rules.evaluateStr model.rules replacement of
                Err errStr -> submitNotification_ core errStr
                Ok evalStr -> let (eModel, cmd) = Evaluate.send (NumSubType_ eqNum root target replacement) evalStr model.evaluator in
                    (updateCore {model | evaluator = eModel}, cmd)
        EvalComplete reply -> let (eModel, c) = Evaluate.finish reply.id model.evaluator in
            let newCore = updateCore {model | evaluator = eModel } in
            case c of
                Nothing -> submitNotification_ newCore "Unable to evaluate a string"
                Just (NumSubType_ eqNum root target replacement) -> if target /= reply.value
                    then submitNotification_ newCore ("Expression evaluates to: " ++ String.fromFloat reply.value ++ ", but expecting: " ++ String.fromFloat target)
                    else case Display.replaceNumber (Rules.toLatex model.rules) eqNum root target replacement model.display of
                        Err errStr -> submitNotification_ newCore errStr
                        Ok dModel -> ({core | dialog = Nothing, swappable = {model | evaluator = eModel, display = dModel}}, updateQuery_ dModel)
                Just (EvalType_ eqNum id) -> case Display.replaceNodeWithNumber (Rules.toLatex model.rules) eqNum id reply.value model.display of
                    Err errStr -> submitNotification_ newCore errStr
                    Ok dModel -> ({core | swappable = {model | evaluator = eModel, display = dModel}}, updateQuery_ dModel)


-- TODO
applyChange_: {from: Matcher.MatchResult Animation.State, replacements: List {name: String, root: Matcher.Replacement}} -> Model -> (Model, Cmd Event)
applyChange_ params model = let swappable = model.swappable in
    case Display.transform (Rules.toLatex swappable.rules) params.replacements params.from swappable.display of
        Err errStr -> submitNotification_ model errStr
        Ok newDisplay -> ({model | dialog = Nothing, swappable = {swappable | display = newDisplay}}, updateQuery_ newDisplay)

submitNotification_: Model -> String -> (Model, Cmd Event)
submitNotification_ model str = let swappable = model.swappable in
    let (nModel, nCmd) = Notification.displayError str (swappable.notification, Cmd.none) in
    ({model | swappable = {swappable | notification = nModel}}, Cmd.map NotificationEvent nCmd)

httpErrorToString_: String -> Http.Error -> String
httpErrorToString_ url err = case err of
    Http.BadUrl _ -> "Invalid URL provided: " ++ url
    Http.Timeout -> "Timed out waitiing for: " ++ url
    Http.NetworkError -> "Unable to reach: " ++ url
    Http.BadStatus code -> "The url returned an error code [" ++ String.fromInt code ++ "]: " ++ url
    Http.BadBody str -> "The file is malformed:\n" ++ str

updateQuery_: Display.Model -> Cmd Event
updateQuery_ = Display.updateQueryCmd
    >> Tuple.second
    >> Cmd.map DisplayEvent

focusTextBar_: String -> Cmd Event
focusTextBar_ id = Dom.focus id |> Task.attempt (\_ -> NoOp)

{-
## UI
-}

view: Model -> Browser.Document Event
view core = let model = core.swappable in
    { title = "Maths"
    , body = Html.Keyed.node "div" [id "body"]
        (   Display.views DisplayEvent model.display
        ++  List.filterMap identity
            [   ("actions", ActionView.view RuleEvent ActionEvent model.rules (Display.getSelected model.display) model.actionView) |> Just
            ,   ("inputPane", div [id "inputPane"]
                [   Html.Keyed.node "div"
                    (id "leftPane" :: if model.showMenu then [HtmlEvent.onClick ToggleMenu] else [class "closed"])
                    (  List.filterMap identity
                        [   Input.view InputEvent model.input
                        ]
                    )
                ,   div (id "rightPane" :: (if model.showMenu then [] else [class "closed"]))
                    [   Menu.view MenuEvent model.menu
                        [   Menu.Section {name = "Settings", icon = Nothing}
                            [   Menu.Content [a [HtmlEvent.onClick (FileSelect SaveFile), class "clickable"] [text "Open"]]
                            ,   Menu.Content [a [HtmlEvent.onClick Save, class "clickable"] [text "Save"]]
                            ,   Menu.Content [a [class "clickable", href "https://github.com/jxz12/math", target "_blank"] [text "Github Source"]]
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

parameterDialog_: Rules.Parameters Animation.State -> Dialog.Model Event
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

substitutionDialog_: Int -> Set.Set Int -> Swappable -> Dialog.Model Event
substitutionDialog_ eqNum selected model =
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
            Just (Dialog.IntValue a) -> ApplySubstitution eqNum selected a
            _ -> NoOp
        )
    ,   cancel = CloseDialog
    ,   focus = Just "eqNum"
    }

numSubDialog_: Int -> Int -> Float -> Dialog.Model Event
numSubDialog_ eqNum root target =
    {   title = "Substitute a number for an expression"
    ,   sections =
        [{  subtitle = "The expression to replace " ++ String.fromFloat target
        ,   lines = [[Dialog.Text {id="expr"}]]
        }]
    ,   success = (\dict -> case Dict.get "expr" dict of
            Just (Dialog.TextValue val) -> ConvertSubString eqNum root target val
            _ -> NoOp
        )
    ,   cancel = CloseDialog
    ,   focus = Just "expr"
    }

{-
## State
-}

loadSources: List String -> Cmd Event
loadSources sources = List.map
    (\url -> Http.get { url = url, expect = Http.expectJson (ProcessSource url) sourceDecoder})
    ("source.json"::sources)
    |> Cmd.batch

sourceDecoder: Decode.Decoder Source
sourceDecoder = Decode.map Source
    (Decode.field "topics" <| Decode.dict Rules.sourceDecoder)

triplet: a -> b -> c -> (a,b,c)
triplet x y z = (x,y,z)

quarter: a -> b -> c -> d -> ((a,b),(c,d))
quarter w x y z = ((w,x),(y,z))

swappableDecoder: (List (Matcher.Equation Animation.State) -> Cmd Display.Event) -> Decode.Decoder Swappable
swappableDecoder updateQuery = Decode.map3 triplet
    (   Decode.map3 triplet
        (Decode.field "display" (Display.decoder setCapture updateQuery))
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
    |> Decode.map (\((display, rules, tutorial),(notification,menu,evaluator),(showMenu,actionView, input)) ->
       Swappable display rules tutorial notification menu evaluator showMenu actionView input
    )

evalTypeDecoder_: Decode.Decoder EvalType
evalTypeDecoder_ = Decode.field "type" Decode.string
    |> Decode.andThen (\t -> case t of
        "numSub" -> Decode.map4 NumSubType_ (Decode.field "eq" Decode.int) (Decode.field "node" Decode.int) (Decode.field "target" Decode.float) (Decode.field "replacement" Matcher.replacementDecoder)
        "eval" -> Decode.map2 EvalType_ (Decode.field "eq" Decode.int) (Decode.field "node" Decode.int)
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
                NumSubType_ eq node f replacement -> Encode.object [("eq",Encode.int eq), ("node",Encode.int node), ("target",Encode.float f),("replacement",Matcher.encodeReplacement replacement),("type",Encode.string "numSub")]
                EvalType_ eq node -> Encode.object [("eq",Encode.int eq),("node",Encode.int node),("type",Encode.string "eval")]
                ) model.evaluator
            )
        ,   ("showMenu", Encode.bool model.showMenu)
        ,   ("actionView", ActionView.encode model.actionView)
        ,   ("input", Input.encode model.input)
        ]
    )
    |> FDownload.string "math.json" "application/json"
