port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvent
import Browser.Navigation as Nav
import Dict
import File
import File.Download as FDownload
import File.Select as FSelect
import Html exposing (Html, a, div, form, input, pre, text)
import Html.Attributes exposing (class, id, name, placeholder, type_)
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
import Components.Display as Display
import Components.Evaluate as Evaluate
import Components.Query as Query
import Components.Rules as Rules
import Components.Tutorial as Tutorial
import Helper
import UI.ActionView as ActionView
import UI.Animation as Animation
import UI.Dialog as Dialog
import UI.Draggable as Draggable
import UI.HistoryView as HistoryView
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
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

-- Types

type alias Model =
    {   swappable: Swappable
    ,   query: Query.Model
    ,   size: Draggable.Size
    ,   dialog: Maybe (Dialog.Model Event, Maybe (Rules.Parameters Display.State))
    }

type alias Swappable =
    {   display: Display.Model
    ,   rules: Rules.Model
    ,   tutorial: Tutorial.Model
    ,   notification: Notification.Model
    ,   menu: Menu.Model
    ,   evaluator: Evaluate.Model EvalType Event
    -- UI fields
    ,   createMode: Maybe (Animation.DeletableElement Int Event)
    ,   nextCreateInt: Int
    ,   showHelp: Bool
    ,   showMenu: Bool
    ,   showHistory: Bool
    ,   historyBox: Draggable.Model
    }

type Event =
    EventUrlRequest Browser.UrlRequest
    | EventUrlChange Url.Url
    | DisplayEvent Display.Event
    | RuleEvent (Rules.Event Display.State)
    | TutorialEvent Tutorial.Event
    | NotificationEvent Notification.Event
    | MenuEvent Menu.Event
    | HistoryEvent HistoryView.Event
    -- Event from the UI
    | NoOp -- For setting focus on textbox
    | PressedKey {ctrl: Bool, shift: Bool, key: String}
    | EnterCreateMode
    | CancelCreateMode
    | DeleteCreateMode Int
    | SubmitEquation String
    | ToggleHelp
    | ToggleMenu
    | Save
    | OpenDialog (Dialog.Model Event)
    | CloseDialog
    | ProcessTopic String (Result Http.Error Rules.Topic)
    | ProcessSource String (Result Http.Error Source)
    | FileSelect LoadableFile
    | FileSelected LoadableFile File.File
    | FileLoaded LoadableFile String
    | ToggleHistory
    | WindowResize Int Int
    -- Rules
    | ApplyParameters (Dict.Dict String Dialog.Extracted)
    | ApplySubstitution Int Int Int -- eqNum root otherEqNum
    | ConvertSubString Int Int Float String -- eqNum root target subExpr
    | EvalComplete {id: Int, value: Float}

type LoadableFile =
    TopicFile
    | SaveFile

type EvalType =
    NumSubType_ Int Int Float (Math.Tree ())
    | EvalType_ Int Int

type alias Source =
    {   topics: Dict.Dict String String
    }

-- Events

init: Decode.Value -> Url.Url -> Nav.Key -> (Model, Cmd Event)
init flags url key =
    let
        query = Query.parseInit url key
        (eqs, errs) = List.foldl parseEquations_ ([], []) query.equations |> \(a, b) -> (List.reverse a, List.reverse b)
        (nModel, nCmd) = List.foldl Notification.displayError (Notification.init, Cmd.none) errs
        newScreen = List.isEmpty eqs
    in
    (   {   swappable = { display = Display.init eqs
            , rules = Rules.init
            , tutorial = Tutorial.init
            , notification = nModel
            , menu = Menu.init (Set.fromList ["Settings", "Equations", "Tutorials"])
            , evaluator = Evaluate.init evaluateString
            , createMode = if newScreen then Just (Animation.newDeletable (uiCancelCmd_ 0) 0) else Nothing
            , nextCreateInt = if newScreen then 1 else 0
            , showHelp = False
            , showMenu = False
            , showHistory = False
            , historyBox = Draggable.init "history" (60, 10) (30, 80)
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
        ,   if newScreen then focusTextBar_ "textInput" else Cmd.none
        ,   loadSources query.sources
        ]
    )

uiCancelCmd_: Int -> () -> Cmd Event
uiCancelCmd_ num _ = Animation.delayEvent 500 (DeleteCreateMode num)

parseEquations_: String -> (List (Matcher.Equation Display.State), List String) -> (List (Matcher.Equation Display.State), List String)
parseEquations_ elem (result, errs) = case Matcher.parseEquation Display.createState Display.updateState elem of
    Result.Ok root -> (root :: result, errs)
    Result.Err err -> (result, err :: errs )

subscriptions: Model -> Sub Event
subscriptions model = Sub.batch
    [   onKeyDown PressedKey
    ,   evaluateResult EvalComplete
    ,   BrowserEvent.onResize WindowResize
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
        DisplayEvent e -> let (dModel, dCmd) = Display.update e model.display in
            (updateCore {model | display = dModel}, Cmd.batch [ Cmd.map DisplayEvent dCmd, updateQuery_ core dModel, updateMathJax ()])
        TutorialEvent e -> let (tModel, tCmd) = Tutorial.update e model.tutorial in
            (updateCore {model | tutorial = tModel}, Cmd.map TutorialEvent tCmd)
        NotificationEvent e -> let (nModel, nCmd) = Notification.update e model.notification in
            (updateCore {model | notification = nModel}, Cmd.map NotificationEvent nCmd)
        MenuEvent e -> (updateCore {model | menu = Menu.update e model.menu}, Cmd.none)
        HistoryEvent e -> case e of
            HistoryView.DraggableEvent de -> let (newBox, action) = Draggable.update core.size de model.historyBox in
                (updateCore {model | historyBox = newBox}, actionToCapture_ action)
            HistoryView.DisplayEvent de -> let (dModel, dCmd) = Display.update de model.display in
                (updateCore {model | display = dModel}, Cmd.batch [ Cmd.map DisplayEvent dCmd, updateQuery_ core dModel, updateMathJax ()])
        NoOp -> (core, Cmd.none)
        PressedKey input -> case (input.ctrl, input.shift, input.key) of
            (_, _, "Escape") -> case (core.dialog, model.createMode) of
                (Just _, _) -> ({core | dialog = Nothing}, Cmd.none)
                (_, Just m) -> Animation.delete m
                    |> \(newM, cmd) -> (updateCore {model | createMode = Just newM, showHelp=False}, cmd)
                _ -> (updateCore {model | showMenu = not model.showMenu}, Cmd.none)
            (True, False, "z") -> case Display.undo model.display of
                Err errStr -> submitNotification_ core errStr
                Ok display -> (updateCore {model | display = display}, Cmd.none)
            (True, True, "z") -> case Display.redo model.display of
                Err errStr -> submitNotification_ core errStr
                Ok display -> (updateCore {model | display = display}, Cmd.none)
            _ -> (core, Cmd.none)
        EnterCreateMode -> if model.createMode |> Maybe.map .deleting |> Maybe.withDefault True
            then (  updateCore
                    {   model
                    |   createMode = Just (Animation.newDeletable (uiCancelCmd_ model.nextCreateInt) model.nextCreateInt)
                    ,   nextCreateInt = model.nextCreateInt + 1
                    }
                ,   focusTextBar_ "textInput"
                )
            else (core, focusTextBar_ "textInput")
        CancelCreateMode -> case model.createMode of
            Nothing -> (core, Cmd.none)
            Just m -> Animation.delete m
                |> \(newM, cmd) -> (updateCore {model | createMode = Just newM, showHelp=False}, cmd)
        DeleteCreateMode num -> case model.createMode of
            Nothing -> (core, Cmd.none)
            Just m -> if m.element /= num then (core, Cmd.none)
                else (updateCore {model | createMode = Nothing}, Cmd.none)
        SubmitEquation str -> if str == "" then (updateCore {model | createMode = Nothing, showHelp=False}, Cmd.none)
            else case Matcher.parseEquation Display.createState Display.updateState str of
                Result.Ok root -> Display.addEquation root model.display
                    |> (\dModel ->
                        (   updateCore {model | createMode = Nothing, display = dModel, showHelp = False}
                        ,   Cmd.batch [updateQuery_ core dModel, updateMathJax ()]
                        )
                    )
                Result.Err err -> submitNotification_ core err
        ToggleHelp -> (updateCore {model | showHelp = not model.showHelp}, focusTextBar_ "textInput")
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
            SaveFile -> Decode.decodeString swappableDecoder str
                |> Result.mapError Decode.errorToString
                |> (\result -> case result of
                    Err errStr -> submitNotification_ core errStr
                    Ok s -> ({core | swappable = s}, updateQuery_ core s.display)
                )
        ToggleHistory -> (updateCore {model | showHistory = not model.showHistory}, Cmd.none)
        WindowResize width height -> ({core | size = (toFloat width, toFloat height)}, Cmd.none)
        RuleEvent e -> case e of
            Rules.Apply p -> if List.length p.matches == 1 && List.isEmpty p.parameters
                then case Helper.listIndex 0 p.matches of
                    Nothing -> submitNotification_ core "Unable to extract the match"
                    Just m -> applyChange_ m core
                else ({ core | dialog = Just (parameterDialog_ p, Just p)}, Cmd.none)
            Rules.Group eqNum root children -> case Display.groupChildren eqNum root children model.display of
                Err errStr -> submitNotification_ core errStr
                Ok dModel -> (updateCore {model | display = dModel}, updateQuery_ core dModel)
            Rules.Ungroup eqNum root selected -> case Display.ungroupChildren eqNum root selected model.display of
                Err errStr -> submitNotification_ core errStr
                Ok dModel -> (updateCore {model | display = dModel}, updateQuery_ core dModel)
            Rules.Substitute eqNum root -> ({core | dialog = Just (substitutionDialog_ eqNum root model, Nothing)} , Cmd.none)
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
                            Dialog.TextValue val -> Math.parse val |> Result.map (\tree -> {r | from = Matcher.addMatch k Dict.empty tree r.from})
                            Dialog.FunctionValue args val -> List.indexedMap Tuple.pair args
                                |> Helper.resultList (\(i, name) dict -> Math.validVariable name |> Result.map (\n -> Dict.insert n i dict) ) Dict.empty
                                |> Result.andThen (\argDict -> Math.parse val
                                    |> Result.map (\tree -> {r | from = Matcher.addMatch k argDict tree r.from})
                                )
                            _ -> Ok r
                        )
                    prev params
                )
                |> (\result -> case result of
                    Err errStr -> submitNotification_ core errStr
                    Ok newParams -> applyChange_ newParams core
                )
            _ -> ({ core | dialog = Nothing}, Cmd.none)
        ApplySubstitution origNum root eqNum -> ({core | dialog = Nothing}, Cmd.none)
        ConvertSubString eqNum root target str -> case Math.parse str |> Result.andThen (Rules.replaceGlobalVar model.rules) of
            Err errStr -> submitNotification_ core errStr
            Ok newTree -> case Rules.evaluateStr model.rules newTree of
                Err errStr -> submitNotification_ core errStr
                Ok evalStr -> let (eModel, cmd) = Evaluate.send (NumSubType_ eqNum root target newTree) evalStr model.evaluator in
                    (updateCore {model | evaluator = eModel}, cmd)
        EvalComplete reply -> let (eModel, c) = Evaluate.finish reply.id model.evaluator in
            let newCore = updateCore {model | evaluator = eModel } in
            case c of
                Nothing -> submitNotification_ newCore "Unable to evaluate a string"
                Just (NumSubType_ eqNum root target tree) -> if target /= reply.value
                    then submitNotification_ newCore ("Expression evaluates to: " ++ String.fromFloat reply.value ++ ", but expecting: " ++ String.fromFloat target)
                    else case Display.replaceNumber eqNum root target tree model.display of
                        Err errStr -> submitNotification_ newCore errStr
                        Ok dModel -> ({core | dialog = Nothing, swappable = {model | evaluator = eModel, display = dModel}}, updateQuery_ core dModel)
                Just (EvalType_ eqNum id) -> case Display.replaceNodeWithNumber eqNum id reply.value model.display of
                    Err errStr -> submitNotification_ newCore errStr
                    Ok dModel -> ({core | swappable = {model | evaluator = eModel, display = dModel}}, updateQuery_ core dModel)


-- TODO
applyChange_: {from: Matcher.MatchResult Display.State, name: String, matcher: Matcher.Matcher} -> Model -> (Model, Cmd Event)
applyChange_ params model = let swappable = model.swappable in
    case Display.transformEquation params.matcher params.from swappable.display of
        Err errStr -> submitNotification_ model errStr
        Ok newDisplay -> ({model | dialog = Nothing, swappable = {swappable | display = newDisplay}}, updateQuery_ model newDisplay)

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

updateQuery_: Model -> Display.Model -> Cmd Event
updateQuery_ model dModel = let query = Query.setEquations (Display.listEquations dModel) model.query in
    Query.pushUrl query


focusTextBar_: String -> Cmd Event
focusTextBar_ id = Dom.focus id |> Task.attempt (\_ -> NoOp)

actionToCapture_: Maybe Draggable.Action -> Cmd Event
actionToCapture_ action = case action of
    Nothing -> Cmd.none
    Just (Draggable.SetCapture eId pId) -> capture {set = True, eId = eId, pId = pId}
    Just (Draggable.ReleaseCapture eId pId) -> capture {set = False, eId = eId, pId = pId}

{-
## UI
-}

view: Model -> Browser.Document Event
view core = let model = core.swappable in
    { title = "Maths"
    , body = Html.Keyed.node "div" [id "body"]
        (   List.filterMap identity
            [   ("display", Display.view DisplayEvent [id "display"] model.display) |> Just
            ,   ("history", HistoryView.view HistoryEvent model.historyBox model.display) |> Helper.maybeGuard model.showHistory
            ,   ("actions", ActionView.view RuleEvent model.rules model.display) |> Just
            ,   ("inputPane", div [id "inputPane"]
                [   Html.Keyed.node "div" [id "leftPane"]
                    (  List.filterMap identity
                        [   ("helpText", pre [id "helpText"] [text Math.notation]) |> Helper.maybeGuard model.showHelp
                        ,   model.createMode |> Maybe.map (inputDiv)
                        ]
                    )
                ,   div (id "rightPane" :: (if model.showMenu then [] else [class "closed"]))
                    [   Menu.view MenuEvent model.menu
                        [   Menu.Section {name = "Settings", icon = Nothing}
                            [   Menu.Content [a [HtmlEvent.onClick (FileSelect SaveFile), class "clickable"] [text "Open"]]
                            ,   Menu.Content [a [HtmlEvent.onClick Save, class "clickable"] [text "Save"]]
                            ,   Menu.Content [a [HtmlEvent.onClick ToggleHistory, class "clickable"] [text "Show History"]]
                            ]
                        ,   Menu.Section {name = "Equations", icon = Just (a [HtmlEvent.onClick EnterCreateMode, class "clickable"] [text "+"])}
                            (Display.menu DisplayEvent model.display)
                        ,   Tutorial.menu TutorialEvent model.tutorial
                        ,   Menu.Section {name = "Topics", icon = Just (a [HtmlEvent.onClick (OpenDialog addTopicDialog_), class "clickable"] [text "+"])}
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

inputDiv: Animation.DeletableElement Int Event -> (String, Html Event)
inputDiv model =
    (   "textbar"++String.fromInt model.element
    ,    form
        (   [class "textbar", HtmlEvent.onSubmitField "equation" SubmitEquation]
        |>  Helper.maybeAppend (Animation.class model)
        )
        [   Icon.equation [id "help", Icon.class "clickable", Icon.class "helpable", HtmlEvent.onClick ToggleHelp]
        ,   input
            (   [ type_ "text"
                , name "equation"
                , id "textInput"
                , placeholder "Type an equation to solve"
                ]
            )
            []
        ]
    )

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

parameterDialog_: Rules.Parameters Display.State -> Dialog.Model Event
parameterDialog_ params =
    {   title = "Set parameters for " ++ params.title
    ,   sections =
            [{   subtitle = "Fill in the parameters"
            ,   lines = params.parameters
                    |> List.map (\param -> if param.args == 0
                        then [Dialog.Info {text = param.name ++ "= "}, Dialog.Text {id = param.name}, Dialog.Info {text = param.description}]
                        else [Dialog.Function {name = param.root, arguments = param.args}, Dialog.Info {text = param.description}]
                    )
            }]
            |> (\sections -> if List.length params.matches <= 1 then sections
                else { subtitle = ""
                    , lines =
                        [   [Dialog.Info {text = "Select the pattern"}]
                        ,   [Dialog.Radio {name = "_method", options = List.indexedMap (\k m -> (k, m.name)) params.matches |> Dict.fromList}]
                        ]
                    }
                    :: sections
            )
    ,   success = ApplyParameters
    ,   cancel = CloseDialog
    ,   focus = Nothing
    }

substitutionDialog_: Int -> Int -> Swappable -> Dialog.Model Event
substitutionDialog_ eqNum root model =
    {   title = "Substitute a variable for a formula"
    ,   sections =
        [{  subtitle = "Select the equation to use for substitution"
        ,   lines = [[
                Dialog.Radio
                {   name = "eqNum"
                ,   options = Dict.filter (\k _ -> k /= eqNum) model.display.equations
                        |> Dict.map (\_ -> History.current >> .root >> Math.toString)
                }
            ]]
        }]
    ,   success = (\dict -> case Dict.get "eqNum" dict of
            Just (Dialog.IntValue a) -> ApplySubstitution eqNum root a
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
    (Decode.oneOf [Decode.field "topics" (Decode.dict Decode.string), Decode.succeed Dict.empty])

triplet: a -> b -> c -> (a,b,c)
triplet x y z = (x,y,z)

quarter: a -> b -> c -> d -> ((a,b),(c,d))
quarter w x y z = ((w,x),(y,z))

swappableDecoder: Decode.Decoder Swappable
swappableDecoder = Decode.map3 triplet
    (   Decode.map3 triplet
        (Decode.field "display" Display.decoder)
        (Decode.field "rules" Rules.decoder)
        (Decode.field "tutorial" Tutorial.decoder)
    )
    (   Decode.map4 (\a b c d -> ((a,b),(c,d)))
        (Decode.field "notification" Notification.decoder)
        (Decode.field "menu" Menu.decoder)
        (Decode.field "evaluator" (Evaluate.decoder evaluateString evalTypeDecoder_))
        (Decode.maybe (Decode.field "createMode" createModeDecoder_))
    )
    (   Decode.map5 (\a b c d e -> ((a,b),(c,d, e)))
        (Decode.field "nextCreateInt" Decode.int)
        (Decode.field "showHelp" Decode.bool)
        (Decode.field "showMenu" Decode.bool)
        (Decode.field "showHistory" Decode.bool)
        (Decode.field "historyBox" Draggable.decoder)
    )
    |> Decode.map (\((display, rules, tutorial),((notification,menu),(evaluator,createMode)),((nextCreateInt,showHelp),(showMenu,showHistory,historyBox))) ->
       Swappable display rules tutorial notification menu evaluator createMode nextCreateInt showHelp showMenu showHistory historyBox
    )

createModeDecoder_: Decode.Decoder (Animation.DeletableElement Int Event)
createModeDecoder_ = Decode.field "element" Decode.int
    |> Decode.andThen (\id -> Animation.decoder Decode.int (uiCancelCmd_ id))

evalTypeDecoder_: Decode.Decoder EvalType
evalTypeDecoder_ = Decode.field "type" Decode.string
    |> Decode.andThen (\t -> case t of
        "numSub" -> Decode.map4 NumSubType_ (Decode.field "eq" Decode.int) (Decode.field "node" Decode.int) (Decode.field "target" Decode.float) (Decode.field "root" (Math.decoder (Decode.succeed ())))
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
                NumSubType_ eq node f tree -> Encode.object [("eq",Encode.int eq), ("node",Encode.int node), ("target",Encode.float f),("tree",Math.encode (\_ -> Encode.null) tree),("type",Encode.string "numSub")]
                EvalType_ eq node -> Encode.object [("eq",Encode.int eq),("node",Encode.int node),("type",Encode.string "eval")]
                ) model.evaluator
            )
        ,   (   "createMode"
            ,   case model.createMode of
                Nothing -> Encode.null
                Just m -> Animation.encode Encode.int m
            )
        ,   ("nextCreateInt", Encode.int model.nextCreateInt)
        ,   ("showHelp", Encode.bool model.showHelp)
        ,   ("showMenu", Encode.bool model.showMenu)
        ,   ("showHistory", Encode.bool model.showHistory)
        ,   ("historyBox", Draggable.encode model.historyBox)
        ]
    )
    |> FDownload.string "math.json" "application/json"
