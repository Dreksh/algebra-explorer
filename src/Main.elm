port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvent
import Browser.Navigation as Nav
import Dict
import File
import File.Select as FSelect
import Json.Decode as Decode
import Html exposing (Html, a, button, div, form, input, pre, text)
import Html.Attributes exposing (class, id, name, type_)
import Http
import Set
import Task
import Url
-- Our imports
import Display
import Evaluate
import Helper
import Matcher
import Math
import Query
import UI.Dialog as Dialog
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.Menu as Menu
import UI.Notification as Notification
import Rules
import Tutorial

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

-- Types

type alias Model =
    {   display: Display.Model
    ,   rules: Rules.Model
    ,   tutorial: Tutorial.Model
    ,   notification: Notification.Model
    ,   query: Query.Model
    ,   menu: Menu.Model
    ,   evaluator: Evaluate.Model (Float, Math.Tree ()) Event
    -- UI fields
        --  Textbox shown, elements are 'add-able' & rules are draggable, for an equation
    ,   createMode: Maybe (Maybe Int)
    ,   showHelp: Bool
    ,   showMenu: Bool
    ,   dialog: Maybe (Dialog.Model Event, Maybe (Rules.Parameters Display.State))
    }

type Event =
    EventUrlRequest Browser.UrlRequest
    | EventUrlChange Url.Url
    | DisplayEvent Display.Event
    | RuleEvent (Rules.Event Display.State)
    | TutorialEvent Tutorial.Event
    | NotificationEvent Notification.Event
    | MenuEvent Menu.Event
    -- Event from the UI
    | NoOp -- For setting focus on textbox
    | PressedKey String -- For catching "Escape"
    | EnterCreateMode
    | CancelCreateMode
    | SubmitEquation (Maybe Int) String
    | ToggleHelp
    | ToggleMenu
    | OpenDialog (Dialog.Model Event)
    | CloseDialog
    | DeleteTopic String
    | ProcessTopic String (Result Http.Error Rules.Topic)
    | ProcessSource String (Result Http.Error Source)
    | FileSelect LoadableFile
    | FileSelected LoadableFile File.File
    | FileLoaded LoadableFile String
    -- Rules
    | ApplyParameters (Dict.Dict String Dialog.Extracted)
    | ApplySubstitution String String
    | ConvertSubString Float String
    | ApplyNumSub {id: Int, value: Float}

type LoadableFile =
    TopicFile
    | SaveFile

type alias Source =
    {   topics: Dict.Dict String String
    }

-- Events

init: Decode.Value -> Url.Url -> Nav.Key -> (Model, Cmd Event)
init _ url key =
    let
        query = Query.parseInit url key
        (eqs, errs) = List.foldl parseEquations_ ([], []) query.equations |> \(a, b) -> (List.reverse a, List.reverse b)
        (nModel, nCmd) = List.foldl Notification.displayError (Notification.init, Cmd.none) errs
        newScreen = List.isEmpty eqs
    in
    (   { display = Display.init eqs
        , rules = Rules.init
        , tutorial = Tutorial.init
        , notification = nModel
        , query = query
        , menu = Menu.init (Set.fromList ["Settings", "Tutorials", "Topics", "Core"])
        , evaluator = Evaluate.init evaluateString
        , createMode = if newScreen then Just Nothing else Nothing
        , showHelp = False
        , showMenu = if newScreen then True else False
        , dialog = Nothing
        }
    ,   Cmd.batch
        [   Cmd.map NotificationEvent nCmd
        ,   if newScreen then focusTextBar_ "textInput" else Cmd.none
        ,   loadSources query.sources
        ]
    )

parseEquations_: String -> (List (Matcher.Equation Display.State), List String) -> (List (Matcher.Equation Display.State), List String)
parseEquations_ elem (result, errs) = case Matcher.parseEquation Display.createState Display.updateState elem of
    Result.Ok root -> (root :: result, errs)
    Result.Err err -> (result, err :: errs )

subscriptions: Model -> Sub Event
subscriptions model = Sub.batch
    [   case (model.createMode, model.dialog) of
            (Nothing, Nothing) -> Sub.none
            _ -> BrowserEvent.onKeyPress (Decode.field "key" Decode.string |> Decode.map PressedKey)
    ,   evaluateResult ApplyNumSub
    ]

{-
## State changes
-}

update: Event -> Model -> ( Model, Cmd Event )
update event model = case event of
    EventUrlRequest _ -> (model, Cmd.none)
    EventUrlChange _ -> (model, Cmd.none)
    DisplayEvent e ->
        let
            (dModel, dCmd) = Display.update e model.display
            query = Query.setEquations (Display.listEquations dModel) model.query
        in
            ({model | display = dModel}, Cmd.batch [ Cmd.map DisplayEvent dCmd, Query.pushUrl query, updateMathJax ()])
    TutorialEvent e -> let (tModel, tCmd) = Tutorial.update e model.tutorial in
        ({model | tutorial = tModel}, Cmd.map TutorialEvent tCmd)
    NotificationEvent e -> let (nModel, nCmd) = Notification.update e model.notification in
        ({model | notification = nModel}, Cmd.map NotificationEvent nCmd)
    MenuEvent e -> ({model | menu = Menu.update e model.menu}, Cmd.none)
    NoOp -> (model, Cmd.none)
    PressedKey str -> if str /= "Escape" then (model, Cmd.none)
        else case (model.dialog, model.createMode) of
            (Just _, _) -> ({model | dialog = Nothing}, Cmd.none)
            (_, Just _) -> ({model | createMode = Nothing, showHelp = False}, Cmd.none)
            _ -> (model, Cmd.none)
    EnterCreateMode -> ({model | createMode = Just Nothing }, focusTextBar_ "textInput")
    CancelCreateMode -> ({model | createMode = Nothing, showHelp=False}, Cmd.none)
    SubmitEquation id str -> case Matcher.parseEquation Display.createState Display.updateState str of
        Result.Ok root -> (
            case id of
                Nothing ->
                    Display.addEquation root model.display
                Just i ->
                    Display.updateEquation i root model.display
            )
            |> (\dModel -> Display.listEquations dModel
                |> (\list -> Query.setEquations list model.query )
                |> (\query -> ({model | createMode = Nothing, display = dModel, showHelp = False}, Cmd.batch [Query.pushUrl query, updateMathJax ()]) )
            )
        Result.Err err -> submitNotification_ model err
    ToggleHelp -> ({model | showHelp = not model.showHelp}, focusTextBar_ "textInput")
    ToggleMenu -> ({model | showMenu = not model.showMenu}, Cmd.none)
    OpenDialog d ->
        (   {model | dialog = Just (d, Nothing)}
        ,   case d.focus of
            Nothing -> Cmd.none
            Just name -> Dialog.fieldID name |> focusTextBar_
        )
    CloseDialog -> ({model | dialog = Nothing}, Cmd.none)
    DeleteTopic name -> ({model | rules = Rules.deleteTopic name model.rules, dialog = Nothing}, Cmd.none)
    ProcessTopic url result -> case result of
        Err err -> httpErrorToString_ url err |> submitNotification_ model
        Ok topic -> case Rules.addTopic topic model.rules of
            Err errStr -> submitNotification_ model errStr
            Ok rModel -> ({model | rules = rModel}, Cmd.none)
    ProcessSource url result -> case result of
        Err err -> httpErrorToString_ url err |> submitNotification_ model
        Ok source -> ({model | rules = Rules.addSources source.topics model.rules}, Cmd.none)
    FileSelect fileType -> (model, FSelect.file ["application/json"] (FileSelected fileType))
    FileSelected fileType file -> ({model | dialog = Nothing}, Task.perform (FileLoaded fileType) (File.toString file))
    FileLoaded fileType str -> case fileType of
        TopicFile -> Decode.decodeString Rules.topicDecoder str
            |> Result.mapError Decode.errorToString
            |> Result.andThen (\topic -> Rules.addTopic topic model.rules)
            |> (\result -> case result of
                Err errStr -> submitNotification_ model errStr
                Ok rModel -> ({model | rules = rModel}, Cmd.none)
            )
        SaveFile -> (model, Cmd.none) -- TODO
    RuleEvent e -> case e of
        Rules.Apply p -> if List.length p.matches == 1 && List.isEmpty p.parameters
            then case Helper.listIndex 0 p.matches of
                Nothing -> submitNotification_ model "Unable to extract the match"
                Just m -> applyChange_ m model
            else ({ model | dialog = Just (parameterDialog_ p, Just p)}, Cmd.none)
        Rules.Group -> case Display.groupChildren model.display of
            Err errStr -> submitNotification_ model errStr
            Ok dModel -> ({model | display = dModel}, Cmd.none)
        Rules.Ungroup -> case Display.ungroupChildren model.display of
            Err errStr -> submitNotification_ model errStr
            Ok dModel -> ({model | display = dModel}, Cmd.none)
        Rules.Substitute -> ({ model | dialog = Just (substitutionDialog_, Nothing)} , Cmd.none)
        Rules.NumericalSubstitution target -> ({ model | dialog = Just (numSubDialog_ target, Nothing)}, Cmd.none)
        Rules.Download url -> ({model | dialog = Nothing}, Http.get { url = url, expect = Http.expectJson (ProcessTopic url) Rules.topicDecoder})
    ApplyParameters params -> case model.dialog of
        Just (_, Just existing) -> (    case Dict.get "_method" params of
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
                Err errStr -> submitNotification_ model errStr
                Ok newParams -> applyChange_ newParams model
            )
        _ -> ({ model | dialog = Nothing}, Cmd.none)
    ApplySubstitution rawIn rawOut -> ({model | dialog = Nothing}, Cmd.none)
    ConvertSubString target str -> case Math.parse str |> Result.andThen (Rules.replaceGlobalVar model.rules) of
        Err errStr -> submitNotification_ model errStr
        Ok newTree -> case Rules.evaluateStr model.rules newTree of
            Err errStr -> submitNotification_ model errStr
            Ok evalStr -> let (eModel, cmd) = Evaluate.send (target, newTree) evalStr model.evaluator in
                ({model | evaluator = eModel}, cmd)
    ApplyNumSub reply -> let (eModel, c) = Evaluate.finish reply.id model.evaluator in
        let newModel = {model | evaluator = eModel } in
        case c of
            Nothing -> submitNotification_ newModel "Unable to evaluate a string"
            Just (target, tree) -> if target /= reply.value
                then submitNotification_ newModel ("Expression evaluates to: " ++ String.fromFloat reply.value ++ ", but expecting: " ++ String.fromFloat target)
                else case Display.replaceNumber newModel.display target tree of
                    Err errStr -> submitNotification_ newModel errStr
                    Ok dModel -> ({newModel | display = dModel, dialog = Nothing}, Cmd.none)

-- TODO
applyChange_: {from: Matcher.MatchResult Display.State, name: String, matcher: Matcher.Matcher} -> Model -> (Model, Cmd Event)
applyChange_ params model = case Display.transformEquation params.matcher params.from model.display of
    Err errStr -> submitNotification_ model errStr
    Ok newDisplay -> ({model | display = newDisplay, dialog = Nothing}, Cmd.none)

submitNotification_: Model -> String -> (Model, Cmd Event)
submitNotification_ model str = let (nModel, nCmd) = Notification.displayError str (model.notification, Cmd.none) in
    ({model | notification = nModel}, Cmd.map NotificationEvent nCmd)

httpErrorToString_: String -> Http.Error -> String
httpErrorToString_ url err = case err of
    Http.BadUrl _ -> "Invalid URL provided: " ++ url
    Http.Timeout -> "Timed out waitiing for: " ++ url
    Http.NetworkError -> "Unable to reach: " ++ url
    Http.BadStatus code -> "The url returned an error code [" ++ String.fromInt code ++ "]: " ++ url
    Http.BadBody str -> "The file is malformed:\n" ++ str

focusTextBar_: String -> Cmd Event
focusTextBar_ id = Dom.focus id |> Task.attempt (\_ -> NoOp)

{-
## UI
-}

view: Model -> Browser.Document Event
view model =
    { title = "Maths"
    , body =
        [   Display.view DisplayEvent [id "display"] model.display
        ,   div [id "inputPane"]
            [   div [id "leftPane"]
                (  inputDiv model
                :: (if model.showHelp then [ pre [id "helpText"] [text Math.notation] ] else [])
                )
            ,   div (id "rightPane" :: (if model.showMenu then [] else [class "closed"]))
                [   div [id "menuToggle"] [Icon.menu [HtmlEvent.onClick ToggleMenu, Icon.class "clickable", Icon.class "helpable"]]
                ,   Menu.view MenuEvent model.menu
                    [   Menu.Section "Settings" True
                        [   Menu.Content [a [] [text "Open"]] -- TODO
                        ,   Menu.Content [a [] [text "Save"]] -- TODO
                        ]
                    ,   Tutorial.menu TutorialEvent model.tutorial
                    ,   Menu.Section "Topics" True
                        (   [   Menu.Content [ a
                                    [HtmlEvent.onClick (OpenDialog addTopicDialog_), class "clickable"]
                                    [text "Add"]
                                ]
                            ,   Menu.Content [ a
                                    [HtmlEvent.onClick (OpenDialog deleteTopicDialog_), class "clickable"]
                                    [text "Delete"]
                                ]
                            ]
                        ++  Rules.menuTopics RuleEvent model.rules (Display.selectedNode model.display)
                        )
                    ]
                ]
            ]
        ,   Tutorial.view TutorialEvent [] model.tutorial
        ,   Notification.view NotificationEvent [id "notification"] model.notification
        ]
        |> Helper.maybeAppend (Maybe.map (Tuple.first >> Dialog.view) model.dialog)
    }

inputDiv: Model -> Html Event
inputDiv model =
    form
    (   id "textbar"
    ::  (   case model.createMode of
            Nothing -> [class "closed", HtmlEvent.onSubmit EnterCreateMode]
            Just eq -> [HtmlEvent.onSubmitField "equation" (SubmitEquation eq)]
        )
    )
    [   Icon.help [id "help", Icon.class "clickable", Icon.class "helpable", HtmlEvent.onClick ToggleHelp]
    ,   input
        [ type_ "text"
        , name "equation"
        , id "textInput"
        ]
        []
    ,   Icon.cancel [Icon.class "clickable", Icon.class "cancelable", HtmlEvent.onClick CancelCreateMode]
    ,   button [type_ "submit", Icon.class "noDefault"] [   case model.createMode of
            Nothing -> Icon.add [Icon.class "clickable", Icon.class "submitable"]
            Just _ -> Icon.tick [Icon.class "clickable", Icon.class "submitable"]
        ]
    ]

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

deleteTopicDialog_: Dialog.Model Event
deleteTopicDialog_ =
    {   title = "Delete an existing Topic"
    ,   sections =
        [   {   subtitle = "Name of the topic:"
            ,   lines = [[Dialog.Text {id = "name"}]]
            }
        ]
    ,   success = (\val -> case Dict.get "name" val of
            Just (Dialog.TextValue a) -> DeleteTopic a
            _ -> NoOp
        )
    ,   cancel = CloseDialog
    ,   focus = Just "name"
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
                        ,   [Dialog.Radio {name = "_method", options = List.map (\m -> m.name) params.matches}]
                        ]
                    }
                    :: sections
            )
    ,   success = ApplyParameters
    ,   cancel = CloseDialog
    ,   focus = Nothing
    }

substitutionDialog_: Dialog.Model Event
substitutionDialog_ =
    {   title = "Substitute a variable for a formula"
    ,   sections =
        -- TODO: Make this into a dropdown for selecting the equation to pick
        [{  subtitle = "For each instance of the specified variable, replace it with the formula"
        ,   lines = [[Dialog.Text {id="var"}, Dialog.Info {text = "="}, Dialog.Text {id="formula"}]]
        }]
    ,   success = (\dict -> case (Dict.get "var" dict, Dict.get "formula" dict) of
            (Just (Dialog.TextValue a), Just (Dialog.TextValue b)) -> ApplySubstitution a b
            _ -> NoOp
        )
    ,   cancel = CloseDialog
    ,   focus = Just "var"
    }

numSubDialog_: Float -> Dialog.Model Event
numSubDialog_ target =
    {   title = "Substitute a number for an expression"
    ,   sections =
        [{  subtitle = "The expression to replace " ++ String.fromFloat target
        ,   lines = [[Dialog.Text {id="expr"}]]
        }]
    ,   success = (\dict -> case Dict.get "expr" dict of
            Just (Dialog.TextValue val) -> ConvertSubString target val
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