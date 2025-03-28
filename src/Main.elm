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

-- Types

type alias Parameters_ =
    {   title: String
    ,   to: Matcher.Matcher
    ,   parameters: List {name: String, description: String, tokens: Set.Set String}
    ,   extracted: Matcher.MatchResult Display.State
    }

type alias Model =
    {   display: Display.Model
    ,   rules: Rules.Model
    ,   tutorial: Tutorial.Model
    ,   notification: Notification.Model
    ,   query: Query.Model
    ,   menu: Menu.Model
    -- UI fields
        --  Textbox shown, elements are 'add-able' & rules are draggable, for an equation
    ,   createMode: Maybe (Maybe Int)
    ,   showHelp: Bool
    ,   showMenu: Bool
    ,   dialog: Maybe (Dialog.Model Event, Maybe Parameters_)
    }

type Event =
    EventUrlRequest Browser.UrlRequest
    | EventUrlChange Url.Url
    | DisplayEvent Display.Event
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
    | DownloadTopic String
    | ProcessTopic String (Result Http.Error Rules.Topic)
    | FileSelect LoadableFile
    | FileSelected LoadableFile File.File
    | FileLoaded LoadableFile String
    -- Rules
    | ApplyRule Parameters_
    | ApplyParameters (Dict.Dict String String)

type LoadableFile =
    TopicFile
    | SaveFile

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
        , menu = Menu.init (Set.fromList ["Settings", "Tutorials", "Constants", "Functions", "Rules"])
        , createMode = if newScreen then Just Nothing else Nothing
        , showHelp = False
        , showMenu = if newScreen then True else False
        , dialog = Nothing
        }
    , Cmd.batch [Cmd.map NotificationEvent nCmd, if newScreen then focusTextBar_ "textInput" else Cmd.none]
    )

parseEquations_: String -> (List (Matcher.Equation Display.State), List String) -> (List (Matcher.Equation Display.State), List String)
parseEquations_ elem (result, errs) = case Matcher.parseEquation {position = (0,0)} elem of
    Result.Ok root -> (root :: result, errs)
    Result.Err err -> (result, err :: errs )

subscriptions: Model -> Sub Event
subscriptions model = case (model.createMode, model.dialog) of
    (Nothing, Nothing) -> Sub.none
    _ -> BrowserEvent.onKeyPress (Decode.field "key" Decode.string |> Decode.map PressedKey)

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
    SubmitEquation id str -> case Matcher.parseEquation {position= (0,0)} str of
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
    DownloadTopic url -> ({model | dialog = Nothing}, Http.get { url = url, expect = Http.expectJson (ProcessTopic url) Rules.topicDecoder})
    ProcessTopic url result -> case result of
        Err err -> httpErrorToString_ url err |> submitNotification_ model
        Ok topic -> case Rules.addTopic topic model.rules of
            Err errStr -> submitNotification_ model errStr
            Ok rModel -> ({model | rules = rModel}, Cmd.none)
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
    ApplyRule details -> if List.isEmpty details.parameters |> not
        then ({ model | dialog = Just (parameterDialog_ details, Just details)}, Cmd.none)
        else applyChange_ details model
    ApplyParameters params -> case model.dialog of
        Just (_, Just existing) ->
            let
                result = Helper.resultDict (\k v r -> Math.parse v
                    |> Result.map (\tree -> {r | extracted = Matcher.addMatch k tree r.extracted})
                    ) existing params
            in case result of
                Err errStr -> submitNotification_ model errStr
                Ok newParams -> applyChange_ newParams model
        _ -> ({ model | dialog = Nothing}, Cmd.none)

-- TODO
applyChange_: Parameters_ -> Model -> (Model, Cmd Event)
applyChange_ params model = (model, Cmd.none)

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
                        ++  Rules.menuTopics ApplyRule model.rules (Display.selectedNode model.display)
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
            ,   inputs = [Dialog.Text {name = Nothing, id = "url"}]
            }
        ,   {   subtitle = "Load from a file"
            ,   inputs = [Dialog.Button {text = "Select a file", event =(FileSelect TopicFile)}]
            }
        ]
    ,   success = Dialog.decoder (Dict.get "url" >> Maybe.withDefault "" >> DownloadTopic) (Set.singleton "url")
    ,   cancel = CloseDialog
    ,   focus = Just "url"
    }

deleteTopicDialog_: Dialog.Model Event
deleteTopicDialog_ =
    {   title = "Delete an existing Topic"
    ,   sections =
        [   {   subtitle = "Name of the topic:"
            ,   inputs = [Dialog.Text {name = Nothing, id = "name"}]
            }
        ]
    ,   success = Dialog.decoder (Dict.get "name" >> Maybe.withDefault "" >> DeleteTopic) (Set.singleton "name")
    ,   cancel = CloseDialog
    ,   focus = Just "name"
    }

parameterDialog_: Parameters_ -> Dialog.Model Event
parameterDialog_ params = let tokens = List.foldl (\elem -> Set.union elem.tokens) Set.empty params.parameters in
    {   title = "Set parameters for " ++ params.title
    ,   sections = List.foldl
            (\param (result, toks) ->
                Set.foldl (\key (nextResult, nextToks) -> if Set.member key nextToks
                    then (Dialog.Text {name = Just key, id = key} :: nextResult, Set.remove key nextToks)
                    else (Dialog.Info {text = key ++ ": Refer to input '" ++ key ++ "' above"} :: nextResult, nextToks)
                )
                ([], toks)
                param.tokens
                |> (\(finalList, finalTokens) ->
                    (   { subtitle = param.name {-, description = param.description -}, inputs = finalList } :: result |> List.reverse
                    ,   finalTokens
                    )
                )
            )
            ([], tokens)
            params.parameters
            |> Tuple.first
    ,   success = Dialog.decoder ApplyParameters tokens
    ,   cancel = CloseDialog
    ,   focus = Nothing
    }