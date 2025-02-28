port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvent
import Browser.Navigation as Nav
import File
import File.Select as FSelect
import Json.Decode as Decode
import Html exposing (Html, a, button, div, form, input, pre, text)
import Html.Attributes exposing (attribute, class, id, name, type_)
import Http
import Set
import Task
import Url
-- Our imports
import Display
import HtmlEvent
import Icon
import Math
import Menu
import Notification
import Query
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
    ,   dialog: Maybe (String, String -> Event)
    }

type Event =
    EventUrlRequest Browser.UrlRequest
    | EventUrlChange Url.Url
    | DisplayEvent Display.Event
    | RulesEvent Rules.Event
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
    | OpenDialog String (String -> Event)
    | CloseDialog
    | DeleteTopic String
    | DownloadTopic String
    | ProcessTopic String (Result Http.Error Rules.Topic)
    | FileSelect LoadableFile
    | FileSelected LoadableFile File.File
    | FileLoaded LoadableFile String

type LoadableFile =
    TopicFile 
    | SaveFile

-- Events

init: Decode.Value -> Url.Url -> Nav.Key -> (Model, Cmd Event)
init _ url key = 
    let
        query = Query.parseInit url key
        (eqs, errs) = List.foldl parseEquations_ ([], []) query.equations
        (nModel, nCmd) = List.foldl Notification.displayError (Notification.init, Cmd.none) errs
        newScreen = List.isEmpty eqs
    in
    (   { display = Display.init eqs
        , rules = Rules.init
        , tutorial = Tutorial.init
        , notification = nModel
        , query = query
        , menu = Menu.init
        , createMode = if newScreen then Just Nothing else Nothing
        , showHelp = False
        , showMenu = if newScreen then True else False
        , dialog = Nothing
        }
    , Cmd.batch [Cmd.map NotificationEvent nCmd, if newScreen then focusTextBar_ else Cmd.none]
    )

parseEquations_: String -> (List (Math.Tree ()), List String) -> (List (Math.Tree ()), List String)
parseEquations_ elem (result, errs) = case Math.parse elem of
    Result.Ok root -> (result ++ [root], errs)
    Result.Err err -> (result, errs ++ [err])

subscriptions: Model -> Sub Event
subscriptions model = case model.createMode of
    Just _ -> BrowserEvent.onKeyPress (Decode.field "key" Decode.string |> Decode.map PressedKey)
    Nothing -> Sub.none

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
    RulesEvent e -> let (rModel, rCmd) = Rules.update e model.rules in
        ({model | rules = rModel}, Cmd.map RulesEvent rCmd)
    TutorialEvent e -> let (tModel, tCmd) = Tutorial.update e model.tutorial in
        ({model | tutorial = tModel}, Cmd.map TutorialEvent tCmd)
    NotificationEvent e -> let (nModel, nCmd) = Notification.update e model.notification in
        ({model | notification = nModel}, Cmd.map NotificationEvent nCmd)
    MenuEvent e -> ({model | menu = Menu.update e model.menu}, Cmd.none)
    NoOp -> (model, Cmd.none)
    PressedKey str -> if str == "Escape" then ({model | createMode = Nothing, showHelp = False}, Cmd.none) else (model, Cmd.none)
    EnterCreateMode -> ({model | createMode = Just Nothing }, focusTextBar_)
    CancelCreateMode -> ({model | createMode = Nothing, showHelp=False}, Cmd.none)
    SubmitEquation id str -> case Math.parse str of
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
    ToggleHelp -> ({model | showHelp = not model.showHelp}, focusTextBar_)
    ToggleMenu -> ({model | showMenu = not model.showMenu}, Cmd.none)
    OpenDialog title generator -> ({model | dialog = Just (title, generator)}, Cmd.none)
    CloseDialog -> ({model | dialog = Nothing}, Cmd.none)
    DeleteTopic name -> ({model | rules = Rules.deleteTopic name model.rules}, Cmd.none)
    DownloadTopic url -> (model, Http.get { url = url, expect = Http.expectJson (ProcessTopic url) Rules.topicDecoder})
    ProcessTopic url result -> case result of
        Err err -> httpErrorToString_ url err |> submitNotification_ model
        Ok topic -> case Rules.addTopic topic model.rules of
            Err errStr -> submitNotification_ model errStr
            Ok rModel -> ({model | rules = rModel}, Cmd.none)
    FileSelect fileType -> (model, FSelect.file ["application/json"] (FileSelected fileType))
    FileSelected fileType file -> (model, Task.perform (FileLoaded fileType) (File.toString file))
    FileLoaded fileType str -> case fileType of
        TopicFile -> Decode.decodeString Rules.topicDecoder str
            |> Result.mapError Decode.errorToString
            |> Result.andThen (\topic -> Rules.addTopic topic model.rules)
            |> (\result -> case result of
                Err errStr -> submitNotification_ model errStr
                Ok rModel -> ({model | rules = rModel}, Cmd.none)
            )
        SaveFile -> (model, Cmd.none) -- TODO

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

focusTextBar_: Cmd Event
focusTextBar_ = Dom.focus "textInput" |> Task.attempt (\_ -> NoOp)

view: Model -> Browser.Document Event
view model =
    { title = "Maths"
    , body =
        [   Display.view DisplayEvent [id "display"] model.display
        ,   div [id "inputPane"]
            [   div [id "leftPane"]
                (   [  inputDiv model  ]
                ++ if model.showHelp then [ pre [id "helpText"] [text Math.notation] ] else []
                )
            ,   div (id "rightPane" :: (if model.showMenu then [] else [class "closed"]))
                [   div [id "menuToggle"] [Icon.menu [HtmlEvent.onClick ToggleMenu, Icon.class "clickable", Icon.class "helpable"]]
                ,   div [id "menu"]
                    (   let menuItem = Menu.menuItem MenuEvent model.menu in
                        menuItem True [] "Settings"
                        [   a [] [text "Load from Save File"] -- TODO
                        ,   a [] [text "Save to Save File"] -- TODO
                        ,   a 
                            [HtmlEvent.onClick (OpenDialog "Enter the url for the topic" DownloadTopic), class "clickable"]
                            [text "Add Topic From URL"]
                        ,   a
                            [HtmlEvent.onClick (FileSelect TopicFile), class "clickable"]
                            [text "Add Topic From File"]
                        ,   a
                            [HtmlEvent.onClick (OpenDialog "Enter the topic to delete" DeleteTopic), class "clickable"]
                            [text "Delete a Topic"]
                        ]
                    ++  Tutorial.menu TutorialEvent menuItem model.tutorial
                    ++  Rules.menuConstants RulesEvent menuItem model.rules
                    ++  Rules.menuFunctions RulesEvent menuItem model.rules
                    ++  Rules.menuRules RulesEvent menuItem model.rules (Display.selectedNode model.display)
                    )
                ]
            ]
        ,   Tutorial.view TutorialEvent [] model.tutorial
        ,   Notification.view NotificationEvent [id "notification"] model.notification
        ]
        ++ (dialogPane_ model.dialog)
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
    ,   button [type_ "submit"] [   case model.createMode of
            Nothing -> Icon.add [Icon.class "clickable", Icon.class "submitable"]
            Just _ -> Icon.tick [Icon.class "clickable", Icon.class "submitable"]
        ]
    ]

dialogPane_: Maybe (String, String -> Event) -> List (Html.Html Event)
dialogPane_ selection = case selection of
    Nothing -> []
    Just (prompt, event) -> [
            Html.node "dialog" [attribute "open" "true"] [
                form [HtmlEvent.onSubmitField "answer" event, attribute "method" "dialog"]
                [   text prompt
                ,   input [type_ "text", name "answer"] []
                ,   Icon.cancel [Icon.class "clickable", Icon.class "cancelable", HtmlEvent.onClick CloseDialog]
                ,   button [type_ "submit"] [Icon.tick [Icon.class "clickable", Icon.class "submitable"]]
                ]
            ]
        ]