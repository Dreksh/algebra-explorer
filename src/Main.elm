port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvent
import Browser.Navigation as Nav
import Dict
import File
import File.Select as FSelect
import Json.Decode as Decode
import Html exposing (Html, a, button, div, form, h2, h3, input, p, pre, section, text)
import Html.Attributes exposing (attribute, class, id, name, type_)
import Http
import Set
import Task
import Url
-- Our imports
import Display
import Helper
import HtmlEvent
import Icon
import Matcher
import Math
import Menu
import Notification
import Query
import Rules
import Tutorial
import Html exposing (label)
import Html.Attributes exposing (for)

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
    ,   dialog: Maybe (String, String -> Event)
    ,   parameters: Maybe Parameters_
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
    | OpenDialog String (String -> Event)
    | CloseDialog
    | DeleteTopic String
    | DownloadTopic String
    | ProcessTopic String (Result Http.Error Rules.Topic)
    | FileSelect LoadableFile
    | FileSelected LoadableFile File.File
    | FileLoaded LoadableFile String
    -- Rules
    | ApplyRule Parameters_

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
        , menu = Menu.init (Set.fromList ["Settings", "Tutorials", "Constants", "Functions", "Rules"])
        , createMode = if newScreen then Just Nothing else Nothing
        , showHelp = False
        , showMenu = if newScreen then True else False
        , dialog = Nothing
        , parameters = Nothing
        }
    , Cmd.batch [Cmd.map NotificationEvent nCmd, if newScreen then focusTextBar_ "textInput" else Cmd.none]
    )

parseEquations_: String -> (List (Matcher.Equation Display.State), List String) -> (List (Matcher.Equation Display.State), List String)
parseEquations_ elem (result, errs) = case Matcher.parseEquation {position = (0,0)} elem of
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
    TutorialEvent e -> let (tModel, tCmd) = Tutorial.update e model.tutorial in
        ({model | tutorial = tModel}, Cmd.map TutorialEvent tCmd)
    NotificationEvent e -> let (nModel, nCmd) = Notification.update e model.notification in
        ({model | notification = nModel}, Cmd.map NotificationEvent nCmd)
    MenuEvent e -> ({model | menu = Menu.update e model.menu}, Cmd.none)
    NoOp -> (model, Cmd.none)
    PressedKey str -> if str == "Escape" then ({model | createMode = Nothing, showHelp = False}, Cmd.none) else (model, Cmd.none)
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
    OpenDialog title generator -> ({model | dialog = Just (title, generator)}, focusTextBar_ "dialogAnswer")
    CloseDialog -> ({model | dialog = Nothing}, Cmd.none)
    DeleteTopic name -> ({model | rules = Rules.deleteTopic name model.rules, dialog = Nothing}, Cmd.none)
    DownloadTopic url -> ({model | dialog = Nothing}, Http.get { url = url, expect = Http.expectJson (ProcessTopic url) Rules.topicDecoder})
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
    ApplyRule details -> if List.isEmpty details.parameters |> not
        then ({ model | parameters = Just details}, Cmd.none)
        else (model, Cmd.none) -- TODO

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
                |> Helper.maybeAppend (Maybe.map parameterDiv_ model.parameters)
                )
            ,   div (id "rightPane" :: (if model.showMenu then [] else [class "closed"]))
                [   div [id "menuToggle"] [Icon.menu [HtmlEvent.onClick ToggleMenu, Icon.class "clickable", Icon.class "helpable"]]
                ,   Menu.view MenuEvent model.menu
                    [   Menu.Section "Settings" True
                        [   Menu.Content [a [] [text "Load from Save File"]] -- TODO
                        ,   Menu.Content [a [] [text "Save to Save File"]] -- TODO
                        ,   Menu.Content [ a
                                [HtmlEvent.onClick (OpenDialog "Enter the url for the topic" DownloadTopic), class "clickable"]
                                [text "Add Topic From URL"]
                            ]
                        ,   Menu.Content [ a
                                [HtmlEvent.onClick (FileSelect TopicFile), class "clickable"]
                                [text "Add Topic From File"]
                            ]
                        ,   Menu.Content [ a
                                [HtmlEvent.onClick (OpenDialog "Enter the topic to delete" DeleteTopic), class "clickable"]
                                [text "Delete a Topic"]
                            ]
                        ]
                    ,   Tutorial.menu TutorialEvent model.tutorial
                    ,   Rules.menuConstants model.rules
                    ,   Rules.menuFunctions model.rules (inCreateMode_ model)
                    ,   Rules.menuRules ApplyRule model.rules (Display.selectedNode model.display)
                    ]
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
                [   text (prompt ++ ":")
                ,   input [type_ "text", name "answer", id "dialogAnswer"] []
                ,   Icon.cancel [Icon.class "clickable", Icon.class "cancelable", HtmlEvent.onClick CloseDialog]
                ,   button [type_ "submit"] [Icon.tick [Icon.class "clickable", Icon.class "submitable"]]
                ]
            ]
        ]

inCreateMode_: Model -> Bool
inCreateMode_ model = case model.createMode of
    Nothing -> False
    Just _ -> True

parameterDiv_: Parameters_ -> Html.Html Event
parameterDiv_ params =
    let
        tokens = List.foldl (\elem -> Set.union elem.tokens) Set.empty params.parameters
        decoder = tokens
            |> Set.foldl (\elem result -> Decode.map2
                (\eq -> Dict.insert elem eq)
                (Math.decoder |> Decode.field elem)
                result
            )
            (Decode.succeed Dict.empty)
            |> Decode.map (\info ->
                {   params
                |   extracted = Dict.foldl Matcher.addMatch params.extracted info
                ,   parameters = []
                }
            )
    in
        div []
        [   h2 [] [text params.title]
        ,   form [id "params", HtmlEvent.onSubmitForm decoder ApplyRule]
            (   (   List.foldl (\param (result, toks) ->
                        Set.foldl (\key (nextResult, nextToks) -> if Set.member key nextToks
                            then (nextResult ++ [label [for key] [input [type_ "text", name key] []]], Set.remove key nextToks)
                            else (nextResult ++ [p [] [text "Refer to input of the same name above"]], nextToks)
                        )
                        ([], toks)
                        param.tokens
                        |> (\(finalList, finalTokens) ->
                            (   result ++ [section []
                                    (   [ h3 [] [text param.name]
                                        , p [] [text param.description]
                                        ]
                                    ++ finalList
                                    )
                                ]
                            ,   finalTokens
                            )
                        )
                    )
                    ([], tokens)
                    params.parameters
                    |> Tuple.first
                )
            ++ [button [type_ "submit"] [text "Apply"]]
            )
        ]