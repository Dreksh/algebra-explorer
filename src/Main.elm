port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvent
import Browser.Navigation as Nav
import Json.Decode as Decode
import Html exposing (Html, a, button, div, form, input, pre, text)
import Html.Attributes exposing (id, name, type_)
import Parser
import Task
import Url
-- Our imports
import Display
import HtmlEvent
import Icon
import Math
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
    -- UI fields
        --  Textbox shown, elements are 'add-able' & rules are draggable, for an equation
    ,   createMode: Maybe (Maybe Int)
    ,   showHelp: Bool
    ,   showRules: Bool
    }

type Event =
    EventUrlRequest Browser.UrlRequest
    | EventUrlChange Url.Url
    | DisplayEvent Display.Event
    | RulesEvent Rules.Event
    | TutorialEvent Tutorial.Event
    | NotificationEvent Notification.Event
    -- Event from the UI
    | NoOp -- For setting focus on textbox
    | PressedKey String -- For catching "Escape"
    | EnterCreateMode
    | CancelCreateMode
    | SubmitEquation (Maybe Int) String
    | ToggleHelp
    | ToggleRules

-- Events

init: Decode.Value -> Url.Url -> Nav.Key -> (Model, Cmd Event)
init _ url key = 
    let
        query = Query.parseInit url key
        (eqs, errs) = List.foldl parseEquations_ ([], []) query.equations
        (nModel, nCmd) = List.foldl (\(a, b) result -> Notification.displayParsingError a b result) (Notification.init, Cmd.none) errs
    in
    (   { display = Display.init eqs
        , rules = Rules.init
        , tutorial = Tutorial.init
        , notification = nModel
        , query = query
        , createMode = if List.isEmpty eqs then Just Nothing else Nothing
        , showHelp = False
        , showRules = False
        }
    , Cmd.map NotificationEvent nCmd
    )

type alias ParseError_ = List Parser.DeadEnd

parseEquations_: String -> (List (Math.Tree ()), List (String, ParseError_)) -> (List (Math.Tree ()), List (String, ParseError_))
parseEquations_ elem (result, errs) = case Math.parse elem of
    Result.Ok root -> (result ++ [root], errs)
    Result.Err err -> (result, errs ++ [(elem, err)])

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
    NoOp -> (model, Cmd.none)
    PressedKey str -> if str == "Escape" then ({model | createMode = Nothing, showHelp = False}, Cmd.none) else (model, Cmd.none)
    EnterCreateMode -> ({model | createMode = Just Nothing }, Dom.focus "textInput" |> Task.attempt (\_ -> NoOp))
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
        Result.Err err -> let (nModel, nCmd) = Notification.displayParsingError str err (model.notification, Cmd.none) in
            ({model | notification = nModel}, Cmd.map NotificationEvent nCmd)
    ToggleHelp -> ({model | showHelp = not model.showHelp}, Cmd.none)
    ToggleRules -> ({model | showRules = not model.showRules}, Cmd.none)

view: Model -> Browser.Document Event
view model =
    { title = "Maths"
    , body =
        [   Display.view DisplayEvent [] model.display
        ,   div [id "inputPane"]
            [   div [id "leftPane"]
                (   [  inputDiv model  ]
                ++ if model.showHelp then [ pre [id "helpText"] [text Math.notation] ] else []
                )
            ,   div [id "rightPane"]
                [   a [id "menu"] [Icon.menu [HtmlEvent.onClick ToggleRules]]
                ,   div [id "sidebar"]
                    [   Rules.view RulesEvent [] model.rules
                    ]
                ]
            ]
        ,   Tutorial.view TutorialEvent [] model.tutorial
        ,   Notification.view NotificationEvent [id "notification"] model.notification
        ]
    }

inputDiv: Model -> Html Event
inputDiv model = case model.createMode of
    Nothing ->
        form [ id "textbar", HtmlEvent.onSubmit EnterCreateMode ]
        [   button [type_ "submit"] [Icon.add []]
        ]
    Just eq ->
        form
        [   id "textbar"
        ,   HtmlEvent.onSubmitField "equation" (SubmitEquation eq)
        ,   HtmlEvent.onBlur CancelCreateMode
        ]
        [   Icon.help [id "help", HtmlEvent.onClick ToggleHelp]
        ,   input
            [ type_ "text"
            , name "equation"
            , id "textInput"
            ]
            []
        ,   button [type_ "submit"]
            [Icon.tick []]
        ]
