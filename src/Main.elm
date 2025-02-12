module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Json.Decode as Decode
import Html exposing (Html, button, div, form, input)
import Url
-- Our imports
import Display
import Notification
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

-- Types

type alias Model =
    {   display: Display.Model
    ,   rule: Rules.Model
    ,   tutorial: Tutorial.Model
    ,   notification: Notification.Model
    }

type Event =
    EventUrlRequest Browser.UrlRequest
    | EventUrlChange Url.Url
    | DisplayEvent Display.Event
    | RulesEvent Rules.Event
    | TutorialEvent Tutorial.Event
    | NotificationEvent Notification.Event

-- Events

init: Decode.Value -> Url.Url -> Nav.Key -> (Model, Cmd Event)
init _ _ _ =
    (   { display = Display.init []
        , rule = Rules.init
        , tutorial = Tutorial.init
        , notification = Notification.init
        }
    , Cmd.none
    )

subscriptions: Model -> Sub Event
subscriptions _ = Sub.none

update: Event -> Model -> ( Model, Cmd Event )
update _ model = (model, Cmd.none)

view: Model -> Browser.Document Event
view model =
    { title = "Maths"
    , body = []
        |> append (wrap div [] (
            append ( wrap div []
                (   append (inputDiv model)
                >>  append (Display.view DisplayEvent [] model.display)
                ))
            >> append (Rules.view RulesEvent [] model.rule)
        ))
        |> append (Tutorial.view TutorialEvent [] model.tutorial)
        |> append (Notification.view NotificationEvent [] model.notification)
    }

inputDiv: Model -> Html.Html Event
inputDiv model = div []
    [   form
        []
        [   input
            []
            []
        ,   button
            []
            []
        ]
    ]

wrap: (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> (List (Html msg) -> List (Html msg)) -> Html msg
wrap tag attrs childrenGenerator = tag attrs (childrenGenerator [])

append: Html msg -> List (Html msg) -> List (Html msg)
append child current = current ++ [child]
