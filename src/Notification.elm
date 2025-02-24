module Notification exposing (
    Model, Event, init, update, view,
    displayParsingError
    )

import Dict
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (class)
import Html.Keyed exposing (node)
import Process exposing (sleep)
import Task
-- Our modules
import HtmlEvent
import Icon
import Parser

type alias Model =
    {   nextID: Int
    ,   notifications: Dict.Dict Int (Bool, String) -- Bool represents whether it's deleting
    }

type Event =
    ClearEvent Int
    | DeleteEvent Int

init: Model
init = {nextID = 0, notifications = Dict.empty}

displayParsingError: String -> List Parser.DeadEnd -> (Model, Cmd Event) -> (Model, Cmd Event)
displayParsingError str list (model, cmd) =
    (   {   model
        |   nextID = model.nextID + 1
        ,   notifications = Dict.insert model.nextID (False, parsingErrorMessage_ str list) model.notifications
        }
    ,   Cmd.batch [ cmd, delayedClear_ model.nextID ]
    )

parsingErrorMessage_: String -> List Parser.DeadEnd -> String
parsingErrorMessage_ str err = "Error parsing \"" ++ str ++ "\":\n" ++ deadEndToString_ err

deadEndToString_: List Parser.DeadEnd -> String
deadEndToString_ = List.map
    (\deadEnd ->
        (   case deadEnd.problem of
                Parser.Expecting str -> "Expecting '" ++ str ++ "'"
                Parser.ExpectingInt -> "Expecting a whole number"
                Parser.ExpectingHex -> "Expecting a hex number"
                Parser.ExpectingOctal -> "Expecting an octal number"
                Parser.ExpectingBinary -> "Expecting a binary number"
                Parser.ExpectingFloat -> "Expecting a decimal number"
                Parser.ExpectingNumber -> "Expecting a number"
                Parser.ExpectingVariable -> "Expecting a variable"
                Parser.ExpectingSymbol str -> "Expecting '" ++ str ++ "'"
                Parser.ExpectingKeyword str -> "Expecting '" ++ str ++ "'"
                Parser.ExpectingEnd -> "Expecting no more characters"
                Parser.UnexpectedChar -> "Unknown symbol"
                Parser.Problem str -> str
                Parser.BadRepeat -> "Bad repeat"
        )
        |> (\str -> str ++ ", at position: " ++ String.fromInt deadEnd.col)
    )
    >> String.join "\n"

delayedClear_: Int -> Cmd Event
delayedClear_ id = Task.perform (\_ -> ClearEvent id) (sleep 15000)

delayedDelete_: Int -> Cmd Event
delayedDelete_ id = Task.perform (\_ -> DeleteEvent id) (sleep 750) -- Match this with css animation

update: Event -> Model -> (Model, Cmd Event)
update e model = case e of 
    ClearEvent id ->
        (   {   model
            |   notifications = Dict.update id (Maybe.map (\(_, str) -> (True, str))) model.notifications
            }
        , delayedDelete_ id
        )
    DeleteEvent id ->
        ({ model | notifications = Dict.remove id model.notifications }, Cmd.none )

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attrs model = node "div" (attrs ++ [])
    (   Dict.foldl
        (  \id val result -> notificationDiv_ converter id val::result )
        []
        model.notifications
    )

notificationDiv_: (Event->msg) -> Int -> (Bool, String) -> (String, Html msg)
notificationDiv_ converter id (deleting, message) =
    ("notification-" ++ (String.fromInt id), div (notificationAttr_ converter id deleting) [Icon.cancel [Icon.class "clickable"], pre [] [text message]])

notificationAttr_: (Event -> msg) -> Int -> Bool -> List (Html.Attribute msg)
notificationAttr_ converter id deleting =
    if deleting then
        [   class "notificationMessage"
        ,   class "deleting"
        ]
    else
        [   class "notificationMessage"
        ,   class "displaying"
        ,   HtmlEvent.onClick (ClearEvent id |> converter)
        ]