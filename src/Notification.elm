module Notification exposing (
    Model, Event, init, update, view,
    displayParsingError
    )

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Process exposing (sleep)
import Task
-- Our modules
import HtmlEvent
import Parser

type alias Model =
    {   nextID: Int
    ,   notifications: List (Int, String)
    }

type Event = ClearEvent Int

init: Model
init = {nextID = 0, notifications = []}

displayParsingError: String -> List Parser.DeadEnd -> (Model, Cmd Event) -> (Model, Cmd Event)
displayParsingError str list (model, cmd) =
    (   {   model
        |   nextID = model.nextID + 1
        ,   notifications =
            (model.nextID, parsingErrorMessage_ str list) :: model.notifications
        }
    ,   Cmd.batch [ cmd, delayedClear_ model.nextID ]
    )

parsingErrorMessage_: String -> List Parser.DeadEnd -> String
parsingErrorMessage_ str err = "Error parsing \"" ++ str ++ "\": " ++ Parser.deadEndsToString err

delayedClear_: Int -> Cmd Event
delayedClear_ id = Task.perform (\_ -> ClearEvent id) (sleep 15000)

update: Event -> Model -> (Model, Cmd Event)
update e model = case e of 
    ClearEvent id ->
        (   {   model
            |   notifications = 
                List.filter (Tuple.first >> (/=) id) model.notifications
            }
        , Cmd.none
        )

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attrs model = div (attrs ++ [])
    (   List.map
        (  \(id, message) -> 
            div 
            [   class "notificationMessage"
            ,   HtmlEvent.onClick (ClearEvent id |> converter)
            ]
            [p [] [text message]]
        )
        model.notifications
    )