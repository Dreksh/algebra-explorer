module Notification exposing (Model, Event, init, update, view)

import Html exposing (Html, div)

type alias Model =
    {   nextID: Int
    ,   notifications: List (Int, String)
    }

type Event =
    Click

init: Model
init = {nextID = 0, notifications = []}

update: Event -> Model -> (Model, Cmd Event)
update _ model = (model, Cmd.none)

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attrs model = div (attrs ++ []) []