module Tutorial exposing (Model, Event, init, menu, update, view)

import Html exposing (Html, div)
-- Ours
import UI.Menu as Menu

type alias Model =
    {   number: Int
    ,   screen: Int
    }

type Event =
    Click

init: Model
init = {number = 0, screen = 0}

update: Event -> Model -> (Model, Cmd Event)
update _ model = (model, Cmd.none)

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attrs model = div (attrs ++ []) []

menu: (Event -> msg) -> Model -> Menu.Part msg
menu converter model = Menu.Section "Tutorials" True []