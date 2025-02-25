module Tutorial exposing (Model, Event, init, menu, update, view)

import Html exposing (Html, div)

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

type alias MenuItem_ msg = Bool -> List (Html.Attribute msg) -> String -> List (Html.Html msg) -> List (Html.Html msg)
menu: (Event -> msg) -> MenuItem_ msg -> Model -> List (Html msg)
menu converter menuItem model = menuItem True [] "Tutorials" []