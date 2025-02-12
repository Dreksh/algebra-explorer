module Rules exposing (Model, Rule, Event, init, update, view)

import Html exposing (Html, div)
import Math

type alias Model =
    {   rules: List Rule
    }

type alias Rule = 
    {   equation: Math.Tree ()
    ,   variables: List String
    ,   requirements: List (Math.Tree ())
    }

type Event =
    Click

init: Model
init = {rules = []}

update: Event -> Model -> (Model, Cmd Event)
update _ model = (model, Cmd.none)

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attrs model = div (attrs ++ []) []