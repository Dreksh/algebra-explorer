module Display exposing (
    Model, Event, init, update, view,
    addEquation
    )

import Dict
import Math
import Html exposing (Html, div)

type alias Model =
    {   equations: List (Math.Tree State, Dict.Dict Int (List Int))
    ,   selected: List Int
    ,   nextNodeNum: Int
    }

type Event =
    Select Int
    | Unselect
    | Reselect Int

type alias State =
    {   position: (Float, Float)
    ,   id: Int
    }

init: List (Math.Tree ()) -> Model
init trees =
    {   equations = []
    ,   selected = []
    ,   nextNodeNum = 0
    }

addEquation: Math.Tree () -> Model -> Model
addEquation tree model = model

update: Event -> Model -> (Model, Cmd Event)
update event model = (model, Cmd.none)

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attr model = div (attr ++ []) []