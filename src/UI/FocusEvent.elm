module UI.FocusEvent exposing (Raw, Model, init, resolve)

import Dict

type alias Raw = {id: String}

type alias Model msg =
    {   map: Dict.Dict String msg
    }

init: Dict.Dict String msg -> Model msg
init dict = Model dict

resolve: Model msg -> msg -> {id: String} -> msg
resolve model noop input = Dict.get input.id model.map |> Maybe.withDefault noop