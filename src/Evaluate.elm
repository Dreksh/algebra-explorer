module Evaluate exposing (Model, finish, init, send)

import Dict

type alias Model context msg =
    {   nextCallID: Int
    ,   ongoing: Dict.Dict Int context
    ,   sender: {id: Int, str: String} -> Cmd msg
    }

init: ({id: Int, str: String} -> Cmd msg) -> Model context msg
init = Model 0 Dict.empty

finish: Int -> Model context msg -> (Model context msg, Maybe context)
finish id model = case Dict.get id model.ongoing of
    Nothing -> (model, Nothing)
    Just c -> ({model | ongoing = Dict.remove id model.ongoing}, Just c)

send: context -> String -> Model context msg -> (Model context msg, Cmd msg)
send c str model =
    (   {   model
        |   nextCallID = model.nextCallID + 1
        ,   ongoing = Dict.insert model.nextCallID c model.ongoing
        }
    ,   model.sender {id = model.nextCallID, str = str}
    )
