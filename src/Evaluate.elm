module Evaluate exposing (Model, finish, init, send, decoder, encode)

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import Helper

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

decoder: ({id: Int, str: String} -> Cmd msg) -> Decode.Decoder context -> Decode.Decoder (Model context msg)
decoder sender innerDec = Decode.map2 (\n o -> {nextCallID = n, ongoing = o, sender = sender})
    (Decode.field "nextCallID" Decode.int)
    (Decode.field "ongoing" <| Helper.intDictDecoder innerDec)

encode: (context -> Encode.Value) -> Model context msg -> Encode.Value
encode converter model = Encode.object
    [   ("nextCallID", Encode.int model.nextCallID)
    ,   ("ongoing", Encode.dict String.fromInt converter model.ongoing)
    ]