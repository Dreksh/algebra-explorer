module Query exposing (Model, parseInit, pushUrl, setEquations)

import Browser.Navigation as Nav
import Dict
import Url
import Math

type alias Model =
    {   current: Url.Url
    ,   key: Nav.Key
    -- Values for calculating the query
    ,   equations: List String
    }

setEquations: Dict.Dict Int (Math.Tree msg) -> Model -> Model
setEquations dict model = Dict.foldl
    (\_ elem result -> Math.toString elem :: result) [] dict
    |> (\result -> {model | equations = result})

parseInit: Url.Url -> Nav.Key -> Model
parseInit url key =
    Maybe.withDefault "" url.query
    |> String.split "&"
    |> List.foldl unmarshalModel_
    {   current = url
    ,   key = key
    ,   equations = []
    }

pushUrl: Model -> Cmd msg
pushUrl model =
    Nav.replaceUrl model.key "str"

unmarshalModel_: String -> Model -> Model
unmarshalModel_ line model =
    (   case String.indices "=" line of
            (x::_) -> (String.left x line, String.dropLeft (x + 1) line |> Url.percentDecode)
            [] -> (line, Nothing)
    )
    |> (\(field, value) ->
        case field of
            "eq" -> case value of
                Just str -> {model | equations = model.equations ++ [str]}
                _ -> model
            _ -> model
    )

marshalEquations_: List String -> String
marshalEquations_ =
    List.foldl (\line result ->
        ("eq=" ++ Url.percentEncode line) :: result
    )
    []
    >> (\list -> String.join "&" list)
