module Components.Query exposing (Model, parseInit, pushUrl, setEquations)

import Browser.Navigation as Nav
import Dict
import Url
-- Ours
import Algo.Math as Math
import Algo.Matcher as Matcher

type alias Model =
    {   current: Url.Url
    ,   key: Nav.Key
    -- Values for calculating the query
    ,   equations: List String
    ,   sources: List String
    }

setEquations: Dict.Dict Int (Matcher.Equation msg) -> Model -> Model
setEquations dict model = dict
    |> Dict.toList
    |> List.map (\(_, elem) -> Math.toString elem.root )
    |> (\result -> {model | equations = result})

parseInit: Url.Url -> Nav.Key -> Model
parseInit url key =
    Maybe.withDefault "" url.query
    |> String.split "&"
    |> List.foldl unmarshalModel_
    {   current = url
    ,   key = key
    ,   equations = []
    ,   sources = []
    }

pushUrl: Model -> Cmd msg
pushUrl model = let eqs = marshalEquations_ model.equations in
    model.current
    |> (\url -> Url.toString { url | query = Just eqs} )
    |> Nav.replaceUrl model.key

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
            "source" -> case value of
                Just str -> {model | sources = model.sources ++ [str]}
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
