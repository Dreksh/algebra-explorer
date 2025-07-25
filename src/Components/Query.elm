module Components.Query exposing (Model, parseInit, pushEquations)

import Browser.Navigation as Nav
import Url
-- Ours
import Algo.Math as Math
import Algo.Matcher as Matcher
import Components.Rules as Rules

type alias Model =
    {   current: Url.Url
    ,   key: Nav.Key
    -- Values for calculating the query
    ,   equations: List String
    ,   sources: List String
    }

-- This time it's flipped, because we want to generate (List (Matcher.Equation msg) -> Cmd msg)
pushEquations: Model -> List (Matcher.Equation a b) -> Cmd msg
pushEquations model list =
    List.map (\elem -> Rules.process (\_ -> String.join "") identity elem.root) list
    |> \result -> {model | equations = result}
    |> pushUrl_

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

pushUrl_: Model -> Cmd msg
pushUrl_ model = let eqs = marshalEquations_ model.equations in
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
