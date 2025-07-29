module UI.SvgDrag exposing (Model, Event(..), Raw, init, resolve)

import Dict

-- For port to use, event is defined by 0: start, 1: move, 2: end (see resolve)
type alias Raw = {event: Int, id: String, x: Float, y: Float, time: Float}

-- These positions (Float, Float) will all be converted into SVG coordinates
type Event =
    Start (Float, Float)
    | Move Float (Float, Float)
    | End Float (Float, Float)

type alias Model msg =
    {   mapping: Dict.Dict String (String -> Event -> Maybe msg)
    }

init: Dict.Dict String (String -> Event -> Maybe msg) -> Model msg
init dict = {mapping = dict}

resolve: Raw -> Model msg -> Maybe msg
resolve input model = Dict.foldl (\key value result -> case result of
    Just _ -> result
    Nothing -> if String.startsWith key input.id
        then Just (String.dropLeft (String.length key) input.id |> value)
        else Nothing
    ) Nothing model.mapping
    |> Maybe.andThen (\convert -> case input.event of
        0 -> convert (Start (input.x, input.y))
        1 -> convert (Move input.time (input.x, input.y))
        2 -> convert (End (input.time) (input.x, input.y))
        _ -> Nothing
    )