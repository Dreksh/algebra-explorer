module HtmlEvent exposing (..)

-- Event is needed to block the js events from propagating upwards

import Html
import Html.Events exposing(preventDefaultOn, stopPropagationOn)
import Json.Decode exposing (field, map, string, succeed)

onClick: msg -> Html.Attribute msg
onClick event = stopPropagationOn "click" (succeed (event, True))

onSubmit: msg -> Html.Attribute msg
onSubmit = Html.Events.onSubmit

onBlur: msg -> Html.Attribute msg
onBlur = Html.Events.onBlur

onSubmitField: String -> (String -> msg) -> Html.Attribute msg
onSubmitField target event = preventDefaultOn "submit"
    (   map (\input -> (event input, True))
        <| field "target"
        <| field target
        <|  field "value" string
    )
