module HtmlEvent exposing (..)

-- Event is needed to block the js events from propagating upwards

import Html
import Html.Events exposing(preventDefaultOn, stopPropagationOn)
import Json.Decode exposing (succeed)

onClick: msg -> Html.Attribute msg
onClick event = stopPropagationOn "click" (succeed (event, True))

onSubmitField: String -> (String -> msg) -> Html.Attribute msg
onSubmitField _ event = preventDefaultOn "submit" (succeed (event "", True))