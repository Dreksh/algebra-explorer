module UI.HtmlEvent exposing (..)

-- Event is needed to block the js events from propagating upwards

import Html
import Html.Events exposing(custom, preventDefaultOn, stopPropagationOn)
import Json.Decode exposing (Decoder, Value, field, float, map, map2, string, succeed, value)

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

onSubmitForm: Decoder msg -> Html.Attribute msg
onSubmitForm target = preventDefaultOn "submit"
    (   map (\input -> (input, True))
        <| field "target"
        <| target
    )

onPointerCapture: (event -> msg) -> (Value -> (Float, Float) -> event) -> Html.Attribute msg
onPointerCapture converter activate = custom "pointerdown"
    (map2 (\pid input -> {message = activate pid input |> converter, stopPropagation = True, preventDefault = True})
        (field "pointerId" value)
        (map2 Tuple.pair (field "clientX" float) (field "clientY" float))
    )


onPointerMove: (event -> msg) -> (Value -> (Float, Float) -> event) -> (Value -> event) -> List (Html.Attribute msg)
onPointerMove converter move cancel =
    let
        positionDecoder = map2 Tuple.pair (field "clientX" float) (field "clientY" float)
        pointerId = field "pointerId" value
    in
        [   custom "pointermove" (map2 (\pid input -> {message = move pid input |> converter, stopPropagation = True, preventDefault = True}) pointerId positionDecoder)
        ,   custom "pointerup" (map (\pid -> {message = cancel pid |> converter, stopPropagation = True, preventDefault = True}) pointerId)
        ,   custom "pointercancel" (map (\pid -> {message = cancel pid |> converter, stopPropagation = True, preventDefault = True}) pointerId)
        ]