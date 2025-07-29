module UI.HtmlEvent exposing (
    onClick, onPointerEnter, onPointerLeave, onSubmit, onFocus, onBlur, onSubmitField, onSubmitForm,
    onPointerCapture, onPointerMove, onKeyDown)

-- Event is needed to block the js events from propagating upwards

import Html
import Html.Events exposing(custom, preventDefaultOn, stopPropagationOn)
import Json.Decode exposing (Decoder, Value, bool, field, float, map, map2, map3, string, succeed, value)
import Set

onClick: msg -> Html.Attribute msg
onClick event = stopPropagationOn "click" (succeed (event, True))

onPointerEnter: msg -> Html.Attribute msg
onPointerEnter event = stopPropagationOn "pointerenter" (succeed (event, True))

onPointerLeave: msg -> Html.Attribute msg
onPointerLeave event = stopPropagationOn "pointerleave" (succeed (event, True))

onSubmit: msg -> Html.Attribute msg
onSubmit = Html.Events.onSubmit

onFocus: msg -> Html.Attribute msg
onFocus = Html.Events.onFocus

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

clientPos_: Decoder (Float, Float)
clientPos_ = map2 Tuple.pair (field "clientX" float) (field "clientY" float)

onPointerCapture: (event -> msg) -> (Value -> (Float, Float) -> event) -> Html.Attribute msg
onPointerCapture converter activate = custom "pointerdown"
    (map2 (\pid input -> {message = activate pid input |> converter, stopPropagation = True, preventDefault = True})
        (field "pointerId" value)
        clientPos_
    )

onPointerMove: (event -> msg) -> (Value -> (Float, Float) -> event) -> (Value -> event) -> List (Html.Attribute msg)
onPointerMove converter move cancel = let pointerId = field "pointerId" value in
    [   custom "pointermove" (map2 (\pid input -> {message = move pid input |> converter, stopPropagation = True, preventDefault = True}) pointerId clientPos_)
    ,   custom "pointerup" (map (\pid -> {message = cancel pid |> converter, stopPropagation = True, preventDefault = True}) pointerId)
    ,   custom "pointercancel" (map (\pid -> {message = cancel pid |> converter, stopPropagation = True, preventDefault = True}) pointerId)
    ]

propagatableEvents_: Set.Set String
propagatableEvents_ = Set.fromList ["Tab", "Escape"]

onKeyDown: ((String, Bool, Bool) -> msg) -> Html.Attribute msg
onKeyDown event = custom "keydown"
    (   map3
        (\key shift cmd ->
            {   message = event (key, shift, cmd)
            ,   stopPropagation = Set.member key propagatableEvents_ |> not
            ,   preventDefault = Set.member key propagatableEvents_ |> not
            }
        )
        (field "key" string)
        (field "shiftKey" bool)
        (field "metaKey" bool)
    )
