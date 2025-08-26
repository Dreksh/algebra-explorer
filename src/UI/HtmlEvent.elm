module UI.HtmlEvent exposing (
    onClick, onPointerEnter, onPointerLeave, onSubmit, onFocus, onBlur, onSubmitForm,
    onPointerCapture, onKeyChange, onKeyDown)

-- Event is needed to block the js events from propagating upwards

import Html
import Html.Events exposing(custom, on, preventDefaultOn, stopPropagationOn)
import Json.Decode exposing (Decoder, Value, bool, field, float, map, map2, map4, string, succeed, value)
import Set

onClick: msg -> Html.Attribute msg
onClick event = stopPropagationOn "click" (succeed (event, True))

onPointerEnter: msg -> Html.Attribute msg
onPointerEnter event = stopPropagationOn "pointerenter" (succeed (event, True))

onPointerLeave: msg -> Html.Attribute msg
onPointerLeave event = stopPropagationOn "pointerleave" (succeed (event, True))

onSubmit: msg -> Html.Attribute msg
onSubmit = Html.Events.onSubmit

onKeyChange: (String -> msg) -> Html.Attribute msg
onKeyChange event = Html.Events.on "keydown" (field "target" <| field "value" <| map event string)

onFocus: msg -> Html.Attribute msg
onFocus = Html.Events.onFocus

onBlur: msg -> Html.Attribute msg
onBlur = Html.Events.onBlur

onSubmitForm: Decoder msg -> Html.Attribute msg
onSubmitForm target = preventDefaultOn "submit"
    (   map (\input -> (input, True))
        <| field "target"
        <| target
    )

clientPos_: Decoder (Float, Float)
clientPos_ = map2 Tuple.pair (field "clientX" float) (field "clientY" float)

onPointerCapture: (event -> msg) -> (Value -> (Float, Float) -> event) -> Html.Attribute msg
onPointerCapture converter activate = on "pointerdown"
    (map2 (\pid input -> activate pid input |> converter)
        (field "pointerId" value)
        clientPos_
    )

propagatableEvents_: Set.Set (Int, String) -- Bool is not comparable
propagatableEvents_ = Set.fromList [(0, "Tab"), (0, "Escape"), (0, "Enter"), (1, "+"), (1, "="), (1, "-")]

onKeyDown: ({key: String, shift: Bool, meta: Bool, time: Float} -> msg) -> Html.Attribute msg
onKeyDown event = custom "keydown"
    (   map4
        (\key shift meta time ->
            {   message = event {key = key, shift = shift, meta = meta, time = time}
            ,   stopPropagation = Set.member (if meta then 1 else 0, key) propagatableEvents_ |> not
            ,   preventDefault = Set.member (if meta then 1 else 0, key) propagatableEvents_ |> not
            }
        )
        (field "key" string)
        (field "shiftKey" bool)
        (field "metaKey" bool)
        (field "timeStamp" float)
    )
