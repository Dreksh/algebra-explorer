module UI.Animation exposing (DeletableElement,
    delayEvent, class,
    newDeletable, delete,
    encode, decoder
    )

import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Task
-- Ours
import Helper

delayEvent: Float -> event -> Cmd event
delayEvent timeout e = Task.perform (\_ -> e) (Process.sleep timeout)

class: DeletableElement elem event -> Maybe (Html.Attribute event)
class element = Html.Attributes.class "deleting" |> Helper.maybeGuard element.deleting

{--
DeletableElement adds an additional state for elements that need to be 'animated out' before it's removed.
This gives it time to run the animation before triggering the proper delete method
--}
type alias DeletableElement elem event =
    {   element: elem
    ,   triggerDelete: () -> Cmd event
    ,   deleting: Bool
    }

newDeletable: (() -> Cmd event) -> elem -> DeletableElement elem event
newDeletable trigger element = {element = element, triggerDelete = trigger, deleting = False}

delete: DeletableElement elem event -> (DeletableElement elem event, Cmd event)
delete element = if element.deleting then (element, Cmd.none) else ({element | deleting = True}, element.triggerDelete ())

{--
Encoding and Decoding to catch the state of the element
--}
encode: (elem -> Encode.Value) -> DeletableElement elem event -> Encode.Value
encode innerEnc element = Encode.object
    [   ("element", innerEnc element.element)
    ,   ("deleting", Encode.bool element.deleting)
    ]

decoder: Decode.Decoder elem -> (() -> Cmd event) -> Decode.Decoder (DeletableElement elem event)
decoder innerDec trigger = Decode.map2 (\e d -> {element = e, triggerDelete = trigger, deleting = d})
    (Decode.field "element" <| innerDec)
    (Decode.field "deleting" <| Decode.bool)