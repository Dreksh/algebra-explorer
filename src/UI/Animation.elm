module UI.Animation exposing (DeletableElement,
    delayEvent, class,
    newDeletable, delete,
    State, createState, updateState,
    EaseState, Vector2, smoothDampFloat, smoothDampVector2,
    encode, decoder, stateDecoder, encodeState
    )

import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Task
-- Ours
import Helper
import Algo.Matcher as Matcher

{- Animation State -}

type alias State =
    {   prevID: Int -- To track where it originated from. If it's the same ID as itself, it's new
    }

createState: Int -> State
createState num = { prevID = num }

updateState: Matcher.State State -> Int -> State
updateState s _ = {prevID = Matcher.getID s}

type alias Vector2 = (Float, Float)
type alias EaseState t =
    {    current: t
    ,    target: t
    ,    velocity: t
    }

-- Easing logic taken from Unity
-- https://github.com/Unity-Technologies/UnityCsReference/blob/4b463aa72c78ec7490b7f03176bd012399881768/Runtime/Export/Math/Vector2.cs#L289
-- note that this doesn't have maxSpeed unlike the Unity original
smoothDamp_: Float -> Float -> Float -> Float -> Float -> (Float, Float)
smoothDamp_ smoothTime deltaTime target velocity current =
    let
        omega = 2 / smoothTime

        x = omega * deltaTime
        exp = 1 / (1 + x + 0.48 * x * x + 0.235 * x * x * x)

        change = current - target

        newTarget = current - change

        temp = (velocity + omega * change) * deltaTime

        output = newTarget + (change + temp) * exp

        origMinusCurrent = target - current
        outMinusOrig = output - target

        stop = origMinusCurrent * outMinusOrig > 0
        newPosition = if stop
            then target
            else output
        newVelocity = if stop
            then 0
            else ((velocity - omega * temp) * exp)
    in
        (newPosition, newVelocity)


smoothDampFloat: Float -> Float -> EaseState Float -> EaseState Float
smoothDampFloat smoothTime deltaTime state =
    let
        (newCurrent, newVelocity) = state.current |> smoothDamp_ smoothTime deltaTime state.target state.velocity
    in
        { state | current = newCurrent, velocity = newVelocity }


smoothDampVector2: Float -> Float -> EaseState Vector2 -> EaseState Vector2
smoothDampVector2 smoothTime deltaTime state =
    let
        (currentX, currentY) = state.current
        (targetX, targetY) = state.target
        (velocityX, velocityY) = state.velocity
        (newCurrentX, newVelocityX) = currentX |> smoothDamp_ smoothTime deltaTime targetX velocityX
        (newCurrentY, newVelocityY) = currentY |> smoothDamp_ smoothTime deltaTime targetY velocityY
    in
        { state | current = (newCurrentX, newCurrentY), velocity = (newVelocityX, newVelocityY) }

{- Old Animation -}

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

decoder: Decode.Decoder elem -> (elem -> () -> Cmd event) -> Decode.Decoder (DeletableElement elem event)
decoder innerDec trigger = Decode.map2 (\e d -> {element = e, triggerDelete = trigger e, deleting = d})
    (Decode.field "element" <| innerDec)
    (Decode.field "deleting" <| Decode.bool)

encodeState: State -> Encode.Value
encodeState s = Encode.object [("prevID", Encode.int s.prevID)]

stateDecoder: Decode.Decoder State
stateDecoder = Decode.map State (Decode.field "prevID" Decode.int)