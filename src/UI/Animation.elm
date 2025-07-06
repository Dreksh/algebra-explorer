module UI.Animation exposing (DeletableElement,
    delayEvent, class,
    newDeletable, delete,
    State, createState, updateState,
    Vector2, minVector2, maxVector2, addVector2, scaleVector2,
    EaseState, newEase, setEase, current, target, smoothDampFloat, smoothDampVector2,
    Tracker, updateTracker,
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
type EaseState t = EaseState {current: t, target: t, velocity: t, changeDuration: Float}

newEase: Float -> t -> t -> EaseState t
newEase duration zero initial = EaseState {current = initial, target = initial, velocity = zero, changeDuration = duration}

setEase: Tracker -> t -> EaseState t -> (EaseState t, Tracker)
setEase tracker value (EaseState n) = if n.target == value
    then (EaseState n, tracker)
    else (EaseState {n | target = value}, max tracker (n.changeDuration*2))

current: EaseState t -> t
current (EaseState n) = n.current

target: EaseState t -> t
target (EaseState n) = n.target

type alias Tracker = Float
-- When update frame is called, tracking how many more frames are needed
updateTracker: Float -> Tracker -> Tracker
updateTracker time count = count - time

minVector2: Vector2 -> Vector2 -> Vector2
minVector2 (x1,y1) (x2,y2) = (min x1 x2, min y1 y2)

maxVector2: Vector2 -> Vector2 -> Vector2
maxVector2 (x1,y1) (x2,y2) = (max x1 x2, max y1 y2)

addVector2: Vector2 -> Vector2 -> Vector2
addVector2 (x1, y1) (x2, y2) = (x1+x2, y1+y2)

scaleVector2: Float -> Vector2 -> Vector2
scaleVector2 scale (x,y) = (x * scale, y * scale)

-- Easing logic taken from Unity
-- https://github.com/Unity-Technologies/UnityCsReference/blob/4b463aa72c78ec7490b7f03176bd012399881768/Runtime/Export/Math/Vector2.cs#L289
-- note that this doesn't have maxSpeed unlike the Unity original
smoothDamp_: Float -> Float -> Float -> Float -> Float -> (Float, Float)
smoothDamp_ smoothTime deltaTime t velocity c =
    let
        omega = 2 / smoothTime

        x = omega * deltaTime
        exp = 1 / (1 + x + 0.48 * x * x + 0.235 * x * x * x)

        change = c - t

        newTarget = c - change

        temp = (velocity + omega * change) * deltaTime

        output = newTarget + (change + temp) * exp

        origMinusCurrent = t- c
        outMinusOrig = output - t

        stop = origMinusCurrent * outMinusOrig > 0
        newPosition = if stop
            then t
            else output
        newVelocity = if stop
            then 0
            else ((velocity - omega * temp) * exp)
    in
        (newPosition, newVelocity)


smoothDampFloat: Float -> EaseState Float -> EaseState Float
smoothDampFloat deltaTime (EaseState state) =
    let
        (newCurrent, newVelocity) = state.current |> smoothDamp_ state.changeDuration deltaTime state.target state.velocity
    in
        EaseState { state | current = newCurrent, velocity = newVelocity }


smoothDampVector2: Float -> EaseState Vector2 -> EaseState Vector2
smoothDampVector2 deltaTime (EaseState state) =
    let
        (currentX, currentY) = state.current
        (targetX, targetY) = state.target
        (velocityX, velocityY) = state.velocity
        (newCurrentX, newVelocityX) = currentX |> smoothDamp_ state.changeDuration deltaTime targetX velocityX
        (newCurrentY, newVelocityY) = currentY |> smoothDamp_ state.changeDuration deltaTime targetY velocityY
    in
        EaseState { state | current = (newCurrentX, newCurrentY), velocity = (newVelocityX, newVelocityY) }

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