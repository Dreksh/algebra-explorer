module UI.Animation exposing (DeletableElement,
    delayEvent, class,
    newDeletable, delete,
    State, createState, updateState,
    Vector2, minVector2, maxVector2, addVector2, scaleVector2,
    EaseState, newEase, newEaseFloat, newEaseVector2,
    setEase, current, target, advance,
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
type EaseState t = EaseState
    {   zero: t
    ,   changeDuration: Float
    -- For displaying
    ,   current: t
    -- For calculationg
    ,   target: t
    ,   firstSpline: t -- coefficient of t^3
    ,   secondSpline: t -- coefficient of 3t^2(t-1)
    ,   addition: t -> t -> t
    ,   scale: Float -> t -> t
    ,   remainingTime: Float -- We're going from t=1 to t=0, where t is remainingTime / changeDuration
    }

newEase: Float -> t -> (t -> t -> t) -> (Float -> t -> t) -> t -> EaseState t
newEase duration zero addition scale initial = EaseState
    {   zero = zero
    ,   changeDuration = duration
    ,   current = initial
    ,   target = initial
    ,   firstSpline = zero
    ,   secondSpline = zero
    ,   addition = addition
    ,   scale = scale
    ,   remainingTime = 0
    }

newEaseVector2: Float -> Vector2 -> EaseState Vector2
newEaseVector2 duration initial = newEase duration (0,0) addVector2 scaleVector2 initial

newEaseFloat: Float -> Float -> EaseState Float
newEaseFloat duration initial = newEase duration 0 (+) (*) initial

setEase: Tracker -> t -> EaseState t -> (EaseState t, Tracker)
setEase tracker value (EaseState n) = if n.target == value
    then (EaseState n, tracker)
    else let diff = n.scale -1 value |> n.addition n.current in
        if n.remainingTime == 0
        then
            (  EaseState
                {   n
                |   remainingTime = n.changeDuration
                ,   firstSpline = diff
                ,   secondSpline = diff
                ,   target = value
                }
            ,   max tracker n.changeDuration
            )
        else let time = n.remainingTime / n.changeDuration in
            (   EaseState
                {   n
                |   remainingTime = n.changeDuration
                ,   firstSpline = diff
                ,   secondSpline = diff
                        |> n.addition (n.scale (-time*time*time) n.firstSpline)
                        |> n.addition (n.scale (time*time*(4*time-3)) n.secondSpline)
                ,   target = value
                }
            , max tracker n.changeDuration
            )

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

advance: Float -> EaseState t -> EaseState t
advance deltaTime (EaseState state) = let remainingTime = state.remainingTime - deltaTime |> max 0 in
    if remainingTime == 0
    then EaseState {state | current = state.target, firstSpline = state.zero, secondSpline = state.zero, remainingTime = 0}
    else let time = remainingTime / state.changeDuration in
        EaseState
        {   state
        |   current = state.addition
                (state.scale (time*time*time*time) state.firstSpline)
                (state.scale (4*time*time*time*(1-time)) state.secondSpline)
                |> state.addition state.target
        ,   remainingTime = remainingTime
        }

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