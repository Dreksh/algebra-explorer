module UI.Animation exposing (
    State, stateOps,
    Vector2, minVector2, maxVector2, addVector2, subVector2, scaleVector2, descaleVector2,
    EaseState, newEase, newEaseFloat, newEaseVector2,
    setEase, easeOut, current, target, advance,
    Tracker, updateTracker,
    stateDecoder, encodeState
    )

import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import Algo.Matcher as Matcher
import Components.Rules as Rules

{- Animation State -}

type alias State =
    {   prevID: Int -- To track where it originated from. If it's the same ID as itself, it's new
    ,   corrID: Int -- to track the original ID of where it came from
    ,   function: Maybe Rules.FunctionProp
    }

stateOps: Matcher.StateOp Rules.FunctionProp State
stateOps =
    {   new = \prop num -> { prevID = num, corrID = num, function = prop}
    ,   copy = \s _ -> {prevID = Matcher.getID s, corrID = Matcher.getState s |> .corrID, function = Matcher.getState s |> .function}
    ,   update = \newProp s -> if newProp == s.function
            then (s, False)
            else ({s | function = newProp}, True)
    ,   extract = .function
    }

type alias Vector2 = (Float, Float)
type EaseState t = EaseState
    {   zero: t
    ,   changeDuration: Float
    -- For displaying
    ,   current: t
    -- For calculating
    ,   target: t
    ,   firstSpline: t -- coefficient of t^5
    ,   secondSpline: t -- coefficient of 5t^4(1-t)
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
            (   EaseState
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
                        |> n.addition (n.scale (-time*time*time*time) n.firstSpline)
                        |> n.addition (n.scale (time*time*time*(5*time-4)) n.secondSpline)
                ,   target = value
                }
            , max tracker n.changeDuration
            )

easeOut: Tracker -> t -> t -> EaseState t -> (EaseState t, Tracker)
easeOut tracker start finish (EaseState n) = let diff = n.scale -1 finish |> n.addition start in
    (   EaseState
        {   n
        |   remainingTime = n.changeDuration
        ,   firstSpline = diff
        ,   secondSpline = diff
        ,   target = finish
        }
    ,   max tracker n.changeDuration
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

subVector2: Vector2 -> Vector2 -> Vector2
subVector2 (x1, y1) (x2, y2) = (x2 - x1, y2-y1)

scaleVector2: Float -> Vector2 -> Vector2
scaleVector2 scale (x,y) = (x * scale, y * scale)

descaleVector2: Float -> Vector2 -> Vector2
descaleVector2 scale (x,y) = (x / scale, y / scale)

advance: Float -> EaseState t -> EaseState t
advance deltaTime (EaseState state) = let remainingTime = state.remainingTime - deltaTime |> max 0 in
    if remainingTime == 0
    then EaseState {state | current = state.target, firstSpline = state.zero, secondSpline = state.zero, remainingTime = 0}
    else let time = remainingTime / state.changeDuration in
        EaseState
        {   state
        |   current = state.addition
                (state.scale (time*time*time*time*time) state.firstSpline)
                (state.scale (5*time*time*time*time*(1-time)) state.secondSpline)
                |> state.addition state.target
        ,   remainingTime = remainingTime
        }

{--
Encoding and Decoding to catch the state of the element
--}

encodeState: State -> Encode.Value
encodeState s = Encode.object
    (   [   ("prevID", Encode.int s.prevID)
        ,   ("corrID", Encode.int s.corrID)
        ]
        ++ case s.function of
            Just function -> Rules.encodeFunctionProp function
            Nothing -> []
    )

stateDecoder: Decode.Decoder State
stateDecoder = Decode.map3 State
    (Decode.field "prevID" Decode.int)
    (Decode.field "corrID" Decode.int)
    (Decode.maybe Rules.functionPropDecoder)