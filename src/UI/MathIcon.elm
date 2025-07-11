module UI.MathIcon exposing (Model, blank, init, set, advanceTime, view, static)

import Dict
import Html
import Svg
import Svg.Attributes exposing (d, fill, opacity, stroke, strokeWidth, viewBox)
-- Ours
import Algo.Matcher as Matcher
import Components.Latex as Latex
import UI.Animation as Animation
import UI.Animation as Animation

type alias State = Matcher.State Animation.State
type alias Vector2 = Animation.Vector2

type Stroke =
    Move Vector2
    | Line Vector2
    | Curve Vector2 Vector2 Vector2

type alias AnimationFrame =
    {   data: List {strokes: List Stroke, origin: Animation.EaseState Vector2, scale: Animation.EaseState Float} -- This is for parts that are split, i.e. commas in the function
    ,   opacity: Animation.EaseState Float
    }
type alias Model =
    {   frames: Dict.Dict Int (Dict.Dict Int AnimationFrame) -- prevID and then regular ID
    ,   topLeft: Animation.EaseState Vector2
    ,   botRight: Animation.EaseState Vector2
    }

blank: Model
blank =
    {   frames = Dict.empty
    ,   topLeft = Animation.newEase 300 (0,0) (0,0)
    ,   botRight = Animation.newEase 300 (0,0) (0,0)
    }

init: Latex.Model State -> Model
init root = blank

advanceTime: Float -> Model -> Model
advanceTime time model =
    {   model
    |   frames = Dict.map (\_ -> Dict.map (\_ animation ->
            {   animation
            |   data = List.map (\data ->
                    {   data
                    |   origin = Animation.smoothDampVector2 time data.origin
                    ,   scale = Animation.smoothDampFloat time data.scale
                    }
                ) animation.data
            ,   opacity = Animation.smoothDampFloat time animation.opacity
            }
            )
        )
        model.frames
    }

set: Animation.Tracker -> Latex.Model State -> Model -> (Model, Animation.Tracker)
set tracker root model = (model, tracker)

{- toFrames -}

type FrameData state =
    BaseFrame {strokes: List Stroke, elem: state}
    | Position (List {frame: Frame state, origin: Vector2, scale: Float})
type alias Frame state =
    {   data: FrameData state
    ,   topLeft: Vector2
    ,   botRight: Vector2
    }

type alias Ref =
    {   body: Float -- how left (most cases)
    ,   top: Float -- superscript
    ,   bot: Float -- subscript
    }

latexToFrames_: Latex.Model state -> Frame state
latexToFrames_ = List.foldl
    (\elem ((list, topLeft, botRight), ref) ->
        symbolsToFrames_ ref elem
        |> \(new, newRef) ->
            (   (   {frame = new, origin = (ref.body,0), scale = 1} :: list
                ,   Animation.minVector2 topLeft (Animation.addVector2 (ref.body,0) new.topLeft)
                ,   Animation.maxVector2 botRight (Animation.addVector2 (ref.body,0) new.botRight)
                )
            ,   newRef
            )
    )
    (([], (0,0), (0, 0)), {body = 0, top = 0, bot = 0})
    >> \((list, topLeft, botRight),_) -> {data = Position list, topLeft = topLeft, botRight = botRight}

symbolsToFrames_: Ref -> Latex.Part state -> (Frame state, Ref)
symbolsToFrames_ ref elem = case elem of
    Latex.Fraction s top bottom ->
        let
            topFrame = latexToFrames_ top
            botFrame = latexToFrames_ bottom
            maxWidth = max (Tuple.first topFrame.botRight) (Tuple.first botFrame.botRight)
            width = maxWidth*0.75 + 0.25
            topOrigin = -(Tuple.second topFrame.botRight)*0.75 - 0.1
            botOrigin = -(Tuple.second botFrame.topLeft)*0.75 + 0.1
            up = topOrigin + (Tuple.second topFrame.topLeft)*0.75
            bot = botOrigin + (Tuple.second botFrame.botRight)*0.75
        in
            (   {   data = Position
                    [   {   frame = {data = BaseFrame {strokes = [Move (0,0), Line (width, 0)], elem = s }, topLeft = (0,0), botRight = (0,width)}
                        , origin = (0,0), scale = 1 }
                    ,   {   frame = topFrame
                        ,   origin = (0.125 + (maxWidth - (Tuple.first topFrame.botRight))*0.375, topOrigin)
                        ,   scale = 0.75
                        }
                    ,   {   frame = botFrame
                        ,   origin = (0.125 + (maxWidth - (Tuple.first botFrame.botRight))*0.375, botOrigin)
                        ,   scale = 0.75
                        }
                    ]
                ,   topLeft = (0, up)
                ,   botRight = (width, bot)
                }
            ,   {body = ref.body + width, top = up, bot = bot}
            )
    Latex.Superscript _ inner -> latexToFrames_ inner
        |> \new ->
            (   {   data = Position [{frame = new, origin = (0,ref.top), scale = 0.5}]
                ,   topLeft = Animation.scaleVector2 0.5 new.topLeft |> Animation.addVector2 (0, ref.top)
                ,   botRight = Animation.scaleVector2 0.5 new.botRight |> Animation.addVector2 (0, ref.top)
                }
            ,   ref
            )
    Latex.Subscript _ inner -> latexToFrames_ inner
        |> \new ->
            (   {   data = Position [{frame = new, origin = (0,ref.top), scale = 0.5}]
                ,   topLeft = Animation.scaleVector2 0.5 new.topLeft |> Animation.addVector2 (0, ref.bot)
                ,   botRight = Animation.scaleVector2 0.5 new.botRight |> Animation.addVector2 (0, ref.bot)
                }
            ,   ref
            )
    Latex.Text s str -> wordStrokes_ s str
        |> \new ->
            (   {   data = Position [{frame = new, origin = (0,0), scale = 1}]
                ,   topLeft = new.topLeft
                ,   botRight = new.botRight
                }
            ,   {body = ref.body + Tuple.first new.botRight, top = new.topLeft |> Tuple.second, bot = new.botRight |> Tuple.second}
            )
    Latex.SymbolPart s str -> symbolStrokes_ s str
        |> \new ->
            (   {   data = Position [{frame = new, origin = (0,0), scale = 1}]
                ,   topLeft = new.topLeft
                ,   botRight = new.botRight
                }
            ,   {body = ref.body + Tuple.first new.botRight, top = new.topLeft |> Tuple.second, bot = new.botRight |> Tuple.second}
            )
    Latex.Bracket s inner -> latexToFrames_ inner
        |> \new ->
            let
                (top, bot) = (Tuple.second new.topLeft, Tuple.second new.botRight)
                (midTop, midBot) = ((2*top + bot)/3, (top + 2*bot)/3 )
            in
            (   {   data = Position
                    [   {frame = new, origin = (0.3, 0), scale = 1}
                    ,   {frame = {data = BaseFrame {strokes = [Move (0.2, top), Curve (0, midTop) (0, midBot) (0.2, bot)], elem = s}, topLeft = (0,top), botRight = (0.3,bot)}, origin = (0,0), scale = 1}
                    ,   {frame = {data = BaseFrame {strokes = [Move (0.1, top), Curve (0.3, midTop) (0.3, midBot) (0.1, bot)], elem = s}, topLeft = (0,top), botRight = (0.3,bot)}, origin = (Tuple.first new.botRight + 0.3, 0), scale = 1}
                    ]
                ,   topLeft = new.topLeft
                ,   botRight = Animation.addVector2 (0.6, 0) new.botRight
                }
            ,   {   ref | body = ref.body + Tuple.first new.botRight + 0.6}
            )
    Latex.Sqrt _ inner -> latexToFrames_ inner
        |> \new ->
            (   {   data = Position [{frame = new, origin = (0.75, 0), scale = 1}] -- TODO: Add the sqrt line
                ,   topLeft = new.topLeft |> Animation.addVector2 (0, 1)
                ,   botRight = new.botRight |> Animation.addVector2 (1, 0)
                }
            ,   {   ref| body = ref.body + 1, top = ref.top + 1 }
            )
    Latex.Argument s _ -> (failedFrame_ s, {body = ref.body + 1, top = -0.5, bot = 0.5}) -- TODO: Create an argument stroke for inputs

processFrame_: (state -> List Stroke -> Vector2 -> Float -> end -> end) -> Vector2 -> Float -> end -> Frame state -> (end, Vector2, Vector2)
processFrame_ combine origin scale initial frame =
    (   case frame.data of
            BaseFrame s -> combine s.elem s.strokes origin scale initial
            Position list -> List.foldl (\elem result ->
                    processFrame_ combine
                    (Animation.scaleVector2 scale elem.origin |> Animation.addVector2 origin)
                    (scale * elem.scale)
                    result
                    elem.frame
                    |> \(a,_,_) -> a
                ) initial list
    ,   Animation.scaleVector2 scale frame.topLeft
    ,   Animation.scaleVector2 scale frame.botRight
    )

{- toStrokes

values are all relative to height, where height is is treated as the unit (for width as well)
1.0 of height is equivalent to the height of short characters, i.e. (a,n,m,o)
origin should be on the left + half-way through the height of the short characters.
-}

failedFrame_: state -> Frame state
failedFrame_ s =
    {   data = BaseFrame {strokes = [Move (0, -0.5), Line (0,0.5), Line (1,0.5), Line (1,-0.5), Line (0, -0.5), Line (1,0.5)], elem = s}
    ,   topLeft  = (0, -0.5)
    ,   botRight = (1,0.5)
    }

symbolStrokes_: state -> Latex.Symbol -> Frame state
symbolStrokes_ s str = case str of
    Latex.AlphaLower -> failedFrame_ s -- {data = BaseFrame {strokes = [], elem = s}, topLeft = (0, -1), botRight = (1.5, 0.5)}
    Latex.BetaLower -> failedFrame_ s
    Latex.CrossMultiplcation -> {data = BaseFrame {strokes = [Move (0.1,-0.2), Line(0.5,0.2), Move (0.1,0.2), Line (0.5,-0.2)], elem = s}, topLeft = (0,-0.3), botRight = (0.6, 0.3)}
    Latex.Integration -> failedFrame_ s -- {data = BaseFrame {strokes = [], elem = s}, topLeft = (0, -1.5), botRight = (0.5, 1.5)}

wordStrokes_: state -> String -> Frame state
wordStrokes_ s = String.foldl (\c res -> case res of
        Err err -> Err err
        Ok (list, (_, top), (right, bot)) -> charStrokes_ c
            |> Result.map (\(strokes, (_, newTop), (newRight, newBot)) ->
                (rightShiftStrokes_ right strokes ++ list, (0, min top newTop), (right + newRight, max bot newBot))
            )
    ) (Ok ([], (0, 0), (0, 0)))
    >> \r -> case r of
        Ok (list, topLeft, botRight) -> {data = BaseFrame {strokes = list, elem = s}, topLeft = topLeft, botRight = botRight}
        Err _ -> failedFrame_ s

charStrokes_: Char -> Result String (List Stroke, Vector2, Vector2)
charStrokes_ c = case c of
    'a' -> Ok ([Move (0.7,-0.3), Curve (0.4,-0.5) (0,0) (0.2,0.2),Curve (0.4,0.4) (0.7,0.2) (0.7,-0.3),Curve (0.7,0) (0.8,0.3) (0.95, 0.3)], (0, -0.5), (1, 0.5))
    'b' -> Ok ([Move (0.1,-0.8), Line (0.1,0.4), Curve (0.1,0) (0.3,-0.3) (0.6,-0.3), Curve (1,-0.3) (0.9,0.7) (0.1,0.2)], (0, -1), (1, 0.5))
    'c' -> Ok ([Move (0.7,-0.2), Curve (0.5,-0.4) (0.1,-0.3) (0.1,0), Curve (0.1,0.3) (0.6,0.5) (0.8,0.2)], (0, -0.5), (0.9, 0.5))
    'd' -> Ok ([Move (0.7,-0.1), Curve (0.4,-0.5) (0,0) (0.2,0.2),Curve (0.4,0.4) (0.7,0.2) (0.7,-0.8),Curve (0.7,0) (0.8,0.3) (0.95, 0.3)], (0, -1), (1, 0.5))
    'e' -> Ok ([Move (0.1,0), Curve (0.5,0.1) (0.8,-0.1) (0.7,-0.2), Curve (0.5,-0.4) (0.1,-0.3) (0.1,0), Curve (0.1,0.3) (0.6,0.5) (0.8,0.2)], (0, -0.5), (0.9, 0.5))
    'f' -> Ok ([Move (0.8,-0.6), Curve (0.5,-1) (0.4,-0.9) (0.4,-0.5), Line (0.4,0.4), Move (0.1,-0.2), Line (0.8,-0.2)], (0, -0.5), (0.9, 0.5))
    'g' -> Ok ([Move (0.7,-0.1), Curve (0.5,-0.5) (0, -0.3) (0.1,0), Curve (0.2,0.3) (0.6,0.4) (0.7,-0.3), Line (0.7,0.5), Curve (0.7,0.9) (0.2,1) (0.1,0.6)], (0, -0.5), (0.8, 1))
    'h' -> Ok ([Move (0.1,-0.9), Curve (0.2,-0.6) (0.2,-0.4) (0.2,0.4), Curve (0.5,-0.5) (0.8,-0.6) (0.8,0.4)], (0, -1), (0.9, 0.5))
    'i' -> Ok ([Move (0.2,-0.5), Curve (0,0.4) (0.2,0.5) (0.4,0.3), Move (0.2,-0.75), Line (0.3,-0.6)], (0, -0.8), (0.5, 0.5))
    'j' -> Ok ([Move (0.3,-0.4), Curve (0.5,0.9) (0.4,0.9) (0.1,0.7), Move (0.2,-0.75), Line(0.3,-0.6)], (0, -0.8), (0.5, 1))
    'k' -> Ok ([Move (0.1,-0.9), Line (0.1,0.4), Move (0.6,-0.4), Line (0.1,-0.1), Line (0.7,0.4)], (0, -0.5), (0.8, 0.5))
    'l' -> Ok ([Move (0.1,-0.9), Curve (0,0.4) (0.2,0.5) (0.4,0.3)], (0, -1), (0.5, 0.5))
    'm' -> Ok ([Move (0.1,-0.4), Curve (0.2,-0.3) (0.2,-0.1) (0.2,0.4), Curve (0.5,-0.5) (0.8,-0.6) (0.8,0.4), Curve (1.1,-0.5) (1.4,-0.6) (1.4,0.4)], (0, -0.5), (1.5, 0.5))
    'n' -> Ok ([Move (0.1,-0.4), Curve (0.2,-0.3) (0.2,-0.1) (0.2,0.4), Curve (0.5,-0.5) (0.8,-0.6) (0.8,0.4)], (0, -0.5), (0.9, 0.5))
    'o' -> Ok ([Move (0.5,-0.4), Curve (0.3,-0.4) (0.1,-0.2) (0.1,0),Curve (0.1,0.2) (0.3,0.4) (0.5,0.4), Curve (0.7,0.4) (0.9,0.2) (0.9,0), Curve (0.9,-0.2) (0.7,-0.4) (0.5,-0.4)], (0, -0.5), (1, 0.5))
    'p' -> Ok ([Move (0,-0.4), Curve (0.1,-0.2) (0.1,0) (0.1,0.9), Move (0.1,-0.1), Curve (0.3,-0.5) (0.5,-0.4) (0.7,-0.2), Curve (0.9,0) (0.7,0.5) (0.1,0.1)], (0, -0.5), (1, 1))
    'q' -> Ok ([Move (0.7,-0.4), Curve (0.6,-0.2) (0.6,0.1) (0.6,0.85), Line (0.9,0.7), Move (0.65,-0.2), Curve (0.4,-0.5) (0.1,-0.2) (0.1,0), Curve (0.1,0.2) (0.4,0.3) (0.6,0.2)], (0, -0.5), (1, 1))
    'r' -> Ok ([Move (0.1,-0.4), Curve (0.2,-0.3) (0.2,-0.1) (0.2,0.4), Curve (0.2,-0.3) (0.4,-0.5) (0.7,-0.2)], (0, -0.5), (0.8, 0.5))
    's' -> Ok ([Move (0.6,-0.3), Curve (0.3,-0.5) (0,-0.2) (0.5,-0.05), Curve (1,0.1) (0.5,0.7) (0.1,0.2)], (0, -0.5), (1, 0.5))
    't' -> Ok ([Move (0.5,-0.8), Curve (0.2,0.2) (0.2,0.5) (0.7,0.2), Move (0.1,-0.3), Line (0.7,-0.4)], (0, -0.9), (0.8, 0.5))
    'u' -> Ok ([Move (0.2,-0.4), Curve (0,1) (0.7,0.2) (0.8,-0.4), Curve (0.8,0.1) (0.8,0.3) (0.9,0.4)], (0, -0.5), (1, 0.5))
    'v' -> Ok ([Move (0.1,-0.4), Curve (0.3,-0.2) (0.4,0.2) (0.5,0.35), Curve (0.6,0.2) (0.8,-0.2) (0.9,-0.4)], (0, -0.5), (1, 0.5))
    'w' -> Ok ([Move (0.2,-0.3), Curve (0,0.5) (0.5,0.4) (0.6,0.1), Curve (0.7,0.5) (1.2,0.5) (1.1,-0.4)], (0, -0.5), (1.2, 0.5))
    'x' -> Ok ([Move (0.1,-0.2), Curve (0.3,-0.7) (0.9,-0.1) (0.1,0.4), Move (0.9,0.2), Curve (0.7,0.7) (0.1,0.1) (0.9,-0.4)], (0, -0.5), (1, 0.5))
    'y' -> Ok ([Move (0.1,-0.4), Curve (0.1,0.3) (0.6,0.3) (0.7,-0.4),Line (0.7,0.5), Curve (0.7,0.9) (0.2,1) (0.1,0.6)], (0, -0.5), (0.8, 1))
    'z' -> Ok ([Move (0.1,-0.3), Curve (0.9,-0.5) (0.7,-0.2) (0.5,0), Curve (0.3,0.2) (-0.3,0.5) (0.9,0.3), Move (0.1,0), Line (0.9,-0.1)], (0, -0.5), (1, 0.5))
    'A' -> Ok ([Move (0.1, 0.4), Line (0.5,-0.75), Line (0.9,0.4), Move (0.2,0), Line (0.8,0)], (0, -1), (1, 0.5))
    'B' -> Ok ([Move (0.2,-0.8), Line (0.2,0.3), Move (0.1,-0.8), Curve (1,-1) (0.6,-0.5) (0.2,-0.4), Curve (1,-0.6) (1.1,0.6) (0.1,0.3)], (0, -1), (0.9, 0.5))
    'C' -> Ok ([Move (0.8,-0.8), Curve (0.3,-1) (0.1,-0.5) (0.1,-0.25), Curve (0.1,0) (0.3,0.5) (0.9,0.2)], (0, -1), (1, 0.5))
    'D' -> Ok ([Move (0.2,-0.8), Line (0.2,0.3), Move (0.1,-0.7), Curve (0.3,-1) (0.6,-0.7) (0.8,-0.3), Curve (0.9,-0.1) (1,0.5) (0.1,0.3)], (0, -1), (1, 0.5))
    'E' -> Ok ([Move (0.2,-0.7), Curve (0,0.3) (0,0.5) (0.9,0.3), Move (0.1,-0.2), Line (0.7,-0.3), Move (0.1,-0.7), Line (0.8,-0.8)], (0, -1), (1, 0.5))
    'F' -> Ok ([Move (0.1,0.4), Line (0.1,-0.8), Line (0.9,-0.8), Move (0.1,-0.3), Line (0.8,-0.3)], (0, -1), (1, 0.5))
    'G' -> Ok ([Move (0.7,-0.8), Curve (0.2,-1) (0.1,-0.5) (0.1,-0.25), Curve (0.1,0) (0.2,0.5) (0.8,0), Move (0.5,-0.2), Line (1.1,-0.2), Move (0.8,-0.2), Line (0.8,0.4)], (0, -1), (1.2, 0.5))
    'H' -> Ok ([Move (0.1,-0.9), Line (0.1,0.4), Move (0.1,-0.3), Line (0.9,-0.3), Move (0.9,-0.9), Line (0.9,0.4)], (0, -1), (1, 0.5))
    'I' -> Ok ([Move (0.1,-0.8), Line (0.9,-0.8), Move (0.5,-0.8), Line (0.5,0.3), Move (0.1,0.3), Line (0.9,0.3)], (0, -1), (1, 0.5))
    'J' -> Ok ([Move (0.3,-0.8), Line (1.1,-0.8), Move (0.7,-0.8), Curve (1,0.5) (0.6,0.4) (0.1,0.2)], (0, -1), (1.2, 0.5))
    'K' -> Ok ([Move (0.2,-0.8), Line (0.2,0.4), Move (0.8,-0.7), Line (0.2,-0.1), Line (0.9,0.4)], (0, -1), (1, 0.5))
    'L' -> Ok ([Move (0.1,-0.8), Line (0.1,0.3), Line (0.8,0.3)], (0, -1), (0.9, 0.5))
    'M' -> Ok ([Move (0.1,0.4), Line (0.1,-0.6), Line (0.5,-0.2), Line (0.9,-0.7), Line (0.9,0.4)], (0, -1), (1, 0.5))
    'N' -> Ok ([Move (0.1,0.3), Line (0.1,-0.7), Line (0.8,0.3), Curve (0.9,0.3) (0.9,0) (0.8,-0.9)], (0, -1), (1, 0.5))
    'O' -> Ok ([Move (0.6,-0.8), Curve (0.2,-0.8) (0.1,-0.5) (0.1,-0.2), Curve (0.1,0.1) (0.2,0.3) (0.6,0.3), Curve (1,0.3) (1.1, 0.1) (1.1,-0.2), Curve (1.1,-0.5) (1,-0.8) (0.6,-0.8)], (0, -1), (1.2, 0.5))
    'P' -> Ok ([Move (0.1,0.4), Line (0.1,-0.8), Curve (1,-1) (1,-0.1) (0.1,-0.2)], (0, -1), (0.9, 0.5))
    'Q' -> Ok ([Move (0.6,-0.8), Curve (0.2,-0.8) (0.1,-0.5) (0.1,-0.2), Curve (0.1,0.1) (0.2,0.3) (0.6,0.3), Curve (1,0.3) (1.1, 0.1) (1.1,-0.2), Curve (1.1,-0.5) (1,-0.8) (0.6,-0.8), Move (0.7,0), Line (1.1,0.4)], (0, -1), (1.2, 0.5))
    'R' -> Ok ([Move (0.1,0.4), Line (0.1,-0.8), Curve (1,-1) (1,-0.1) (0.1,-0.2), Curve (0.2,-0.2) (0.5,0) (0.8,0.4)], (0, -1), (0.9, 0.5))
    'S' -> Ok ([Move (0.8,-0.8), Curve (0.2,-1.1) (0,-0.4) (0.4,-0.4), Curve (1,-0.4) (1,0.3) (0.5,0.3), Curve (0.4,0.3) (0.2,0.3) (0.1,0.2)], (0, -1), (1, 0.5))
    'T' -> Ok ([Move (0.1,-0.8), Line (0.9,-0.8), Move (0.5,-0.8), Line (0.5,0.4)], (0, -1), (1, 0.5))
    'U' -> Ok ([Move (0.1,-0.8), Curve (0,0.7) (0.9,0.7) (0.8,-0.8), Curve (0.8,0.1) (0.8,0.3) (0.9,0.4)], (0, -1), (1, 0.5))
    'V' -> Ok ([Move (0.1,-0.8), Curve (0.4,0.3) (0.5,0.3) (0.5,0.4), Curve (0.5,0.3) (0.6,0.3) (0.9,-0.8)], (0, -1), (1, 0.5))
    'W' -> Ok ([Move (0.1,-0.8), Curve (0.3,0.3) (0.4,0.3) (0.4,0.4), Curve (0.4,0.3) (0.7,-0.2) (0.7,-0.3), Curve (0.7,-0.2) (1,0.3) (1,0.4), Curve (1,0.3) (1.1,0.3) (1.3,-0.8)], (0, -1), (1.4, 0.5))
    'X' -> Ok ([Move (0.1,-0.8), Line (0.9,0.4), Move (0.1,0.4), Line (0.9,-0.8)], (0, -1), (1, 0.5))
    'Y' -> Ok ([Move (0.1,-0.8), Line (0.5,-0.2), Line (0.9,-0.8), Move (0.5,-0.2), Line (0.5,0.4)], (0, -1), (1, 0.5))
    'Z' -> Ok ([Move (0.1,-0.8), Line (0.8,-0.8), Line (0.2,0.3), Line (0.9,0.3), Move (0.1,-0.3), Line (0.9,-0.3)], (0, -1), (1, 0.5))
    ',' -> Ok ([Move (0.1,0.3), Line (0.2,0.4), Curve (0.2,0.6) (0.15,0.65) (0.1,0.7)], (0, 0.3), (0.25, 0.7))
    '.' -> Ok ([Move (0.1,0.3), Line (0.2,0.4)], (0, 0.3), (0.25, 0.4))
    '0' -> Ok ([Move (0.4,-0.8), Curve (0,-0.8) (0,0.3) (0.4,0.3), Curve (0.8,0.3) (0.8,-0.8) (0.4,-0.8)], (0, -1), (0.8, 0.5))
    '1' -> Ok ([Move (0.25,-0.8), Curve (0.3, -0.3) (0.3, 0) (0.3, 0.4)], (0, -1), (0.5, 0.5))
    '2' -> Ok ([Move (0.1,-0.5), Curve (0.5,-1) (0.9,-0.7) (0.4,0), Curve (0,0.6) (0,0.2) (0.3,0.2), Curve (0.4, 0.2) (0.6,0.4) (0.9, 0.3)], (0, -1), (1, 1))
    '3' -> Ok ([Move (0.1,-0.8), Curve (1,-1) (0.6,-0.5) (0.2,-0.4), Curve (1,-0.6) (1.1,0.6) (0.1,0.3)], (0, -1), (0.9, 0.5))
    '4' -> Ok ([Move (0.6,-0.8), Line (0.2,0), Line (0.9,0), Move (0.7,-0.6), Line (0.7,0.4)], (0, -1), (1, 0.5))
    '5' -> Ok ([Move (0.8,-0.8), Line (0.1,-0.8), Line (0.1,-0.3), Curve (0.3,-0.4) (0.8,-0.4) (0.8,0), Curve (0.8,0.45) (0.1,0.45) (0.1,0.1)], (0, -1), (1, 0.5))
    '6' -> Ok ([Move (0.7,-0.8), Curve (-0.2,-0.7) (0,0.4) (0.5,0.4), Curve (1,0.4) (1,-0.2) (0.5,-0.2), Curve (0.3,-0.2) (0.2,-0.1) (0.1,0.1)], (0, -1), (1, 0.5))
    '7' -> Ok ([Move (0.1,-0.8), Line (0.8,-0.8), Line (0.5,0.4), Move (0.2,-0.3), Line (0.9,-0.3)], (0, -1), (1, 0.5))
    '8' -> Ok ([Move (0.5,-0.8), Curve (0.1,-0.8) (0.1,-0.3) (0.5,-0.3), Curve (1,-0.3) (1,0.4) (0.5,0.4), Curve (0,0.4) (0,-0.3) (0.5,-0.3), Curve (0.9,-0.3) (0.9,-0.8) (0.5,-0.8)], (0, -1), (1, 0.5))
    '9' -> Ok ([Move (0.8,-0.7), Curve (0.6,-1) (0.1,-0.7) (0.1,-0.4), Curve (0.1,-0.1) (0.7,0) (0.8,-0.8), Line (0.8,0.4)], (0, -1), (1, 0.5))
    '-' -> Ok ([Move (0.1,0), Line (0.5,0)], (0, -0.1), (0.6, 0.1))
    '+' -> Ok ([Move (0.3,-0.2), Line (0.3,0.2), Move (0.1,0), Line (0.5,0)], (0, -0.3), (0.6, 0.3))
    '=' -> Ok ([Move (0.1,-0.2), Line (0.5,-0.2), Move (0.1,0.1), Line (0.5,0.1)], (0, -0.3), (0.6, 0.2))
    _ -> Err ("invalid character found: " ++ (Char.toCode c |> String.fromInt))

rightShiftStrokes_: Float -> List Stroke -> List Stroke
rightShiftStrokes_ left = List.map (\elem -> case elem of
    Move (x, y) -> Move (x + left, y)
    Line (x, y) -> Line (x + left, y)
    Curve (x1, y1) (x2,y2) (x3,y3) -> Curve (x1+left,y1) (x2+left,y2) (x3+left,y3)
    )

{- UI -}

static: List (Html.Attribute msg) -> Latex.Model a -> Html.Html msg
static attrs l = latexToFrames_ l
    |> processFrame_ (\_ strokes origin scale list ->
        Svg.path [d (strokeToPath_ origin scale strokes), stroke "currentColor", strokeWidth "1", fill "none"] []
        :: list
        ) (0,0) 20 []
    |> \(children, topLeft, botRight) -> Svg.svg (toViewBox_ topLeft botRight :: attrs) children

view: (Int -> List (Svg.Attribute msg)) -> List (Html.Attribute msg) -> Model -> Html.Html msg
view convert attrs model = Dict.toList model.frames
    |> List.concatMap (\(_, f) -> Dict.toList f
        |> List.concatMap (\(id, frame) ->
            List.map (\s ->
                Svg.path (d (strokeToPath_ (Animation.current s.origin) (Animation.current s.scale) s.strokes) :: (opacity (Animation.current frame.opacity |> String.fromFloat) ::convert id)) []
            ) frame.data
        )
    )
    |> Svg.svg (toViewBox_ (Animation.current model.topLeft) (Animation.current model.botRight) :: attrs)

toViewBox_: Vector2 -> Vector2 -> Html.Attribute msg
toViewBox_ (left, top) (right, bot) =
    (   String.fromFloat left ++ " " ++
        String.fromFloat top ++ " " ++
        String.fromFloat (right - left) ++ " " ++
        String.fromFloat (bot - top)
    )
    |> viewBox

-- For inputting (i.e. with virtual keyboard that lists out all the available functions)
-- https://stackoverflow.com/a/37202118
-- input: Math.Tree elem -> Html.Html msg
-- input root = Html.p [] [Html.text "TODO"]

{- UI -}

strokeToPath_: Vector2 -> Float -> List Stroke -> String
strokeToPath_ origin scale =
    let
        shifted = Animation.scaleVector2 scale >> Animation.addVector2 origin
    in
    List.map (\stroke -> case stroke of
        Move v -> "M" ++ vectorToString_ (shifted v)
        Line v -> "L" ++ vectorToString_ (shifted v)
        Curve v1 v2 v3 -> "C" ++ vectorToString_ (shifted v1) ++
            " " ++ vectorToString_ (shifted v2) ++
            " " ++ vectorToString_ (shifted v3)
    )
    >> String.join ""

vectorToString_: Vector2 -> String
vectorToString_ (x,y) = String.fromFloat x ++ " " ++ String.fromFloat y