module UI.MathIcon exposing (Model, blank, init, set, advanceTime, view)

import Dict
import Html
import Html.Attributes exposing (class)
import Svg
import Svg.Attributes exposing (d, opacity, viewBox)
-- Ours
import Algo.Matcher as Matcher
import Components.Latex as Latex
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
    ,   opacity: Animation.EaseState Float
    }

blank: Model
blank =
    {   frames = Dict.empty
    ,   topLeft = Animation.EaseState (0,0) (0,0) (0,0)
    ,   botRight = Animation.EaseState (0,0) (0,0) (0,0)
    ,   opacity = Animation.EaseState 0 0 0
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
                    |   origin = Animation.smoothDampVector2 300 time data.origin
                    ,   scale = Animation.smoothDampFloat 300 time data.scale
                    }
                ) animation.data
            ,   opacity = Animation.smoothDampFloat 300 time animation.opacity
            }
            )
        )
        model.frames
    }

set: Latex.Model State -> Model -> Model
set root model = model

{- toFrames -}

type alias Ref =
    {   body: Float -- how left (most cases)
    ,   top: Float -- superscript
    ,   bot: Float -- subscript
    }

latexToFrames_: Latex.Model state -> Frame state
latexToFrames_ = List.foldl
    (\elem ((list, topLeft, botRight), ref) -> symbolsToFrames_ ref elem
        |> \(new, newRef) ->
            (   (   {frame = new, origin = (0,0), scale = 1} :: list
                ,   Animation.minVector2 topLeft new.topLeft
                ,   Animation.maxVector2 botRight new.botRight
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
            topOrigin = -(Tuple.second topFrame.botRight)*0.75 - 0.25
            botOrigin = -(Tuple.second botFrame.topLeft)*0.75 + 0.25
            up = topOrigin - (Tuple.second topFrame.topLeft)*0.75
            bot = botOrigin + (Tuple.second botFrame.botRight)*0.75
        in
            (   {   data = Position
                    [   {   frame = {data = BaseFrame {strokes = [Move (0, 0.25), Line (width, 0.25)], elem = s }, topLeft = (0,0), botRight = (0,width)}
                        , origin = (0,0), scale = 1 }
                    ,   {   frame = topFrame
                        ,   origin = (0.125 + (maxWidth - (Tuple.first topFrame.botRight))*0.75, topOrigin)
                        ,   scale = 0.75
                        }
                    ,   {   frame = botFrame
                        ,   origin = (0.125 + (maxWidth - (Tuple.first botFrame.botRight))*0.75, botOrigin)
                        ,   scale = 0.75
                        }
                    ]
                ,   topLeft = (ref.body, up)
                ,   botRight = (ref.body + width, bot)
                }
            ,   {body = ref.body + width, top = up, bot = bot}
            )
    Latex.Superscript _ inner -> latexToFrames_ inner
        |> \new ->
            (   {   data = Position [{frame = new, origin = (0,ref.top), scale = 0.5}]
                ,   topLeft = Animation.scaleVector2 0.5 new.topLeft |> Animation.addVector2 (ref.body, ref.top)
                ,   botRight = Animation.scaleVector2 0.5 new.botRight |> Animation.addVector2 (ref.body, ref.top)
                }
            ,   ref
            )
    Latex.Subscript _ inner -> latexToFrames_ inner
        |> \new ->
            (   {   data = Position [{frame = new, origin = (0,ref.top), scale = 0.5}]
                ,   topLeft = Animation.scaleVector2 0.5 new.topLeft |> Animation.addVector2 (ref.body, ref.bot)
                ,   botRight = Animation.scaleVector2 0.5 new.botRight |> Animation.addVector2 (ref.body, ref.bot)
                }
            ,   ref
            )
    Latex.Text s str -> wordStrokes_ s str
        |> \new ->
            (   {   data = Position [{frame = new, origin = (0,0), scale = 0.5}]
                ,   topLeft = Animation.addVector2 (ref.body,0) new.topLeft
                ,   botRight = Animation.addVector2 (ref.body,0) new.botRight
                }
            ,   {body = ref.body + Tuple.first new.botRight, top = new.topLeft |> Tuple.second, bot = new.botRight |> Tuple.second}
            )
    Latex.SymbolPart s str -> symbolStrokes_ s str
        |> \new ->
            (   {   data = Position [{frame = new, origin = (0,0), scale = 0.5}]
                ,   topLeft = Animation.addVector2 (ref.body,0) new.topLeft
                ,   botRight = Animation.addVector2 (ref.body,0) new.botRight
                }
            ,   {body = ref.body + Tuple.first new.botRight, top = new.topLeft |> Tuple.second, bot = new.botRight |> Tuple.second}
            )
    Latex.Bracket s inner -> latexToFrames_ inner
        |> \new -> let (top, bot) = (Tuple.second new.topLeft, Tuple.second new.botRight) in
            (   {   data = Position
                    [   {frame = new, origin = (0.25, 0), scale = 1}
                    ,   {frame = {data = BaseFrame {strokes = [], elem = s}, topLeft = (0,top), botRight = (0.25,bot)}, origin = (0,0), scale = 1}
                    ,   {frame = {data = BaseFrame {strokes = [], elem = s}, topLeft = (0,top), botRight = (0.25,bot)}, origin = (Tuple.first new.botRight + 0.25, 0), scale = 1}
                    ]
                ,   topLeft = Animation.addVector2 (ref.body, 0) new.topLeft
                ,   botRight = Animation.addVector2 (ref.body + 0.5, 0) new.botRight
                }
            ,   {   ref | body = ref.body + Tuple.first new.botRight + 0.5}
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

{- toStrokes

values are all relative to height, where height is is treated as the unit (for width as well)
1.0 of height is equivalent to the height of short characters, i.e. (a,n,m,o)
origin should be on the left + half-way through the height of the short characters.
-}

type FrameData state =
    BaseFrame {strokes: List Stroke, elem: state}
    | Position (List {frame: Frame state, origin: Vector2, scale: Float})
type alias Frame state =
    {   data: FrameData state
    ,   topLeft: Vector2
    ,   botRight: Vector2
    }

failedFrame_: state -> Frame state
failedFrame_ s =
    {   data = BaseFrame {strokes = [Move (0, -0.5), Line (0,0.5), Line (1,0.5), Line (1,-0.5), Line (0, -0.5), Line (1,0.5)], elem = s}
    ,   topLeft  = (0, -0.5)
    ,   botRight = (1,0.5)
    }

symbolStrokes_: state -> Latex.Symbol -> Frame state
symbolStrokes_ s str = case str of
    Latex.AlphaLower -> {data = BaseFrame {strokes = [], elem = s}, topLeft = (0, -1), botRight = (1.5, 0.5)}
    Latex.Integration -> {data = BaseFrame {strokes = [], elem = s}, topLeft = (0, -1.5), botRight = (0.5, 1.5)}
    _ -> failedFrame_ s

wordStrokes_: state -> String -> Frame state
wordStrokes_ s = String.foldl (\c res -> case res of
        Err err -> Err err
        Ok (list, (_, top), (right, bot)) -> charStrokes_ c
            |> Result.map (\(strokes, (_, newTop), (newRight, newBot)) ->
                (rightShiftStrokes_ right list ++ strokes, (0, min top newTop), (max right newRight, max bot newBot))
            )
    ) (Ok ([], (0, 0), (0, 0)))
    >> \r -> case r of
        Ok (list, topLeft, botRight) -> {data = BaseFrame {strokes = list, elem = s}, topLeft = topLeft, botRight = botRight}
        Err _ -> failedFrame_ s

charStrokes_: Char -> Result String (List Stroke, Vector2, Vector2)
charStrokes_ c = case c of
    'a' -> Ok ([], (0, -0.5), (1, 0.5))
    ',' -> Ok ([], (0, 0.75), (0.25, 1.25))
    '.' -> Ok ([], (0, 0.75), (0.25, 1.25))
    '0' -> Ok ([], (0, -1), (0.5, 0.5))
    _ -> Err ("invalid character found: " ++ (Char.toCode c |> String.fromInt))

rightShiftStrokes_: Float -> List Stroke -> List Stroke
rightShiftStrokes_ left = List.map (\elem -> case elem of
    Move (x, y) -> Move (x + left, y)
    Line (x, y) -> Line (x + left, y)
    Curve (x1, y1) (x2,y2) (x3,y3) -> Curve (x1+left,y1) (x2+left,y2) (x3+left,y3)
    )


{- UI -}

view: (Int -> List (Svg.Attribute msg)) -> List (Html.Attribute msg) -> Model -> Html.Html msg
view convert attrs model =
    let
        (left, top) = model.topLeft.current
        (right, bot) = model.botRight.current
        vbString = String.fromFloat left ++ " " ++
            String.fromFloat top ++ " " ++
            String.fromFloat (right - left) ++ " " ++
            String.fromFloat (bot - top)
    in
    Dict.toList model.frames
    |> List.concatMap (\(_, f) -> Dict.toList f
        |> List.concatMap (\(id, frame) ->
            List.map (\s ->
                Svg.path (d (strokeToPath_ s.origin.current s.scale.current s.strokes) :: (opacity (String.fromFloat frame.opacity.current) ::convert id)) []
            ) frame.data
        )
    )
    |> Svg.svg (viewBox vbString :: attrs)

-- For inputting (i.e. with virtual keyboard that lists out all the available functions)
-- https://stackoverflow.com/a/37202118
-- input: Math.Tree elem -> Html.Html msg
-- input root = Html.p [] [Html.text "TODO"]

{- UI -}

strokeToPath_: Vector2 -> Float -> List Stroke -> String
strokeToPath_ origin scale = List.map (\stroke -> case stroke of
        Move v -> "M" ++ vectorToString_ v
        Line v -> "L" ++ vectorToString_ v
        Curve v1 v2 v3 -> "C" ++ vectorToString_ v1 ++ " " ++ vectorToString_ v2 ++ " " ++ vectorToString_ v3
    )
    >> String.join ""

vectorToString_: Vector2 -> String
vectorToString_ (x,y) = String.fromFloat x ++ " " ++ String.fromFloat y