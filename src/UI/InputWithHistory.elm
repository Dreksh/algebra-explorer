module UI.InputWithHistory exposing (Model, Event(..), init, update, view, advance, open, close)

import Array
import Dict
import Html
import Html.Attributes exposing (class, id, name, placeholder, type_, value)
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import Helper
import Algo.Math as Math
import Components.Latex as Latex
import Components.Rules as Rules
import UI.Animation as Animation
import UI.Display as Display
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.Input as Input
import UI.MathIcon as MathIcon

-- Will be placed as a percentage
maxWidth_: Float
maxWidth_ = 70

maxHeight_: Float
maxHeight_ = 50

animationDuration_: Float
animationDuration_ = 750

type alias Model msg =
    {   options: List Selection
    ,   openCount: Int
    ,   input: Input.Model msg
    ,   current: Maybe (Int, Animation.EaseState Float, Animation.EaseState Float)
    ,   closing: Dict.Dict Int (Animation.EaseState Float, Animation.EaseState Float)
    ,   focusCmd: String -> Cmd msg
    }

type Selection =
    Default Input.Scope
    | Previous Input.Scope

type Event =
    Click Input.Scope
    | Submit
    | InputEvent Input.Event

defaultOptions_: List Selection
defaultOptions_ =
    [   Default (Input.Scope {fixed = True} [Input.StrElement "x+4=5"])
    ,   Default
        (   Input.Scope {fixed = True}
            [   Input.Fixed
                {   text = "\\f"
                ,   latex =
                    [   Latex.Text () "f"
                    ,   Latex.Bracket () [Latex.Argument () 1]
                    ]
                ,   params = Array.fromList
                    [(Input.Scope {fixed = False} [Input.StrElement "x"]
                    ,{up = Nothing, down = Nothing, left = Nothing, right = Nothing}
                    )]
                ,   firstNode = Just 1
                ,   lastNode = Just 1
                }
            ,   Input.StrElement "=x+3"
            ]
        )
    ,   Default (Input.Scope {fixed = True} [Input.StrElement "x(x+2)=-1" ])
    ,   Default (Input.Scope {fixed = True} [Input.StrElement "2x+y=5"])
    ,   Default (Input.Scope {fixed = True} [Input.StrElement "4x+3y=11"])
    ]

init: Bool -> (Encode.Value -> (Float, Float) -> Cmd msg) -> (String -> Cmd msg) -> Model msg
init show mouseCmd focusCmd =
    {   options = defaultOptions_
    ,   openCount = if show then 1 else 0
    ,   input = Input.init mouseCmd focusCmd  "mainInput"
    ,   current = if show
            then Just
                (   0
                ,   Animation.newEaseFloat animationDuration_ maxWidth_
                ,   Animation.newEaseFloat animationDuration_ 0
                )
            else Nothing
    ,   closing = Dict.empty
    ,   focusCmd = focusCmd
    }

advance: Float -> Model msg -> Model msg
advance time model =
    {   model
    |   current = model.current
        |> Maybe.map (\(num, w, h) -> (num, Animation.advance time w, Animation.advance time h))
    ,   closing = model.closing
        |> Dict.map (\_ (w,h) -> (Animation.advance time w, Animation.advance time h))
        |> Dict.filter (\_ (w,h) -> Animation.current h /= 0 || Animation.current w /= 0)
    }

open: Animation.Tracker -> Model msg -> (Model msg, Animation.Tracker)
open tracker model = case model.current of
    Just _ -> (model, tracker)
    Nothing -> let (newEase, newT) = Animation.newEaseFloat animationDuration_ 0 |> Animation.setEase tracker maxWidth_ in
        (   {  model
            |   openCount = model.openCount + 1
            ,   current = Just (model.openCount, newEase, Animation.newEaseFloat animationDuration_ 0)
            }
        ,   newT
        )

close: Animation.Tracker -> Model msg -> (Model msg, Animation.Tracker)
close tracker model = case model.current of
    Nothing -> (model, tracker)
    Just (id, width, height) ->
        let
            (newW, newT) = Animation.setEase tracker 0 width
            (newH, finalT) = Animation.setEase newT 0 height
        in
            (   {   model
                |   current = Nothing
                ,   closing = Dict.insert id (newW, newH) model.closing
                ,   input = Input.clear model.input
                }
            , finalT
            )

update: (Event -> msg) -> Animation.Tracker -> Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp}-> Event -> Model msg -> ((Model msg, Animation.Tracker), Result String (Maybe Display.FullEquation), Cmd msg)
update convert tracker funcDict event model = case event of
    Click input -> (({model | input = Input.set input model.input}, tracker), Ok Nothing, model.focusCmd "mainInput-input")
    Submit -> let (Input.Scope _ children) = model.input.entry in
        if List.isEmpty children then (close tracker model, Ok Nothing, Cmd.none)
        else case Input.toTree funcDict model.input of
            Err err -> ((model, tracker), Err err, Cmd.none)
            Ok tree -> let (newModel, newT) = close tracker model in
                (   (   {   newModel
                        |   options = if List.isEmpty children
                                then newModel.options
                                else Previous model.input.entry :: newModel.options
                        }
                    ,   newT
                    )
                ,   Ok (Just tree)
                ,   Cmd.none
                )
    InputEvent e -> let (inModel, errStr, inCmd) = Input.update e model.input in
        if String.isEmpty errStr |> not
        then ((model, tracker), Err errStr, Cmd.none)
        else case (e, model.current) of
            (Input.ShowCursor, Just (id, width, height)) ->
                let (newH, newT) = Animation.setEase tracker maxHeight_ height in
                (({model | current = Just (id, width, newH), input = inModel}, newT), Ok Nothing, inCmd)
            _ -> (({model | input = inModel}, tracker), Ok Nothing, inCmd)

view: (Event -> msg) -> Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Model msg -> List (String, Html.Html msg)
view converter funcDict model =
    Dict.toList model.closing
    |> List.map (\(id, (width, height)) -> createView_ converter funcDict model id width height)
    |> \list -> case model.current of
        Nothing -> list
        Just (id, width, height) -> list ++ [createView_ converter funcDict model id width height]

createView_: (Event -> msg) -> Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Model msg -> Int -> Animation.EaseState Float -> Animation.EaseState Float -> (String, Html.Html msg)
createView_ converter funcDict model inputNum width height =
    (   "textbar"++String.fromInt inputNum
    ,    Html.div
        [   class "input"
        ,   Html.Attributes.style "max-width" ((Animation.current width |> String.fromFloat) ++"dvw")
        ]
        [   Html.form
            [   class "textbar"
            ,   HtmlEvent.onSubmit (converter Submit)
            ]
            [   Icon.equation []
            ,   Input.view (InputEvent >> converter) funcDict [] model.input
            ]
        ,   Html.ul
            [Html.Attributes.style "max-height" ((Animation.current height |> String.fromFloat) ++"dvh")]
            (   List.map
                (\entry -> case entry of
                    Default val -> Html.li [HtmlEvent.onClick (Click val)]
                        [   Icon.default []
                        , Html.a [class "clickable"] [Input.toLatex False [] val |> MathIcon.static []]
                        ]
                    Previous val -> Html.li [HtmlEvent.onClick (Click val)]
                        [   Icon.history []
                        , Html.a [class "clickable"] [Input.toLatex False [] val |> MathIcon.static []]
                        ]
                )
                model.options
            )
            |> Html.map converter
        ]
    )
