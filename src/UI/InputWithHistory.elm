module UI.InputWithHistory exposing (Model, Event, init, update, view, advance, open, close, decoder, encode)

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

type alias Model =
    {   options: List Selection
    ,   openCount: Int
    ,   input: Input.Model
    ,   current: Maybe (Int, Animation.EaseState Float, Animation.EaseState Float)
    ,   closing: Dict.Dict Int (Animation.EaseState Float, Animation.EaseState Float)
    }

type Selection =
    Default Input.Entry
    | Previous Input.Entry

type Event =
    Click Input.Entry
    | Submit
    | ShowOptions

defaultOptions_: List Selection
defaultOptions_ =
    [   Default {latex =[Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} "x+4=5"], funcName = Dict.empty}
    ,   Default
        {   latex =
            [   Latex.Text {immutable = False, scope = {function = 1, argument = Nothing, fixedArgs = False}} "f"
            ,   Latex.Bracket {immutable = False, scope = {function = 1, argument = Nothing, fixedArgs = False}} [Latex.Text {immutable = False, scope = {function = 1, argument = Just 1, fixedArgs = False}} "x"]
            ,   Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} "=x+3"
            ]
        ,   funcName = Dict.singleton 1 "f"
        }
    ,   Default
        {   latex =
            [   Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} "x"
            ,   Latex.Bracket {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} [Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} "x+2"]
            ,   Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} "=-1"
            ]
        ,   funcName = Dict.empty
        }
    ,   Default {latex = [Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} "2x+y=5"], funcName = Dict.empty}
    ,   Default {latex = [Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} "4x+3y=11"], funcName = Dict.empty}
    ]

init: Bool -> Model
init show =
    {   options = defaultOptions_
    ,   openCount = if show then 1 else 0
    ,   input = Input.init
    ,   current = if show
            then Just
                (   0
                ,   Animation.newEaseFloat animationDuration_ maxWidth_
                ,   Animation.newEaseFloat animationDuration_ 0
                )
            else Nothing
    ,   closing = Dict.empty
    }

advance: Float -> Model -> Model
advance time model =
    {   model
    |   current = model.current
        |> Maybe.map (\(num, w, h) -> (num, Animation.advance time w, Animation.advance time h))
    ,   closing = model.closing
        |> Dict.map (\_ (w,h) -> (Animation.advance time w, Animation.advance time h))
        |> Dict.filter (\_ (w,h) -> Animation.current h /= 0 || Animation.current w /= 0)
    }

open: Animation.Tracker -> Model -> (Model, Animation.Tracker)
open tracker model = case model.current of
    Just _ -> (model, tracker)
    Nothing -> let (newEase, newT) = Animation.newEaseFloat animationDuration_ 0 |> Animation.setEase tracker maxWidth_ in
        (   {  model
            |   openCount = model.openCount + 1
            ,   current = Just (model.openCount, newEase, Animation.newEaseFloat animationDuration_ 0)
            }
        ,   newT
        )

close: Animation.Tracker -> Model -> (Model, Animation.Tracker)
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
                ,   input = Input.init
                }
            , finalT
            )

update: Animation.Tracker -> Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp}-> Event -> Model -> (Model, Result String (Maybe Display.FullEquation), Animation.Tracker)
update tracker funcDict event model = case event of
    Click input -> ({model | input = Input.set input}, Ok Nothing, tracker)
    Submit -> case Input.toTree funcDict model.input of
        Err err -> (model, Err err, tracker)
        Ok tree -> let (newModel, newT) = close tracker model in
            (   {   newModel
                |   options = if List.isEmpty model.input.entry.latex
                        then newModel.options
                        else Previous model.input.entry :: newModel.options
                }
            ,   Ok (Just tree)
            ,   newT
            )
    ShowOptions -> case model.current of
        Nothing -> (model, Ok Nothing, tracker)
        Just (id, width, height) ->
            let (newH, newT) = Animation.setEase tracker maxHeight_ height in
                ({model | current = Just (id, width, newH)}, Ok Nothing, newT)

view: (Event -> msg) -> Model -> List (String, Html.Html msg)
view converter model =
    Dict.toList model.closing
    |> List.map (\(id, (width, height)) -> createView_ converter model id width height)
    |> \list -> case model.current of
        Nothing -> list
        Just (id, width, height) -> list ++ [createView_ converter model id width height]

createView_: (Event -> msg) -> Model -> Int -> Animation.EaseState Float -> Animation.EaseState Float -> (String, Html.Html msg)
createView_ converter model inputNum width height =
    (   "textbar"++String.fromInt inputNum
    ,    Html.div
        [   class "input"
        ,   Html.Attributes.style "max-width" ((Animation.current width |> String.fromFloat) ++"dvw")
        ]
        [   Html.form
            [   class "textbar"
            ,   HtmlEvent.onSubmitField "equation" (\_ -> Submit)
            ]
            [   Icon.equation []
            ,   Input.view
                [ name "equation"
                , id "textInput"
                , placeholder "Type an equation to solve"
                , HtmlEvent.onFocus ShowOptions
                ]
                model.input
            ]
        ,   Html.ul
            [Html.Attributes.style "max-height" ((Animation.current height |> String.fromFloat) ++"dvh")]
            (   List.map
                (\entry -> case entry of
                    Default val -> Html.li [HtmlEvent.onClick (Click val)]
                        [   Icon.default []
                        , Html.a [class "clickable"] [MathIcon.static [] val.latex]
                        ]
                    Previous val -> Html.li [HtmlEvent.onClick (Click val)]
                        [   Icon.history []
                        , Html.a [class "clickable"] [MathIcon.static [] val.latex]
                        ]
                )
                model.options
            )
        ]
        |> Html.map converter
    )

encode: Model -> Encode.Value
encode model = Encode.object
    [   ("open", Encode.int model.openCount)
    ,   ("current", case model.current of
            Nothing -> Encode.null
            Just (id, width, height) -> Encode.object
                [   ("id", Encode.int id)
                ,   ("width", Animation.target width |> Encode.float)
                ,   ("height", Animation.target height |> Encode.float)
                ]
        )
    ]

decoder: Decode.Decoder Model
decoder = Decode.map2 (\a b -> Model defaultOptions_ a Input.init b Dict.empty)
    (Decode.field "open" Decode.int)
    (Decode.maybe <| Decode.field "current" <| Decode.map3 (\a b c -> (a,b,c))
        (Decode.field "id" <| Decode.int)
        (Decode.field "width" <| Decode.map (Animation.newEaseFloat animationDuration_) Decode.float)
        (Decode.field "height" <| Decode.map (Animation.newEaseFloat animationDuration_) Decode.float)
    )