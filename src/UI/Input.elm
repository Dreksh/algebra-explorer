module UI.Input exposing (Model, Event, init, update, view, advance, open, close, decoder, encode)

import Dict
import Html
import Html.Attributes exposing (class, id, name, placeholder, type_, value)
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import Helper
import UI.Animation as Animation
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon

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
    ,   selected: Maybe (String, Bool) -- The bool is to toggle to signal when state is changing to control overwriting the value
    ,   current: Maybe (Int, Animation.EaseState Float, Animation.EaseState Float)
    ,   closing: Dict.Dict Int (Animation.EaseState Float, Animation.EaseState Float)
    }

type Selection =
    Default String
    | Previous String

type Event =
    Click String
    | Submit String
    | ShowOptions

defaultOptions_: List Selection
defaultOptions_ =
    [   Default "x+4=5"
    ,   Default "\\f(x)=x+3"
    ,   Default "x(x+2)=-1"
    ,   Default "4x+3y=11"
    ,   Default "2x+y=5"
    ]

init: Bool -> Model
init show =
    {   options = defaultOptions_
    ,   openCount = if show then 1 else 0
    ,   selected = Nothing
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
                ,   selected = Nothing
                }
            , finalT
            )

update: Animation.Tracker -> Event -> Model -> (Model, Maybe String, Animation.Tracker)
update tracker event model = case event of
    Click input -> case model.selected of
        Nothing -> ({model | selected = Just (input, False)}, Nothing, tracker)
        Just (_, token) -> ({model | selected = Just (input, not token)}, Nothing, tracker)
    Submit input -> let (newModel, newT) = close tracker model in
        (   {   newModel
            |   options = if String.isEmpty input
                    then newModel.options
                    else Previous input :: newModel.options
            }
        ,   Just input
        ,   newT
        )
    ShowOptions -> case model.current of
        Nothing -> (model, Nothing, tracker)
        Just (id, width, height) ->
            let (newH, newT) = Animation.setEase tracker maxHeight_ height in
                ({model | current = Just (id, width, newH)}, Nothing, newT)

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
            ,   HtmlEvent.onSubmitField "equation" Submit
            ]
            [   Icon.equation []
            ,   Html.input
                (   [ type_ "text"
                    , name "equation"
                    , id "textInput"
                    , placeholder "Type an equation to solve"
                    , HtmlEvent.onFocus ShowOptions
                    ]
                    |> Helper.maybeAppend (Maybe.map (\(text, _) -> value text) model.selected)
                )
                []
            ]
        ,   Html.ul
            [Html.Attributes.style "max-height" ((Animation.current height |> String.fromFloat) ++"dvh")]
            (   List.map
                (\entry -> case entry of
                    Default val -> Html.li [HtmlEvent.onClick (Click val)]
                        [   Icon.default []
                        , Html.a [class "clickable"] [Html.text val]
                        ]
                    Previous val -> Html.li [HtmlEvent.onClick (Click val)]
                        [   Icon.history []
                        , Html.a [class "clickable"] [Html.text val]
                        ]
                )
                model.options
            )
        ]
        |> Html.map converter
    )

encode: Model -> Encode.Value
encode model = Encode.object
    [   ("options", Encode.list Encode.string <| List.filterMap (\e -> case e of
            Default _ -> Nothing
            Previous value -> Just value
            ) model.options
        )
    ,   ("open", Encode.int model.openCount)
    ,   ("selected", case model.selected of
            Nothing -> Encode.null
            Just (val, token) -> Encode.object [("value", Encode.string val), ("token", Encode.bool token)]
        )
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
decoder = Decode.map4 (\a b c d -> Model a b c d Dict.empty)
    (Decode.field "options" <| Decode.map (\prev -> List.map Previous prev ++ defaultOptions_) <| Decode.list <| Decode.string)
    (Decode.field "open" Decode.int)
    (Decode.maybe <| Decode.field "selected" <|
        Decode.map2 Tuple.pair (Decode.field "value" Decode.string) (Decode.field "token" Decode.bool)
    )
    (Decode.maybe <| Decode.field "current" <| Decode.map3 (\a b c -> (a,b,c))
        (Decode.field "id" <| Decode.int)
        (Decode.field "width" <| Decode.map (Animation.newEaseFloat animationDuration_) Decode.float)
        (Decode.field "height" <| Decode.map (Animation.newEaseFloat animationDuration_) Decode.float)
    )