module UI.Draggable exposing (Model, Event, Size, div, update, encode, decoder)

import Html exposing (span)
import Html.Attributes exposing (style, class)
import Json.Decode as Decode
import Json.Encode as Encode

-- These will be in raw pixel amounts (technically ints)
type alias Size = (Float, Float)
-- These will all be in dvw / dvh
type alias Point = Size

type alias Model =
    {   position: Point
    ,   size: Point
    }

type Direction =
    Left
    | Right
    | Top
    | Bottom
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight

-- Events are in raw px units
type Event =
    Drag Point Point
    | Resize Direction Point Point

div: (Event -> msg) -> Model -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
div converter model attrs children = Html.div (attrs ++ divAttrs_ converter model)
    ([  span [class "border", style "left" "0", style "top" "0", style "width" "100%", style "height" "1rem"] []
    ,   span [class "border", style "left" "0", style "top" "0", style "height" "100%", style "width" "1rem"] []
    ,   span [class "border", style "left" "0", style "bottom" "0", style "width" "100%", style "height" "1rem"] []
    ,   span [class "border", style "right" "0", style "top" "0", style "height" "100%", style "width" "1rem"] []
    ,   span [class "border", style "left" "0", style "top" "0", style "height" "1rem", style "width" "1rem"] []
    ,   span [class "border", style "left" "0", style "bottom" "0", style "height" "1rem", style "width" "1rem"] []
    ,   span [class "border", style "right" "0", style "top" "0", style "height" "1rem", style "width" "1rem"] []
    ,   span [class "border", style "right" "0", style "bottom" "0", style "height" "1rem", style "width" "1rem"] []
    ]
    ++  children
    )

divAttrs_: (Event -> msg) -> Model -> List (Html.Attribute msg)
divAttrs_ converter model =
    let
        (left, top) = model.position
        (width, height) = model.size
    in
    [   style "left" (String.fromFloat left ++ "dvw")
    ,   style "top" (String.fromFloat top ++ "dvh")
    ,   style "width" (String.fromFloat width ++ "dvw")
    ,   style "height" (String.fromFloat height ++ "dvh")
    ,   style "position" "absolute"
    ]

update: Size -> Event -> Model -> Model
update (screenX, screenY) e model = case e of
    Drag from to -> let (dx, dy) = diff from to in
        {model | position = add (dx/screenX, dy/screenY) model.position }
    Resize direction from to ->
        let
            (dx, dy) = diff from to
            addPoint = add model.size
        in case direction of
            Left -> { model | size = addPoint ( -dx/screenX, 0) }
            Right -> { model | size = addPoint (dx/screenX, 0) }
            Top -> { model | size = addPoint ( 0, -dy/screenY) }
            Bottom -> { model | size = addPoint (0, dy/screenY) }
            TopLeft -> { model | size = addPoint (-dx/screenX, -dy/screenY) }
            TopRight -> { model | size = addPoint (dx/screenX, -dy/screenY) }
            BottomLeft -> { model | size = addPoint (-dx/screenX, dy/screenY) }
            BottomRight -> { model | size = addPoint (dx/screenX, dy/screenY) }

diff: Point -> Point -> Point
diff (fromX, fromY) (toX, toY) = (toX - fromX, toY - fromY)

add: Point -> Point -> Point
add (fromX, fromY) (towardsX, towardsY) = (fromX + towardsX |> min 2, fromY + towardsY |> min 2)

encode: Model -> Encode.Value
encode model = Encode.object
    [   (   "position"
        ,   let (x, y) = model.position in Encode.object [("x", Encode.float x), ("y", Encode.float y)]
        )
    ,   (   "size"
        ,   let (x, y) = model.size in Encode.object [("width", Encode.float x), ("height", Encode.float y)]
        )
    ]

decoder: Decode.Decoder Model
decoder = Decode.map2 Model
    (   Decode.field "position"
        <| Decode.map2 Tuple.pair (Decode.field "x" Decode.float) (Decode.field "y" Decode.float)
    )
    (   Decode.field "size"
        <| Decode.map2 Tuple.pair (Decode.field "width" Decode.float) (Decode.field "height" Decode.float)
    )