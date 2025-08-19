module UI.Draggable exposing (Action, Model, Event(..), Size, init, div, update, encode, decoder)

import Html exposing (span)
import Html.Attributes exposing (class, id, style)
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import UI.HtmlEvent exposing (onPointerCapture)
import UI.SvgDrag as SvgDrag

-- These will be in raw pixel amounts (technically ints)
type alias Size = (Float, Float)
-- These will all be in dvw / dvh
type alias Point = Size

type alias Model =
    {   coordinates: Coordinates
    ,   drag: Maybe {direction: Direction, original: Coordinates, from: Point}
    ,   id: String
    }

init: String -> Point -> Size -> Model
init id (left, top) (width, height) =
    {   coordinates = {left = left, right = left + width, top = top, bottom = top + height}
    ,   drag = Nothing
    ,   id = id
    }

type alias Coordinates = {left: Float, right: Float, top: Float, bottom: Float}

type Direction =
    Left
    | Right
    | Top
    | Bottom
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight

type alias Action = {id: String, pID: Encode.Value}

type Event =
    DragStart Direction Encode.Value Point
    | Drag SvgDrag.Event

div: (Event -> msg) -> Model -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
div converter model attrs children = Html.div (divAttrs_ model)
    [  span
        [ class "border", onPointerCapture converter (DragStart Top)
        , style "left" "0", style "top" "0", style "width" "100%", style "height" "1rem", style "cursor" "move"
        , style "text-align" "center"
        ]
        [Html.text model.id]
    ,   span
        [ class "border", onPointerCapture converter (DragStart Left)
        , style "left" "0", style "top" "0", style "height" "100%", style "width" "0.2rem", style "cursor" "ew-resize"
        ]
        []
    ,   span
        [ class "border", onPointerCapture converter (DragStart Bottom)
        , style "left" "0", style "bottom" "0", style "width" "100%", style "height" "0.2rem", style "cursor" "ns-resize"
        ]
        []
    ,   span
        [ class "border", onPointerCapture converter (DragStart Right)
        , style "right" "0", style "top" "0", style "height" "100%", style "width" "0.2rem", style "cursor" "ew-resize"
        ]
        []
    ,   span
        [ class "border", onPointerCapture converter (DragStart TopLeft)
        , style "left" "0", style "top" "0", style "height" "0.2rem", style "width" "0.2rem", style "cursor" "nwse-resize"
        ]
        []
    ,   span
        [ class "border", onPointerCapture converter (DragStart BottomLeft)
        , style "left" "0", style "bottom" "0", style "height" "0.2rem", style "width" "0.2rem", style "cursor" "nesw-resize"
        ]
        []
    ,   span
        [ class "border", onPointerCapture converter (DragStart TopRight)
        , style "right" "0", style "top" "0", style "height" "0.2rem", style "width" "0.2rem", style "cursor" "nesw-resize"
        ]
        []
    ,   span
        [   class "border", onPointerCapture converter (DragStart BottomRight)
        ,   style "right" "0", style "bottom" "0", style "height" "0.2rem", style "width" "0.2rem", style "cursor" "nwse-resize"
        ]
        []
    ,   Html.div attrs children
    ]

divAttrs_: Model -> List (Html.Attribute msg)
divAttrs_ model =
    [   style "left" (String.fromFloat model.coordinates.left ++ "dvw")
    ,   style "top" (String.fromFloat model.coordinates.top ++ "dvh")
    ,   style "width" (String.fromFloat (model.coordinates.right - model.coordinates.left) ++ "dvw")
    ,   style "height" (String.fromFloat (model.coordinates.bottom - model.coordinates.top) ++ "dvh")
    ,   style "position" "absolute"
    ,   style "padding" "1rem 0.2rem 0.2rem 0.2rem"
    ,   id model.id
    ,   class "draggable"
    ]

update: Size -> Event -> Model -> (Model, Maybe Action)
update size e model = case e of
    DragStart d pid p ->
        (   {   model
            |   drag = Just {direction = d, original = model.coordinates, from = p}
            }
        ,   Just {id = model.id, pID = pid}
        )
    Drag dragEvent -> case dragEvent of
        SvgDrag.Start _ -> (model, Nothing)
        SvgDrag.Move _ to -> case model.drag of
            Nothing -> (model, Nothing)
            Just start -> ({ model | coordinates = updateCoordinate_ size start to}, Nothing)
        SvgDrag.End _ to -> case model.drag of
            Nothing -> ({model | drag = Nothing}, Nothing)
            Just start -> ({model | drag = Nothing, coordinates = updateCoordinate_ size start to}, Nothing)

updateCoordinate_: Point -> {direction: Direction, original: Coordinates, from: Point} -> Point -> Coordinates
updateCoordinate_ (screenX, screenY) start to =
    let
        (diffX, diffY) = diff start.from to
        (dx, dy) = (diffX * 100 / screenX, diffY * 100 / screenY)
        orig = start.original
    in case start.direction of
        Top -> let newDy = dy |> max (-orig.top) |> min (99 - orig.top) in
            let newDx = dx |> max (1 - orig.right)  |> min (99 - orig.left) in
            {left = orig.left + newDx, right = orig.right + newDx, top = orig.top + newDy, bottom = orig.bottom + newDy }
        Left -> {orig |left = orig.left + dx |> min (orig.right - 2) }
        Right -> {orig | right = orig.right + dx |> max (orig.left + 2)}
        Bottom -> {orig | bottom = orig.bottom + dy |> max (orig.top + 2)}
        TopLeft -> {orig | left = orig.left + dx |> min (orig.right - 2), top = orig.top + dy |> min (orig.bottom - 2)}
        TopRight -> {orig | right = orig.right + dx |> max (orig.left + 2), top = orig.top + dy |> min (orig.bottom - 2)}
        BottomLeft -> {orig | left = orig.left + dx |> min (orig.right - 2), bottom = orig.bottom + dy |> max (orig.top + 2) }
        BottomRight -> {orig | right = orig.right + dx |> max (orig.left + 2), bottom = orig.bottom + dy |> max (orig.top + 2) }

diff: Point -> Point -> Point
diff (fromX, fromY) (toX, toY) = (toX - fromX, toY - fromY)

encode: Model -> Encode.Value
encode model = Encode.object
    [   ("left", Encode.float model.coordinates.left)
    ,   ("right", Encode.float model.coordinates.right)
    ,   ("top", Encode.float model.coordinates.top)
    ,   ("bottom", Encode.float model.coordinates.bottom)
    ,   ("id", Encode.string model.id)
    ]

decoder: Decode.Decoder Model
decoder = Decode.map5 (\l r t b id -> Model (Coordinates l r t b) Nothing id)
    (Decode.field "left" Decode.float)
    (Decode.field "right" Decode.float)
    (Decode.field "top" Decode.float)
    (Decode.field "bottom" Decode.float)
    (Decode.field "id" Decode.string)