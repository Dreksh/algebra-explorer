module UI.Draggable exposing (Action(..), Model, Event, Size, init, div, update, encode, decoder)

import Dict
import Html exposing (span)
import Html.Attributes exposing (class, id, style)
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import UI.HtmlEvent exposing (onPointerDraw)

-- These will be in raw pixel amounts (technically ints)
type alias Size = (Float, Float)
-- These will all be in dvw / dvh
type alias Point = Size

type alias Model =
    {   coordinates: Coordinates
    ,   drags: Dict.Dict String {direction: Maybe Direction, original: Coordinates, from: Point}
    ,   id: String
    }

init: String -> Point -> Size -> Model
init id (left, top) (width, height) =
    {   coordinates = {left = left, right = left + width, top = top, bottom = top + height}
    ,   drags = Dict.empty
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

type Action =
    SetCapture String Encode.Value
    | ReleaseCapture String Encode.Value

-- Events are in raw px units
type Event =
    DragStart (Maybe Direction) Encode.Value Point
    | Drag Encode.Value Point
    | DragEnd Encode.Value
    | NoOp

div: (Event -> msg) -> Model -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
div converter model attrs children = Html.div (attrs ++ divAttrs_ converter model)
    ([  span
        (   [class "border", style "left" "0", style "top" "0", style "width" "100%", style "height" "1rem"]
        ++  onPointerDraw converter (DragStart (Just Top)) Drag DragEnd
        )
        []
    ,   span
        (   [class "border", style "left" "0", style "top" "0", style "height" "100%", style "width" "1rem"]
        ++  onPointerDraw converter (DragStart (Just Left)) Drag DragEnd
        )
        []
    ,   span
        (   [class "border", style "left" "0", style "bottom" "0", style "width" "100%", style "height" "1rem"]
        ++  onPointerDraw converter (DragStart (Just Bottom)) Drag DragEnd
        )
        []
    ,   span
        (   [class "border", style "right" "0", style "top" "0", style "height" "100%", style "width" "1rem"]
        ++  onPointerDraw converter (DragStart (Just Right)) Drag DragEnd
        )
        []
    ,   span
        (   [class "border", style "left" "0", style "top" "0", style "height" "1rem", style "width" "1rem"]
        ++  onPointerDraw converter (DragStart (Just TopLeft)) Drag DragEnd
        )
        []
    ,   span
        (   [class "border", style "left" "0", style "bottom" "0", style "height" "1rem", style "width" "1rem"]
        ++  onPointerDraw converter (DragStart (Just BottomLeft)) Drag DragEnd
        )
        []
    ,   span
        (   [class "border", style "right" "0", style "top" "0", style "height" "1rem", style "width" "1rem"]
        ++  onPointerDraw converter (DragStart (Just TopRight)) Drag DragEnd
        )
        []
    ,   span
        (   [class "border", style "right" "0", style "bottom" "0", style "height" "1rem", style "width" "1rem"]
        ++  onPointerDraw converter (DragStart (Just BottomRight)) Drag DragEnd
        )
        []
    ]
    ++  children
    )

divAttrs_: (Event -> msg) -> Model -> List (Html.Attribute msg)
divAttrs_ converter model =
    [   style "left" (String.fromFloat model.coordinates.left ++ "dvw")
    ,   style "top" (String.fromFloat model.coordinates.top ++ "dvh")
    ,   style "width" (String.fromFloat (model.coordinates.right - model.coordinates.left) ++ "dvw")
    ,   style "height" (String.fromFloat (model.coordinates.bottom - model.coordinates.top) ++ "dvh")
    ,   style "position" "absolute"
    ,   id model.id
    ]
    ++ onPointerDraw converter (DragStart Nothing) Drag DragEnd

update: Size -> Event -> Model -> (Model, Maybe Action)
update (screenX, screenY) e model = case e of
    DragStart d pid p ->
        ({model | drags = Dict.insert (Encode.encode 0 pid) {direction = d, original = model.coordinates, from = p} model.drags}, Just (SetCapture model.id pid))
    Drag pid to -> case Dict.get (Encode.encode 0 pid) model.drags of
        Nothing -> (model, Nothing)
        Just start ->
            let
                (diffX, diffY) = diff start.from to
                (dx, dy) = (diffX * 100 / screenX, diffY * 100 / screenY)
                orig = start.original
            in case start.direction of
                Nothing ->
                    (   {   model
                        | coordinates = {left = orig.left + dx, right = orig.right + dx, top = orig.top + dy, bottom = orig.bottom + dy }
                        }
                    ,   Nothing
                    )
                Just Left -> ({ model | coordinates = {orig |left = orig.left + dx |> min (orig.right - 2) } }, Nothing)
                Just Right -> ({ model | coordinates = {orig | right = orig.right + dx |> max (orig.left + 2)} }, Nothing)
                Just Top -> ({ model | coordinates = {orig | top = orig.top + dy |> min (orig.bottom - 2)}}, Nothing)
                Just Bottom -> ({ model | coordinates = {orig | bottom = orig.bottom + dy |> max (orig.top + 2) }}, Nothing)
                Just TopLeft -> ({ model | coordinates = {orig | left = orig.left + dx |> min (orig.right - 2), top = orig.top + dy |> min (orig.bottom - 2)} }, Nothing)
                Just TopRight -> ({ model | coordinates = {orig | right = orig.right + dx |> max (orig.left + 2), top = orig.top + dy |> min (orig.bottom - 2)} }, Nothing)
                Just BottomLeft -> ({ model | coordinates = {orig | left = orig.left + dx |> min (orig.right - 2), bottom = orig.bottom + dy |> max (orig.top + 2) }}, Nothing)
                Just BottomRight -> ({ model | coordinates = {orig | right = orig.right + dx |> max (orig.left + 2), bottom = orig.bottom + dy |> max (orig.top + 2) } }, Nothing)
    DragEnd pid -> ({model | drags = Dict.remove (Encode.encode 0 pid) model.drags }, Just (ReleaseCapture model.id pid))
    NoOp -> (model, Nothing)

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
decoder = Decode.map5 (\l r t b id -> Model (Coordinates l r t b) Dict.empty id)
    (Decode.field "left" Decode.float)
    (Decode.field "right" Decode.float)
    (Decode.field "top" Decode.float)
    (Decode.field "bottom" Decode.float)
    (Decode.field "id" Decode.string)