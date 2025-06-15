module UI.BrickSvg exposing (bricks, brick)

import Html exposing (Html, div)
import Svg exposing (svg, g, rect, text_, text, Attribute)
import Svg.Attributes exposing (viewBox, width, height, x, y, strokeWidth, opacity, class, pointerEvents)
-- ours
import Helper


strokeWidth_: Float
strokeWidth_ = 0.08
horizontalPad_: Float  -- space between adjacent blocks on same row
horizontalPad_ = 0.1

bricks: Float -> Float -> List (Html event) -> Html event
bricks xMax yMax children =
    svg
    [   viewBox
        (   "0.0 "
        ++  String.fromFloat -yMax
        ++  " "
        ++  String.fromFloat xMax
        ++  " "
        ++  String.fromFloat yMax
        )
    ,   class "bricks"
    ,   width "100%"
    ,   height "100%"
    ]
    children

brick: Float -> Float -> Float -> Float -> Float -> Bool -> Bool -> Attribute event -> String -> Html event
brick xMin xMax yMin yMax opacity_ canHover selected onClick label =
    let
        x_ = xMin + (strokeWidth_ / 2) + (horizontalPad_ / 2)
        y_ = -(yMax - (strokeWidth_ / 2))
        width_ = (xMax - xMin) - strokeWidth_ - horizontalPad_
        height_ = (yMax - yMin) - strokeWidth_
        pointerEvents_ = if canHover then "auto" else "none"
    in
    g
    (  List.filterMap identity
        [   onClick |> Just
        ,   class "brick" |> Just
        ,   class "selected" |> Helper.maybeGuard selected
        ]
    )
    [   rect
        [   x (String.fromFloat x_)
        ,   y (String.fromFloat y_)
        ,   width (String.fromFloat width_)
        ,   height (String.fromFloat height_)
        ,   strokeWidth (String.fromFloat strokeWidth_)
        ,   class "brickRect"
        ,   opacity (String.fromFloat opacity_)
        ,   pointerEvents pointerEvents_
        ]
        []
    ,   text_
        [   x (String.fromFloat (x_ + width_ / 2))
        ,   y (String.fromFloat (y_ + height_ / 2))
        ,   class "brickText"
        ,   opacity (String.fromFloat opacity_)
        ,   pointerEvents pointerEvents_
        ]
        [ text label ]
    ]
