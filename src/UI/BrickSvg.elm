module UI.BrickSvg exposing (bricks, brick)

import Html exposing (Html, div)
import Svg exposing (svg, g, rect, text_, text, Attribute)
import Svg.Attributes exposing (viewBox, width, height, x, y, strokeWidth, opacity, class, pointerEvents, rx)
-- ours
import Helper


strokeWidth_: Float
strokeWidth_ = 0.08
horizontalPad_: Float  -- space between adjacent blocks on same row
horizontalPad_ = 0.1
rectRadius_: Float
rectRadius_ = 0.2

bricks: Float -> Float -> List (Html event) -> Html event
bricks xMax yMax children =
    svg
    [   viewBox
        (   String.fromFloat (horizontalPad_ / 2)
        ++  " "
        ++  String.fromFloat -yMax
        ++  " "
        ++  String.fromFloat (xMax - horizontalPad_)
        ++  " "
        ++  String.fromFloat yMax
        )
    ,   class "bricks"
    ,   width "100%"
    ,   height "100%"
    ]
    children

brick: Float -> Float -> Float -> Float -> Float -> Bool -> List (Attribute event) -> String -> Html event
brick xMin xMax yMin yMax opacity_ canHover attrs label =
    let
        -- TODO: it would be nice for rects to overlap slightly
        --   but need to bring the hovered rect to the front for the :hover stroke to look nice
        x_ = xMin + (strokeWidth_ / 2) + (horizontalPad_ / 2)
        y_ = -(yMax - (strokeWidth_ / 2))  -- use yMax because the y-axis in SVG extends downwards
        width_ = (xMax - xMin) - strokeWidth_ - horizontalPad_
        height_ = (yMax - yMin) - strokeWidth_
        pointerEvents_ = if canHover then "auto" else "none"
    in
        g
        ( class "brick" :: attrs )
        [   rect
            [   x (String.fromFloat x_)
            ,   y (String.fromFloat y_)
            ,   width (String.fromFloat width_)
            ,   height (String.fromFloat height_)
            ,   strokeWidth (String.fromFloat strokeWidth_)
            ,   class "brickRect"
            ,   opacity (String.fromFloat opacity_)
            ,   pointerEvents pointerEvents_
            ,   rx (String.fromFloat rectRadius_)  -- ideally this would be in css but it doesn't work in Safari
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
