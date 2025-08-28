module UI.BrickSvg exposing (bricks, brick, Colour(..))

import Html exposing (Html)
import Svg exposing (svg, g, rect, Attribute)
import Svg.Attributes exposing (viewBox, width, height, x, y, strokeWidth, opacity, class, pointerEvents, rx, transform)
-- ours
import UI.Animation as Animation
import UI.MathIcon as MathIcon


type Colour =
    Leaf
    | Branch
    | Root

strokeWidth_: Float
strokeWidth_ = 0.05
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

brick: Float -> Float -> Float -> Float -> Float -> Colour -> Bool -> List (Attribute event) -> MathIcon.Model -> Html event
brick xMin xMax yMin yMax opacity_ colour_ canHover attrs label =
    let
        x_ = xMin + (strokeWidth_ / 2) + (horizontalPad_ / 2)
        y_ = -(yMax - (strokeWidth_ / 2))  -- use yMax because the y-axis in SVG extends downwards
        width_ = (xMax - xMin) - strokeWidth_ - horizontalPad_
        height_ = (yMax - yMin) - strokeWidth_
        labelOrigin =
            (   (width_ - (Animation.current label.botRight |> Tuple.first))/2
            ,   height_/2 + 0.25 - (Animation.current label.botRight |> Tuple.second)
            )
        pointerEvents_ = if canHover then "auto" else "none"
        colour = case colour_ of
            Leaf -> "leaf"
            Branch -> "branch"
            Root -> "root"
    in
        g
        (   [   class "brick"
            ,   transformAttr_ x_ y_
            ]
        ++ attrs
        )
        [   rect
            [   width (String.fromFloat width_)
            ,   height (String.fromFloat height_)
            ,   strokeWidth (String.fromFloat strokeWidth_)
            ,   class "brickRect"
            ,   class colour
            ,   opacity (String.fromFloat opacity_)
            ,   pointerEvents pointerEvents_
            ,   rx (String.fromFloat rectRadius_)  -- ideally this would be in css but it doesn't work in Safari
            ] []
        ,   MathIcon.toSvgGroup
            [   transformAttr_ (Tuple.first labelOrigin) (Tuple.second labelOrigin)
            ,   class "brickText"
            ,   opacity (String.fromFloat opacity_)
            ,   pointerEvents pointerEvents_
            ]
            label
        ]

transformAttr_: Float -> Float -> Svg.Attribute msg
transformAttr_ x y = transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")")
