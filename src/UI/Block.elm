module UI.Block exposing (blocks, block)

import Html exposing (Html)
import Svg exposing (svg, g, animate, rect, text_, text, Attribute)
import Svg.Attributes exposing (viewBox, width, height, x, y, fill, attributeName, values, dur, textAnchor, dominantBaseline, rx)
import Html.Attributes exposing (class, style)


scale_: number
scale_ = 10

blocks: Int -> Int -> List (Html event) -> Html event
blocks xMax yMax children =
    svg
    [   viewBox
        (   "-0.5 "
        ++  String.fromFloat (scale_ * (toFloat -yMax) - 0.5)
        ++  " "
        ++  String.fromInt (scale_ * (xMax + 1))
        ++  " "
        ++  String.fromInt (scale_ * (yMax + 1))
        )
    ,   width ((String.fromInt (2 * (xMax + 1))) ++ "rem")
    ,   height ((String.fromInt (2 * (yMax + 1))) ++ "rem")
    ]
    children

block: Int -> Int -> Int -> Int -> List (Attribute event) -> String -> Html event
block xMin xMax yMin yMax attrs name =
    let
        x_ = scale_ * toFloat xMin
        y_ = scale_ * toFloat -yMin
        width_ = (scale_ * toFloat (xMax - xMin))  -- TODO: make this expand if name is longer
        height_ = toFloat scale_
    in
    g []
    [   rect
        (
            [   x (String.fromFloat x_)
            ,   y (String.fromFloat y_)
            ,   width (String.fromFloat width_)
            ,   height (String.fromFloat height_)
            ,   rx "2"
            ,   fill "#FFF"
            ,   style "stroke-width" "1"
            ,   style "stroke" "black"
            ] ++ attrs
        )
        []
        -- [ animate [ attributeName "height", values "0;1", dur "1s" ] [] ]
    ,   text_
        [   x (String.fromFloat (x_ + width_/2))
        ,   y (String.fromFloat (y_ + height_/2))
        ,   textAnchor "middle"
        ,   dominantBaseline "central"
        ,   style "font" "5pt monospace"
        ,   fill "black"
        ]
        [ text name ]
    ]
