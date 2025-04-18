module UI.Block exposing (blocks, block)

import Html exposing (Html)
import Svg exposing (svg, g, animate, rect, text_, text, Attribute)
import Svg.Attributes exposing (viewBox, width, height, x, y, strokeWidth, fill, attributeName, values, dur, class)


strokeWidth_ = 0.1
horizontalPad_ = 0.1

blocks: Int -> Int -> List (Html event) -> Html event
blocks xMax yMax children =
    svg
    [   viewBox
        (   "-0.5 "
        ++  String.fromFloat (toFloat -yMax - 0.5)
        ++  " "
        ++  String.fromInt (xMax + 1)
        ++  " "
        ++  String.fromInt (yMax + 1)
        )
    ,   width ((String.fromInt (2 * (xMax + 1))) ++ "rem")
    ,   height ((String.fromInt (2 * (yMax + 1))) ++ "rem")
    ,   class "blocks"
    ]
    children

block: Int -> Int -> Int -> Int -> Bool -> Attribute event -> String -> Html event
block xMin xMax yMin yMax selected onClick name =
    let
        x_ = toFloat xMin
        y_ = toFloat -yMin
        width_ = (toFloat (xMax - xMin))  -- TODO: make this expand if name is longer
        height_ = 1
    in
    g []
    [   rect
        (
            [   x (String.fromFloat x_)
            ,   y (String.fromFloat y_)
            ,   width (String.fromFloat (width_ - strokeWidth_ - horizontalPad_))
            ,   height (String.fromFloat (height_ - strokeWidth_))
            ,   strokeWidth (String.fromFloat strokeWidth_)
            ,   onClick
            ,   class "block"
            ] ++ ( if selected then [class "selected"] else [] )
        )
        []
        -- [ animate [ attributeName "height", values "0;1", dur "1s" ] [] ]
    ,   text_
        [   x (String.fromFloat (x_ + width_/2 - strokeWidth_/2 - horizontalPad_/2))
        ,   y (String.fromFloat (y_ + height_/2 - strokeWidth_/2))
        ,   class "blockText"
        ]
        [ text name ]
    ]
