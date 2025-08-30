module UI.Block exposing (blocks, block)

import Html exposing (Html)
import Svg exposing (svg, g, animate, rect, text_, text, Attribute)
import Svg.Attributes exposing (viewBox, width, height, x, y, strokeWidth, fill, attributeName, values, dur, class)
-- ours
import Helper


strokeWidth_: Float
strokeWidth_ = 0.08
horizontalPad_: Float
horizontalPad_ = 0.1

scaleWidth_: Int -> Float
scaleWidth_ width = (toFloat width)*(1+horizontalPad_) - horizontalPad_ - strokeWidth_

scaleHeight_: Int -> Float
scaleHeight_ height = toFloat height - strokeWidth_

blocks: Int -> Int -> List (Html event) -> Html event
blocks xMax yMax children =
    svg
    [   viewBox
        (   "-0.5 "
        ++  String.fromFloat (-(scaleHeight_ yMax) - 0.5)
        ++  " "
        ++  String.fromFloat (scaleWidth_ xMax + 1)
        ++  " "
        ++  String.fromFloat (scaleHeight_ yMax + 1)
        )
    ,   width ((String.fromInt (2 * (xMax + 1))) ++ "rem")
    ,   height ((String.fromInt (2 * (yMax + 1))) ++ "rem")
    ,   class "blocks"
    ]
    children

block: Int -> Int -> Int -> Int -> Bool -> Attribute event -> String -> Html event
block xMin xMax yMin yMax selected onClick name =
    let
        x_ = (toFloat xMin) / (1-horizontalPad_)
        y_ = toFloat -yMin
        width_ = scaleWidth_ (xMax - xMin)  -- TODO: make this expand if name is longer
        height_ = scaleHeight_ 1
    in
    g
    (  List.filterMap identity
        [   onClick |> Just
        ,   class "block" |> Just
        ,   class "selected" |> Helper.maybeGuard selected
        ]
    )
    [   rect
        [   x (String.fromFloat x_)
        ,   y (String.fromFloat y_)
        ,   width (String.fromFloat width_)
        ,   height (String.fromFloat height_)
        ,   strokeWidth (String.fromFloat strokeWidth_)
        ,   class "blockRect"
        ]
        []
        -- [ animate [ attributeName "height", values "0;1", dur "1s" ] [] ]
    ,   text_
        [   x (String.fromFloat (x_ + width_/2))
        ,   y (String.fromFloat (y_ + height_/2))
        ,   class "blockText"
        ]
        [ text name ]
    ]
