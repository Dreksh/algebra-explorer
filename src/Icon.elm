module Icon exposing (..)

import Html
import Svg exposing (circle, path, svg, text, text_)
import Svg.Attributes exposing (cx, cy, d, fill, r, stroke, viewBox, x, y)

-- SVG's "class" returns "class", while Html's "class" returns "className"
class: String -> Html.Attribute msg
class = Svg.Attributes.class

help: List (Html.Attribute msg) -> Html.Html msg
help attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor"] []
    ,   text_ [x "12", y "20", fill "none", stroke "1"] [text "?"] 
    ]

menu: List (Html.Attribute msg) -> Html.Html msg
menu attr = svg (attr ++ [viewBox "0 0 150 50"])
    [   path [d "M5 0H145A5 5 0 0 1 145 10H5A5 5 0 0 1 5 0Z", stroke "none", fill "currentColor"] []
    ,   path [d "M5 20H145A5 5 0 0 1 145 30H5A5 5 0 0 1 5 20Z", stroke "none", fill "currentColor"] []
    ,   path [d "M5 40H145A5 5 0 0 1 145 50H5A5 5 0 0 1 5 40Z", stroke "none", fill "currentColor"] []
    ]

tick: List (Html.Attribute msg) -> Html.Html msg
tick attr = svg (attr ++ [viewBox "0 0 24 24"])
    [

    ]

add: List (Html.Attribute msg) -> Html.Html msg
add attr = svg (attr ++ [viewBox "0 0 24 24"])
    [

    ]