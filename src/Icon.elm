module Icon exposing (..)

import Html
import Svg exposing (circle, defs, path, rect, svg, text, text_)
import Svg.Attributes exposing (cx, cy, d, fill, height, id, mask, r, stroke, strokeWidth, viewBox, width, x, y)

-- SVG's "class" returns "class", while Html's "class" returns "className"
class: String -> Html.Attribute msg
class = Svg.Attributes.class

help: List (Html.Attribute msg) -> Html.Html msg
help attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   defs []
        [ Svg.mask [id "mask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   text_ [x "8", y "17", fill "#000"] [text "?"]
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#mask)"] []
    ]

menu: List (Html.Attribute msg) -> Html.Html msg
menu attr = svg (attr ++ [viewBox "0 0 150 50"])
    [   path [d "M5 0H145A5 5 0 0 1 145 10H5A5 5 0 0 1 5 0Z", stroke "none", fill "currentColor"] []
    ,   path [d "M5 20H145A5 5 0 0 1 145 30H5A5 5 0 0 1 5 20Z", stroke "none", fill "currentColor"] []
    ,   path [d "M5 40H145A5 5 0 0 1 145 50H5A5 5 0 0 1 5 40Z", stroke "none", fill "currentColor"] []
    ]

tick: List (Html.Attribute msg) -> Html.Html msg
tick attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   defs []
        [ Svg.mask [id "mask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M6 15L11 19L17 6", fill "none", stroke "#000", strokeWidth "2"] []
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#mask)"] []
    ]

add: List (Html.Attribute msg) -> Html.Html msg
add attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   defs []
        [ Svg.mask [id "mask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M12 6V18M6 12H18", fill "none", stroke "#000", strokeWidth "2"] []
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#mask)"] []
    ]

cancel: List (Html.Attribute msg) -> Html.Html msg
cancel attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   defs []
        [ Svg.mask [id "mask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M6 6L18 18M6 18L18 6", fill "none", stroke "#000", strokeWidth "2"] []
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#mask)"] []
    ]