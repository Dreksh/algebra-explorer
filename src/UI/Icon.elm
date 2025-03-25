module UI.Icon exposing (..)

import Html
import Svg exposing (circle, defs, path, rect, svg, text, text_)
import Svg.Attributes exposing (cx, cy, d, fill, height, id, mask, r, stroke, strokeWidth, viewBox, width, x, y)

-- SVG's "class" returns "class", while Html's "class" returns "className"
class: String -> Html.Attribute msg
class = Svg.Attributes.class

help: List (Html.Attribute msg) -> Html.Html msg
help attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   defs []
        [ Svg.mask [id "helpMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   text_ [x "8", y "17", fill "#000"] [text "?"]
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#helpMask)"] []
    ]

menu: List (Html.Attribute msg) -> Html.Html msg
menu attr = svg (attr ++ [viewBox "0 0 150 50"])
    [   defs []
        [ Svg.mask [id "menuMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M50 15H100M50 35H100", stroke "#000", strokeWidth "10", fill "none"] []
            ]
        ]
    ,   path [d "M0 50C25 50 25 0 50 0H100C125 0 125 50 150 50Z", stroke "none", fill "currentColor", mask "url(#menuMask)"] []
    ]

tick: List (Html.Attribute msg) -> Html.Html msg
tick attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   defs []
        [ Svg.mask [id "tickMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M6 15L11 19L17 6", fill "none", stroke "#000", strokeWidth "2"] []
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#tickMask)"] []
    ]

add: List (Html.Attribute msg) -> Html.Html msg
add attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   defs []
        [ Svg.mask [id "addMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M12 6V18M6 12H18", fill "none", stroke "#000", strokeWidth "2"] []
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#addMask)"] []
    ]

cancel: List (Html.Attribute msg) -> Html.Html msg
cancel attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   defs []
        [ Svg.mask [id "cancelMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M6 6L18 18M6 18L18 6", fill "none", stroke "#000", strokeWidth "2"] []
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#cancelMask)"] []
    ]

rightArrow: List (Html.Attribute msg) -> Html.Html msg
rightArrow attr = svg (attr ++ [viewBox "0 0 24 24"])
    [   path [d "M3 5L18 12 L3 19 L9 12Z", stroke "none", fill "currentColor"] []
    ]