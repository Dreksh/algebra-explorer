module UI.Icon exposing (class, help, menu, tick, add, cancel, shown, hidden)

import Html
import Svg exposing (circle, defs, path, rect, svg, text, text_)
import Svg.Attributes exposing (cx, cy, d, fill, height, id, mask, r, stroke, strokeWidth, viewBox, width, x, y)

-- SVG's "class" returns "class", while Html's "class" returns "className"
class: String -> Html.Attribute msg
class = Svg.Attributes.class

help: List (Html.Attribute msg) -> Html.Html msg
help attr = svg (viewBox "0 0 24 24" :: attr)
    [   defs []
        [ Svg.mask [id "helpMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   text_ [x "8", y "17", fill "#000"] [text "?"]
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#helpMask)"] []
    ]

menu: List (Html.Attribute msg) -> Html.Html msg
menu attr = svg (viewBox "0 0 150 50" :: attr)
    [   defs []
        [ Svg.mask [id "menuMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M50 15H100M50 35H100", stroke "#000", strokeWidth "10", fill "none"] []
            ]
        ]
    ,   path [d "M0 50C25 50 25 0 50 0H100C125 0 125 50 150 50Z", stroke "none", fill "currentColor", mask "url(#menuMask)"] []
    ]

tick: List (Html.Attribute msg) -> Html.Html msg
tick attr = svg (viewBox "0 0 24 24" :: attr)
    [   defs []
        [ Svg.mask [id "tickMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M6 15L11 19L17 6", fill "none", stroke "#000", strokeWidth "2"] []
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#tickMask)"] []
    ]

add: List (Html.Attribute msg) -> Html.Html msg
add attr = svg (viewBox "0 0 24 24" :: attr)
    [   defs []
        [ Svg.mask [id "addMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M12 6V18M6 12H18", fill "none", stroke "#000", strokeWidth "2"] []
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#addMask)"] []
    ]

cancel: List (Html.Attribute msg) -> Html.Html msg
cancel attr = svg (viewBox "0 0 24 24" :: attr)
    [   defs []
        [ Svg.mask [id "cancelMask"]
            [   rect [width "100%", height "100%", fill "#fff", x "0", y "0"] []
            ,   path [d "M6 6L18 18M6 18L18 6", fill "none", stroke "#000", strokeWidth "2"] []
            ]
        ]
    ,   circle [r "12", cx "12", cy "12", stroke "none", fill "currentColor", mask "url(#cancelMask)"] []
    ]

eye_: List (Svg.Svg msg)
eye_ = [   path [d "M1 13A12 12 1 0 1 23 13", stroke "currentColor", strokeWidth "1", fill "none"] []
    ,   path [d "M2 12A12 9 0 0 0 22 12", stroke "currentColor", strokeWidth "1", fill "none"] []
    ,   circle [cx "12", cy "11", r "5", stroke "currentColor", strokeWidth "1", fill "none"] []
    ]

shown: List (Html.Attribute msg) -> Html.Html msg
shown attr = svg (viewBox "0 0 24 24" :: attr) eye_

hidden: List (Html.Attribute msg) -> Html.Html msg
hidden attr = svg (viewBox "0 0 24 24" :: attr)
    (path [d "M2 22 L 22 2", stroke "currentColor", strokeWidth "1", fill "none"] [] :: eye_)