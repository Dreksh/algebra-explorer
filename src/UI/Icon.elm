module UI.Icon exposing (class, menu, equation, tick, cancel, shown, hidden)

import Html
import Svg exposing (circle, defs, path, rect, svg, text, text_)
import Svg.Attributes exposing (cx, cy, d, fill, height, id, mask, r, stroke, strokeWidth, viewBox, width, x, y)

-- SVG's "class" returns "class", while Html's "class" returns "className"
class: String -> Html.Attribute msg
class = Svg.Attributes.class

menu: List (Html.Attribute msg) -> Html.Html msg
menu attr = svg (viewBox "0 0 24 24" :: attr)
    [   path [d "M2 0H22Q24 0 24 2Q24 4 22 4H2Q0 4 0 2Q0 0 2 0Z", fill "currentColor", mask "url(#menuMask)"] []
    ,   path [d "M2 10H22Q24 10 24 12Q24 14 22 14H2Q0 14 0 12Q0 10 2 10Z", fill "currentColor", mask "url(#menuMask)"] []
    ,   path [d "M2 20H22Q24 20 24 22Q24 24 22 24H2Q0 24 0 22Q0 20 2 20Z", fill "currentColor", mask "url(#menuMask)"] []
    ]

equation: List (Html.Attribute msg) -> Html.Html msg
equation attr = svg (viewBox "0 0 24 24" :: attr)
    [   Svg.text_ [] [Svg.text "f(x)"]
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