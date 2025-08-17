module UI.Icon exposing (class, download, menu, tick, cancel, shown, hidden, left, right,
    verticalLine, history, default, bin)

import Html
import Svg exposing (circle, defs, line, path, rect, svg, text, text_)
import Svg.Attributes exposing (cx, cy, d, fill, height, id, mask, r, stroke, strokeWidth, viewBox, width, x, y, x1, x2, y1, y2)

-- SVG's "class" returns "class", while Html's "class" returns "className"
class: String -> Html.Attribute msg
class = Svg.Attributes.class

menu: List (Html.Attribute msg) -> Html.Html msg
menu attr = svg (viewBox "0 0 24 24" :: attr)
    [   path [d "M2 0H22Q24 0 24 2Q24 4 22 4H2Q0 4 0 2Q0 0 2 0Z", fill "currentColor", stroke "none"] []
    ,   path [d "M2 10H22Q24 10 24 12Q24 14 22 14H2Q0 14 0 12Q0 10 2 10Z", fill "currentColor", stroke "none"] []
    ,   path [d "M2 20H22Q24 20 24 22Q24 24 22 24H2Q0 24 0 22Q0 20 2 20Z", fill "currentColor", stroke "none"] []
    ]

download: List (Html.Attribute msg) -> Html.Html msg
download attr = svg (viewBox "0 0 24 24" :: attr)
    [   path [d "M12 18L6 12L10 12L10 4L14 4L14 12L18 12Z", fill "currentColor", stroke "none"] []
    ,   path [d "M4 20H20", fill "none", stroke "currentColor", strokeWidth "4"] []
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
eye_ = [   path [d "M1 9A12 12 1 0 1 23 9", stroke "currentColor", strokeWidth "1", fill "none"] []
    ,   path [d "M2 8A12 9 0 0 0 22 8", stroke "currentColor", strokeWidth "1", fill "none"] []
    ,   circle [cx "12", cy "7", r "5", stroke "currentColor", strokeWidth "1", fill "none"] []
    ]

shown: List (Html.Attribute msg) -> Html.Html msg
shown attr = svg (viewBox "0 0 24 15" :: attr) eye_

hidden: List (Html.Attribute msg) -> Html.Html msg
hidden attr = svg (viewBox "0 0 24 15" :: attr)
    (path [d "M6 14 L 18 2", stroke "currentColor", strokeWidth "1", fill "none"] [] :: eye_)

left: List (Html.Attribute msg) -> Html.Html msg
left attr = svg (viewBox "0 0 24 24" :: attr)
    [   path [d "M8 12L16 6V18Z", stroke "none", fill "currentColor"] []
    ]

right: List (Html.Attribute msg) -> Html.Html msg
right attr = svg (viewBox "0 0 24 24" :: attr)
    [   path [d "M16 12L8 6V18Z", stroke "none", fill "currentColor"] []
    ]

verticalLine: List (Html.Attribute msg) -> Html.Html msg
verticalLine attr = svg (width "12" :: attr)
    [   line [x1 "6", x2 "6", y1 "20%", y2 "80%", stroke "currentColor", strokeWidth "4"] []
    ]

history: List (Html.Attribute msg) -> Html.Html msg
history attr = svg (viewBox "0 0 24 24" :: attr)
    [   circle [cx "12", cy "12", r "11", stroke "currentColor", strokeWidth "1", fill "none"] []
    ,   path [d "M6 8.5L12 12L21.4 6", stroke "currentColor", strokeWidth "1", fill "none"] []
    ]

default: List (Html.Attribute msg) -> Html.Html msg
default attr = svg (viewBox "0 0 24 24" :: attr)
    [   path [d "M2 22L5 17V5A3 3 0 0 1 8 2H20A3 3 0 0 1 23 5V17A3 3 0 0 1 20 20H8Z", stroke "currentColor", strokeWidth "1", fill "none"] []
    ,   circle [cx "14", cy "9", r "5", stroke "currentColor", strokeWidth "1", fill "none"] []
    ,   path [d "M11 13V17A5 5 0 0 0 17 17V13", stroke "currentColor", strokeWidth "1", fill "none"] []
    ,   path [d "M11 15A5 5 0 0 0 16 15.5", stroke "currentColor", strokeWidth "1", fill "none"] []
    ,   path [d "M13 14A8 8 0 0 0 12 10A8 8 0 0 0 16 10A8 8 0 0 0 15 14", stroke "currentColor", strokeWidth "1", fill "none"] []
    ]

bin: List (Html.Attribute msg) -> Html.Html msg
bin attr = svg (viewBox "0 0 24 24" :: attr)
    [ path [d "M 7 3 C 7 3 1 3 1 5 C 1 8 23 8 23 5 C 23 3 17 3 17 3", stroke "currentColor", strokeWidth "2", fill "none"] []
    , path [d "M 8 5 C 8 1 16 1 16 5", stroke "currentColor", strokeWidth "2", fill "none"] []
    , path [d "M 2.5 8 L 4 21 C 8 23 16 23 20 21 L 21.5 8", stroke "currentColor", strokeWidth "2", fill "none"] []
    , path [d "M 8 9 L 9 20 M 15 20 L 16 9", stroke "currentColor", strokeWidth "2", fill "none"] []
    ]