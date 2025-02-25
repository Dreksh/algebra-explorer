module Menu exposing (Model, Event, init, menuItem, update)

import Html exposing (div, h1, text)
import Html.Attributes exposing (class)
import Set
import HtmlEvent
import Icon

type alias Model =
    {   selected: Set.Set String
    }

type Event =
    Click String

init: Set.Set String -> Model
init opened = { selected = opened }

update: Event -> Model -> Model
update event model = case event of
    Click entry -> if Set.member entry model.selected
        then {model | selected = Set.remove entry model.selected}
        else {model | selected = Set.insert entry model.selected}

-- TODO: Have an arrow to indicate if it's expanded
menuItem: (Event->msg) -> Model -> Bool -> List (Html.Attribute msg) -> String -> List (Html.Html msg) -> List (Html.Html msg)
menuItem converter model = (\display attr name children -> let shown = display && Set.member name model.selected in
    [   div (class "menuTitle"::if display then [class "shown"] else [])
        [   Icon.rightArrow (HtmlEvent.onClick (Click name |> converter)::if shown then [Icon.class "shown"] else [])
        ,   h1 attr [text name]
        ]
    ,   div (class "subMenu"::if shown then [class "shown"] else []) children
    ]
    )