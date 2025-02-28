module Menu exposing (Model, Event, init, menuItem, update)

import Html exposing (div, h1, text)
import Html.Attributes exposing (class)
import Set
import HtmlEvent
import Icon

type alias Model =
    {   closed: Set.Set String
    }

type Event =
    Click String

init: Model
init = { closed = Set.empty }

update: Event -> Model -> Model
update event model = case event of
    Click entry -> if Set.member entry model.closed
        then {model | closed = Set.remove entry model.closed}
        else {model | closed = Set.insert entry model.closed}

-- TODO: Have an arrow to indicate if it's expanded
menuItem: (Event->msg) -> Model -> Bool -> List (Html.Attribute msg) -> String -> List (Html.Html msg) -> List (Html.Html msg)
menuItem converter model = (\display attr name children -> let closed = not display || Set.member name model.closed in
    [   div (class "menuTitle"::if closed then [] else [class "shown"])
        [   Icon.rightArrow (HtmlEvent.onClick (Click name |> converter)::if closed then [] else [Icon.class "shown"])
        ,   h1 attr [text name]
        ]
    ,   div (class "subMenu"::if closed then [] else [class "shown"]) children
    ]
    )