module UI.Menu exposing (Model, Event, Part(..),
    init, update, view,
    encode, decoder
    )

import Html exposing (h1, li, nav, ul, text)
import Html.Attributes exposing (class, id)
import Json.Decode as Decode
import Json.Encode as Encode
import Set
-- Ours
import UI.HtmlEvent

type alias Model =
    {   shown: Set.Set String
    }

type Part msg =
    Section String Bool (List (Part msg))
    | Content (List (Html.Html msg))

type Event =
    Click String

init: Set.Set String -> Model
init shown = { shown = shown }

update: Event -> Model -> Model
update event model = case event of
    Click entry -> if Set.member entry model.shown
        then {model | shown = Set.remove entry model.shown}
        else {model | shown = Set.insert entry model.shown}

view: (Event->msg) -> Model -> List (Part msg) -> Html.Html msg
view converter model children = nav [id "menu"] [ul [] (children |> List.map (partToHtml_ converter model) ) ]

partToHtml_: (Event -> msg) -> Model -> Part msg -> Html.Html msg
partToHtml_ converter model part = case part of
    Content children -> li [] children
    Section name display children -> let shown = display && Set.member name model.shown in
        li (if display then [class "menuSection", class "shown"] else [class "menuSection"])
        [   h1 ((if shown then [class "shown"] else []) ++ [class "menuTitle", UI.HtmlEvent.onClick (Click name |> converter), class "clickable"]) [ text name ]
        ,   ul (if shown then [class "subMenu", class "shown"] else [class "subMenu"]) (children |> List.map (partToHtml_ converter model))
        ]

encode: Model -> Encode.Value
encode model = Encode.set Encode.string model.shown

decoder: Decode.Decoder Model
decoder = Decode.list Decode.string |> Decode.map (\s -> {shown = Set.fromList s})