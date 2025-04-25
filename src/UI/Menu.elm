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
import Helper

type alias Model =
    {   shown: Set.Set String
    }

type Part msg =
    Section {name: String, icon: Maybe (Html.Html msg)} (List (Part msg))
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
    Section title children -> let shown = Set.member title.name model.shown in
        li [class "menuSection"]
        [   h1
            ([class "menuTitle", UI.HtmlEvent.onClick (Click title.name |> converter), class "clickable"] |> Helper.maybeAppend (Helper.maybeGuard shown (class "shown")))
            ([ text title.name ] |> Helper.maybeAppend (title.icon))
        ,   ul (if shown then [class "subMenu", class "shown"] else [class "subMenu"]) (children |> List.map (partToHtml_ converter model))
        ]

encode: Model -> Encode.Value
encode model = Encode.set Encode.string model.shown

decoder: Decode.Decoder Model
decoder = Decode.list Decode.string |> Decode.map (\s -> {shown = Set.fromList s})