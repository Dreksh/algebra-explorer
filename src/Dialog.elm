module Dialog exposing (Input(..), Model, Section, decoder, fieldID, view)

import Dict
import Html exposing (form, text, h1, h2, input, label, node, button, span, p)
import Html.Attributes as Attr
import Json.Decode as Decode
import Set
-- Ours
import HtmlEvent
import Icon

type alias Model msg =
    {   title: String
    ,   sections: List (Section msg)
    ,   success: Decode.Decoder msg
    ,   cancel: msg
    ,   focus: Maybe String -- id of the field
    }

type alias Section msg =
    {   subtitle: String
    ,   inputs: List (Input msg)
    }

type Input msg =
    Text {name: Maybe String, id: String} -- ":" will be appended to the title, if present
    | Button {text: String, event: msg}
    | Info {text: String}

view: Model msg -> Html.Html msg
view model =
    node "dialog" [Attr.attribute "open" "true"]
    [   h1 [] [text model.title]
    ,   form [HtmlEvent.onSubmitForm model.success, Attr.attribute "method" "dialog"]
        (   List.map (\section -> Html.section []
                (   h2 [] [text section.subtitle]
                ::  List.map inputView_ section.inputs
                )
            )
            model.sections
        ++  [   Icon.cancel [Icon.class "clickable", Icon.class "cancelable", HtmlEvent.onClick model.cancel]
            ,   button [Attr.type_ "submit", Attr.class "noDefault"] [Icon.tick [Icon.class "clickable", Icon.class "submitable"]]
            ]
        )
    ]

inputView_: Input msg -> Html.Html msg
inputView_ input = case input of
    Text t -> label [Attr.for t.id]
        (   (case t.name of
                Nothing -> []
                Just n -> [text (n ++ ":")]
            )
        ++  [Html.input [Attr.type_ "text", Attr.name t.id, Attr.id (fieldID t.id)] []]
        )
    Button m -> button [Attr.type_ "button", HtmlEvent.onClick m.event, Icon.class "clickable"] [text m.text]
    Info i -> p [] [text i.text]

decoder: (Dict.Dict String String -> msg) -> Set.Set String -> Decode.Decoder msg
decoder map keys = Set.foldl (\key ->
        Decode.map2
        (\new -> Dict.insert key new)
        (Decode.string |> Decode.field "value" |> Decode.field key)
    )
    (Decode.succeed Dict.empty)
    keys
    |> Decode.map map

fieldID: String -> String
fieldID = (++) "dialog_"