module UI.Dialog exposing (Extracted(..), Input(..), Model, Section, fieldID, view)

import Dict
import Html exposing (a, button, form, h1, h2, input, label, node, p, span, text)
import Html.Attributes as Attr
import Json.Decode as Decode
-- Ours
import UI.HtmlEvent
import UI.Icon

type alias Model msg =
    {   title: String
    ,   sections: List (Section msg)
    ,   success: (Dict.Dict String Extracted -> msg)
    ,   cancel: msg
    ,   focus: Maybe String -- id of the field
    }

type alias Section msg =
    {   subtitle: String
    ,   lines: List (List (Input msg))
    }

type Input msg =
    Text {id: String}
    | Button {text: String, event: msg}
    | Info {text: String}
    | Radio {name: String, options: Dict.Dict Int String}
    | Function {name: String, arguments: Int}
    | Link {url: String} -- Should open a new tab / window

-- Autogenerate from Model
type Extracted =
    TextValue String
    | IntValue Int
    | FunctionValue (List String) String

view: Model msg -> Html.Html msg
view model =
    node "dialog" [Attr.attribute "open" "true"]
    [   h1 [] [text model.title]
    ,   form [UI.HtmlEvent.onSubmitForm (decoder_ model), Attr.attribute "method" "dialog"]
        (   List.map (\section -> Html.section []
                (   h2 [] [text section.subtitle]
                ::  List.map listView_ section.lines
                )
            )
            model.sections
        ++  [   UI.Icon.cancel [UI.Icon.class "clickable", UI.Icon.class "cancelable", UI.HtmlEvent.onClick model.cancel]
            ,   button [Attr.type_ "submit", Attr.class "noDefault"] [UI.Icon.tick [UI.Icon.class "clickable", UI.Icon.class "submitable"]]
            ]
        )
    ]

listView_: List (Input msg) -> Html.Html msg
listView_ = List.map (\input -> case input of
        Text t -> label [Attr.for t.id] [Html.input [Attr.type_ "text", Attr.name t.id, Attr.id (fieldID t.id)] []]
        Button m -> button [Attr.type_ "button", UI.HtmlEvent.onClick m.event, Attr.class "clickable"] [text m.text]
        Info i -> text i.text
        Radio r -> r.options
            |> Dict.toList
            |> List.concatMap (\(num, t) -> let n = String.fromInt num in
                let id = r.name ++ n |> fieldID in
                [   Html.br [] []
                ,   Html.input [Attr.type_ "radio", Attr.name r.name, Attr.id id, Attr.value n] []
                ,   Html.label [Attr.for id] [text t]
                ]
            )
            |> span []
        Function f -> span []
            (   text (f.name ++ "(")
                :: ( List.map
                    (\index -> let n = f.name ++ String.fromInt index in
                        Html.label [Attr.for n] [Html.input [Attr.type_ "text", Attr.name n, Attr.id (fieldID n)] []]
                    )
                    (List.range 1 f.arguments)
                    |> List.intersperse (text ",")
                )
                ++ [ text ") = ", Html.label [Attr.for f.name] [Html.input [Attr.type_ "text", Attr.name f.name, Attr.id (fieldID f.name)] []]]
            )
        Link l -> a [Attr.class "clickable", Attr.target "_blank", Attr.href l.url] [text l.url]
    )
    >> span []

decoder_: Model msg -> Decode.Decoder msg
decoder_ model = let valueDecoder name = Decode.field "value" Decode.string |> Decode.field name in
    model.sections
    |> List.foldl (\section d ->
        List.foldl (\lines d1 ->
            List.foldl (\input dict -> case input of
                    Text s -> Decode.map2 (\val -> Dict.insert s.id (TextValue val)) (valueDecoder s.id) dict
                    Radio s -> Decode.map2 Tuple.pair (valueDecoder s.name) dict
                        |> Decode.andThen (\(val, map) -> case String.toInt val of
                            Nothing -> Decode.fail "Selection was not a number"
                            Just num -> Decode.succeed (Dict.insert s.name (IntValue num) map)
                        )
                    Function s -> List.range 1 s.arguments
                        |> List.map (\num -> s.name ++ String.fromInt num)
                        |> List.foldl (\name -> Decode.map2 (::) (valueDecoder name)) (Decode.succeed [])
                        |> Decode.map3
                            (\orig equal list -> Dict.insert s.name (FunctionValue list equal) orig)
                            dict
                            (valueDecoder s.name)
                    _ -> dict
            ) d1 lines
        )
        d section.lines
    )
    (Decode.succeed Dict.empty)
    |> Decode.map model.success

fieldID: String -> String
fieldID = (++) "dialog_"