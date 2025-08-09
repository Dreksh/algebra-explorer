module UI.Dialog exposing (Extracted(..), Input(..), Model, Event, Section,
    fieldID, processMathInput, update, view)

import Array
import Dict
import Html exposing (a, button, form, h1, h2, input, label, node, p, span, text)
import Html.Attributes as Attr
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import Algo.Math as Math
import Components.Latex as Latex
import Components.Rules as Rules
import UI.HtmlEvent
import UI.Icon
import UI.Input as Input

type alias Model msg =
    {   title: String
    ,   sections: List (Section msg)
    ,   success: (Dict.Dict String Extracted -> msg)
    ,   cancel: msg
    ,   focus: Maybe String -- id of the field
    ,   inputFields: Dict.Dict String (Input.Model msg)
    }

type Event =
    InputEvent String Input.Event

type alias Section msg =
    {   subtitle: String
    ,   lines: List (List (Input msg))
    }

type Input msg =
    Text {id: String}
    | Button {text: String, event: msg}
    | Info {text: String}
    | Radio {name: String, options: Dict.Dict Int (Html.Html msg)}
    | MathInput {id: String}
    | ParameterInput {id: String, args: List String, example: String}
    | Link {url: String} -- Should open a new tab / window

processMathInput: (String -> Encode.Value -> (Float, Float) -> Cmd msg) -> (String -> Cmd msg)
    -> Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Model msg -> Model msg
processMathInput mouseCmd focusCmd funcDict model =
    {   model
    |   inputFields = model.sections
        |> List.foldl (\section dict -> section.lines
            |> List.foldl (\line dict1 -> line
                |> List.foldl (\input dict2 -> case input of
                    MathInput n -> let objId = fieldID n.id in
                        Dict.insert n.id (Input.init (mouseCmd objId) focusCmd objId) dict2
                    ParameterInput n -> let objId = fieldID n.id in
                        toInput_ funcDict n.id n.args n.example
                        |> \(scope, caret) -> Input.initWithFixed (mouseCmd objId) focusCmd objId scope caret
                        |> \res -> Dict.insert n.id res dict2
                    _ -> dict2
                ) dict1
            ) dict
        ) Dict.empty
    }

toInput_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp}
    -> String -> List String -> String -> (List Input.ScopeElement, List Int)
toInput_ funcProp name args example =
    let
        size = List.length args
        inputToString str = Input.fromString funcProp str
            |> Result.toMaybe
            |> Maybe.withDefault [Input.StrElement str]
        exampleList = inputToString example
        parameters = List.indexedMap (\i str ->
                (   Input.Scope Input.defaultScopeDetail (inputToString str)
                ,   {   up = Nothing
                    ,   down = Nothing
                    ,   left = if i == 0 then Nothing else Just (i - 1)
                    ,   right = if i + 1 == size then Nothing else Just (i + 1)
                    }
                )
            ) args
            |> Array.fromList
    in
    (   [   Input.Fixed
            {   text = name
            ,   latex =
                (   Latex.Text () name
                ::  case args of
                    [] -> []
                    _ ->
                        [   Latex.Bracket ()
                            (   List.indexedMap (\i _ -> Latex.Argument () (i+1)) args
                            |>  List.intersperse (Latex.Text () ",")
                            )
                        ]
                )
            ,   params = parameters
            ,   firstNode = if size == 0 then Nothing else Just 0
            ,   lastNode = if size == 0 then Nothing else Just (size - 1)
            }
        ,   Input.StrElement "="
        ,   Input.InnerScope (Input.Scope Input.defaultScopeDetail exampleList)
        ]
    ,   [2,0,1]
    )

update: Event -> Model msg -> (Model msg, String, Cmd msg)
update event model = case event of
    InputEvent id inE -> case Dict.get id model.inputFields of
        Nothing -> (model, "", Cmd.none)
        Just inModel -> Input.update inE inModel
            |> \(newModel, errStr, cmd) ->
                (   {   model
                    |   inputFields = Dict.insert id newModel model.inputFields
                    }
                ,   errStr
                ,   cmd
                )

-- Autogenerate from Model
type Extracted =
    TextValue String
    | IntValue Int
    | MathValue String

view: (Event -> msg) -> Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp}
    -> Model msg -> Html.Html msg
view convert funcProp model =
    node "dialog" [Attr.attribute "open" "true"]
    [   h1 [] [text model.title]
    ,   form [UI.HtmlEvent.onSubmitForm (decoder_ model), Attr.attribute "method" "dialog"]
        (   List.map (\section -> Html.section []
                (   h2 [] [text section.subtitle]
                ::  List.map (listView_ convert funcProp model.inputFields) section.lines
                )
            )
            model.sections
        ++  [   UI.Icon.cancel [UI.Icon.class "clickable", UI.Icon.class "cancelable", UI.HtmlEvent.onClick model.cancel]
            ,   button [Attr.type_ "submit", Attr.class "noDefault"] [UI.Icon.tick [UI.Icon.class "clickable", UI.Icon.class "submitable"]]
            ]
        )
    ]

listView_: (Event -> msg) -> Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp}
    -> Dict.Dict String (Input.Model msg) -> List (Input msg) -> Html.Html msg
listView_ convert funcDict inputs = List.filterMap (\input -> case input of
        Text t -> Just (label [Attr.for t.id] [Html.input [Attr.type_ "text", Attr.name t.id, Attr.id (fieldID t.id)] []])
        Button m -> Just (button [Attr.type_ "button", UI.HtmlEvent.onClick m.event, Attr.class "clickable"] [text m.text])
        Info i -> Just (text i.text)
        Radio r -> r.options
            |> Dict.toList
            |> List.concatMap (\(num, t) -> let n = String.fromInt num in
                let id = r.name ++ n |> fieldID in
                [   Html.br [] []
                ,   Html.input [Attr.type_ "radio", Attr.name r.name, Attr.id id, Attr.value n] []
                ,   Html.label [Attr.for id, Attr.class "clickable"] [t]
                ]
            )
            |> span [Attr.class "radioSelection"] |> Just
        MathInput n -> Dict.get n.id inputs
            |> Maybe.map (Input.view (InputEvent n.id >> convert) funcDict [])
        ParameterInput n -> Dict.get n.id inputs
            |> Maybe.map (Input.view (InputEvent n.id >> convert) funcDict [])
        Link l -> Just (a [Attr.class "clickable", Attr.target "_blank", Attr.href l.url] [text l.url])
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
                    MathInput n -> case Dict.get n.id model.inputFields of
                        Nothing -> dict
                        Just inField -> case Input.toString inField of
                            Err errStr -> Decode.fail errStr
                            Ok str -> Decode.map (Dict.insert n.id (MathValue str)) dict
                    ParameterInput n -> case Dict.get n.id model.inputFields of
                        Nothing -> dict
                        Just inField -> case Input.toString inField of
                            Err errStr -> Decode.fail errStr
                            Ok str  -> Decode.map (Dict.insert n.id (MathValue str)) dict
                    _ -> dict
            ) d1 lines
        )
        d section.lines
    )
    (Decode.succeed Dict.empty)
    |> Decode.map model.success

fieldID: String -> String
fieldID = (++) "dialog_"