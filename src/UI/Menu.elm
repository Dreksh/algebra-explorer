module UI.Menu exposing (Model, Event, Part(..),
    init, update, view, rules,
    encode, decoder
    )

import Dict
import Html exposing (a, h1, h3, li, nav, p, text, ul)
import Html.Attributes exposing (class, href, id, title)
import Json.Decode as Decode
import Json.Encode as Encode
import Set
-- Ours
import Components.Latex as Latex
import Components.Rules as Rules
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.MathIcon as MathIcon
import Helper

type alias Model =
    {   shown: Set.Set String
    }

type Part msg =
    Section {name: String, icon: Maybe (String -> Html.Html msg)} (List (Part msg))
    | Content (List (Html.Attribute msg)) (List (Html.Html msg))

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
    Content childrenAttr children -> li childrenAttr children
    Section title children -> let shown = Set.member title.name model.shown in
        li [class "menuSection"]
        [   Html.div
            ([class "menuTitle"] |> Helper.maybeAppend (Helper.maybeGuard shown (class "shown")))
            (   [ h1 [HtmlEvent.onClick (Click title.name |> converter), class "clickable"] [text title.name] ]
                |> Helper.maybeAppend (Maybe.map (\func -> func "menuAction") title.icon)
            )
        ,   ul (if shown then [class "subMenu", class "shown"] else [class "subMenu"]) (children |> List.map (partToHtml_ converter model))
        ]

encode: Model -> Encode.Value
encode model = Encode.set Encode.string model.shown

decoder: Decode.Decoder Model
decoder = Decode.list Decode.string |> Decode.map (\s -> {shown = Set.fromList s})

rules: (Rules.Event -> msg) -> Rules.Model -> List (Part msg)
rules converter model = Dict.foldl (\k t -> (::)
        (case t of
            Rules.NotInstalled source -> Section
                {name = k, icon = Just (\c -> Icon.download [HtmlEvent.onClick (converter (Rules.Download source.url)), Icon.class "clickable", Icon.class c])}
                [   Content [] [a [href source.url, class "clickable"] [text "View source"]]
                ,   Content [] [p [] [text source.description]]
                ]
            Rules.Installed s topic -> Section
                {name = topic.name, icon = Just (\c -> a [HtmlEvent.onClick (converter (Rules.Delete topic.name)), class "clickable", class c] [text "x"])}
                (   [   case s of
                            Just source -> Content [] [a [href source.url, class "clickable"] [text "View source"] ]
                            Nothing -> Content [] [p [] [text "This is an uploaded topic"]]
                    ,   Content [] [p [] [text (Maybe.map .description s |> Maybe.withDefault "<No description provided>")]]
                    ]
                ++ List.map (\rule -> Section {name = rule.title, icon = Nothing}
                    [  Content [] ( List.concat
                        [ [h3 [] [text "Rules"]]
                        , List.map (\match -> p [] [
                            match.from.latex
                            ++ (    Latex.SymbolPart {state=(), style=Just Latex.Faded} Latex.RightArrow
                                ::  (   List.map .latex match.to
                                    |> List.intersperse [Latex.Text {state=(), style=Just Latex.Faded} ", "]
                                    |> List.concat
                                    )
                            )
                            |> MathIcon.static []
                            ]) rule.matches
                        , [   h3 [] [text "Description"], p [] [text rule.description]]
                        ])
                    ]
                )
                    topic.rules
                )
        )
    )
    [   Section {name = "Core", icon = Nothing}
        [   Content [] [p [] [text "Covers the basic interactions in this block representation"]]
        ,   Section {name = "Evaluate", icon = Nothing}
            [   Content []
                [   h3 [] [text "Convert expression into a single number"]
                ,   p [] [text "If the section does not contain any unknown variables, then the calculator can crunch the numbers to return a value."]
                ]
            ]
        ,   Section {name = "Expand", icon = Nothing}
            [   Content []
                [   h3 [] [text "Given x=y, f(x)=f(y)"]
                ,   p [] [text "Modify the number based on some calculation. Use this to split the number up into small things, i.e. using 2+3=5 to make 5 into 2+3"]
                ]
            ]
        ,   Section {name = "Substitute", icon = Nothing}
            [   Content []
                [   h3 [] [text "Given x=y, f(x)=f(y)"]
                ,   p [] [text "Since the equation provided means that both sides have the same value, the statement will remain true when replacing all occurrences with one by the other."]
                ]
            ]
        ]
    ]
    model.topics
    |> List.reverse