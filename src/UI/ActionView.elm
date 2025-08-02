module UI.ActionView exposing (
    Model, Event(..),
    init, update, view, isOpen, hide, encode, decoder, contextualActions
    )

import Dict
import Html
import Html.Attributes exposing (id, class)
import Html.Keyed exposing (node)
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import Components.Actions as Actions
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon

type Event =
    Next Int
    | Toggle Int

type Model =
    Current Int Bool


init: Model
init = Current 0 False

update: Event -> Model -> Model
update e m = case e of
    Next num -> Current num True
    Toggle num -> case m of
        Current cNum show -> if cNum == num then Current cNum (not show)
            else Current num True

hide: Model -> Model
hide (Current num _) = Current num False

isOpen: Model -> Bool
isOpen (Current _ open) = open

-- UI-related

-- could I make this return both all the rules and the contextual toolbar?
-- or could I have a function that returns a list of Actions to feed into both
view: (Event -> msg) -> (Actions.Event -> msg) -> Dict.Dict String (List (Actions.Action)) -> Model -> Html.Html msg
view converter actionConvert actions vModel =
    let
        (current, show) = case vModel of
            Current c s -> (min c (Dict.size actions), s)
    in
        Html.div [id "actions"]
        [   Icon.left ( if current <= 0 then [] else [HtmlEvent.onClick (Next (current - 1) |> converter), Icon.class "clickable"])
        ,   node "ul" []
            (   actions
                |> Dict.foldl (\topic actionList (foldList, idx) ->
                    (   (topic, actionList |> displayTopic_ converter actionConvert current show topic idx) :: foldList
                    ,   idx + 1
                    )
                    ) ([], 0)
                |> Tuple.first
            )
        ,   Icon.right ( if current >= Dict.size actions then [] else [HtmlEvent.onClick (Next (current + 1) |> converter), Icon.class "clickable"])
        ]

displayTopic_: (Event -> msg) -> (Actions.Event -> msg) -> Int -> Bool -> String -> Int -> List Actions.Action -> Html.Html msg
displayTopic_ converter actionConvert selected show title current actions = Html.li []
    [   Html.h2
        ((if current == selected then [class "selected"] else []) ++ [class "clickable", HtmlEvent.onClick (Toggle current |> converter)])
        [Html.text title]
    ,   Html.div (if current == selected && show then [] else [class "closed"])
        (   actions |> List.map (actionToHtml_ actionConvert [])
        )
    ]

actionToHtml_: (Actions.Event -> msg) -> List (Html.Attribute msg) -> Actions.Action -> Html.Html msg
actionToHtml_ actionConvert attrs action = case action of
    Actions.DisplayOnly name -> Html.div [] [Html.text name]
    Actions.Disallowed name -> Html.div [class "disallowed"] [Html.text name]
    Actions.Allowed event name ->
        let
            unhoverable = Html.div
                (attrs ++
                [   class "clickable"
                ,   HtmlEvent.onClick (actionConvert event)
                ] ++ attrs)
                [Html.text name]

            hoverable = Html.div
                ( attrs ++
                [   class "clickable"
                ,   HtmlEvent.onMouseEnter (actionConvert event)
                ,   HtmlEvent.onMouseLeave (actionConvert Actions.Reset)
                ,   HtmlEvent.onClick (actionConvert Actions.Commit)
                ])
                [Html.text name]
        in case event of
            Actions.NumericalSubstitution _ _ -> unhoverable
            Actions.Substitute -> unhoverable
            Actions.Apply application -> if Dict.isEmpty application.parameters
                then hoverable
                else unhoverable
            _ -> hoverable


contextualActions: (Actions.Event -> msg) -> Dict.Dict String (List Actions.Action) -> List (Html.Html msg)
contextualActions actionConvert actionDict =
    actionDict
    |> Dict.foldl (\topic actions foldTopics ->
        (   Html.div
            [class "contextualTopic"]
            (   List.foldl (\action foldActions -> case action of
                    Actions.Allowed aEvent name -> actionToHtml_ actionConvert [class "contextualAction"] action :: foldActions
                    _ -> foldActions
                ) [] actions
            )
        ) :: foldTopics
        ) []

{- Encoding and Decoding -}

encode: Model -> Encode.Value
encode model = case model of
    Current cNum show -> Encode.object [("current", Encode.int cNum),("show", Encode.bool show)]

decoder: Decode.Decoder Model
decoder = Decode.map2 Current
    (Decode.field "current" Decode.int)
    (Decode.field "show" Decode.bool)