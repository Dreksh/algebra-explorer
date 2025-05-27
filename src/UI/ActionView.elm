module UI.ActionView exposing (Model, Event(..), init, update, view, encode, decoder)

import Dict
import Html
import Html.Attributes exposing (attribute, id, class)
import Html.Keyed exposing (node)
import Json.Decode as Decode
import Json.Encode as Encode
import Set
-- Ours
import Algo.History as History
import Algo.Matcher as Matcher
import Algo.Math as Math
import Components.Rules as Rules
import UI.Display as Display
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

-- UI-related

type State =
    DisplayOnly
    | Disallowed
    | Allowed (Rules.Event Display.State)

view: (Rules.Event Display.State -> msg) -> (Event -> msg) -> Rules.Model -> Maybe Display.SelectedNode -> Model -> Html.Html msg
view ruleConvert eventConvert rModel selectedNode vModel =
    let
        loadedTopics = Rules.loadedTopics rModel
        (current, show) = case vModel of
            Current c s -> (min c (List.length loadedTopics), s)
    in
    Html.div [id "actions"]
    [    Icon.left ( if current <= 0 then [] else [HtmlEvent.onClick (Next (current - 1) |> eventConvert), Icon.class "clickable"])
    ,   node "ul" []
        (   ("Core", coreTopic_ rModel selectedNode |> coreToList_ |> displayTopic_ ruleConvert eventConvert current show "Core" 0)
        ::  (   List.indexedMap (\i topic ->
                (   topic.name
                ,   List.map (matchRule_ selectedNode) topic.rules
                    |> displayTopic_ ruleConvert eventConvert current show topic.name (i+1)
                )
                )
                loadedTopics
            )
        )
    ,   Icon.right ( if current >= List.length loadedTopics then [] else [HtmlEvent.onClick (Next (current + 1) |> eventConvert), Icon.class "clickable"])
    ]

displayTopic_: (Rules.Event Display.State -> msg) -> (Event -> msg) -> Int -> Bool -> String -> Int -> List (String, State) -> Html.Html msg
displayTopic_ ruleConvert eventConvert selected show title current actions = Html.li []
    [   Html.h2
        ((if current == selected then [class "selected"] else []) ++ [class "clickable", HtmlEvent.onClick (Toggle current |> eventConvert)])
        [Html.text title]
    ,   Html.div (if current == selected && show then [] else [class "closed"])
        (   List.map (\(name, state) -> Html.a
                (case state of
                    DisplayOnly -> []
                    Disallowed -> [class "disallowed"]
                    Allowed event -> [HtmlEvent.onClick (ruleConvert event), class "clickable"]
                )
                [Html.text name]
            )
            actions
        )
    ]

type alias CoreTopicState =
    {   substitute: State
    ,   group: State
    ,   ungroup: State
    ,   numSubstitute: State
    ,   evaluate: State
    }

coreTopic_: Rules.Model -> Maybe Display.SelectedNode -> CoreTopicState
coreTopic_ rModel selection = case selection of
    Nothing -> CoreTopicState DisplayOnly DisplayOnly DisplayOnly DisplayOnly DisplayOnly
    Just selected ->
        let
            evaluateState = case Rules.evaluateStr rModel selected.tree of
                Err _ -> Disallowed
                Ok str -> Rules.Evaluate selected.eq selected.root str |> Allowed
            result = CoreTopicState (Rules.Substitute selected.eq selected.selected |> Allowed) Disallowed Disallowed Disallowed evaluateState
        in
        case selected.tree of
            Math.BinaryNode n -> if not n.associative then result
                else
                    let
                        sameBinaryNode = List.any
                            (\child -> case child of
                                Math.BinaryNode m -> n.name == m.name
                                _ -> False
                            )
                            n.children
                        selectedChildren = List.filter (\child -> Set.member (Math.getState child |> Matcher.getID) selected.nodes) n.children |> List.length
                    in
                    let ungroupRes = if sameBinaryNode then {result | ungroup = Rules.Ungroup selected.eq selected.root selected.nodes |> Allowed} else result in
                    if List.length n.children == selectedChildren || selectedChildren < 2 then ungroupRes
                    else {ungroupRes | group = Rules.Group selected.eq selected.root selected.nodes |> Allowed }
            Math.RealNode n -> {result | numSubstitute = Rules.NumericalSubstitution selected.eq selected.root n.value |> Allowed, substitute = Disallowed}
            _ -> result

coreToList_: CoreTopicState -> List (String, State)
coreToList_ state =
    [   ("Evaluate", state.evaluate)
    ,   ("Number Substitution", state.numSubstitute)
    ,   ("Substitution", state.substitute)
    ,   ("Group", state.group)
    ,   ("Ungroup", state.ungroup)
    ]

matchRule_: Maybe Display.SelectedNode -> Rules.Rule -> (String, State)
matchRule_ selected rule = (rule.title,
    case selected of
        Nothing -> DisplayOnly
        Just n ->
            let
                matches = List.filterMap (\m -> Matcher.matchSubtree n.nodes m.from.root n.tree
                    |> Maybe.map (\result -> {from = result, replacements = m.to})
                    ) rule.matches
            in
                if List.isEmpty matches then Disallowed
                else
                    Rules.Apply
                    { title = rule.title
                    , parameters = rule.parameters
                    , matches = matches
                    }
                    |> Allowed
    )

{- Encoding and Decoding -}

encode: Model -> Encode.Value
encode model = case model of
    Current cNum show -> Encode.object [("current", Encode.int cNum),("show", Encode.bool show)]

decoder: Decode.Decoder Model
decoder = Decode.map2 Current
    (Decode.field "current" Decode.int)
    (Decode.field "show" Decode.bool)