module UI.ActionView exposing (view)

import Dict
import Html
import Html.Attributes exposing (attribute, id, class)
import Html.Keyed exposing (node)
import Set
-- Ours
import Algo.History as History
import Algo.Matcher as Matcher
import Algo.Math as Math
import Components.Display as Display
import Components.Rules as Rules
import UI.HtmlEvent as HtmlEvent

type State =
    DisplayOnly
    | Disallowed
    | Allowed (Rules.Event Display.State)

type alias SelectedNode_ =
    {   eq: Int
    ,   root: Int
    ,   tree: Math.Tree (Matcher.State Display.State)
    ,   selected: Set.Set Int
    ,   nodes: Set.Set Int
    }

view: (Rules.Event Display.State -> msg) -> Rules.Model -> Display.Model -> Html.Html msg
view converter rModel dModel = let selectedNode = selectedNode_ dModel in
    Html.div [id "actions"]
    [   node "ul" []
        (   ("Core", coreTopic_ rModel selectedNode |> coreToList_ |> displayTopic_ "Core")
        ::  (   Rules.loadedTopics rModel
            |> List.map (\topic -> (topic.name, List.map (matchRule_ selectedNode) topic.rules |> displayTopic_ topic.name) )
            )
        )
    ]
    |> Html.map converter

displayTopic_: String -> List (String, State) -> Html.Html (Rules.Event Display.State)
displayTopic_ title actions = Html.li []
    [   Html.h2 [] [Html.text title]
    ,   Html.div []
        (   List.map (\(name, state) -> Html.a
                (case state of
                    DisplayOnly -> []
                    Disallowed -> [attribute "disabled" ""]
                    Allowed event -> [HtmlEvent.onClick event, class "clickable"]
                )
                [Html.text name]
            )
            actions
        )
    ]

selectedNode_: Display.Model -> Maybe SelectedNode_
selectedNode_ model = model.selected
    |> Maybe.andThen (\(eqNum, ids) -> Dict.get eqNum model.equations
        |> Maybe.andThen (History.current >> Matcher.selectedSubtree ids >> Result.toMaybe)
        |> Maybe.map (\(root, nodes, tree) -> {eq = eqNum, root = root, nodes = nodes, selected = ids, tree = tree})
    )

type alias CoreTopicState =
    {   substitute: State
    ,   group: State
    ,   ungroup: State
    ,   numSubstitute: State
    ,   evaluate: State
    }

coreTopic_: Rules.Model -> Maybe SelectedNode_ -> CoreTopicState
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

matchRule_: Maybe SelectedNode_ -> Rules.Rule -> (String, State)
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