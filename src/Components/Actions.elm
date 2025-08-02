module Components.Actions exposing (Event(..), Application, Action(..), Selection, rulesToActions)

import Dict
import Set
-- Ours
import Algo.Math as Math
import Algo.Matcher as Matcher
import UI.Animation as Animation
import Components.Rules as Rules

-- note that there is no Model in this file
-- it is a helper to bridge the gap between matching Rules and Display
-- it could go into Rules.elm but it is nice to have it separated out
-- and it does not need a Model because it can be fully derived from upstream Models


type Event =
    Commit
    | Reset
    | Apply Application
    | Group Int (Set.Set Int) -- root children
    | Ungroup Int -- root
    | NumericalSubstitution Int Float -- root matching value
    | Substitute
    | Evaluate Int String -- root evalString

type Action =
    DisplayOnly String
    | Disallowed String
    | Allowed Event String

type alias Application =
    {   title: String
    ,   parameters: Dict.Dict String Rules.Parameter
    ,   matches: List {from: Matcher.MatchResult Rules.FunctionProp Animation.State, replacements: List {name: String, root: Matcher.Replacement Rules.FunctionProp}}
    }

type alias Selection =
    {   tree: Matcher.Equation Rules.FunctionProp Animation.State
    ,   root: Int
    ,   nodes: Set.Set Int
    }

rulesToActions: Rules.Model -> Int -> Maybe Selection -> Dict.Dict String (List Action)
rulesToActions rModel numEqs selected =
    let
        loadedTopics = Rules.loadedTopics rModel

        -- group these together to reuse the result of getNode
        selectedNode = selected
            |> Maybe.andThen (\n -> Matcher.getNode n.root n.tree |> Maybe.map (\m -> (n, m)))
    in
        Dict.fromList
        (
            ("Core", coreTopic_ rModel numEqs selectedNode |> coreToList_)
        ::  (List.map (\topic ->
                (   topic.name
                ,   List.map (matchRule_ selectedNode) topic.rules
                )
                )
                loadedTopics
            )
        )

-- just a helper class to make code more succinct
type alias CoreTopicAction_ =
    {   substitute: (String -> Action)
    ,   group: (String -> Action)
    ,   ungroup: (String -> Action)
    ,   numSubstitute: (String -> Action)
    ,   evaluate: (String -> Action)
    }

coreToList_: CoreTopicAction_ -> List Action
coreToList_ actions =
    [   actions.evaluate "Evaluate"
    ,   actions.numSubstitute "Expand"
    ,   actions.substitute "Substitution"
    ,   actions.group "Group"
    ,   actions.ungroup "Ungroup"
    ]

coreTopic_: Rules.Model -> Int -> Maybe (Selection, Math.Tree (Matcher.State Animation.State)) -> CoreTopicAction_
coreTopic_ rModel numEqs selection = case selection of
    Nothing -> CoreTopicAction_ DisplayOnly DisplayOnly DisplayOnly DisplayOnly DisplayOnly
    Just (selected, root) ->
        let
            evaluateAction = case Rules.evaluateStr rModel root of
                Err _ -> Disallowed
                Ok str -> Evaluate selected.root str |> Allowed
            substituteAction = if numEqs > 1
                then Substitute |> Allowed
                else Disallowed
            result = CoreTopicAction_ substituteAction Disallowed Disallowed Disallowed evaluateAction
        in
        case root of
            Math.BinaryNode n -> if not n.associative then result
                else
                    let
                        sameBinaryNode = case Dict.get selected.root selected.tree.tracker.parent of
                            Nothing -> False
                            Just parent -> case Matcher.getNode parent selected.tree of
                                Just (Math.BinaryNode m) -> m.name == n.name
                                _ -> False
                        selectedChildren = List.filter (\child -> Set.member (Math.getState child |> Matcher.getID) selected.nodes) n.children |> List.length
                    in
                    let ungroupRes = if sameBinaryNode then {result | ungroup = Ungroup selected.root |> Allowed} else result in
                    if List.length n.children == selectedChildren || selectedChildren < 2 then ungroupRes
                    else {ungroupRes | group = Group selected.root selected.nodes |> Allowed }
            Math.RealNode n -> {result | numSubstitute = NumericalSubstitution selected.root n.value |> Allowed, substitute = Disallowed}
            _ -> result

matchRule_: Maybe (Selection, Math.Tree (Matcher.State Animation.State)) -> Rules.Rule -> Action
matchRule_ selected rule = rule.title |>
    case selected of
        Nothing -> DisplayOnly
        Just (n, root) ->
            let
                matches = List.filterMap (\m -> Matcher.matchSubtree n.nodes m.from.root root
                    |> Maybe.map (\result -> {from = result, replacements = m.to})
                    ) rule.matches
            in
                if List.isEmpty matches then Disallowed
                else
                    (   Apply
                        (   Application
                            rule.title
                            rule.parameters
                            matches
                        )
                    )
                    |> Allowed
