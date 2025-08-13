module UI.Actions exposing (
    Event(..), MatchedRule, Action(..), Selection,
    matchRules, replacementsToLatex,
    view, viewContextual
    )

import Dict
import Set
import Html
import Html.Attributes exposing (id, class)
import Html.Keyed exposing (node)
-- Ours
import Algo.Math as Math
import Algo.Matcher as Matcher
import Components.Rules as Rules
import UI.Animation as Animation
import UI.MathIcon as MathIcon
import UI.HtmlEvent as HtmlEvent


-- note that there is no Model in this file
-- it is a helper to bridge the gap between matching Rules and Display
-- and it does not need a Model because it is stored in Display
-- TODO: HOWEVER it would be better to store actions here instead of in Display
--   because it would allow us to use it as an entry-point for tutorials


type Event =
    Commit
    | Reset
    | Apply MatchedRule
    | Group Int (Set.Set Int) -- root children
    | Ungroup Int -- root
    | NumericalSubstitution Int Float -- root matching value
    | Substitute
    | Evaluate Int String -- root evalString

type Action =
    DisplayOnly String
    | Disallowed String
    | Allowed Event String  -- TODO: probably don't need the String in this one because it can be infered from the Event
    -- TODO: add another event here to signal the cooldown period after an action is committed

type alias MatchedRule =
    {   title: String
    ,   parameters: Dict.Dict String Rules.Parameter
    ,   matches: List {from: Matcher.MatchResult Rules.FunctionProp Animation.State, replacements: List {name: String, root: Matcher.Replacement Rules.FunctionProp}}
    }

type alias Selection =
    {   tree: Matcher.Equation Rules.FunctionProp Animation.State
    ,   root: Int
    ,   nodes: Set.Set Int
    }

matchRules: Rules.Model -> Int -> Maybe Selection -> List (String, (List Action))
matchRules rules numEqs selected =
    let
        loadedTopics = Rules.loadedTopics rules

        -- group these together to reuse the result of getNode
        selectedNode = selected
            |> Maybe.andThen (\n -> Matcher.getNode n.root n.tree |> Maybe.map (\m -> (n, m)))
    in
        -- return List instead of Dict because we don't want alphabetical order
        (
            ("Core", coreTopic_ rules numEqs selectedNode |> coreToList_)
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
coreTopic_ rules numEqs selection = case selection of
    Nothing -> CoreTopicAction_ DisplayOnly DisplayOnly DisplayOnly DisplayOnly DisplayOnly
    Just (selected, root) ->
        let
            evaluateAction = case Rules.evaluateStr rules root of
                Err _ -> Disallowed
                Ok str -> Evaluate selected.root str |> Allowed
            substituteAction = if numEqs > 1
                then Substitute |> Allowed
                else Disallowed
            result = CoreTopicAction_ substituteAction Disallowed Disallowed Disallowed evaluateAction
        in
        case root of
            Math.BinaryNode n -> if n.associative == Nothing then result
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
            Math.UnaryNode n -> if n.name /= "-" then result
                else case n.child of
                    Math.RealNode m -> {result | numSubstitute = NumericalSubstitution selected.root -m.value |> Allowed, substitute = Disallowed}
                    _ -> result
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
                        (   MatchedRule
                            rule.title
                            rule.parameters
                            matches
                        )
                    )
                    |> Allowed


-- UI-related

view: (Event -> msg) -> List (String, (List (Action))) -> Html.Html msg
view converter topics =
    Html.ul [class "topics"]
    (   topics |> List.map (\(name, actions) ->
            Html.li [class "topic"]
            [   Html.h2 [] [Html.text name]
            ,   Html.ul [class "actions"]
                (   actions |> List.map (\action ->
                        Html.li [class "action"] (displayAction_ converter action)
                        )
                )
            ]
            )
    )


displayAction_: (Event -> msg) -> Action -> List (Html.Html msg)
displayAction_ converter action = case action of
    DisplayOnly name -> [Html.h3 [class "displayOnly"] [Html.text name]]
    Disallowed name -> [Html.h3 [class "disallowed"] [Html.text name]]
    Allowed event name ->
        let
            unhoverable ev =
                [   class "clickable"
                ,   HtmlEvent.onClick (converter ev)
                ]
            hoverable ev =
                [   class "clickable"
                ,   HtmlEvent.onPointerEnter (converter ev)
                ,   HtmlEvent.onPointerLeave (converter Reset)
                ,   HtmlEvent.onClick (converter Commit)
                ]
        in case event of
            NumericalSubstitution _ _ -> [Html.h3 (unhoverable event) [Html.text name]]
            Substitute -> [Html.h3 (unhoverable event) [Html.text name]]
            Apply matched -> let noParams = Dict.isEmpty matched.parameters in
                case matched.matches of
                [_] -> [Html.h3 ((if noParams then hoverable else unhoverable) event) [Html.text name]]
                _ ->
                    [   Html.h3 [] [Html.text name]
                    ,   Html.ul [class "matches"]
                        (matched.matches |> List.map (\match ->
                            -- only want one match per button so that hover+click is deterministic
                            Html.li [class "match"]
                            [   replacementsToLatex
                                match.replacements
                                (   (Apply {matched | matches = List.singleton match})
                                    |> if noParams then hoverable else unhoverable
                                )
                            ]
                            )
                        )
                    ]
            _ -> [Html.h3 (hoverable event) [Html.text name]]


replacementsToLatex: List {name: String, root: Matcher.Replacement Rules.FunctionProp} -> List (Html.Attribute msg) -> Html.Html msg
replacementsToLatex replacements attrs =
    let toMathIcon = Matcher.replacementToEq >> Rules.toLatex >> MathIcon.static []
    in List.map (.root >> toMathIcon) replacements |> List.intersperse (Html.text ", ") |> Html.span attrs


viewContextual: (Event -> msg) -> List (String, (List Action)) -> List (Html.Html msg)
viewContextual converter topics = topics
    |> List.map (\(_, actions) ->
        (   Html.div
            [class "contextualTopic"]
            (   actions
                |> List.concatMap (\action -> case action of
                    Allowed _ _ -> [displayContextualAction_ converter action]
                    _ -> []
                    )
            )
        )
        )

-- TODO: look at the event, change behaviour depending on how many matches
--   also might need to keep track of long click
displayContextualAction_: (Event -> msg) -> Action -> Html.Html msg
displayContextualAction_ converter action = let inner n = [Html.div [class "contextualActionLabel"] [Html.text n]] in case action of
    DisplayOnly name -> Html.div [] [Html.div [] [Html.text name]]
    Disallowed name -> Html.div [class "disallowed"] (inner name)
    Allowed event name ->
        let
            unhoverable = Html.div
                [   class "contextualAction"
                ,   class "clickable"
                ,   HtmlEvent.onClick (converter event)
                ]
                (inner name)
            hoverable = Html.div
                [   class "contextualAction"
                ,   class "clickable"
                ,   HtmlEvent.onPointerEnter (converter event)
                ,   HtmlEvent.onPointerLeave (converter Reset)
                ,   HtmlEvent.onClick (converter Commit)
                ]
                (inner name)
        in case event of
            NumericalSubstitution _ _ -> unhoverable
            Substitute -> unhoverable
            Apply matchedRule ->
                if Dict.isEmpty matchedRule.parameters |> not
                then unhoverable
                else case matchedRule.matches of
                    [_] -> hoverable
                    _ -> unhoverable
            _ -> hoverable
