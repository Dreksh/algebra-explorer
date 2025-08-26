module UI.Actions exposing (
    Event(..), MatchedRule, SingleMatch, Action(..), Selection,
    matchRules, matchToLatex, viewContextual
    )

import Dict
import Set
import Html
import Html.Attributes exposing (class)
-- Ours
import Algo.Math as Math
import Algo.Matcher as Matcher
import Components.Latex as Latex
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
    | NumericalSubstitution Int Float -- root matching value
    | Substitute
    | Evaluate Int String -- root evalString

type Action =
    DisplayOnly String
    | Disallowed String
    | Allowed Event String  -- TODO: probably don't need the String in this one because it can be infered from the Event

type alias MatchedRule =
    {   title: String
    ,   parameters: Dict.Dict String Rules.Parameter
    ,   matches: List SingleMatch
    }
type alias SingleMatch =
    {   from: Matcher.MatchResult Rules.FunctionProp Animation.State
    ,   fromLatex: Latex.Model ()
    ,   replacements: List {name: String, root: Matcher.Replacement Rules.FunctionProp, latex: Latex.Model ()}
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
    ,   numSubstitute: (String -> Action)
    ,   evaluate: (String -> Action)
    }

coreToList_: CoreTopicAction_ -> List Action
coreToList_ actions =
    [   actions.evaluate "Evaluate"
    ,   actions.numSubstitute "Expand"
    ,   actions.substitute "Substitution"
    ]

coreTopic_: Rules.Model -> Int -> Maybe (Selection, Math.Tree (Matcher.State Animation.State)) -> CoreTopicAction_
coreTopic_ rules numEqs selection = case selection of
    Nothing -> CoreTopicAction_ DisplayOnly DisplayOnly DisplayOnly
    Just (selected, root) ->
        let
            evaluateAction = case Rules.evaluateStr rules root of
                Err _ -> Disallowed
                Ok str -> Evaluate selected.root str |> Allowed
            substituteAction = if numEqs > 1
                then Substitute |> Allowed
                else Disallowed
            result = CoreTopicAction_ substituteAction Disallowed evaluateAction
        in
        case root of
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
                    |> Maybe.map (\result -> {from = result, replacements = m.to, fromLatex = m.from.latex})
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
                            [   matchToLatex
                                (   (Apply {matched | matches = List.singleton match})
                                    |> if noParams then hoverable else unhoverable
                                )
                                match
                            ]
                            )
                        )
                    ]
            _ -> [Html.h3 (hoverable event) [Html.text name]]

matchToLatex: List (Html.Attribute msg) -> SingleMatch -> Html.Html msg
matchToLatex attrs match =
    match.fromLatex
    ++ (    Latex.SymbolPart {state=(), style=Just Latex.Faded} Latex.RightArrow
        ::  (   List.map .latex match.replacements
            |> List.intersperse [Latex.Text {state=(), style=Just Latex.Faded} ", "]
            |> List.concat
            )
    )
    |> MathIcon.static []
    |> \child -> Html.span attrs [child]

viewContextual: (Event -> msg) -> Bool -> Bool -> msg -> List (String, (List Action)) -> List (String, Html.Html msg)
viewContextual converter previewOnHover unsuspendOnEnter unsuspendHover topics =
    topics
    |> List.foldl (\(_, actions) foldEvents ->
        -- first convert to a dict so that actions across topics that share the same name can be mapped to the same button
        actions
        |> List.foldl (\action foldEventsInner ->
            case action of
                Allowed event name ->
                    let
                        singleMatches = case event of
                            Apply matched -> matched.matches |> List.map (\match -> Apply {matched | matches = List.singleton match})
                            _ -> [event]
                    in foldEventsInner |> Dict.update name (\prev -> (prev |> Maybe.withDefault []) ++ singleMatches |> Just)
                _ -> foldEventsInner
            ) foldEvents
        ) Dict.empty
    |> Dict.toList
    |> List.map (\(name, events) -> let key = name ++ if previewOnHover then "-hover" else "" in
        case events of
            [event] -> (key, displayContextualAction_ converter previewOnHover unsuspendOnEnter unsuspendHover "contextualAction" (Html.text name) event)
            _ -> (key, displayContextualActions_ converter previewOnHover unsuspendOnEnter unsuspendHover name events)
        )

displayContextualAction_: (Event -> msg) -> Bool -> Bool -> msg -> String -> Html.Html msg -> Event -> Html.Html msg
displayContextualAction_ converter previewOnHover unsuspendOnEnter unsuspendHover cls label event =
    let
        inner = [Html.div [class "contextualActionButton"] [label]]

        unhoverable = Html.div
            ([  class cls
            ,   class "clickable"
            ,   HtmlEvent.onClick (converter event)
            ,   HtmlEvent.onPointerEnter unsuspendHover  -- needed to avoid blip in hover
            ]
            )
            inner  -- TODO: add a symbol to indicate that there is a popup modal

        hoverable = Html.div
            (   [   class cls
                ,   HtmlEvent.onClick (converter Commit)
                ] ++
                if previewOnHover
                then
                [   HtmlEvent.onPointerEnter (converter event)
                ,   HtmlEvent.onPointerLeave (converter Reset)
                ,   class "clickable"
                ]
                else
                ([  HtmlEvent.onClick (converter event)
                ,   class "clickableNoHover"
                ]
                ++  if unsuspendOnEnter
                    then [HtmlEvent.onPointerEnter unsuspendHover]
                    else []
                )
            )
            inner
    in case event of
        NumericalSubstitution _ _ -> unhoverable
        Substitute -> unhoverable
        Apply matchedRule ->
            if Dict.isEmpty matchedRule.parameters |> not
            then unhoverable
            else case matchedRule.matches of
                [_] -> hoverable
                _ -> unhoverable  -- note that we should now only ever have one match
        _ -> hoverable

displayContextualActions_: (Event -> msg) -> Bool -> Bool -> msg -> String -> List Event -> Html.Html msg
displayContextualActions_ converter previewOnHover unsuspendOnEnter unsuspendHover name events =
    Html.div [class "contextualSelect", class "contextualAction", HtmlEvent.onPointerEnter unsuspendHover]
    [   Html.div [class "contextualActionButton"]
        [   Html.text name
        ,   Html.div [class "contextualOptions", class "hideScrollbar"]
            (events |> List.map (\event ->
                let
                    (label, cls) = case event of
                        Apply matchedRule ->
                            (   List.head matchedRule.matches
                                |> Maybe.map (matchToLatex [])
                                |> Maybe.withDefault (Html.text name)  -- note that this should never default since one match is the invariant
                            ,   "contextualLatex"
                            )
                        _ -> (Html.text name, "contextualText")
                in
                    displayContextualAction_ converter previewOnHover unsuspendOnEnter unsuspendHover cls label event
                )
            )
        ]
    ]
