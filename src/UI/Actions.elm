module UI.Actions exposing (
    Event(..), MatchedRule, SingleMatch, Action(..), Selection,
    matchRules, matchToLatex, view, viewSubactions
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
import UI.Icon as Icon
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
    | Substitute (List (Matcher.Equation Rules.FunctionProp Animation.State))
    | Evaluate Int String -- root evalString
    | ShowSubactions (List MatchedRule)
    | HideSubactions

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
    {   eq: Matcher.Equation Rules.FunctionProp Animation.State
    ,   otherEqs: List (Matcher.Equation Rules.FunctionProp Animation.State)
    ,   ids: Set.Set Int
    ,   subtree: Math.Tree (Matcher.State Animation.State)
    ,   subtreeId: Int
    }

matchRules: Rules.Model
    -> Set.Set Int
    -> Maybe (Matcher.Equation Rules.FunctionProp Animation.State)
    -> List (Matcher.Equation Rules.FunctionProp Animation.State)
    -> List (String, (List Action))
matchRules rules ids selected others =
    let
        loadedTopics = Rules.loadedTopics rules

        selection = selected |> Maybe.andThen (\eq ->
            Matcher.selectedSubtree ids eq
            |> Result.toMaybe
            |> Maybe.andThen (\(subtreeId, _) ->
                Matcher.getNode subtreeId eq
                |> Maybe.map (\subtree -> Selection eq others ids subtree subtreeId)
                )
            )
    in
        -- return List instead of Dict because we don't want alphabetical order
        (
            ("Core", coreTopic_ rules selection |> coreToList_)
        ::  (List.map (\topic ->
                (   topic.name
                ,   List.map (matchRule_ selection) topic.rules
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

coreTopic_: Rules.Model -> Maybe Selection -> CoreTopicAction_
coreTopic_ rules selection = case selection of
    Nothing -> CoreTopicAction_ DisplayOnly DisplayOnly DisplayOnly
    Just sel ->
        let
            evaluateAction = case Rules.evaluateStr rules sel.subtree of
                Err _ -> Disallowed
                Ok str -> Evaluate sel.subtreeId str |> Allowed

            substituteAction = sel.otherEqs
                |> List.filter (\otherEq -> case Matcher.replaceAllOccurrences sel.ids otherEq sel.eq of
                    Err _ -> False
                    Ok _ -> True
                    )
                |> (\subs -> case subs of
                    [] -> Disallowed
                    _ -> Substitute subs |> Allowed
                    )

            result = CoreTopicAction_ substituteAction Disallowed evaluateAction
        in case sel.subtree of
            Math.RealNode n -> {result | numSubstitute = NumericalSubstitution sel.subtreeId n.value |> Allowed}
            Math.UnaryNode n -> if n.name /= "-" then result
                else case n.child of
                    Math.RealNode m -> {result | numSubstitute = NumericalSubstitution sel.subtreeId -m.value |> Allowed}
                    _ -> result
            _ -> result

matchRule_: Maybe Selection -> Rules.Rule -> Action
matchRule_ selection rule = rule.title |>
    case selection of
        Nothing -> DisplayOnly
        Just sel ->
            let
                matches = List.filterMap (\m -> Matcher.matchSubtree sel.ids m.from.root sel.subtree
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

view: (Event -> msg) -> Bool -> Bool -> msg -> List (String, (List Action)) -> List (String, Html.Html msg)
view converter previewOnHover unsuspendOnEnter unsuspendHover topics =
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
            [event] -> (key, displayAction_ converter previewOnHover unsuspendOnEnter unsuspendHover "action" (Html.text name) event)
            _ -> (key, displayActionMultipleMatches_ converter name events)
        )

viewSubactions: (Event -> msg) -> Bool -> Bool -> msg -> List MatchedRule -> List (Html.Html msg)
viewSubactions converter previewOnHover unsuspendOnEnter unsuspendHover matchedRules = matchedRules
    |> List.map (\matchedRule ->
        let
            (label, cls) =
                (   List.head matchedRule.matches
                    |> Maybe.map (matchToLatex [])
                    |> Maybe.withDefault (Html.text "Invariant violated!")
                ,   "actionLatex"
                )
        in
            displayAction_ converter previewOnHover unsuspendOnEnter unsuspendHover cls label (Apply matchedRule)
        )

displayAction_: (Event -> msg) -> Bool -> Bool -> msg -> String -> Html.Html msg -> Event -> Html.Html msg
displayAction_ converter previewOnHover unsuspendOnEnter unsuspendHover cls label event =
    let

        unhoverable = Html.div
            ([  class cls
            ,   class "clickable"
            ,   HtmlEvent.onClick (converter event)
            ,   HtmlEvent.onPointerEnter unsuspendHover  -- needed to avoid blip in hover
            ]
            )
            [Html.div [class "actionButton"] [label, Html.div [class "actionIcon"] [Icon.popup []]]]

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
            [Html.div [class "actionButton"] [label]]

    in case event of
        NumericalSubstitution _ _ -> unhoverable
        Substitute eqs -> case eqs of
            [_] -> hoverable
            _ -> unhoverable
        Apply matchedRule ->
            if Dict.isEmpty matchedRule.parameters |> not
            then unhoverable
            else case matchedRule.matches of
                [_] -> hoverable
                _ -> unhoverable  -- note that we should now only ever have one match
        _ -> hoverable

displayActionMultipleMatches_: (Event -> msg) -> String -> List Event -> Html.Html msg
displayActionMultipleMatches_ converter name events =
    let
        matchedRules = events |> List.foldr (\event foldList -> case event of
            Apply matchedRule -> matchedRule::foldList
            _ -> foldList  -- this case should never match since only Apply rules can have multiple matches
            ) []
    in
        Html.div
        [   class "actionSelect"
        ,   class "action"
        ,   class "clickableNoCursor"
        ,   HtmlEvent.onPointerEnter (ShowSubactions matchedRules |> converter)
        ,   HtmlEvent.onPointerLeave (HideSubactions |> converter)
        ]
        [   Html.div [class "actionButton"] [ Html.text name ]
        ]
