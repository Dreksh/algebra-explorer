module Rules exposing (Model, Event(..), Parameters, Topic, init,
    addTopic, deleteTopic, topicDecoder,
    menuTopics
    )

import Dict
import Html exposing (a, h2, p, text)
import Html.Attributes exposing (class)
import Json.Decode as Dec
import Set
-- Ours
import Helper
import Matcher
import Math
import UI.Menu as Menu
import UI.HtmlEvent as HtmlEvent

{-
## Modeling rules
-}

type alias FunctionProperties_ =
    {   arguments: Int -- Number of arguments it takes
    ,   associative: Bool -- Allows processing in different order
    ,   commutative: Bool -- Allows swapping argument ordering
    }

type Token_ =
    FunctionToken Int -- Number of arguments
    | AnyToken

type alias Expression_ =
    {   name: String
    ,   root: Matcher.Matcher
    ,   tokens: Dict.Dict String Token_
    }

type alias Rule_ =
    {   title: String
    ,   description: String
    ,   reversible: Bool
    ,   lhs: Expression_
    ,   rhs: Expression_
    ,   parameters: List (Expression_, String)
    }

type alias Topic =
    {   name: String
    ,   constants: Set.Set String
    ,   functions: Dict.Dict String FunctionProperties_
    ,   rules: List Rule_
    }

type alias Parameters state =
    {   title: String
    ,   to: Matcher.Matcher
    ,   parameters: List {name: String, description: String, tokens: Set.Set String}
    ,   extracted: Matcher.MatchResult state
    }

{-
## Elm-y bits
-}
type alias Model =
    {   functions: Dict.Dict String (FunctionProperties_, Int) -- Properties + Number of topics that contain this function
    ,   constants: Dict.Dict String Int -- Number of topics that rely on the constant
    ,   topics: Dict.Dict String Topic
    ,   createMode: Bool
    }

type Event treeState =
    Apply (Parameters treeState)
    | Group
    | Ungroup
    | Substitute

init: Model
init =
    {   functions = Dict.empty
    ,   constants = Dict.empty
    ,   topics = Dict.empty
    ,   createMode = False
    }

addTopic: Topic -> Model -> Result String Model
addTopic topic m = let id = String.toLower topic.name in
    let model = deleteTopic id m in -- Clear Existing topic
    topic.functions |> Helper.resultDict (\name props dict -> case Dict.get name dict of
        Nothing -> Ok (Dict.insert name (props, 1) dict)
        Just (p, count) -> if p.arguments == props.arguments && p.associative == props.associative && p.commutative == props.commutative
            then Ok (Dict.insert name (p, count + 1) dict)
            else Err ("'" ++ name ++ "' differs from existing definition from other topics")
    )
    model.functions
    |> (\res -> case res of
        Err errStr -> Err errStr
        Ok functions -> topic.constants |> Helper.resultSet (\name dict -> case Dict.get name dict of
                Nothing -> Ok (Dict.insert name 1 dict)
                Just count -> Ok (Dict.insert name (count + 1) dict)
            )
            model.constants
            |> Result.map (\constants -> {model | functions = functions, constants = constants, topics = Dict.insert id topic model.topics})
    )

deleteTopic: String -> Model -> Model
deleteTopic name model = let id = String.toLower name in
    case Dict.get id model.topics of
        Nothing -> model
        Just topic ->
            let
                newFunctions = topic.functions |> Dict.foldl (\n _ newDict -> case Dict.get n newDict of
                        Nothing -> newDict
                        Just (props, i) -> if i < 2 then Dict.remove n newDict else Dict.insert n (props,i - 1) newDict
                    )
                    model.functions
                newConstants = topic.constants |> Set.foldl (\n newSet -> case Dict.get n newSet of
                        Nothing -> newSet
                        Just i -> if i < 2 then Dict.remove n newSet else Dict.insert n (i - 1) newSet
                    )
                    model.constants
            in
                {   model
                |   topics = Dict.remove id model.topics
                ,   constants = newConstants
                ,   functions = newFunctions
                }

menuTopics: (Event state -> msg) -> Model -> Maybe (Math.Tree (Matcher.State state), Int) -> List (Menu.Part msg)
menuTopics converter model selectedNode =
    let
        ruleName rule = rule.lhs.name ++ (if rule.reversible then "↔" else "→") ++ rule.rhs.name
        clickEvent result rule to =
            let
                parameters = rule.parameters
                    |> List.filter (\(exp, _) -> Dict.diff exp.tokens result.matches |> Dict.isEmpty |> not)
                    |> List.map (\(exp, description) -> {name = exp.name, description = description, tokens = Dict.keys exp.tokens |> Set.fromList})
            in
                HtmlEvent.onClick (Apply {title = ruleName rule, to = to rule |> .root, parameters = parameters, extracted = result} |> converter)
        individualRule display lMatcher rMatcher rule = Menu.Section
            (ruleName rule)
            display
            ([  Menu.Content
                [   h2 [] [text rule.title]
                ,   p [] [text rule.description]
                ]
            ]
            |> Helper.maybeAppend (lMatcher
                |> Maybe.map (\result -> Menu.Content [a [clickEvent result rule .rhs, class "clickable"] [text "Apply forwards"]])
            )
            |> Helper.maybeAppend (rMatcher
                |> Maybe.map (\result -> Menu.Content [a [clickEvent result rule .lhs, class "clickable"] [text "Apply backwards"]])
            ))
    in
    Dict.values model.topics
    |> List.map (\topic -> Menu.Section topic.name True
        (   List.map (\rule -> case selectedNode of
                Nothing -> individualRule True Nothing Nothing rule
                Just (node, _) ->
                    let
                        display = Matcher.matchNode rule.lhs.root node || (rule.reversible && Matcher.matchNode rule.rhs.root node)
                        lhs = Matcher.matchSubtree rule.lhs.root node
                        rhs = if rule.reversible then Matcher.matchSubtree rule.rhs.root node else Nothing
                    in
                        -- TODO: Also display if the root function matches, but no apply button
                        case (lhs, rhs) of
                            (Nothing, Nothing) -> individualRule display Nothing Nothing rule
                            _ -> individualRule True lhs rhs rule
            )
            topic.rules
        )
    )
    |> (::) (coreTopic_ converter selectedNode)

-- TODO: Maybe having information about which nodes are highlighted will be useful
coreTopic_: (Event state -> msg) -> Maybe (Math.Tree s, Int) -> Menu.Part msg
coreTopic_ converter root =
    let
        applyButton e = Menu.Content [a [HtmlEvent.onClick (converter e), class "clickable"] [text "apply"]]
        (subApply, groupApply, ungroupApply) = case root of
            Nothing -> (Nothing, Nothing, Nothing)
            Just (Math.BinaryNode n, childCount) -> if not n.associative then (Just (applyButton Substitute), Nothing, Nothing)
                else
                    let
                        sameBinaryNode = List.any
                            (\child -> case child of
                                Math.BinaryNode m -> n.name == m.name
                                _ -> False
                            )
                            n.children
                    in
                    (   Just (applyButton Substitute)
                    ,   if List.length n.children == childCount || childCount < 2 then Nothing else Just (applyButton Group) -- grouping everything is a noop
                    ,   if sameBinaryNode then Just (applyButton Ungroup) else Nothing
                    )
            _ -> (Just (applyButton Substitute), Nothing, Nothing)
    in
        Menu.Section "Core" True
        [   Menu.Section "Substitute" True
            (   [   Menu.Content
                    [   h2 [] [text "Given x=y, f(x)=f(y)"]
                    ,   p [] [text "Since the equation provided means that both sides have the same value, the statement will remain true when replacing all occurances with one by the other."]
                    ]
                ]
            |> Helper.maybeAppend subApply
            )
        ,   Menu.Section "Group" True
            (   [   Menu.Content
                    [   h2 [] [text "Focus on a specific part"]
                    ,   p [] [text "Associative operators can be done in any order. These include Addition and Multiplication. The parts that are in focus will be brought together."]
                    ]
                ]
            |> Helper.maybeAppend groupApply
            )
        ,   Menu.Section "Ungroup" True
            (   [   Menu.Content
                    [   h2 [] [text "Return the group with the rest"]
                    ,   p [] [text "Associative operators can be done in any order. These include Additional and Multiplication. The parts that were in focus will be back with the other to see "]
                    ]
                ]
            |> Helper.maybeAppend ungroupApply
            )
        ]

{-
## Parser
-}

topicDecoder: Dec.Decoder Topic
topicDecoder = Dec.map3 (\a b c -> (a,b,c))
    (Dec.field "name" Dec.string)
    (Dec.field "functions" functionDecoder_)
    (Dec.field "constants" (Dec.list Dec.string))
    |> Dec.andThen ( \(name, functions, variables) -> let vars = Set.fromList variables in
        Dec.field "equations" (Dec.list (ruleDecoder_ functions vars))
        |> Dec.map (\eqs -> Topic name vars functions eqs)
    )

functionDecoder_: Dec.Decoder (Dict.Dict String FunctionProperties_)
functionDecoder_ = Dec.dict
    <| Dec.map3 FunctionProperties_
        (Dec.field "arguments" Dec.int)
        (Dec.field "associative" Dec.bool)
        (Dec.field "commutative" Dec.bool)

ruleDecoder_: Dict.Dict String FunctionProperties_ -> Set.Set String -> Dec.Decoder Rule_
ruleDecoder_ functions constants = Dec.map6 (\a b c d e f -> ((a,b,c),(d,e,f)))
    (Dec.field "title" Dec.string)
    (Dec.field "description" Dec.string)
    (Dec.maybe (Dec.field "reversible" Dec.bool) |> Dec.map (Maybe.withDefault False))
    (Dec.field "lhs" (Dec.string |> Dec.andThen (parseExpression_ functions constants >> Helper.resultToDecoder)))
    (Dec.field "rhs" (Dec.string |> Dec.andThen (parseExpression_ functions constants >> Helper.resultToDecoder)))
    (Dec.maybe (Dec.field "parameters" (
        Dec.keyValuePairs Dec.string
        |>  Dec.andThen (
            Helper.resultList (\(key, description) (others, dict) ->
                parseExpression_ functions constants key
                |> Result.andThen (\exp ->
                    Helper.resultDict (\name tok newDict -> case Dict.get name newDict of
                        Nothing -> Ok (Dict.insert name tok newDict)
                        Just oldTok -> case (oldTok, tok) of
                            ((FunctionToken oArgs), (FunctionToken args)) ->
                                if oArgs == args then Ok newDict
                                else Err ("Different function signature for '" ++ name ++ "' detected within parameters")
                            (AnyToken, AnyToken) -> Ok newDict
                            _ -> Err ("Different usages of '" ++ name ++ "' detected within parameters")
                    )
                    dict
                    exp.tokens
                    |> Result.map (\newDict -> (others ++ [(exp, description)], newDict))
                )
            )
            ([], Dict.empty)
            >> Helper.resultToDecoder
        )))
        |> Dec.map (Maybe.withDefault ([], Dict.empty))
    )
    |> Dec.andThen (\((title, description, reversible), (originalLhs, originalRhs, (parameters, pDict))) -> -- Validate that both lhs and rhs has symbols covered
        extractUniqueTokens_ functions constants pDict originalLhs.tokens
        |> Result.andThen (\lhs ->
            extractUniqueTokens_ functions constants pDict originalRhs.tokens
            |> Result.andThen (\rhs ->
                Set.union (Dict.keys lhs |> Set.fromList) (Dict.keys rhs |> Set.fromList)
                |> Helper.resultSet
                    (\name _ -> case (Dict.get name lhs, Dict.get name rhs) of
                        (Nothing, Nothing) -> Err ("Something went wrong, unable to find name '" ++ name ++ "'in lhs nor rhs")
                        (Nothing, Just _) -> Ok ()
                        (Just _, Nothing) -> Ok ()
                        (Just AnyToken, Just AnyToken) -> Ok ()
                        (Just (FunctionToken lArgs), Just (FunctionToken rArgs)) ->
                            if lArgs == rArgs then Ok ()
                            else Err ("inconsistent function signature for '" ++ name ++ "' in lhs and rhs")
                        _ -> Err ("lhs and rhs are treating '" ++ name ++ "' differently")
                    )
                    ()
                |> Result.map (\_ -> Rule_ title description reversible originalLhs originalRhs parameters)
            )
        )
        |> Helper.resultToDecoder
    )

extractUniqueTokens_: Dict.Dict String FunctionProperties_ -> Set.Set String -> Dict.Dict String Token_ -> Dict.Dict String Token_ -> Result String (Dict.Dict String Token_)
extractUniqueTokens_ functions constants parameters = Helper.resultDict
    (\name token dict ->
        if Dict.member name functions
        then case token of
            AnyToken -> Err ("'" ++ name ++ "' is use as a variable, even though it is defined as a function")
            FunctionToken _ -> Ok dict  -- This was verified in parse expression
        else if Set.member name constants
        then case token of
            FunctionToken _ -> Err ("'" ++ name ++ "' is used as a function, even though it is defined as a constant")
            AnyToken -> Ok dict
        else case (Dict.get name parameters, token) of
            (Nothing, _) -> Ok (Dict.insert name token dict)
            (Just (AnyToken), AnyToken) -> Ok dict
            (Just (FunctionToken tArgs), FunctionToken args) -> if tArgs == args
                then Ok dict
                else Err ("Inconsistent function signature '" ++ name ++ "' between expression and parameter")
            _ -> Err ("Inconsistent usage of '" ++ name ++ "' between expression and parameter")
    )
    Dict.empty

parseExpression_: Dict.Dict String FunctionProperties_ -> Set.Set String -> String -> Result String Expression_
parseExpression_ functions constants str = case Math.parse str of
    Err errStr -> Err ("'" ++ str ++ "' is not a valid expression: " ++ errStr)
    Ok root -> treeToMatcher_ functions constants root
        |> Result.map (\(matcher, tokens) -> {name = str, root = matcher, tokens = tokens})

treeToMatcher_: Dict.Dict String FunctionProperties_ -> Set.Set String -> Math.Tree () -> Result String (Matcher.Matcher, Dict.Dict String Token_)
treeToMatcher_ functions constants root = case root of
    Math.RealNode s -> Ok (Matcher.RealMatcher {value = s.value}, Dict.empty)
    Math.VariableNode s -> if Set.member s.name constants
        then Ok (Matcher.ExactMatcher {name = s.name, arguments = []}, Dict.empty)
        else Ok (Matcher.AnyMatcher {name = s.name, arguments = []}, Dict.singleton s.name AnyToken)
    Math.UnaryNode s -> treeToMatcher_ functions constants s.child
        |> Result.map (\(childMatcher, tokens) -> (Matcher.ExactMatcher {name = s.name, arguments = [childMatcher]}, tokens))
    Math.BinaryNode s -> processChildren_ functions constants s.children
        |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children}, tokens)) -- TODO: Switch to CommutativeAssociativeMatcher
    Math.DeclarativeNode s -> processChildren_ functions constants s.children
        |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children}, tokens))
    Math.GenericNode s -> case Dict.get s.name functions of
        Nothing -> processChildren_ functions constants s.children
            |> Result.map (\(children, tokens) ->
                (Matcher.AnyMatcher {name = s.name, arguments = children}, Dict.insert s.name (List.length children |> FunctionToken) tokens)
            )
        Just prop -> if prop.associative || prop.commutative
            then processChildren_ functions constants s.children
                |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children}, tokens))
            else processChildren_ functions constants s.children
                |> Result.map (\(children, tokens) -> (Matcher.ExactMatcher {name = s.name, arguments = children}, tokens))

processChildren_: Dict.Dict String FunctionProperties_ -> Set.Set String -> List (Math.Tree ()) -> Result String (List Matcher.Matcher, Dict.Dict String Token_)
processChildren_ functions constants = Helper.resultList (\child (list, tokens) -> treeToMatcher_ functions constants child
        |> Result.andThen (\(childMatcher, childToken) -> Helper.resultDict
            (\k v total -> case Dict.get k total of
                Nothing -> Ok (Dict.insert k v total)
                Just other -> case (other, v) of
                    (AnyToken, AnyToken) -> Ok total
                    (FunctionToken lArgs, FunctionToken rArgs) -> if lArgs == rArgs
                        then Ok total
                        else Err ("'" ++ k ++ "' has different number of arguments")
                    _ -> Err ("'" ++ k ++ "' is used as a variable and a function")
            )
            tokens
            childToken
            |> Result.map (\newToken -> (list ++ [childMatcher], newToken))
        )
    )
    ([], Dict.empty)