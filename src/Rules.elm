module Rules exposing (Model, Event(..), Parameters, Topic, init,
    addTopic, deleteTopic, topicDecoder,
    replaceGlobalVar, evaluateStr,
    menuTopics
    )

import Dict
import Html exposing (a, h3, p, text)
import Html.Attributes exposing (class, title)
import Json.Decode as Dec
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
    ,   javascript: Javascript_
    }

type Javascript_ =
    InfixOp String
    | PrefixOp String
    | FuncOp String

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
    ,   parameters: List {match: Expression_, description: String}
    ,   matches: List {from: Expression_, to: Expression_}
    }

type alias Topic =
    {   name: String
    ,   constants: Dict.Dict String String
    ,   functions: Dict.Dict String FunctionProperties_
    ,   rules: List Rule_
    }

type alias Parameters state =
    {   title: String
    ,   parameters: List {name: String, description: String, root: String, args: Int}
    ,   matches: List {from: Matcher.MatchResult state, name: String, matcher: Matcher.Matcher}
    }

{-
## Elm-y bits
-}
type alias Model =
    {   functions: Dict.Dict String (FunctionProperties_, Int) -- Properties + Number of topics that contain this function
    ,   constants: Dict.Dict String (String, Int) -- Number of topics that rely on the constant
    ,   topics: Dict.Dict String Topic
    ,   createMode: Bool
    }

type Event treeState =
    Apply (Parameters treeState)
    | Group
    | Ungroup
    | NumericalSubstitution Float
    | Substitute

init: Model
init =
    {   functions = Dict.empty
    ,   constants = Dict.empty
    ,   topics = Dict.empty
    ,   createMode = False
    }

{-
## Topics
-}

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
        Ok functions -> topic.constants |> Helper.resultDict (\name js dict -> case Dict.get name dict of
                Nothing -> Ok (Dict.insert name (js, 1) dict)
                Just (prev, count) -> if prev /= js then Err (name ++ " has different javascript values")
                    else Ok (Dict.insert name (prev, count + 1) dict)
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
                newConstants = topic.constants |> Dict.foldl (\n _ newSet -> case Dict.get n newSet of
                        Nothing -> newSet
                        Just (js, i) -> if i < 2 then Dict.remove n newSet else Dict.insert n (js, i - 1) newSet
                    )
                    model.constants
            in
                {   model
                |   topics = Dict.remove id model.topics
                ,   constants = newConstants
                ,   functions = newFunctions
                }

{-
## Verification
-}

replaceGlobalVar: Model -> Math.Tree state -> Result String (Math.Tree state)
replaceGlobalVar model root =
    let
        processChildren = Helper.resultList (\child list -> replaceGlobalVar model child |> Result.map (\c -> c::list)) []
            >> Result.map List.reverse
    in
    case root of
        Math.UnaryNode s -> replaceGlobalVar model s.child |> Result.map (\child -> Math.UnaryNode {s | child = child})
        Math.BinaryNode s -> processChildren s.children |> Result.map (\children -> Math.BinaryNode {s | children = children})
        Math.DeclarativeNode s -> processChildren s.children |> Result.map (\children -> Math.DeclarativeNode {s | children = children})
        Math.GenericNode s -> processChildren s.children |> Result.andThen (\children -> case Dict.get s.name model.functions of
            Nothing -> Ok (Math.GenericNode {s | children = children})
            Just (f, _) -> if f.arguments == 2 && (f.associative || f.commutative)
                then Ok (Math.BinaryNode {state = s.state, name = s.name, associative = f.associative, commutative = f.commutative, children = children})
                else if List.length s.children /= f.arguments then Err ("Unexpected number of arguments in " ++ s.name)
                else case children of
                    [child] -> Ok (Math.UnaryNode {state = s.state, name = s.name, child = child})
                    _ -> Ok (Math.GenericNode {s | children = children})
            )
        _ -> Ok root

toJavascriptString_: Model -> String -> List String -> Result String String
toJavascriptString_ model name children = case Dict.get name model.functions of
    Nothing -> Err "Unable to evaluate the unknown function"
    Just (f, _) -> case f.javascript of
        InfixOp jsName -> Ok (String.join jsName children)
        PrefixOp jsName -> if List.length children /= 1 then Err "Prefix can only be for unary operators"
            else Ok (jsName ++ String.join "" children)
        FuncOp jsName -> Ok (jsName ++ "(" ++ String.join "," children ++ ")")

evaluateStr: Model -> Math.Tree s -> Result String String
evaluateStr model root = (
    case root of
        Math.RealNode s -> Ok (String.fromFloat s.value)
        Math.VariableNode s -> case Dict.get s.name model.constants of
            Nothing -> Err "Unable to evaluate an unknown variable"
            Just (str, _) -> Ok str
        Math.UnaryNode s -> evaluateStr model s.child |> Result.andThen (\child -> toJavascriptString_ model s.name [child])
        Math.BinaryNode s -> Helper.resultList (\child list -> evaluateStr model child |> Result.map (\c -> c::list)) [] s.children
            |> Result.andThen (List.reverse >> toJavascriptString_ model s.name)
        Math.DeclarativeNode _ -> Err "Cannot evaluate a declaration"
        Math.GenericNode s -> Helper.resultList (\child list -> evaluateStr model child |> Result.map (\c -> c::list)) [] s.children
            |> Result.andThen (List.reverse >> toJavascriptString_ model s.name)
    ) |> Result.map (\str -> "(" ++ str ++ ")" )

{-
## UI
-}

menuTopics: (Event state -> msg) -> Model -> Maybe (Math.Tree (Matcher.State state), Int) -> List (Menu.Part msg)
menuTopics converter model selectedNode =
    let
        individualRule rule = let (display, params) = matchRule_ selectedNode rule in
            Menu.Section rule.title display
            [  Menu.Content
                (   [   applyButton converter (Maybe.map Apply params)
                    ,   h3 [] [text "Rules"]
                    ]
                    ++ List.map (\match -> p [] [text (match.from.name ++"→"++ match.to.name)]) rule.matches
                    ++ [   h3 [] [text "Description"]
                    ,   p [] [text rule.description]
                    ]
                )
            ]
    in
    Dict.values model.topics
    |> List.map (\topic -> Menu.Section topic.name True
        ( List.map individualRule topic.rules )
    )
    |> (::) (coreTopic_ converter selectedNode)

matchRule_: Maybe (Math.Tree (Matcher.State state), Int) -> Rule_ -> (Bool, Maybe (Parameters state))
matchRule_ selected rule = case selected of
    Nothing -> (True, Nothing)
    Just (node, _) -> if List.any (\m -> Matcher.matchNode m.from.root node) rule.matches |> not
        then (False, Nothing)
        else
            let
                matches = List.foldl (\m -> Matcher.matchSubtree m.from.root node
                    |> Maybe.map (\result -> {from = result, name = m.to.name, matcher = m.to.root})
                    |> Helper.maybeAppend
                    ) [] rule.matches
            in
                if List.isEmpty matches then (True, Nothing)
                else (  True
                    , Just
                        { title = rule.title
                        , parameters = List.map (\p ->
                            {name = p.match.name, description = p.description, root = Matcher.getName p.match.root, args = Matcher.countChildren p.match.root}
                        ) rule.parameters
                        , matches = matches
                        }
                    )

applyButton : (Event state -> msg) -> Maybe (Event state) -> Html.Html msg
applyButton converter e = case e of
    Nothing -> a [title "Cannot be applied"] [text "apply"]
    Just event -> a [HtmlEvent.onClick (converter event), class "clickable"] [text "apply"]

coreTopic_: (Event state -> msg) -> Maybe (Math.Tree s, Int) -> Menu.Part msg
coreTopic_ converter root =
    let
        shouldDisplay a conv = a.empty || ( case conv a of
                Just _ -> True
                Nothing -> False
            )
        apply = let applies = {subApply = Nothing, groupApply=Nothing, ungroupApply=Nothing, numApply=Nothing, empty=False} in
            case root of
            Nothing -> {applies | empty = True}
            Just (Math.BinaryNode n, childCount) -> if not n.associative then {applies | subApply = (Just Substitute)}
                else
                    let
                        sameBinaryNode = List.any
                            (\child -> case child of
                                Math.BinaryNode m -> n.name == m.name
                                _ -> False
                            )
                            n.children
                    in
                    {   applies
                    |   subApply = Just Substitute
                    ,   groupApply = if List.length n.children == childCount || childCount < 2 then Nothing else Just Group -- grouping everything is a noop
                    ,   ungroupApply = if sameBinaryNode then Just Ungroup else Nothing
                    }
            Just (Math.RealNode n, _ ) -> {applies | numApply = Just (NumericalSubstitution n.value)}
            _ -> {applies | subApply = Just Substitute}
    in
        Menu.Section "Core" True
        [   Menu.Section "Number Substitution" (shouldDisplay apply .numApply)
            [   Menu.Content
                [  applyButton converter apply.numApply
                ,   h3 [] [text "Given x=y, f(x)=f(y)"]
                ,   p [] [text "Modify the number based on some calculation. Use this to split the number up into small things, i.e. using 2+3=5 to make 5 into 2+3"]
                ]
            ]
        ,   Menu.Section "Substitute" (shouldDisplay apply .subApply)
            [   Menu.Content
                [  applyButton converter apply.subApply
                ,   h3 [] [text "Given x=y, f(x)=f(y)"]
                ,   p [] [text "Since the equation provided means that both sides have the same value, the statement will remain true when replacing all occurances with one by the other."]
                ]
            ]
        ,   Menu.Section "Group" (shouldDisplay apply .groupApply)
            [   Menu.Content
                [   applyButton converter apply.groupApply
                ,   h3 [] [text "Focus on a specific part"]
                ,   p [] [text "Associative operators can be done in any order. These include Addition and Multiplication. The parts that are in focus will be brought together."]
                ]
            ]
        ,   Menu.Section "Ungroup" (shouldDisplay apply .ungroupApply)
            [   Menu.Content
                [   applyButton converter apply.ungroupApply
                ,   h3 [] [text "Return the group with the rest"]
                ,   p [] [text "Associative operators can be done in any order. These include Additional and Multiplication. The parts that were in focus will be back with the other to see "]
                ]
            ]
        ]

{-
## Parser
-}

topicDecoder: Dec.Decoder Topic
topicDecoder = Dec.map3 (\a b c -> (a,b,c))
    (Dec.field "name" Dec.string)
    (Dec.field "functions" functionDecoder_)
    (Dec.field "constants" (Dec.dict (Dec.string |> Dec.andThen (\str -> if String.left 5 str /= "Math."
        then Dec.fail "Only constants from Math are allowed"
        else if String.dropLeft 5 str |> String.all Char.isAlphaNum then Dec.succeed str
        else Dec.fail "Unknown characters after 'Math.'"
    ))))
    |> Dec.andThen ( \(name, functions, vars) ->
        Dec.field "actions" (Dec.list (ruleDecoder_ functions vars))
        |> Dec.map (\eqs -> Topic name vars functions eqs)
    )

functionDecoder_: Dec.Decoder (Dict.Dict String FunctionProperties_)
functionDecoder_ = Dec.dict
    <| Dec.map4 FunctionProperties_
        (Dec.field "arguments" Dec.int)
        (Dec.oneOf [Dec.field "associative" Dec.bool, Dec.succeed False])
        (Dec.oneOf [Dec.field "commutative" Dec.bool, Dec.succeed False])
        (Dec.field "javascript" javascriptDecoder_)

javascriptDecoder_: Dec.Decoder Javascript_
javascriptDecoder_ = Dec.map2 Tuple.pair
    (Dec.field "type" Dec.string)
    (Dec.field "symbol" Dec.string)
    |> Dec.andThen (\(t, symbol) -> case t of
        "infix" -> if String.all (\s -> String.contains (String.fromChar s) "0123456789+-*/") symbol then Dec.succeed (InfixOp symbol)
            else Dec.fail "Disallowed char in javascript function"
        "prefix" -> if String.all (\s -> String.contains (String.fromChar s) "0123456789+-*/") symbol then Dec.succeed (PrefixOp symbol)
            else Dec.fail "Disallowed char in javascript function"
        "function" -> if String.left 5 symbol /= "Math." then Dec.fail "Only functions from the Math object can be used"
            else if String.dropLeft 5 symbol |> String.all Char.isAlphaNum then Dec.succeed (FuncOp symbol)
            else Dec.fail "Unexpected symbols after 'Math.'"
        _ -> Dec.fail "Unknown type of javascript function"
    )

ruleDecoder_: Dict.Dict String FunctionProperties_ -> Dict.Dict String String -> Dec.Decoder Rule_
ruleDecoder_ functions constants = Dec.map3 (\a b c -> (a,b,c))
    (Dec.field "title" Dec.string)
    (Dec.field "description" Dec.string)
    (Dec.maybe (Dec.field "parameters" (
        Dec.keyValuePairs Dec.string
        |>  Dec.andThen (
            Helper.resultList (\(key, description) (others, dict) -> Math.parse key
                |> Result.andThen (treeToMatcher_ True functions constants)
                |> Result.andThen (\(matcher, tokens) -> case matcher of
                    Matcher.AnyMatcher _ -> Ok (matcher, tokens)
                    _ -> Err "Parameters can only be variables or functions"
                )
                |> Result.andThen (\(matcher, tokens) ->
                    mergeTokens_ dict tokens
                    |> Result.map (\newDict -> (others ++ [{match= {name = key, root = matcher, tokens = tokens}, description = description}], newDict))
                )
            )
            ([], Dict.empty)
            >> Helper.resultToDecoder
        )))
        |> Dec.map (Maybe.withDefault ([], Dict.empty))
    )
    |> Dec.andThen (\((title, description, (parameters, pDict))) -> -- Validate that both lhs and rhs has symbols covered
        Dec.list (
            Dec.map2 Tuple.pair
            (Dec.field "from" (expressionDecoder_ True functions constants))
            (Dec.field "to" (expressionDecoder_ False functions constants))
            |> Dec.andThen (verifyMatch_ pDict)
        )
        |> Dec.field "matches"
        |> Dec.map (Rule_ title description parameters)
    )

verifyMatch_: Dict.Dict String Token_ -> (Expression_, Expression_) -> Dec.Decoder {from: Expression_, to: Expression_}
verifyMatch_ paramDict (from, to) = mergeTokens_ paramDict from.tokens
    |> Result.andThen (\fromDict ->
        if Dict.diff to.tokens fromDict |> Dict.isEmpty then Ok {from = from, to = to}
        else Err "Not enough information for the transformation"
    )
    |> Helper.resultToDecoder

mergeTokens_: Dict.Dict String Token_ -> Dict.Dict String Token_ -> Result String (Dict.Dict String Token_)
mergeTokens_ parameters new = Helper.resultDict (\name tok dict -> case Dict.get name dict of
        Nothing -> Ok (Dict.insert name tok dict)
        Just oldTok -> case (oldTok, tok) of
            ((FunctionToken oArgs), (FunctionToken args)) ->
                if oArgs == args then Ok dict
                else Err ("Different function signature for '" ++ name ++ "' detected within parameters")
            (AnyToken, AnyToken) -> Ok dict
            _ -> Err ("Different usages of '" ++ name ++ "' detected within parameters")
    )
    parameters
    new

expressionDecoder_: Bool -> Dict.Dict String FunctionProperties_ -> Dict.Dict String String -> Dec.Decoder Expression_
expressionDecoder_ from functions constants =
    Dec.andThen
    (\str -> case Math.parse str |> Result.andThen (treeToMatcher_ from functions constants) of
        Err errStr -> Dec.fail ("'" ++ str ++ "' is not a valid expression: " ++ errStr)
        Ok (matcher, tokens) -> Dec.succeed {name = str, root = matcher, tokens = tokens}
    )
    Dec.string

treeToMatcher_: Bool -> Dict.Dict String FunctionProperties_ -> Dict.Dict String String -> Math.Tree () -> Result String (Matcher.Matcher, Dict.Dict String Token_)
treeToMatcher_ from functions constants root =
    let
        processChildren = Helper.resultList (\child (list, tokens) -> treeToMatcher_ from functions constants child
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
    in
    case root of
        Math.RealNode s -> Ok (Matcher.RealMatcher {value = s.value}, Dict.empty)
        Math.VariableNode s -> if Dict.member s.name constants
            then Ok (Matcher.ExactMatcher {name = s.name, arguments = []}, Dict.empty)
            else Ok (Matcher.AnyMatcher {name = s.name, arguments = []}, Dict.singleton s.name AnyToken)
        Math.UnaryNode s -> treeToMatcher_ from functions constants s.child
            |> Result.map (\(childMatcher, tokens) -> (Matcher.ExactMatcher {name = s.name, arguments = [childMatcher]}, tokens))
        Math.BinaryNode s -> processChildren s.children
            |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children}, tokens)) -- TODO: Switch to CommutativeAssociativeMatcher
        Math.DeclarativeNode s -> processChildren s.children
            |> Result.map (\(children, tokens) -> (Matcher.DeclarativeMatcher {name = s.name, commutative = True, arguments = children}, tokens)) -- TODO: Not all of them are commutative (like <)
        Math.GenericNode s -> case Dict.get s.name functions of
            Nothing -> processChildren s.children
                |> Result.andThen (\(children, tokens) ->
                    (if from then Matcher.variableArgsOnly children else Ok Dict.empty)
                    |> Result.map (\_ -> (Matcher.AnyMatcher {name = s.name, arguments = children}, Dict.insert s.name (List.length children |> FunctionToken) tokens))
                )
            Just prop -> if prop.associative || prop.commutative
                then processChildren s.children
                    |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children}, tokens))
                else processChildren s.children
                    |> Result.map (\(children, tokens) -> (Matcher.ExactMatcher {name = s.name, arguments = children}, tokens))
