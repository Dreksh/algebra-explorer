module Components.Rules exposing (Model, Event(..), Parameters, Topic, init,
    addTopic, deleteTopic, addSources, topicDecoder,
    replaceGlobalVar, evaluateStr,
    menuTopics,
    encode, decoder, encodeParameters, parameterDecoder
    )

import Dict
import Html exposing (a, h3, p, text)
import Html.Attributes exposing (class, title)
import Json.Decode as Dec
import Json.Encode as Enc
import Set
-- Ours
import Helper
import Algo.Matcher as Matcher
import Algo.Math as Math
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
    ,   topics: Dict.Dict String (LoadState_ Topic)
    }

type LoadState_ obj =
    NotInstalled_ String
    | Installed_ (Maybe String) obj

type Event treeState =
    Apply (Parameters treeState)
    | Group
    | Ungroup
    | NumericalSubstitution Float
    | Substitute
    | Download String
    | Evaluate

init: Model
init =
    {   functions = Dict.fromList
            [   ("+",({arguments = 2, associative = True, commutative = True, javascript = InfixOp "+"},1))
            ,   ("*",({arguments = 2, associative = True, commutative = True, javascript = InfixOp "*"},1))
            ,   ("-",({arguments = 1, associative = False, commutative = False, javascript = PrefixOp "-"},1))
            ,   ("/",({arguments = 1, associative = False, commutative = False, javascript = PrefixOp "1/"},1))
            ]
    ,   constants = Dict.empty
    ,   topics = Dict.empty
    }

{-
## Topics
-}

addTopic: Topic -> Model -> Result String Model
addTopic topic m = let model = deleteTopic topic.name m in -- Clear Existing topic
    topic.functions
    |> Helper.resultDict (\name props dict -> case Dict.get name dict of
        Nothing -> Ok (Dict.insert name (props, 1) dict)
        Just (p, count) -> if p.arguments == props.arguments && p.associative == props.associative && p.commutative == props.commutative
            then Ok (Dict.insert name (p, count + 1) dict)
            else Err ("'" ++ name ++ "' differs from existing definition from other topics")
    )
    model.functions
    |> (\res -> case res of
        Err errStr -> Err errStr
        Ok functions -> topic.constants
            |> Helper.resultDict (\name js dict -> case Dict.get name dict of
                Nothing -> Ok (Dict.insert name (js, 1) dict)
                Just (prev, count) -> if prev /= js then Err (name ++ " has different javascript values")
                    else Ok (Dict.insert name (prev, count + 1) dict)
            )
            model.constants
            |> Result.map (\constants ->
                {   model
                |   functions = functions
                ,   constants = constants
                ,   topics = case Dict.get topic.name model.topics of
                    Nothing -> Dict.insert topic.name (Installed_ Nothing topic) model.topics
                    Just (NotInstalled_ url) -> Dict.insert topic.name (Installed_ (Just url) topic) model.topics
                    Just (Installed_ url _) -> Dict.insert topic.name (Installed_ url topic) model.topics
                }
            )
    )

deleteTopic: String -> Model -> Model
deleteTopic name model = case Dict.get name model.topics of
    Just (Installed_ url topic) ->
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
            |   topics = case url of
                    Nothing -> Dict.remove name model.topics
                    Just existing -> Dict.insert name (NotInstalled_ existing) model.topics
            ,   constants = newConstants
            ,   functions = newFunctions
            }
    _ -> model

addSources: Dict.Dict String String -> Model -> Model
addSources map model =
    {   model
    |   topics = Dict.foldl (\name url dict ->
            case Dict.get name dict of
                Nothing -> Dict.insert name (NotInstalled_ url) dict
                Just existing -> case existing of
                    Installed_ Nothing obj -> Dict.insert name (Installed_ (Just url) obj) dict
                    _ -> dict -- Don't override existing topics
        )
        model.topics
        map
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

type alias SelectedNode_ state = {eq: Int, root: Math.Tree (Matcher.State state), nodes: Set.Set Int, childCount: Int}

menuTopics: (Event state -> msg) -> Model -> Maybe (SelectedNode_ state) -> List (Menu.Part msg)
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
    Dict.foldl (\k t -> (::)
        (case t of
            NotInstalled_ url -> Menu.Section k True
                [   Menu.Content [a [HtmlEvent.onClick (converter (Download url)), class "clickable"] [text "Download"]]
                ]
            Installed_ _ topic -> Menu.Section topic.name True
                ( List.map individualRule topic.rules )
        )
    )
    [coreTopic_ converter selectedNode]
    model.topics
    |> List.reverse

matchRule_: Maybe (SelectedNode_ state) -> Rule_ -> (Bool, Maybe (Parameters state))
matchRule_ selected rule = case selected of
    Nothing -> (True, Nothing)
    Just n -> if List.any (\m -> Matcher.matchNode m.from.root n.root) rule.matches |> not
        then (False, Nothing)
        else
            let
                matches = List.foldl (\m -> Matcher.matchSubtree n.nodes m.from.root n.root
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
    Nothing -> a [title "Cannot be applied"] [text "Apply"]
    Just event -> a [HtmlEvent.onClick (converter event), class "clickable"] [text "Apply"]

coreTopic_: (Event state -> msg) -> Maybe (SelectedNode_ state) -> Menu.Part msg
coreTopic_ converter selected =
    let
        shouldDisplay a conv = a.empty || ( case conv a of
                Just _ -> True
                Nothing -> False
            )
        apply = let applies = {subApply = Nothing, groupApply=Nothing, ungroupApply=Nothing, numApply=Nothing, empty=False, evalApply=Just Evaluate} in
            case selected of
            Nothing -> {applies | empty = True, evalApply=Nothing}
            Just val -> case val.root of
                Math.BinaryNode n -> if not n.associative then {applies | subApply = (Just Substitute)}
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
                        ,   groupApply = if List.length n.children == val.childCount || val.childCount < 2 then Nothing else Just Group -- grouping everything is a noop
                        ,   ungroupApply = if sameBinaryNode then Just Ungroup else Nothing
                        }
                Math.RealNode n -> {applies | numApply = Just (NumericalSubstitution n.value)}
                Math.DeclarativeNode _ -> {applies | subApply = Just Substitute, evalApply = Nothing}
                _ -> {applies | subApply = Just Substitute}
    in
        Menu.Section "Core" True
        [   Menu.Section "Evaluate" (shouldDisplay apply .evalApply)
            [   Menu.Content
                [   applyButton converter apply.evalApply
                ,   h3 [] [text "Convert expression into a single number"]
                ,   p [] [text "If the section does not contain any unknown variables, then the calculator can crunch the numbers to return a value."]
                ]
            ]
        ,   Menu.Section "Number Substitution" (shouldDisplay apply .numApply)
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
## Topic Parser
-}

topicDecoder: Dec.Decoder Topic
topicDecoder = Dec.map3 (\a b c -> (a,b,c))
    (Dec.field "name" Dec.string)
    (Dec.field "functions" (Dec.dict functionDecoder_))
    (Dec.field "constants" (Dec.dict (Dec.string |> Dec.andThen (\str -> if String.left 5 str /= "Math."
        then Dec.fail "Only constants from Math are allowed"
        else if String.dropLeft 5 str |> String.all Char.isAlphaNum then Dec.succeed str
        else Dec.fail "Unknown characters after 'Math.'"
    ))))
    |> Dec.andThen ( \(name, functions, vars) ->
        Dec.field "actions" (Dec.list (ruleDecoder_ functions vars))
        |> Dec.map (\eqs -> Topic name vars functions eqs)
    )

functionDecoder_: Dec.Decoder FunctionProperties_
functionDecoder_ = Dec.map4 FunctionProperties_
    (Dec.field "arguments" Dec.int)
    (Dec.oneOf [Dec.field "associative" Dec.bool, Dec.succeed False])
    (Dec.oneOf [Dec.field "commutative" Dec.bool, Dec.succeed False])
    (Dec.field "javascript" javascriptDecoder_)
    |> Dec.andThen (\f ->
        if (f.associative || f.commutative) && f.arguments /= 2
        then Dec.fail "Associative and Commutative properties only apply to binary functions"
        else if f.associative && not f.commutative
        then Dec.fail "Only associative binary nodes is not supported yet"
        else Dec.succeed f
    )

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
                |> Result.andThen (\(matcher, _) -> case matcher of
                    Matcher.AnyMatcher m -> case Dict.get m.name dict of
                        Just _ -> Err "Parameters are duplicated"
                        Nothing -> Ok
                            (   others ++ [{match = {name = key, root = matcher}, description = description}]
                            ,   case m.arguments of
                                [] -> Dict.insert m.name AnyToken dict
                                n -> Dict.insert m.name (FunctionToken (List.length n)) dict
                            )
                    _ -> Err "Parameters can only be variables or functions"
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

verifyMatch_: Dict.Dict String Token_ -> ((Expression_, Dict.Dict String (Bool, Token_)), (Expression_, Dict.Dict String (Bool, Token_))) -> Dec.Decoder {from: Expression_, to: Expression_}
verifyMatch_ paramDict ((from, fromTok), (to, toTok)) = mergeTokens_ paramDict fromTok
    |> Result.andThen (\fromDict -> if Dict.diff toTok fromDict |> Dict.isEmpty |> not
        then Err "Not enough information for the transformation"
        else Helper.resultDict (\k (lSingle,lTok) set ->
            if not lSingle then Ok set
            else case Dict.get k toTok of
                Just (rSingle, rTok) -> case (lTok, rTok) of
                    ((FunctionToken oArgs), (FunctionToken args)) ->
                        if oArgs == args then Ok (if lSingle && rSingle then Set.insert k set else set)
                        else Err ("Different function signature for '" ++ k ++ "' between from and to")
                    (AnyToken, AnyToken) -> Ok (if lSingle && rSingle then Set.insert k set else set)
                    _ -> Err ("Different usages of '" ++ k ++ "' between from and to")
                _ -> Ok set
        ) Set.empty fromDict
        |> Result.map (\nSet -> {from = {from | root = setOthers_ nSet from.root}, to = {to | root = setOthers_ nSet to.root}} )
    )
    |> Helper.resultToDecoder

setOthers_: Set.Set String -> Matcher.Matcher -> Matcher.Matcher
setOthers_ others m = case m of
    Matcher.AnyMatcher s -> Matcher.AnyMatcher {s | arguments = List.map (setOthers_ others) s.arguments}
    Matcher.RealMatcher _ -> m
    Matcher.ExactMatcher s -> Matcher.ExactMatcher {s | arguments = List.map (setOthers_ others) s.arguments}
    Matcher.DeclarativeMatcher s -> Matcher.DeclarativeMatcher {s | arguments = List.map (setOthers_ others) s.arguments}
    Matcher.CommutativeMatcher s -> case s.others of
        Just _ -> Matcher.CommutativeMatcher {s | arguments = List.map (setOthers_ others) s.arguments}
        Nothing -> List.indexedMap Tuple.pair s.arguments
            |> List.foldl (\(index, n) res -> case n of
                Matcher.AnyMatcher c -> if List.isEmpty c.arguments && Set.member c.name others then Just (index, c.name)
                    else res
                _ -> res
            ) Nothing
            |> (\res -> case res of
                Nothing -> Matcher.CommutativeMatcher {s | arguments = List.map (setOthers_ others) s.arguments}
                Just (index, name) -> Matcher.CommutativeMatcher
                    {   s
                    |   arguments = List.map (setOthers_ others) (List.take index s.arguments ++ List.drop (index+1) s.arguments)
                    ,   others = Just name
                    }
            )

mergeTokens_: Dict.Dict String Token_ -> Dict.Dict String (Bool, Token_) -> Result String (Dict.Dict String (Bool, Token_))
mergeTokens_ parameters new = Helper.resultDict (\name tok dict -> case Dict.get name dict of
        Nothing -> Ok (Dict.insert name (False, tok) dict)
        Just (_, oldTok) -> case (oldTok, tok) of
            ((FunctionToken oArgs), (FunctionToken args)) ->
                if oArgs == args then Ok (Dict.insert name (False, oldTok) dict)
                else Err ("Different function signature for '" ++ name ++ "' detected within parameters")
            (AnyToken, AnyToken) -> Ok (Dict.insert name (False, oldTok) dict)
            _ -> Err ("Different usages of '" ++ name ++ "' detected within parameters")
    )
    new
    parameters

expressionDecoder_: Bool -> Dict.Dict String FunctionProperties_ -> Dict.Dict String String -> Dec.Decoder (Expression_, Dict.Dict String (Bool, Token_))
expressionDecoder_ from functions constants =
    Dec.andThen
    (\str -> case Math.parse str |> Result.andThen (treeToMatcher_ from functions constants) of
        Err errStr -> Dec.fail ("'" ++ str ++ "' is not a valid expression: " ++ errStr)
        Ok (matcher, tokens) -> Dec.succeed ({name = str, root = matcher}, tokens)
    )
    Dec.string

updateTokens_: String -> (Bool, Token_) -> Dict.Dict String (Bool, Token_) -> Result String (Dict.Dict String (Bool, Token_))
updateTokens_ k (single, v) total = case Dict.get k total of
    Nothing -> Ok (Dict.insert k (single, v) total)
    Just (_, other) -> case (other, v) of
        (AnyToken, AnyToken) -> Ok (Dict.insert k (False, other) total)
        (FunctionToken lArgs, FunctionToken rArgs) -> if lArgs == rArgs
            then Ok (Dict.insert k (False, other) total)
            else Err ("'" ++ k ++ "' has different number of arguments")
        _ -> Err ("'" ++ k ++ "' is used as a variable and a function")

treeToMatcher_: Bool -> Dict.Dict String FunctionProperties_ -> Dict.Dict String String -> Math.Tree () -> Result String (Matcher.Matcher, Dict.Dict String (Bool, Token_))
treeToMatcher_ from functions constants root =
    let
        processChildren = Helper.resultList (\child (list, tokens) -> treeToMatcher_ from functions constants child
                |> Result.andThen (\(childMatcher, childToken) -> Helper.resultDict updateTokens_ tokens childToken
                    |> Result.map (\newToken -> (list ++ [childMatcher], newToken))
                )
            )
            ([], Dict.empty)
    in
    case root of
        Math.RealNode s -> Ok (Matcher.RealMatcher {value = s.value}, Dict.empty)
        Math.VariableNode s -> if Dict.member s.name constants
            then Ok (Matcher.ExactMatcher {name = s.name, arguments = []}, Dict.empty)
            else Ok (Matcher.AnyMatcher {name = s.name, arguments = []}, Dict.singleton s.name (True, AnyToken))
        Math.UnaryNode s -> treeToMatcher_ from functions constants s.child
            |> Result.map (\(childMatcher, tokens) -> (Matcher.ExactMatcher {name = s.name, arguments = [childMatcher]}, tokens))
        Math.BinaryNode s -> if s.commutative then processChildren s.children |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children, others = Nothing}, tokens))
            else processChildren s.children |> Result.map (\(children, tokens) -> (Matcher.ExactMatcher {name = s.name, arguments = children}, tokens))
        Math.DeclarativeNode s -> processChildren s.children
            |> Result.map (\(children, tokens) -> (Matcher.DeclarativeMatcher {name = s.name, commutative = True, arguments = children}, tokens)) -- TODO: Not all of them are commutative (like <)
        Math.GenericNode s -> case Dict.get s.name functions of
            Nothing -> processChildren s.children
                |> Result.andThen (\(children, tokens) ->
                    (if from then Matcher.variableArgsOnly children else Ok Dict.empty)
                    |> Result.andThen (\_ -> updateTokens_ s.name (True, List.length children |> FunctionToken) tokens)
                    |> Result.map (\newToks -> (Matcher.AnyMatcher {name = s.name, arguments = children}, newToks))
                )
            Just prop -> if prop.commutative then processChildren s.children |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children, others = Nothing}, tokens))
                else processChildren s.children |> Result.map (\(children, tokens) -> (Matcher.ExactMatcher {name = s.name, arguments = children}, tokens))

{-
## Encoding and Decoding
-}

encode: Model -> Enc.Value
encode model = Enc.object
    [   ("functions", Enc.dict identity (\(prop, count) -> Enc.object [("properties", encodeFProp_ prop), ("count", Enc.int count)] ) model.functions)
    ,   ("constants", Enc.dict identity (\(name, count) -> Enc.object [("name", Enc.string name), ("count", Enc.int count)]) model.constants)
    ,   ("topics", Enc.dict identity (\loadState -> case loadState of
            NotInstalled_ url -> Enc.object [("type", Enc.string "notInstalled"),("url", Enc.string url)]
            Installed_ url topic -> Enc.object
                (   [   ("type", Enc.string "installed")
                    ,   ("topic", encodeTopic_ topic)
                    ]
                |> Helper.maybeAppend (Maybe.map (\str -> ("url", Enc.string str)) url)
                )
            ) model.topics
        )
    ]

encodeFProp_: FunctionProperties_ -> Enc.Value
encodeFProp_ prop = Enc.object
    [   ("arguments", Enc.int prop.arguments)
    ,   ("associative", Enc.bool prop.associative)
    ,   ("commutative", Enc.bool prop.commutative)
    ,   ("javascript", case prop.javascript of
            InfixOp js -> Enc.object [("type", Enc.string "infix"),("symbol",Enc.string js)]
            PrefixOp js ->Enc.object [("type", Enc.string "prefix"),("symbol",Enc.string js)]
            FuncOp js ->Enc.object [("type", Enc.string "function"),("symbol",Enc.string js)]
        )
    ]

encodeTopic_: Topic -> Enc.Value
encodeTopic_ topic = Enc.object
    [   ("name", Enc.string topic.name)
    ,   ("constants", Enc.dict identity Enc.string topic.constants)
    ,   ("functions", Enc.dict identity encodeFProp_ topic.functions)
    ,   ("actions", Enc.list encodeRule_ topic.rules)
    ]

-- WARNING: This does not print out all of the inner state, especailly for expressions (only the name/string form is returned)
encodeRule_: Rule_ -> Enc.Value
encodeRule_ rule = Enc.object
    [   ("title", Enc.string rule.title)
    ,   ("description", Enc.string rule.description)
    ,   ("parameters", Enc.object (List.map (\param -> (param.match.name, Enc.string param.description)) rule.parameters))
    ,   ("matches", Enc.list (\match -> Enc.object [("from", Enc.string match.from.name), ("to", Enc.string match.to.name)]) rule.matches)
    ]

decoder: Dec.Decoder Model
decoder = Dec.map3 (\f c t -> {functions = f, constants = c, topics = t})
    (Dec.field "functions" <| Dec.dict <| Dec.map2 Tuple.pair (Dec.field "properties" functionDecoder_)  (Dec.field "count" Dec.int))
    (Dec.field "constants" <| Dec.dict <| Dec.map2 Tuple.pair (Dec.field "name" Dec.string) (Dec.field "count" Dec.int))
    (Dec.field "topics" <| Dec.dict <| Dec.andThen (\s -> case s of
        "notInstalled" -> Dec.map NotInstalled_ (Dec.field "url" Dec.string)
        "installed" -> Dec.map2 Installed_ (Dec.maybe <| Dec.field "url" Dec.string) (Dec.field "topic" topicDecoder)
        _ -> Dec.fail ("Unknown loadState: " ++ s)
    ) <| Dec.field "type" Dec.string
    )

encodeParameters: (state -> Enc.Value) -> Parameters state -> Enc.Value
encodeParameters convert param = Enc.object
    [   ("title", Enc.string param.title)
    ,   ("parameters", Enc.list (\p -> Enc.object
            [("name", Enc.string p.name),("description", Enc.string p.description),("root",Enc.string p.root),("args",Enc.int p.args)]
        ) param.parameters)
    ,   ("matches", Enc.list (\m -> Enc.object
            [   ("from", Matcher.encodeMatchResult convert m.from)
            ,   ("name", Enc.string m.name)
            ,   ("matcher", Matcher.encodeMatcher m.matcher)
            ]
            )
            param.matches
        )
    ]

parameterDecoder: Dec.Decoder state -> Dec.Decoder (Parameters state)
parameterDecoder innerDec = Dec.map3 (\t p m -> {title = t, parameters = p, matches = m})
    (Dec.field "title" <| Dec.string)
    (Dec.field "paramters" <| Dec.list <| Dec.map4 (\n d r a -> {name = n, description = d, root = r, args = a})
        (Dec.field "name" Dec.string)
        (Dec.field "description" Dec.string)
        (Dec.field "root" Dec.string)
        (Dec.field "args" Dec.int)
    )
    (Dec.field "matches" <| Dec.list <| Dec.map3 (\f n matcher -> {from = f, name = n, matcher = matcher})
        (Dec.field "from" <| Matcher.matchResultDecoder innerDec)
        (Dec.field "name" Dec.string)
        (Dec.field "matcher" Matcher.matcherDecoder)
    )