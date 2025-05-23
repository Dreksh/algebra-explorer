module Components.Rules exposing (Model, Event(..), Parameters, Topic, Rule, Source, init,
    addTopic, deleteTopic, addSources, topicDecoder, loadedTopics, functionProperties,
    evaluateStr, menuTopics, encode, decoder, sourceDecoder
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
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.Menu as Menu
import UI.Menu as Menu

{-
## Modeling rules
-}

type alias FunctionProperties_ =
    {   properties: Math.FunctionProperty
    ,   javascript: Javascript_
    }

type Javascript_ =
    InfixOp String
    | PrefixOp String
    | FuncOp String

type alias Parameter =
    {   name: String
    ,   arguments: Int
    ,   description: String
    }

type alias Rule =
    {   title: String
    ,   description: String
    ,   parameters: Dict.Dict String Parameter
    ,   matches: List {from: {name: String, root: Matcher.Matcher}, to: List {name: String, root: Matcher.Replacement}}
    }

type alias Topic =
    {   name: String
    ,   constants: Dict.Dict String String
    ,   functions: Dict.Dict String FunctionProperties_
    ,   rules: List Rule
    }

type alias Parameters state =
    {   title: String
    ,   parameters: Dict.Dict String Parameter
    ,   matches: List {from: Matcher.MatchResult state, replacements: List {name: String, root: Matcher.Replacement}}
    }

{-
## Elm-y bits
-}
type alias Model =
    {   functions: Dict.Dict String (FunctionProperties_, Int) -- Properties + Number of topics that contain this function
    ,   constants: Dict.Dict String (String, Int) -- Number of topics that rely on the constant
    ,   topics: Dict.Dict String (LoadState_ Topic)
    }

type alias Source =
    {   url: String
    ,   description: String
    }

type LoadState_ obj =
    NotInstalled_ Source
    | Installed_ (Maybe Source) obj

type Event treeState =
    Apply (Parameters treeState)
    | Group Int Int (Set.Set Int) -- eq root children
    | Ungroup Int Int (Set.Set Int) -- eq root selected
    | NumericalSubstitution Int Int Float -- eq root matching value
    | Substitute Int (Set.Set Int) -- eq selected
    | Download String
    | Evaluate Int Int String -- eq nodeID evalString
    | Delete String

init: Model
init =
    {   functions = Dict.fromList
            [   ("+",({properties= Math.BinaryNode {state = (), name = "", associative = True, commutative = True, identity = 0, children = []}, javascript = InfixOp "+"},1))
            ,   ("*",({properties= Math.BinaryNode {state = (), name = "", associative = True, commutative = True, identity = 1, children = []}, javascript = InfixOp "*"},1))
            ,   ("-",({properties= Math.UnaryNode {state = (), name = "", child = Math.RealNode {state = (), value = 0}}, javascript = PrefixOp "-"},1))
            ,   ("/",({properties= Math.UnaryNode {state = (), name = "", child = Math.RealNode {state = (), value = 0}}, javascript = PrefixOp "1/"},1))
            ]
    ,   constants = Dict.empty
    ,   topics = Dict.empty
    }

functionProperties: Model -> Math.FunctionProperties
functionProperties model = Dict.map (\_ (f, _) -> f.properties) model.functions
    |> \dict -> Dict.foldl (\k _ -> Math.addConstant k) dict model.constants

{-
## Topics
-}

addTopic: Topic -> Model -> Result String Model
addTopic topic m = let model = deleteTopic topic.name m in -- Clear Existing topic
    topic.functions
    |> Helper.resultDict (\name props dict -> case Dict.get name dict of
        Nothing -> Ok (Dict.insert name (props, 1) dict)
        Just (p, count) -> if Math.equal (\_ _ -> True) p.properties props.properties && p.javascript == props.javascript
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

loadedTopics: Model -> List Topic
loadedTopics model = Dict.toList model.topics
    |> List.filterMap (\(_, state) -> case state of
        Installed_ _ obj -> Just obj
        _ -> Nothing
    )

addSources: Dict.Dict String {url: String, description: String} -> Model -> Model
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

menuTopics: (Event state -> msg) -> Model -> List (Menu.Part msg)
menuTopics converter model = Dict.foldl (\k t -> (::)
        (case t of
            NotInstalled_ source -> Menu.Section
                {name = k, icon = Just (\c -> Icon.download [HtmlEvent.onClick (converter (Download source.url)), Icon.class "clickable", Icon.class c])}
                [   Menu.Content [p [] [text source.description]]
                ]
            Installed_ s topic -> Menu.Section
                {name = topic.name, icon = Just (\c -> a [HtmlEvent.onClick (converter (Delete topic.name)), class "clickable", class c] [text "x"])}
                (   Menu.Content [p [] [text (Maybe.map .description s |> Maybe.withDefault "<No description provided>")]]
                :: List.map (\rule -> Menu.Section {name = rule.title, icon = Nothing}
                    [  Menu.Content ( List.concat
                        [ [h3 [] [text "Rules"]]
                        , List.map (\match -> p [] [text (match.from.name ++"→"++ (List.map .name match.to |> String.join ", "))]) rule.matches
                        , [   h3 [] [text "Description"], p [] [text rule.description]]
                        ])
                    ]
                )
                    topic.rules
                )
        )
    )
    [   Menu.Section {name = "Core", icon = Nothing}
        [   Menu.Content [p [] [text "Covers the basic interactions in this block representation"]]
        ,   Menu.Section {name = "Evaluate", icon = Nothing}
            [   Menu.Content
                [   h3 [] [text "Convert expression into a single number"]
                ,   p [] [text "If the section does not contain any unknown variables, then the calculator can crunch the numbers to return a value."]
                ]
            ]
        ,   Menu.Section {name = "Number Substitution", icon = Nothing}
            [   Menu.Content
                [   h3 [] [text "Given x=y, f(x)=f(y)"]
                ,   p [] [text "Modify the number based on some calculation. Use this to split the number up into small things, i.e. using 2+3=5 to make 5 into 2+3"]
                ]
            ]
        ,   Menu.Section {name = "Substitute", icon = Nothing}
            [   Menu.Content
                [   h3 [] [text "Given x=y, f(x)=f(y)"]
                ,   p [] [text "Since the equation provided means that both sides have the same value, the statement will remain true when replacing all occurances with one by the other."]
                ]
            ]
        ,   Menu.Section {name = "Group", icon = Nothing}
            [   Menu.Content
                [   h3 [] [text "Focus on a specific part"]
                ,   p [] [text "Associative operators can be done in any order. These include Addition and Multiplication. The parts that are in focus will be brought together."]
                ]
            ]
        ,   Menu.Section {name = "Ungroup", icon = Nothing}
            [   Menu.Content
                [   h3 [] [text "Return the group with the rest"]
                ,   p [] [text "Associative operators can be done in any order. These include Additional and Multiplication. The parts that were in focus will be back with the other to see "]
                ]
            ]
        ]
    ]
    model.topics
    |> List.reverse

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
        if Dict.size (Dict.diff vars functions) /= Dict.size vars then Dec.fail "Can't have a variable named as a function as well"
        else let knownProps = Dict.map (\_ -> .properties) functions |> \dict -> Dict.foldl (\k _ -> Math.addConstant k) dict vars in
            Dec.field "actions" (Dec.list (ruleDecoder_ knownProps))
            |> Dec.map (\eqs -> Topic name vars functions eqs)
    )

functionDecoder_: Dec.Decoder FunctionProperties_
functionDecoder_ = Dec.map2 FunctionProperties_
    Math.functionPropertyDecoder
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

parameterDecoder_: Math.FunctionProperties -> Dec.Decoder (Dict.Dict String Parameter, Dict.Dict String (Int, Bool))
parameterDecoder_ knownFuncs = Dec.keyValuePairs Dec.string
    |> Dec.andThen ( Helper.resultList (\(key, description) (others, dict) -> Math.parse Dict.empty key
            |> Result.andThen (\tree -> case tree of
                Math.VariableNode m -> case Dict.get m.name dict of
                    Just _ -> Err "Parameters are duplicated"
                    Nothing -> case Dict.get m.name knownFuncs of
                        Just _ -> Err "Known constants cannot be used as a parameter"
                        Nothing -> Ok
                            (   Dict.insert m.name {name = key, arguments = 0, description = description} others
                            ,   Dict.insert m.name (0, False) dict
                            )
                Math.GenericNode m -> case Dict.get m.name dict of
                    Just _ -> Err "Parameters are duplicated"
                    Nothing -> case Dict.get m.name knownFuncs of
                        Just _ -> Err "Known function cannot be used as a parameter"
                        Nothing -> Ok
                            (   Dict.insert m.name {name = key, arguments = List.length m.children, description = description} others
                            ,   Dict.insert m.name (List.length m.children, False) dict
                            )
                _ -> Err "Parameters can only be variables or functions"
            )
        )
        (Dict.empty, Dict.empty)
        >> Helper.resultToDecoder
    )

ruleDecoder_: Math.FunctionProperties -> Dec.Decoder Rule
ruleDecoder_ knownFuncs = Dec.map3 (\a b c -> (a,b,c))
    (Dec.field "title" Dec.string)
    (Dec.field "description" Dec.string)
    (Dec.maybe (Dec.field "parameters" (parameterDecoder_ knownFuncs)) |> Dec.map (Maybe.withDefault (Dict.empty, Dict.empty)))
    |> Dec.andThen (\((title, description, (parameters, args))) -> -- Validate that both lhs and rhs has symbols covered
        Dec.list (
            (Dec.field "from" (expressionDecoder_ knownFuncs args))
            |> Dec.andThen (\(from, newArgs) ->
                Dec.field "to"
                (replacementDecoder_ knownFuncs newArgs |> Dec.list)
                |> Dec.map (\to -> let otherSet = Dict.toList newArgs |> List.filter (\(_, (_, oneUse)) -> oneUse) |> List.map Tuple.first |> Set.fromList in
                    {from = {from | root = setOthers_ otherSet from.root}, to = to}
                )
            )
        )
        |> Dec.field "matches"
        |> Dec.map (Rule title description parameters)
    )

setOthers_: Set.Set String -> Matcher.Matcher -> Matcher.Matcher
setOthers_ others m = case m of
    Matcher.AnyMatcher _ -> m
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

expressionDecoder_: Math.FunctionProperties -> Dict.Dict String (Int, Bool) -> Dec.Decoder ({name: String, root: Matcher.Matcher}, Dict.Dict String (Int, Bool))
expressionDecoder_ funcProps args =
    let
        checkUnknowns name numArgs dict = case Dict.get name dict of
            Nothing -> Ok (Dict.insert name (0, True) dict)
            Just (n, _) -> if n == numArgs then Ok (Dict.insert name (n, False) dict)
                else if n == 0 || numArgs == 0 then Err "Variable cannot be used as a function"
                else Err "Functions has different number of inputs"
    in
    Dec.andThen
    (\str -> case Matcher.parseMatcher checkUnknowns funcProps args str of
        Err errStr -> Dec.fail ("'" ++ str ++ "' is not a valid expression: " ++ errStr)
        Ok (matcher, newArgs) -> Dec.succeed ({name = str, root = matcher}, newArgs)
    )
    Dec.string

replacementDecoder_: Math.FunctionProperties -> Dict.Dict String (Int, Bool) -> Dec.Decoder {name: String, root: Matcher.Replacement}
replacementDecoder_ knownFunc args = Dec.string
    |> Dec.andThen (\str ->
        Dict.toList args
        |> List.indexedMap (\index (var, (argNum, _)) -> (var, (argNum, index)))
        |> \argMap -> Matcher.toReplacement knownFunc True (Dict.fromList argMap) str
        |> Result.map (\replacement -> {name = str, root = replacement})
        |> Helper.resultToDecoder
    )

{-
## Encoding and Decoding
-}

encode: Model -> Enc.Value
encode model = Enc.object
    [   ("functions", Enc.dict identity (\(prop, count) -> Enc.object [("properties", encodeFProp_ prop), ("count", Enc.int count)] ) model.functions)
    ,   ("constants", Enc.dict identity (\(name, count) -> Enc.object [("name", Enc.string name), ("count", Enc.int count)]) model.constants)
    ,   ("topics", Enc.dict identity (\loadState -> case loadState of
            NotInstalled_ source -> Enc.object [("type", Enc.string "notInstalled"),("url", Enc.string source.url),("description", Enc.string source.description)]
            Installed_ source topic -> Enc.object
                (   [   ("type", Enc.string "installed")
                    ,   ("topic", encodeTopic_ topic)
                    ]
                ++ (Maybe.map (\s -> [("url", Enc.string s.url),("description", Enc.string s.description)]) source |> Maybe.withDefault [])
                )
            ) model.topics
        )
    ]

encodeFProp_: FunctionProperties_ -> Enc.Value
encodeFProp_ prop = Enc.object
    ( ("javascript", case prop.javascript of
            InfixOp js -> Enc.object [("type", Enc.string "infix"),("symbol",Enc.string js)]
            PrefixOp js ->Enc.object [("type", Enc.string "prefix"),("symbol",Enc.string js)]
            FuncOp js ->Enc.object [("type", Enc.string "function"),("symbol",Enc.string js)]
        )
    ::  Math.encodeFunctionProperty prop.properties
    )

encodeTopic_: Topic -> Enc.Value
encodeTopic_ topic = Enc.object
    [   ("name", Enc.string topic.name)
    ,   ("constants", Enc.dict identity Enc.string topic.constants)
    ,   ("functions", Enc.dict identity encodeFProp_ topic.functions)
    ,   ("actions", Enc.list encodeRule_ topic.rules)
    ]

-- WARNING: This does not print out all of the inner state, especailly for expressions (only the name/string form is returned)
encodeRule_: Rule -> Enc.Value
encodeRule_ rule = Enc.object
    [   ("title", Enc.string rule.title)
    ,   ("description", Enc.string rule.description)
    ,   ("parameters", Enc.object (Dict.toList rule.parameters |> List.map (\(_, p) -> (p.name, Enc.string p.description) )))
    ,   ("matches", Enc.list (\match -> Enc.object [("from", Enc.string match.from.name), ("to", Enc.list (.name >> Enc.string) match.to)]) rule.matches)
    ]

decoder: Dec.Decoder Model
decoder = Dec.map3 (\f c t -> {functions = f, constants = c, topics = t})
    (Dec.field "functions" <| Dec.dict <| Dec.map2 Tuple.pair (Dec.field "properties" functionDecoder_)  (Dec.field "count" Dec.int))
    (Dec.field "constants" <| Dec.dict <| Dec.map2 Tuple.pair (Dec.field "name" Dec.string) (Dec.field "count" Dec.int))
    (Dec.field "topics" <| Dec.dict <| Dec.andThen (\s -> case s of
        "notInstalled" -> Dec.map NotInstalled_ sourceDecoder
        "installed" -> Dec.map2 Installed_ (Dec.maybe <| sourceDecoder) (Dec.field "topic" topicDecoder)
        _ -> Dec.fail ("Unknown loadState: " ++ s)
    ) <| Dec.field "type" Dec.string
    )

sourceDecoder: Dec.Decoder Source
sourceDecoder = Dec.map2 Source
    (Dec.field "url" Dec.string)
    (Dec.field "description" Dec.string)
