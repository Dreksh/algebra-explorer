module Components.Rules exposing (Model, Event(..), LoadState(..), Parameter, Topic, Rule, Source, init,
    FunctionProp, negateProp, functionProperties, toLatex, toSymbol, process,
    addTopic, deleteTopic, addSources, topicDecoder, loadedTopics,
    evaluateStr, encode, decoder, sourceDecoder,
    encodeFunctionProp, functionPropDecoder
    )

import Dict
import Json.Decode as Dec
import Json.Encode as Enc
import Set
-- Ours
import Helper
import Algo.Matcher as Matcher
import Algo.Math as Math
import Components.Latex as Latex

{-
## Modeling rules
-}

type alias FunctionProp =
    {   javascript: Maybe Javascript_
    ,   latex: Maybe (Latex.Model ())
    ,   alternativeNames: List String
    }
negateProp: FunctionProp
negateProp =
    {   javascript = Just (PrefixOp "-")
    ,   latex = Just [Latex.Text {state=(), style=Nothing} "-"]
    ,   alternativeNames = ["minus", "subtract"]
    }
divisionProp_: FunctionProp
divisionProp_ =
    {   javascript = Just (PrefixOp "1/")
    ,   latex = Just [Latex.SymbolPart {state=(), style=Nothing} Latex.Division]
    ,   alternativeNames = ["over", "divide"]
    }

type Javascript_ =
    InfixOp String
    | PrefixOp String
    | FuncOp String

type alias Parameter =
    {   name: String
    ,   arguments: List String
    ,   description: String
    ,   example: String
    }

type alias Rule =
    {   title: String
    ,   description: String
    ,   parameters: Dict.Dict String Parameter
    ,   matches: List Pattern
    }
type alias Pattern =
    {   from: {name: String, root: Matcher.Matcher, latex: Latex.Model ()}
    ,   to: List {name: String, root: Matcher.Replacement FunctionProp, latex: Latex.Model ()}
    }

type alias Topic =
    {   name: String
    ,   constants: Dict.Dict String (Maybe String)
    ,   functions: Dict.Dict String {property: Math.FunctionProperty FunctionProp}
    ,   rules: List Rule
    }

{-
## Elm-y bits
-}
type alias Model =
    {   functions: Dict.Dict String {property: Math.FunctionProperty FunctionProp, count: Int}
    ,   constants: Dict.Dict String (Maybe String, Int) -- Number of topics that rely on the constant
    ,   topics: Dict.Dict String (LoadState Topic)
    }

type alias Source =
    {   url: String
    ,   description: String
    ,   preinstall: Bool
    }

type LoadState obj =
    NotInstalled Source
    | Installed (Maybe Source) obj

type Event =
    Download String
    | Delete String

coreFunctions_: Dict.Dict String {property: Math.FunctionProperty FunctionProp}
coreFunctions_ = Dict.fromList
    [   ("+",{property= Math.BinaryNode {state = {javascript = InfixOp "+" |> Just, latex = Just [Latex.Text {state=(), style=Nothing} "+"], alternativeNames = ["plus", "add"]}, name = "", associative = Just 0, commutative = True, children = []}})
    ,   ("*",{property= Math.BinaryNode {state = {javascript = InfixOp "*" |> Just, latex = Just [Latex.SymbolPart {state=(), style=Nothing} Latex.CrossMultiplcation], alternativeNames = ["times", "multiply"]}, name = "", associative = Just 1, commutative = True, children = []}})
    ,   ("-",{property= Math.UnaryNode {state = negateProp, name = "", child = Math.RealNode {state = negateProp, value = 0}}})
    ,   ("/",{property= Math.UnaryNode {state = divisionProp_, name = "", child = Math.RealNode {state = divisionProp_, value = 0}}})
    ,   ("=",{property= Math.DeclarativeNode {state = {javascript = InfixOp "=" |> Just, latex = Just [Latex.Text {state=(), style=Nothing} "="], alternativeNames = ["equals"]}, name = "", children = []}})
    ]

init: Model
init =
    {   functions = Dict.map (\_ p -> {property = p.property, count = 1}) coreFunctions_
    ,   constants = Dict.empty
    ,   topics = Dict.empty
    }

functionProperties: Model -> Dict.Dict String {property: Math.FunctionProperty FunctionProp, count: Int}
functionProperties model = model.constants
    |> Dict.foldl
        (\k (javascript, count) d -> Math.createConstant {javascript = Maybe.map PrefixOp javascript, latex = Just [Latex.Text {state=(), style=Just Latex.Emphasis} k], alternativeNames = []} k
            |> \entry -> Dict.insert k {property = entry, count = count} d
        ) model.functions
    |> \dict -> Dict.foldl (\name symbol d -> Math.createConstant {javascript = Nothing, latex = Just [Latex.SymbolPart {state=(), style=Just Latex.Emphasis} symbol], alternativeNames = []} name
        |> \entry -> Dict.insert name {property = entry, count = 1} d
        ) dict Latex.greekLetters

{- Functions -}

-- Currently x10 of the ones defined in https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
-- Except for "/", where we need it to have less precedence than "*" for proper allocation of brackets
priority_: Math.Tree s -> Int
priority_ root = case Math.getName root of
    "-" -> 20 -- Following the unary operator
    "/" -> 25
    "*" -> 30
    "+" -> 40
    "=" -> 70 -- Equivalent to == in programming languages
    _ -> 10 -- Assume other functions will be called as "func()"

getDivisionProps_: Math.Tree s -> Maybe {name: String, state: s, child: Math.Tree s}
getDivisionProps_ root = case root of
    Math.UnaryNode n -> if n.name == "/" then Just n else Nothing
    _ -> Nothing

process: (state -> List a -> a) -> (String -> a) -> Math.Tree state -> a
process combine convert tree =
    let
        addInverseBrackets parent child =
            if priority_ child > priority_ parent
            then [convert "(", process combine convert child, convert ")"]
            else [process combine convert child]

        addBrackets parent child =
            if priority_ child >= priority_ parent
            then [convert "(", process combine convert child, convert ")"]
            else [process combine convert child]

        infixStr root =
            List.foldl (\elem children ->
                addBrackets root elem
                |> \inner ->
                    if List.isEmpty children then inner
                    else children ++ (convert (Math.getName root) :: inner)
            ) [] (Math.getChildren root)

        plusStr root =
            List.foldl (\elem children ->
                addBrackets root elem
                |> \inner ->
                    if List.isEmpty children then inner
                    else if Math.getName elem == "-" then children ++ inner  -- this abbreviates +- to just -
                    else children ++ (convert (Math.getName root) :: inner)
            ) [] (Math.getChildren root)
        multStr root =
            List.foldl (\elem children -> if List.isEmpty children
                then addBrackets root elem
                else case getDivisionProps_ elem of
                    Just divProp -> addBrackets (Math.UnaryNode divProp) divProp.child
                        |> \inner -> children ++ (convert "/" :: inner)
                    _ -> addBrackets root elem
                        |> \inner -> children ++ (convert (Math.getName root) :: inner)

            ) [] (Math.getChildren root)
    in
        (
            case tree of
            Math.RealNode n -> String.fromFloat n.value |> convert |> List.singleton
            Math.VariableNode n -> [convert (if String.length n.name == 1 then n.name else "\\" ++ n.name)]
            Math.UnaryNode n -> case n.name of
                "-" -> convert "-" :: addInverseBrackets tree n.child
                "/" -> convert "1/" :: addInverseBrackets tree n.child
                _ -> [convert ("\\" ++ n.name ++ "("), process combine convert n.child, convert ")"]
            _ -> case Math.getName tree of
                "+" -> plusStr tree
                "*" -> multStr tree
                "=" -> infixStr tree
                _ ->
                    (convert ("\\" ++ Math.getName tree ++ "("))
                    ::
                    ((Math.getChildren tree |> List.map (process combine convert) |> List.intersperse (convert ",")) ++ [convert ")"])
        )
        |> combine (Math.getState tree)

toLatex: Matcher.Equation FunctionProp s -> Latex.Model (Matcher.State s)
toLatex eq = toLatex_
    (Matcher.getState >> eq.tracker.ops.extract)
    identity
    True eq.root

toLatex_: (a -> Maybe FunctionProp) -> (a -> b) -> Bool -> Math.Tree a -> Latex.Model b
toLatex_ extractor converter complete tree =
    let
        treeState = Math.getState tree |> converter
        funcLatex = Math.getState tree |> extractor |> Maybe.andThen .latex
        genericFunction root = case funcLatex of
            Nothing -> List.foldl (\n list -> toLatex_ extractor converter complete n |> \new -> new :: list) [] (Math.getChildren root)
                |> \list ->
                    [   Latex.Text {state=treeState, style=Just Latex.Emphasis} (Math.getName root)
                    ,   Latex.Bracket {state=treeState, style=Just Latex.Emphasis}
                        (List.intersperse [Latex.Text {state=treeState, style=Just Latex.Emphasis} ","] list |> List.reverse |> List.concat)
                    ]
            Just l -> substituteArgs_ extractor converter complete treeState (Math.getChildren root) l
        bracket parent child = toLatex_ extractor converter complete child
            |> \inner -> if priority_ child >= priority_ parent
                then [Latex.Bracket {state=Math.getState child |> converter, style = Nothing} inner]
                else inner
        infixFunction root = case funcLatex of
            Nothing -> genericFunction root
            Just l -> List.foldl
                (\elem list -> bracket root elem
                    |> \inner -> if List.isEmpty list
                        then inner
                        else list ++ (Latex.map (\_ -> treeState) l) ++ inner
                )
                [] (Math.getChildren root)
    in
    case tree of
        Math.RealNode n -> [ String.fromFloat n.value |> Latex.Text {state=treeState, style=Nothing} ]
        Math.VariableNode n -> case funcLatex of
            Nothing -> [Latex.Text {state=treeState, style=Nothing} n.name]
            Just l -> Latex.map (\_ -> treeState) l
        Math.UnaryNode n -> case n.name of
            "-" -> toLatex_ extractor converter complete n.child
                |> \inner -> if priority_ n.child > priority_ tree
                    then [Latex.Text {state=treeState, style=Nothing} "-", Latex.Bracket {state=Math.getState n.child |> converter, style=Nothing} inner]
                    else Latex.Text {state=treeState, style=Nothing} "-" :: inner
            "/" -> toLatex_ extractor converter complete n.child
                |> \inner -> if priority_ n.child > priority_ tree
                    then [Latex.Text {state=treeState, style=Just Latex.Faded} "1"
                        , Latex.SymbolPart {state=treeState, style=Nothing} Latex.Division, Latex.Bracket {state=Math.getState n.child |> converter, style=Nothing} inner]
                    else [Latex.Text {state=treeState, style=Just Latex.Faded} "1"
                        , Latex.SymbolPart {state=treeState, style=Nothing} Latex.Division] ++ inner
            _ -> genericFunction tree
        Math.BinaryNode n -> case n.name of
            "*" -> n.children
                |> List.foldl (\elem res -> if List.isEmpty res
                    then bracket tree elem
                    else case getDivisionProps_ elem of
                    Just p -> bracket (Math.UnaryNode p) p.child
                        |> \inner -> res ++ (Latex.SymbolPart {state=converter p.state, style=Nothing} Latex.Division :: inner)
                    Nothing -> bracket tree elem
                        |> \newList -> res ++ (Latex.SymbolPart {state=treeState, style=Nothing} Latex.CrossMultiplcation :: newList)
                ) []
            "+" -> n.children
                |> List.foldl (\elem res -> if List.isEmpty res
                    then bracket tree elem
                    else if Math.getName elem == "-"
                    then bracket tree elem |> \inner -> res ++ inner
                    else bracket tree elem |> \inner -> res ++ (Latex.Text {state=treeState, style=Nothing} "+" :: inner)
                ) []
            _ -> infixFunction tree
        Math.DeclarativeNode _ -> infixFunction tree
        _ -> genericFunction tree

substituteArgs_: (a -> Maybe FunctionProp) -> (a -> b) -> Bool -> b -> List (Math.Tree a) -> Latex.Model () -> Latex.Model b
substituteArgs_ extractor convert complete state args = let newState old = {state=state, style=old.style} in
    List.concatMap (\elem -> case elem of
        Latex.Fraction s top bottom ->
            [Latex.Fraction (newState s) (substituteArgs_ extractor convert complete state args top) (substituteArgs_ extractor convert complete state args bottom)]
        Latex.Superscript s inner -> [Latex.Superscript (newState s) (substituteArgs_ extractor convert complete state args inner)]
        Latex.Subscript s inner -> [Latex.Subscript (newState s) (substituteArgs_ extractor convert complete state args inner)]
        Latex.Bracket s inner -> [Latex.Bracket (newState s) (substituteArgs_ extractor convert complete state args inner)]
        Latex.Sqrt s inner -> [Latex.Sqrt (newState s) (substituteArgs_ extractor convert complete state args inner)]
        Latex.Border s inner -> [Latex.Border (newState s) (substituteArgs_ extractor convert complete state args inner)]
        Latex.Argument s n -> if complete
            then (case getN_ (n-1) args of
                    Nothing -> [Latex.Argument (newState s) n] -- Display missing info
                    Just t -> toLatex_ extractor convert True t
                )
            else [Latex.Argument (newState s) n] -- Display missing info
        Latex.Param s n -> case getN_ (n-1) args of
            Nothing -> [Latex.Argument (newState s) n] -- Display missing info
            Just t -> toLatex_ extractor convert True t
        Latex.Text s str ->  [Latex.Text (newState s) str]
        Latex.SymbolPart s  str -> [Latex.SymbolPart (newState s) str]
        Latex.Caret s  -> [Latex.Caret (newState s)]
    )

getN_: Int -> List a -> Maybe a
getN_ num list = if num < 0 then Nothing
    else if num == 0 then List.head list
    else getN_ (num-1) (List.drop 1 list)

toSymbol: (state -> Maybe FunctionProp) -> Math.Tree (Matcher.State state) -> Latex.Model (Matcher.State state)
toSymbol convert root = let treeState = {state=Math.getState root, style=Just Latex.Emphasis} in
    case Math.getState root |> Matcher.getState |> convert |> Maybe.andThen .latex of
        Nothing -> if Math.getChildren root |> List.isEmpty
            then [Latex.Text treeState (Math.getName root)]
            else [Latex.Text treeState (Math.getName root), Latex.Bracket treeState [] ]
        Just l -> substituteArgs_ (Matcher.getState >> convert) identity False (Math.getState root) (Math.getChildren root) l

{-
## Topics
-}

addTopic: Maybe String -> Topic -> Model -> Result String Model
addTopic url topic m = let model = deleteTopic topic.name m in -- Clear Existing topic
    topic.functions
    |> Helper.resultDict (\name props dict -> case Dict.get name dict of
        Nothing -> Ok (Dict.insert name {property = props.property, count = 1} dict)
        Just original -> if Math.equal (==) original.property props.property
            then Ok (Dict.insert name {original | count = original.count + 1} dict)
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
                ,   topics = (  case (Dict.get topic.name model.topics, url) of
                        (Nothing, Nothing) -> Installed Nothing topic
                        (Nothing, Just u) -> Installed (Just {url = u, description = "No description provided", preinstall = False}) topic
                        (Just (NotInstalled s), Nothing) -> Installed Nothing topic
                        (Just (NotInstalled s), Just u) -> if s.url == u then Installed (Just s) topic
                            else Installed (Just {url = u, description = "No description provided", preinstall = False}) topic
                        (Just (Installed _ _), Nothing) -> Installed Nothing topic
                        (Just (Installed Nothing _), Just u) -> Installed (Just {url = u, description = "No description provided", preinstall = False}) topic
                        (Just (Installed (Just s) _), Just u) -> if s.url == u then Installed (Just s) topic
                            else Installed (Just {url = u, description = "No description provided", preinstall = False}) topic
                    )
                    |> \t -> Dict.insert topic.name t model.topics
                }
            )
    )

deleteTopic: String -> Model -> Model
deleteTopic name model = case Dict.get name model.topics of
    Just (Installed url topic) ->
        let
            newFunctions = topic.functions |> Dict.foldl (\n _ newDict -> case Dict.get n newDict of
                    Nothing -> newDict
                    Just props -> if props.count < 2
                        then Dict.remove n newDict
                        else Dict.insert n {props | count = props.count - 1} newDict
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
                    Just existing -> Dict.insert name (NotInstalled existing) model.topics
            ,   constants = newConstants
            ,   functions = newFunctions
            }
    _ -> model

loadedTopics: Model -> List Topic
loadedTopics model = Dict.toList model.topics
    |> List.filterMap (\(_, state) -> case state of
        Installed _ obj -> Just obj
        _ -> Nothing
    )

addSources: Dict.Dict String Source -> Model -> Model
addSources map model =
    {   model
    |   topics = Dict.foldl (\name url dict ->
            case Dict.get name dict of
                Nothing -> Dict.insert name (NotInstalled url) dict
                Just existing -> case existing of
                    Installed Nothing obj -> Dict.insert name (Installed (Just url) obj) dict
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
    Just prop -> case Math.getState prop.property |> .javascript of
        Nothing -> Err (name ++ " cannot be evaluated")
        Just (InfixOp jsName) -> Ok (String.join jsName children)
        Just (PrefixOp jsName) -> if List.length children /= 1 then Err "Prefix can only be for unary operators"
            else Ok (jsName ++ String.join "" children)
        Just (FuncOp jsName) -> Ok (jsName ++ "(" ++ String.join "," children ++ ")")

evaluateStr: Model -> Math.Tree s -> Result String String
evaluateStr model root = case root of
    Math.RealNode _ -> Err "Evaluating a Real would just return itself"
    -- TODO: do not evaluate anything that would just become itself e.g. -x or 1/x
    _ -> evaluateStr_ model root

evaluateStr_: Model -> Math.Tree s -> Result String String
evaluateStr_ model root = (
    case root of
        Math.RealNode s -> Ok (String.fromFloat s.value)
        Math.VariableNode s -> case Dict.get s.name model.constants of
            Nothing -> Err "Unable to evaluate an unknown variable"
            Just (str, _) -> str |> Result.fromMaybe ("javascript value is not provided for '" ++ s.name ++ "'")
        Math.UnaryNode s -> evaluateStr_ model s.child |> Result.andThen (\child -> toJavascriptString_ model s.name [child])
        Math.BinaryNode s -> Helper.resultList (\child list -> evaluateStr_ model child |> Result.map (\c -> c::list)) [] s.children
            |> Result.andThen (List.reverse >> toJavascriptString_ model s.name)
        Math.DeclarativeNode _ -> Err "Cannot evaluate a declaration"
        Math.GenericNode s -> Helper.resultList (\child list -> evaluateStr_ model child |> Result.map (\c -> c::list)) [] s.children
            |> Result.andThen (List.reverse >> toJavascriptString_ model s.name)
    ) |> Result.map (\str -> "(" ++ str ++ ")" )

{-
## Topic Parser
-}

topicDecoder: Dec.Decoder Topic
topicDecoder = Dec.map3 (\a b c -> (a,b,c))
    (Dec.field "name" Dec.string)
    (Dec.field "functions" (Dec.dict (functionDecoder_ |> Dec.map (\func -> {property = func}))))
    (Dec.field "constants" (Dec.dict (Dec.string |> Dec.andThen (\str ->
        (   if String.isEmpty str then Dec.succeed ""
            else if String.left 5 str == "Math." then String.dropLeft 5 str |> Dec.succeed
            else if String.left 7 str == "Number." then String.dropLeft 7 str |> Dec.succeed
            else Dec.fail "Only constants from Math or Number are allowed"
        )
        |> Dec.andThen (\children -> if String.all (\c -> Char.isAlpha c || c == '_') children
            then Dec.succeed (if String.isEmpty str then Nothing else Just str)
            else Dec.fail "Unknown characters after 'Math.' / 'Number.'"
        )
    ))))
    |> Dec.andThen ( \(name, functions, vars) ->
        if Dict.size (Dict.diff vars functions) /= Dict.size vars then Dec.fail "Can't have a variable named as a function as well"
        else
            let
                knownProps = Dict.foldl (\k _ inner ->  Math.createConstant
                        {   javascript = FuncOp k |> Just
                        ,   latex = case Dict.get k Latex.greekLetters of
                            Nothing -> Just [Latex.Text {state=(), style=Just Latex.Emphasis} k]
                            Just symb -> Just [Latex.SymbolPart {state=(), style=Just Latex.Emphasis} symb]
                        ,   alternativeNames = []
                        } k
                        |> \entry -> Dict.insert k {property = entry} inner
                    ) functions vars
            in
                Dict.merge
                (\_ _ -> Result.map identity)
                (\key left right -> Result.andThen (\initial ->
                    if left.property == right.property then Ok initial
                    else Err ("The function '" ++ key ++ "' has a conflicting definition")
                ))
                (\key value -> Result.map (Dict.insert key value))
                knownProps
                coreFunctions_
                (Ok knownProps)
            |> Helper.resultToDecoder
            |> Dec.andThen (\allFuncs -> Dec.field "actions" (Dec.list (ruleDecoder_ allFuncs))
                |> Dec.map (\eqs -> Topic name vars functions eqs)
            )
    )

functionDecoder_: Dec.Decoder (Math.FunctionProperty FunctionProp)
functionDecoder_ = Math.functionPropertyDecoder functionPropDecoder

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

parameterDecoder_: Dict.Dict String {a | property: Math.FunctionProperty FunctionProp} -> Dec.Decoder (Dict.Dict String Parameter, Dict.Dict String (Int, Bool))
parameterDecoder_ knownFuncs =
    Dec.dict (Dec.map2 Tuple.pair (Dec.field "description" Dec.string) (Dec.field "example" Dec.string))
    |> Dec.andThen (Helper.resultDict (\key (description, example) (others, dict) ->
            Math.parse Dict.empty key
            |> Result.andThen (\tree -> case tree of
                Math.VariableNode m -> case Dict.get m.name dict of
                    Just _ -> Err "Parameters are duplicated"
                    Nothing -> case Dict.get m.name knownFuncs of
                        Just _ -> Err "Known constants cannot be used as a parameter"
                        Nothing -> Ok
                            (   Dict.insert m.name {name = m.name, arguments = [], description = description, example = example} others
                            ,   Dict.insert m.name (0, False) dict
                            )
                Math.GenericNode m -> case Dict.get m.name dict of
                    Just _ -> Err "Parameters are duplicated"
                    Nothing -> case Dict.get m.name knownFuncs of
                        Just _ -> Err "Known function cannot be used as a parameter"
                        Nothing -> Ok
                            (   Dict.insert m.name {name = m.name, arguments = List.map Math.getName m.children, description = description, example = example} others
                            ,   Dict.insert m.name (List.length m.children, False) dict
                            )
                _ -> Err "Parameters can only be variables or functions"
            )
        )
        (Dict.empty, Dict.empty)
        >> Helper.resultToDecoder
    )

ruleDecoder_: Dict.Dict String {a | property: Math.FunctionProperty FunctionProp} -> Dec.Decoder Rule
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
                |> Dec.map (\to ->
                    let
                        otherSet = Dict.toList newArgs
                            |> List.filter (\(_, (_, oneUse)) -> oneUse)
                            |> List.map Tuple.first
                            |> Set.fromList
                    in
                        {   from = {from | root = setOthers_ otherSet from.root}
                        ,   to = to
                        }
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

expressionDecoder_: Dict.Dict String {a | property: Math.FunctionProperty FunctionProp} -> Dict.Dict String (Int, Bool)
    -> Dec.Decoder ({name: String, root: Matcher.Matcher, latex: Latex.Model ()}, Dict.Dict String (Int, Bool))
expressionDecoder_ funcProps args =
    let
        checkUnknowns name numArgs dict = case Dict.get name dict of
            Nothing -> Ok (Dict.insert name (numArgs, True) dict)
            Just (n, _) -> if n == numArgs then Ok (Dict.insert name (n, False) dict)
                else if n == 0 || numArgs == 0 then Err "Variable cannot be used as a function"
                else Err "Functions has different number of inputs"
    in
    Dec.andThen
    (\str -> case Math.parse funcProps str of
        Err errStr -> Dec.fail ("'" ++ str ++ "' is not a valid expression: " ++ errStr)
        Ok tree -> case Matcher.parseMatcher checkUnknowns args tree of
            Err errStr -> Dec.fail ("'" ++ str ++ "' is not a valid expression: " ++ errStr)
            Ok (matcher, newArgs) -> Dec.succeed ({name = str, root = matcher, latex = toLatex_ identity (\_ -> ()) True tree}, newArgs)
    )
    Dec.string

replacementDecoder_: Dict.Dict String {a | property: Math.FunctionProperty FunctionProp} -> Dict.Dict String (Int, Bool) -> Dec.Decoder {name: String, root: Matcher.Replacement FunctionProp, latex: Latex.Model ()}
replacementDecoder_ knownFunc args = Dec.string
    |> Dec.andThen (\str ->
        Dict.toList args
        |> List.indexedMap (\index (var, (argNum, _)) -> (var, (argNum, index)))
        |> \argMap -> Math.parse knownFunc str
            |> Result.andThen (\tree -> Matcher.toReplacement identity True (Dict.fromList argMap) tree
                |> Result.map (\replacement -> {name = str, root = replacement, latex = toLatex_ identity (\_ -> ()) True tree})
            )
        |> Helper.resultToDecoder
    )

{-
## Encoding and Decoding
-}

encode: Model -> Enc.Value
encode model = Enc.object
    [   ("functions", Enc.dict identity (\prop -> Enc.object [("properties", encodeFProp_ prop.property), ("count", Enc.int prop.count)] ) model.functions)
    ,   ("constants", Enc.dict identity (\(name, count) -> Enc.object [("name", encMaybeString name), ("count", Enc.int count)]) model.constants)
    ,   ("topics", Enc.dict identity (\loadState -> case loadState of
            NotInstalled source -> Enc.object [("type", Enc.string "notInstalled"),("url", Enc.string source.url),("description", Enc.string source.description)]
            Installed source topic -> Enc.object
                (   [   ("type", Enc.string "installed")
                    ,   ("topic", encodeTopic_ topic)
                    ]
                ++ (Maybe.map (\s -> [("url", Enc.string s.url),("description", Enc.string s.description)]) source |> Maybe.withDefault [])
                )
            ) model.topics
        )
    ]

encodeFProp_: Math.FunctionProperty FunctionProp -> Enc.Value
encodeFProp_ = Math.encodeFunctionProperty encodeFunctionProp

encodeFunctionProp: FunctionProp -> List (String, Enc.Value)
encodeFunctionProp prop =
    [   ("javascript", case prop.javascript of
            Nothing -> Enc.null
            Just (InfixOp js) -> Enc.object [("type", Enc.string "infix"),("symbol",Enc.string js)]
            Just (PrefixOp js) ->Enc.object [("type", Enc.string "prefix"),("symbol",Enc.string js)]
            Just (FuncOp js) ->Enc.object [("type", Enc.string "function"),("symbol",Enc.string js)]
        )
    ,   ("latex", case prop.latex of
            Nothing -> Enc.null
            Just l -> Latex.unparse l |> Enc.string
        )
    ]

functionPropDecoder: Dec.Decoder FunctionProp
functionPropDecoder = Dec.map3 (\a b c -> FunctionProp a b (Maybe.withDefault [] c))
    (Dec.maybe <| Dec.field "javascript" javascriptDecoder_)
    (Dec.maybe <| Dec.field "latex" <| Dec.andThen (Helper.resultToDecoder << Latex.parse) <| Dec.string)
    (Dec.maybe <| Dec.field "alternativeNames" <| Dec.list <| Dec.string)

encodeTopic_: Topic -> Enc.Value
encodeTopic_ topic = Enc.object
    [   ("name", Enc.string topic.name)
    ,   ("constants", Enc.dict identity encMaybeString topic.constants)
    ,   ("functions", Enc.dict identity (.property >> encodeFProp_) topic.functions)
    ,   ("actions", Enc.list encodeRule_ topic.rules)
    ]

encMaybeString: Maybe String -> Enc.Value
encMaybeString mStr = case mStr of
    Nothing -> Enc.null
    Just n -> Enc.string n

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
    (Dec.field "functions" <| Dec.dict <| Dec.map2 (\p c -> {property = p, count = c}) (Dec.field "properties" functionDecoder_)  (Dec.field "count" Dec.int))
    (Dec.field "constants" <| Dec.dict <| Dec.map2 Tuple.pair (Dec.maybe <| Dec.field "name" Dec.string) (Dec.field "count" Dec.int))
    (Dec.field "topics" <| Dec.dict <| Dec.andThen (\s -> case s of
        "notInstalled" -> Dec.map NotInstalled sourceDecoder
        "installed" -> Dec.map2 Installed (Dec.maybe <| sourceDecoder) (Dec.field "topic" topicDecoder)
        _ -> Dec.fail ("Unknown loadState: " ++ s)
    ) <| Dec.field "type" Dec.string
    )

sourceDecoder: Dec.Decoder Source
sourceDecoder = Dec.map3 Source
    (Dec.field "url" Dec.string)
    (Dec.field "description" Dec.string)
    (Dec.map (Maybe.withDefault False) <|  Dec.maybe <| Dec.field "preinstall" Dec.bool)
