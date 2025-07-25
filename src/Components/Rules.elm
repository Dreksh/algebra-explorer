module Components.Rules exposing (Model, Event(..), Parameters, Topic, Rule, Source, init,
    FunctionProp, negateProp, functionProperties, toLatex, toSymbol, process,
    addTopic, deleteTopic, addSources, topicDecoder, loadedTopics,
    evaluateStr, menuTopics, encode, decoder, sourceDecoder,
    encodeFunctionProp, functionPropDecoder
    )

import Dict
import Html exposing (a, h3, p, text)
import Html.Attributes exposing (class, href, title)
import Json.Decode as Dec
import Json.Encode as Enc
import Set
-- Ours
import Helper
import Algo.Matcher as Matcher
import Algo.Math as Math
import Components.Latex as Latex
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.Menu as Menu

{-
## Modeling rules
-}

type alias FunctionProp =
    {   javascript: Maybe Javascript_
    ,   latex: Maybe (Latex.Model ())
    }
negateProp: FunctionProp
negateProp =
    {   javascript = Just (PrefixOp "-")
    ,   latex = Just [Latex.Text () "-", Latex.Argument () 1]
    }
divisionProp_: FunctionProp
divisionProp_ =
    {   javascript = Just (PrefixOp "1/")
    ,   latex = Just [Latex.SymbolPart () Latex.Division, Latex.Argument () 1]
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
    ,   matches: List {from: {name: String, root: Matcher.Matcher}, to: List {name: String, root: Matcher.Replacement FunctionProp}}
    }

type alias Topic =
    {   name: String
    ,   constants: Dict.Dict String String
    ,   functions: Dict.Dict String {property: Math.FunctionProperty FunctionProp}
    ,   rules: List Rule
    }

type alias Parameters state =
    {   title: String
    ,   parameters: Dict.Dict String Parameter
    ,   matches: List {from: Matcher.MatchResult FunctionProp state, replacements: List {name: String, root: Matcher.Replacement FunctionProp}}
    }

{-
## Elm-y bits
-}
type alias Model =
    {   functions: Dict.Dict String {property: Math.FunctionProperty FunctionProp, count: Int}
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
    | Ungroup Int Int -- eq root
    | NumericalSubstitution Int Int Float -- eq root matching value
    | Substitute Int (Set.Set Int) -- eq selected
    | Download String
    | Evaluate Int Int String -- eq nodeID evalString
    | Delete String

init: Model
init =
    {   functions = Dict.fromList
            [   ("+",{property= Math.BinaryNode {state = {javascript = InfixOp "+" |> Just, latex = Just [Latex.Text () "+"]}, name = "", associative = True, commutative = True, identity = 0, children = []}, count = 1})
            ,   ("*",{property= Math.BinaryNode {state = {javascript = InfixOp "*" |> Just, latex = Just [Latex.SymbolPart () Latex.CrossMultiplcation]}, name = "", associative = True, commutative = True, identity = 1, children = []}, count = 1})
            ,   ("-",{property= Math.UnaryNode {state = negateProp, name = "", child = Math.RealNode {state = negateProp, value = 0}}, count = 1})
            ,   ("/",{property= Math.UnaryNode {state = divisionProp_, name = "", child = Math.RealNode {state = divisionProp_, value = 0}}, count = 1})
            ,   ("=",{property= Math.DeclarativeNode {state = {javascript = InfixOp "=" |> Just, latex = Just [Latex.Text () "="]}, name = "", children = []}, count = 1})
            ]
    ,   constants = Dict.empty
    ,   topics = Dict.empty
    }

functionProperties: Model -> Dict.Dict String {property: Math.FunctionProperty FunctionProp, count: Int}
functionProperties model = Dict.foldl
    (\k (_, count) d -> Math.createConstant {javascript = FuncOp k |> Just , latex = Just [Latex.Text () k] } k
        |> \entry -> Dict.insert k {property = entry, count = count} d
    ) model.functions model.constants

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
        addSubBrackets parent child =
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
                "-" -> convert "-" :: addSubBrackets tree n.child
                "/" -> convert "1/" :: addBrackets tree n.child
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
toLatex eq = toLatex_ eq.tracker.ops.extract True eq.root

toLatex_: (state -> Maybe FunctionProp) -> Bool -> Math.Tree (Matcher.State state) -> Latex.Model (Matcher.State state)
toLatex_ converter complete tree =
    let
        treeState = Math.getState tree
        funcLatex = Math.getState tree |> Matcher.getState |> converter |> Maybe.andThen .latex
        genericFunction root = case funcLatex of
            Nothing -> List.foldl (\n list -> toLatex_ converter complete n |> \new -> new :: list) [] (Math.getChildren root)
                |> \list -> [Latex.Text treeState (Math.getName root), Latex.Bracket treeState (List.intersperse [Latex.Text treeState ","] list |> List.concat) ]
            Just l -> substituteArgs_ converter complete treeState (Math.getChildren root) l
        bracket parent child = toLatex_ converter complete child
            |> \inner -> if priority_ child >= priority_ parent
                then [Latex.Bracket (Math.getState child) inner]
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
        Math.RealNode n -> [ String.fromFloat n.value |> Latex.Text treeState ]
        Math.VariableNode n -> [Latex.Text treeState n.name]
        Math.UnaryNode n -> case n.name of
            "-" -> toLatex_ converter complete n.child
                |> \inner -> if priority_ n.child > priority_ tree
                    then [Latex.Text treeState "-", Latex.Bracket (Math.getState n.child) inner]
                    else Latex.Text treeState "-" :: inner
            "/" -> bracket tree n.child
                |> \inner -> [Latex.Text treeState "1", Latex.SymbolPart treeState Latex.Division] ++ inner
            _ -> genericFunction tree
        Math.BinaryNode n -> case n.name of
            "*" -> n.children
                |> List.foldl (\elem res -> if List.isEmpty res
                    then bracket tree elem
                    else case getDivisionProps_ elem of
                    Just p -> bracket (Math.UnaryNode p) p.child
                        |> \inner -> res ++ (Latex.SymbolPart p.state Latex.Division :: inner)
                    Nothing -> bracket tree elem
                        |> \newList -> res ++ (Latex.SymbolPart n.state Latex.CrossMultiplcation :: newList)
                ) []
            "+" -> n.children
                |> List.foldl (\elem res -> if List.isEmpty res
                    then bracket tree elem
                    else if Math.getName elem == "-"
                    then bracket tree elem |> \inner -> res ++ inner
                    else bracket tree elem |> \inner -> res ++ (Latex.Text treeState "+" :: inner)
                ) []
            _ -> infixFunction tree
        Math.DeclarativeNode _ -> infixFunction tree
        _ -> genericFunction tree

substituteArgs_: (state -> Maybe FunctionProp) -> Bool -> Matcher.State state -> List (Math.Tree (Matcher.State state)) -> Latex.Model () -> Latex.Model (Matcher.State state)
substituteArgs_ convert complete state args = List.concatMap (\elem -> case elem of
    Latex.Fraction _ top bottom ->
        [Latex.Fraction state (substituteArgs_ convert complete state args top) (substituteArgs_ convert complete state args bottom)]
    Latex.Superscript _ inner -> [Latex.Superscript state (substituteArgs_ convert complete state args inner)]
    Latex.Subscript _ inner -> [Latex.Subscript state (substituteArgs_ convert complete state args inner)]
    Latex.Bracket _ inner -> [Latex.Bracket state (substituteArgs_ convert complete state args inner)]
    Latex.Sqrt _ inner -> [Latex.Sqrt state (substituteArgs_ convert complete state args inner)]
    Latex.Argument _ n -> if complete
        then (case getN_ (n-1) args of
                Nothing -> [Latex.Argument state n] -- Display missing info
                Just t -> toLatex_ convert True t
            )
        else [Latex.Argument state n] -- Display missing info
    Latex.Param _ n -> case getN_ (n-1) args of
        Nothing -> [Latex.Argument state n] -- Display missing info
        Just t -> toLatex_ convert True t
    Latex.Text _ str ->  [Latex.Text state str]
    Latex.SymbolPart _  str -> [Latex.SymbolPart state str]
    )

getN_: Int -> List a -> Maybe a
getN_ num list = if num < 0 then Nothing
    else if num == 0 then List.head list
    else getN_ (num-1) (List.drop 1 list)

toSymbol: (state -> Maybe FunctionProp) -> Math.Tree (Matcher.State state) -> Latex.Model (Matcher.State state)
toSymbol convert root = let treeState = Math.getState root in
    case Math.getState root |> Matcher.getState |> convert |> Maybe.andThen .latex of
        Nothing -> if Math.getChildren root |> List.isEmpty
            then [Latex.Text treeState (Math.getName root)]
            else [Latex.Text treeState (Math.getName root), Latex.Bracket treeState [] ]
        Just l -> substituteArgs_ convert False treeState (Math.getChildren root) l

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
                        (Nothing, Nothing) -> Installed_ Nothing topic
                        (Nothing, Just u) -> Installed_ (Just {url = u, description = "No description provided"}) topic
                        (Just (NotInstalled_ s), Nothing) -> Installed_ Nothing topic
                        (Just (NotInstalled_ s), Just u) -> if s.url == u then Installed_ (Just s) topic
                            else Installed_ (Just {url = u, description = "No description provided"}) topic
                        (Just (Installed_ _ _), Nothing) -> Installed_ Nothing topic
                        (Just (Installed_ Nothing _), Just u) -> Installed_ (Just {url = u, description = "No description provided"}) topic
                        (Just (Installed_ (Just s) _), Just u) -> if s.url == u then Installed_ (Just s) topic
                            else Installed_ (Just {url = u, description = "No description provided"}) topic
                    )
                    |> \t -> Dict.insert topic.name t model.topics
                }
            )
    )

deleteTopic: String -> Model -> Model
deleteTopic name model = case Dict.get name model.topics of
    Just (Installed_ url topic) ->
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
    Just prop -> case Math.getState prop.property |> .javascript of
        Nothing -> Err (name ++ " cannot be evaluated")
        Just (InfixOp jsName) -> Ok (String.join jsName children)
        Just (PrefixOp jsName) -> if List.length children /= 1 then Err "Prefix can only be for unary operators"
            else Ok (jsName ++ String.join "" children)
        Just (FuncOp jsName) -> Ok (jsName ++ "(" ++ String.join "," children ++ ")")

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
                [   Menu.Content [a [href source.url, class "clickable"] [text "View source"]]
                ,   Menu.Content [p [] [text source.description]]
                ]
            Installed_ s topic -> Menu.Section
                {name = topic.name, icon = Just (\c -> a [HtmlEvent.onClick (converter (Delete topic.name)), class "clickable", class c] [text "x"])}
                (   [   case s of
                            Just source -> Menu.Content [a [href source.url, class "clickable"] [text "View source"] ]
                            Nothing -> Menu.Content [p [] [text "This is an uploaded topic"]]
                    ,   Menu.Content [p [] [text (Maybe.map .description s |> Maybe.withDefault "<No description provided>")]]
                    ]
                ++ List.map (\rule -> Menu.Section {name = rule.title, icon = Nothing}
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
        ,   Menu.Section {name = "Expand", icon = Nothing}
            [   Menu.Content
                [   h3 [] [text "Given x=y, f(x)=f(y)"]
                ,   p [] [text "Modify the number based on some calculation. Use this to split the number up into small things, i.e. using 2+3=5 to make 5 into 2+3"]
                ]
            ]
        ,   Menu.Section {name = "Substitute", icon = Nothing}
            [   Menu.Content
                [   h3 [] [text "Given x=y, f(x)=f(y)"]
                ,   p [] [text "Since the equation provided means that both sides have the same value, the statement will remain true when replacing all occurrences with one by the other."]
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
    (Dec.field "functions" (Dec.dict (functionDecoder_ |> Dec.map (\func -> {property = func}))))
    (Dec.field "constants" (Dec.dict (Dec.string |> Dec.andThen (\str -> if String.left 5 str /= "Math."
        then Dec.fail "Only constants from Math are allowed"
        else if String.dropLeft 5 str |> String.all Char.isAlphaNum then Dec.succeed str
        else Dec.fail "Unknown characters after 'Math.'"
    ))))
    |> Dec.andThen ( \(name, functions, vars) ->
        if Dict.size (Dict.diff vars functions) /= Dict.size vars then Dec.fail "Can't have a variable named as a function as well"
        else
            let
                knownProps = Dict.foldl (\k _ inner -> Math.createConstant {javascript = FuncOp k |> Just, latex = Just [Latex.Text () k] } k
                        |> \entry -> Dict.insert k {property = entry} inner
                    ) functions vars
            in
                Dec.field "actions" (Dec.list (ruleDecoder_ knownProps))
                |> Dec.map (\eqs -> Topic name vars functions eqs)
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

expressionDecoder_: Dict.Dict String {a | property: Math.FunctionProperty FunctionProp} -> Dict.Dict String (Int, Bool) -> Dec.Decoder ({name: String, root: Matcher.Matcher}, Dict.Dict String (Int, Bool))
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

replacementDecoder_: Dict.Dict String {a | property: Math.FunctionProperty FunctionProp} -> Dict.Dict String (Int, Bool) -> Dec.Decoder {name: String, root: Matcher.Replacement FunctionProp}
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
    [   ("functions", Enc.dict identity (\prop -> Enc.object [("properties", encodeFProp_ prop.property), ("count", Enc.int prop.count)] ) model.functions)
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
functionPropDecoder = Dec.map2 FunctionProp
    (Dec.maybe <| Dec.field "javascript" javascriptDecoder_)
    (Dec.maybe <| Dec.field "latex" <| Dec.andThen (Helper.resultToDecoder << Latex.parse) <| Dec.string)

encodeTopic_: Topic -> Enc.Value
encodeTopic_ topic = Enc.object
    [   ("name", Enc.string topic.name)
    ,   ("constants", Enc.dict identity Enc.string topic.constants)
    ,   ("functions", Enc.dict identity (.property >> encodeFProp_) topic.functions)
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
    (Dec.field "functions" <| Dec.dict <| Dec.map2 (\p c -> {property = p, count = c}) (Dec.field "properties" functionDecoder_)  (Dec.field "count" Dec.int))
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
