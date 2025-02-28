module Rules exposing (Model, Event, Topic, init, update,
    addTopic, deleteTopic, topicDecoder,
    menuConstants, menuFunctions, menuRules
    )

import Dict
import Html exposing (Html, h2, p, text)
import Json.Decode as Dec
import Set
import Math
import HtmlEvent

{-
## Modeling rules
-}

type alias FunctionProperties_ =
    {   args: Int -- Number of arguments it takes
    ,   params: Int -- Number of parameters it takes
    ,   associative: Bool -- Allows processing in different order
    ,   commutative: Bool -- Allows swapping argument ordering
    }

type Token_ =
    FunctionToken Int Int -- (args and parameters)
    | AnyToken

type alias Expression_ =
    {   root: Math.Tree ()
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

{-
## Elm-y bits
-}
type alias Model =
    {   functions: Dict.Dict String (FunctionProperties_, Int) -- Properties + Number of topics that contain this function
    ,   constants: Dict.Dict String Int -- Number of topics that rely on the constant
    ,   topics: Dict.Dict String Topic
    ,   createMode: Bool
    }

type Event =
    ClickVariable String
    | ClickFunction String
    | ClickRule String Int

init: Model
init =
    {   functions = Dict.empty
    ,   constants = Dict.empty
    ,   topics = Dict.empty
    ,   createMode = False
    }

addTopic: Topic -> Model -> Result String Model
addTopic topic m = let model = deleteTopic topic.name m in -- Clear Existing topic
    topic.functions |> Dict.foldl (\name props result -> case result of
        Err _ -> result
        Ok dict -> case Dict.get name dict of
            Nothing -> Ok (Dict.insert name (props, 1) dict)
            Just (p, count) -> if p.args == props.args && p.params == props.params && p.associative == props.associative && p.commutative == props.commutative
                then Ok (Dict.insert name (p, count + 1) dict) 
                else Err ("'" ++ name ++ "' differs from existing definition from other topics")
    )
    (Ok model.functions)
    |> (\res -> case res of
        Err errStr -> Err errStr
        Ok functions -> topic.constants |> Set.foldl (\name result -> case result of
                Err _ -> result
                Ok dict -> case Dict.get name dict of
                    Nothing -> Ok (Dict.insert name 1 dict)
                    Just count -> Ok (Dict.insert name (count + 1) dict) 
            )
            (Ok model.constants)
            |> Result.map (\constants -> {model | functions = functions, constants = constants, topics = Dict.insert topic.name topic model.topics})
    )

deleteTopic: String -> Model -> Model
deleteTopic name model = case Dict.get name model.topics of
    Nothing -> model
    Just topic ->
        let
            newFunctions = topic.functions |> Dict.foldl (\n _ newDict -> case Dict.get n newDict of
                    Nothing -> newDict
                    Just (props, i) -> if i < 2 then Dict.remove n newDict else Dict.insert n (props,i-1) newDict
                )
                model.functions
            newConstants = topic.constants |> Set.foldl (\n newSet -> case Dict.get n newSet of
                    Nothing -> newSet
                    Just i -> if i < 2 then Dict.remove n newSet else Dict.insert n (i-1) newSet
                )   
                model.constants
        in
            {   model
            |   topics = Dict.remove name model.topics
            ,   constants = newConstants
            ,   functions = newFunctions
            }

update: Event -> Model -> (Model, Cmd Event)
update _ model = (model, Cmd.none)

type alias MenuItem_ msg = Bool -> List (Html.Attribute msg) -> String -> List (Html.Html msg) -> List (Html.Html msg)
menuRules: (Event -> msg) -> MenuItem_ msg -> Model -> Maybe (Math.Tree state) -> List (Html msg)
menuRules converter menuItem model selectedNode = menuItem True [] "Rules"
    (   Dict.foldl (\_ topic result -> result ++
            menuItem True [] topic.name
            (   List.foldl (\rule (index, res) -> (index + 1, res ++
                    menuItem
                    (Maybe.map (matchRuleToNode_ rule) selectedNode |> Maybe.withDefault False)
                    [HtmlEvent.onClick (ClickRule topic.name index |> converter)]
                    (expressionToString_ rule.lhs ++ (if rule.reversible then "↔" else "→") ++ expressionToString_ rule.rhs)
                    [   h2 [] [text rule.title]
                    ,   p [] [text rule.description]
                    ]
                )
                )
                (0,[])
                topic.rules
                |> Tuple.second
            )
        )
        []
        model.topics
    )

matchRuleToNode_: Rule_ -> Math.Tree state -> Bool
matchRuleToNode_ rule node = 
    matchExpressionToNode_ rule.lhs node || (rule.reversible && matchExpressionToNode_ rule.rhs node)

matchExpressionToNode_: Expression_ -> Math.Tree state -> Bool
matchExpressionToNode_ exp node = case exp.root of
    Math.VariableNode _ _ -> True
    Math.RealNode _ num -> case node of
        Math.RealNode _ actual -> actual == num
        _ -> False
    Math.FunctionNode _ fProp -> case node of
        Math.FunctionNode _ actual -> actual.name == fProp.name
        _ -> False

menuFunctions: (Event -> msg) -> MenuItem_ msg -> Model -> List (Html msg)
menuFunctions converter menuItem model = menuItem True [] "Functions"
    (   Dict.foldl (\name (props, _) result -> result ++
            menuItem True [HtmlEvent.onClick (ClickFunction name |> converter)] name
            [   p [] [text ("Arguments: " ++ String.fromInt props.args)]
            ,   p [] [text ("Parameters: " ++ String.fromInt props.params)]
            ,   p [] [text ("Associative: " ++ if props.associative then "Yes" else "No")]
            ,   p [] [text ("Commutative: " ++ if props.commutative then "Yes" else "No")]
            ]
        )
        []
        model.functions
    )

menuConstants: (Event -> msg) -> MenuItem_ msg -> Model -> List (Html msg)
menuConstants converter menuItem model = menuItem True [] "Constants"
    (   Dict.foldl (\name _ result -> result ++
            [h2 [HtmlEvent.onClick (ClickVariable name |> converter)] [text name]]
        )
        []
        model.constants
    )

expressionToString_: Expression_ -> String
expressionToString_ exp = Math.symbolicate exp.root |> mathSymbolsToString_

mathSymbolsToString_: Math.Symbol () -> String
mathSymbolsToString_ root = case root of
    Math.Text str -> str
    Math.Node _ list -> list |> List.map mathSymbolsToString_ |> String.join ""

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
    <| Dec.map4 FunctionProperties_
        (Dec.field "arguments" Dec.int)
        (Dec.field "parameters" Dec.int)
        (Dec.field "associative" Dec.bool)
        (Dec.field "commutative" Dec.bool)

ruleDecoder_: Dict.Dict String FunctionProperties_ -> Set.Set String -> Dec.Decoder Rule_
ruleDecoder_ functions constants = Dec.map6 (\a b c d e f -> ((a,b,c),(d,e,f)))
    (Dec.field "title" Dec.string)
    (Dec.field "description" Dec.string)
    (Dec.maybe (Dec.field "reversible" Dec.bool) |> Dec.map (Maybe.withDefault False))
    (Dec.field "lhs" (Dec.string |> Dec.andThen (parseExpression_ functions >> resultToParser_)))
    (Dec.field "rhs" (Dec.string |> Dec.andThen (parseExpression_ functions >> resultToParser_)))
    (Dec.maybe (Dec.field "parameters"
        (   Dec.keyValuePairs Dec.string
        |>  Dec.andThen (\list ->
            List.foldl (\(key, description) res -> case res of
                Err _ -> res
                Ok (others, dict) -> case parseExpression_ functions key of
                    Err str -> Err str
                    Ok exp -> Dict.foldl
                        (\name tok result -> case result of
                            Err _ -> result
                            Ok newDict -> case Dict.get name newDict of
                                Nothing -> Ok (Dict.insert name tok newDict)
                                Just oldTok -> case (oldTok, tok) of
                                    ((FunctionToken oArgs oNum), (FunctionToken args num)) -> 
                                        if oArgs == args && oNum == num then Ok newDict
                                        else Err ("Different function signature for '" ++ name ++ "' detected within parameters")
                                    (AnyToken, AnyToken) -> Ok newDict
                                    _ -> Err ("Different usages of '" ++ name ++ "' detected within parameters")
                        )
                        (Ok dict)
                        exp.tokens
                        |> (\result -> case result of
                            Err errStr -> Err errStr
                            Ok newDict -> Ok (others ++ [(exp, description)], newDict)
                        )
            )
            (Ok ([], Dict.empty))
            list
            |> resultToParser_
        ))
    )
    |> Dec.map (Maybe.withDefault ([], Dict.empty))
    )
    |> Dec.andThen (\((title, description, reversible), (originalLhs, originalRhs, (parameters, pDict))) -> -- Validate that both lhs and rhs has symbols covered
        case originalLhs.tokens |> extractUniqueTokens_ functions constants pDict of
            Err errStr -> Dec.fail errStr
            Ok lhs -> case originalRhs.tokens |> extractUniqueTokens_ functions constants pDict of
                Err errStr -> Dec.fail errStr
                Ok rhs -> 
                    Set.union (Dict.keys lhs |> Set.fromList) (Dict.keys rhs |> Set.fromList)
                    |> Set.foldl
                        (\name result -> case result of
                            Err _ -> result
                            Ok _ -> case (Dict.get name lhs, Dict.get name rhs) of
                                (Nothing, Nothing) -> Err ("Something went wrong, unable to find name '" ++ name ++ "'in lhs nor rhs")
                                (Nothing, Just _) -> Ok ()
                                (Just _, Nothing) -> Ok ()
                                (Just AnyToken, Just AnyToken) -> Ok ()
                                (Just (FunctionToken lArgs lParam), Just (FunctionToken rArgs rParam)) ->
                                    if lArgs == rArgs && lParam == rParam then Ok ()
                                    else Err ("inconsistent function signature for '" ++ name ++ "' in lhs and rhs")
                                _ -> Err ("lhs and rhs are treating '" ++ name ++ "' differently")
                        )
                        (Ok ())
                    |> Result.map (\_ -> Rule_ title description reversible originalLhs originalRhs parameters)
                    |> resultToParser_
    )

extractUniqueTokens_: Dict.Dict String FunctionProperties_ -> Set.Set String -> Dict.Dict String Token_ -> Dict.Dict String Token_ -> Result String (Dict.Dict String Token_)
extractUniqueTokens_ functions constants parameters = Dict.foldl
    (\name token result -> case result of
        Err _ -> result
        Ok dict -> if Dict.member name functions
            then case token of
                AnyToken -> Err ("'" ++ name ++ "' is use as a variable, even though it is defined as a function")
                FunctionToken _ _ -> Ok dict  -- This was verified in parse expression
            else if Set.member name constants
            then case token of
                FunctionToken _ _ -> Err ("'" ++ name ++ "' is used as a function, even though it is defined as a constant")
                AnyToken -> Ok dict
            else case (Dict.get name parameters, token) of
                (Nothing, _) -> Ok (Dict.insert name token dict)
                (Just (AnyToken), AnyToken) -> Ok dict
                (Just (FunctionToken tArgs tParams), FunctionToken args params) -> if tArgs == args && tParams == params
                    then Ok dict
                    else Err ("Inconsistent function signature '" ++ name ++ "' between expression and parameter")
                _ -> Err ("Inconsistent usage of '" ++ name ++ "' between expression and parameter")
    )
    (Ok Dict.empty)

resultToParser_: Result String a -> Dec.Decoder a
resultToParser_ res = case res of
    Err str -> Dec.fail str
    Ok b -> Dec.succeed b

parseExpression_: Dict.Dict String FunctionProperties_ -> String -> Result String Expression_
parseExpression_ functions str = case Math.parse str of
    Err errStr -> Err ("'" ++ str ++ "' is not a valid expression: " ++ errStr)
    Ok root -> case Math.process (tokenExtractor_ str functions) (Ok Dict.empty) root |> Tuple.first of
        Err errStr -> Err errStr
        Ok tokens -> Ok {root = root, tokens = tokens}

tokenExtractor_: String -> Dict.Dict String FunctionProperties_ -> Math.Processor () () (Result String (Dict.Dict String Token_))
tokenExtractor_ str functions =
    {   function = (\recurse res (_, fProp) -> (case res of
            Err _ -> res
            Ok seenDict -> (
                case (Dict.get fProp.name functions, Dict.get fProp.name seenDict) of
                    (Nothing, Nothing) -> Ok (Dict.insert fProp.name (FunctionToken (List.length fProp.args) (List.length fProp.parameters)) seenDict)
                    (Nothing, Just seen) -> case seen of
                        AnyToken -> Err ("'" ++ fProp.name ++ "' is used as both a variable and a function in: " ++ str)
                        FunctionToken numArgs numParams -> if (List.length fProp.args) == numArgs && (List.length fProp.parameters) == numParams
                            then Ok seenDict
                            else Err ("The function signature of '" ++ fProp.name ++ "' differs within " ++ str)
                    (Just given, Nothing) -> if matchFunction_ given fProp
                            then Ok seenDict
                            else Err ("The function usage of '" ++ fProp.name ++ "', in '" ++ str ++ "' differs with the declared function")
                    (Just _, Just _) -> Err ("Processing error: Added function '" ++ fProp.name ++ "' as a token")
                )
                |> (\result -> case result of
                    Err _ -> result
                    Ok _ -> case List.foldl (\child r -> recurse r child |> Tuple.first) result fProp.args of
                        Err errStr -> Err errStr
                        Ok dict -> List.foldl (\child r -> recurse r child |> Tuple.first) (Ok dict) fProp.parameters
                )
        , Just (Math.FunctionNode () fProp)
        )
    )
    ,   var = (\res (_, name) -> case res of
            Err _ -> (res, Just (Math.VariableNode () name))
            Ok dict -> case Dict.get name functions of
                Just _ -> (Err ("'" ++ name ++ "' is treated as a variable in: " ++ str), Just (Math.VariableNode () name))
                Nothing -> case Dict.get name dict of
                    Nothing -> (Ok (Dict.insert name AnyToken dict), Just (Math.VariableNode () name))
                    Just AnyToken -> (Ok dict, Just (Math.VariableNode () name))
                    Just (FunctionToken _ _) -> (Err (name ++ " is shown as both a variable and a function"), Just (Math.VariableNode () name))
        )
    ,   real = (\res (_, val) -> (res, Just (Math.RealNode () val))) -- Numbers don't need to be tracked
    }

matchFunction_: FunctionProperties_ -> Math.Function msg -> Bool
matchFunction_ props node =
    if props.associative && props.commutative then
        List.isEmpty node.parameters && (List.length node.args) > 1
    else
        props.args == List.length node.args && props.params == List.length node.parameters
