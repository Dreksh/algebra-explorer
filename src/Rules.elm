module Rules exposing (Model, Event(..), Parameters, Topic, init,
    addTopic, deleteTopic, topicDecoder,
    menuTopics
    )

import Dict
import Html exposing (a, h2, h3, p, text)
import Html.Attributes exposing (class, title)
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
    ,   parameters: List {match: Expression_, description: String}
    ,   matches: List {from: Expression_, to: Expression_}
    }

type alias Topic =
    {   name: String
    ,   constants: Set.Set String
    ,   functions: Dict.Dict String FunctionProperties_
    ,   rules: List Rule_
    }

type alias Parameters state =
    {   title: String
    ,   parameters: List {name: String, description: String, tokens: Set.Set String}
    ,   matches: List {from: Matcher.MatchResult state, name: String, matcher: Matcher.Matcher}
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
        else (  True
            , Just {    title = rule.title
                , parameters = List.map (\p -> {name = p.match.name, description = p.description, tokens = Dict.keys p.match.tokens |> Set.fromList}) rule.parameters
                , matches = List.foldl (\m -> Matcher.matchSubtree m.from.root node
                    |> Maybe.map (\result -> {from = result, name = m.to.name, matcher = m.to.root})
                    |> Helper.maybeAppend
                    ) [] rule.matches
                }
            )

applyButton : (Event state -> msg) -> Maybe (Event state) -> Html.Html msg
applyButton converter e = case e of
    Nothing -> a [title "Cannot be applied"] [text "apply"]
    Just event -> a [HtmlEvent.onClick (converter event), class "clickable"] [text "apply"]

-- TODO: Maybe having information about which nodes are highlighted will be useful
coreTopic_: (Event state -> msg) -> Maybe (Math.Tree s, Int) -> Menu.Part msg
coreTopic_ converter root =
    let
        (subApply, groupApply, ungroupApply) = case root of
            Nothing -> (Nothing, Nothing, Nothing)
            Just (Math.BinaryNode n, childCount) -> if not n.associative then (Just Substitute, Nothing, Nothing)
                else
                    let
                        sameBinaryNode = List.any
                            (\child -> case child of
                                Math.BinaryNode m -> n.name == m.name
                                _ -> False
                            )
                            n.children
                    in
                    (   Just Substitute
                    ,   if List.length n.children == childCount || childCount < 2 then Nothing else Just Group -- grouping everything is a noop
                    ,   if sameBinaryNode then Just Ungroup else Nothing
                    )
            _ -> (Just Substitute, Nothing, Nothing)
    in
        Menu.Section "Core" True
        [   Menu.Section "Substitute" True
            [   Menu.Content
                [  applyButton converter subApply
                ,   h2 [] [text "Given x=y, f(x)=f(y)"]
                ,   p [] [text "Since the equation provided means that both sides have the same value, the statement will remain true when replacing all occurances with one by the other."]
                ]
            ]
        ,   Menu.Section "Group" True
            [   Menu.Content
                [   applyButton converter groupApply
                ,   h2 [] [text "Focus on a specific part"]
                ,   p [] [text "Associative operators can be done in any order. These include Addition and Multiplication. The parts that are in focus will be brought together."]
                ]
            ]
        ,   Menu.Section "Ungroup" True
            [   Menu.Content
                [   applyButton converter ungroupApply
                ,   h2 [] [text "Return the group with the rest"]
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
    (Dec.field "constants" (Dec.list Dec.string))
    |> Dec.andThen ( \(name, functions, variables) -> let vars = Set.fromList variables in
        Dec.field "actions" (Dec.list (ruleDecoder_ functions vars))
        |> Dec.map (\eqs -> Topic name vars functions eqs)
    )

functionDecoder_: Dec.Decoder (Dict.Dict String FunctionProperties_)
functionDecoder_ = Dec.dict
    <| Dec.map3 FunctionProperties_
        (Dec.field "arguments" Dec.int)
        (Dec.oneOf [Dec.field "associative" Dec.bool, Dec.succeed False])
        (Dec.oneOf [Dec.field "commutative" Dec.bool, Dec.succeed False])

ruleDecoder_: Dict.Dict String FunctionProperties_ -> Set.Set String -> Dec.Decoder Rule_
ruleDecoder_ functions constants = Dec.map3 (\a b c -> (a,b,c))
    (Dec.field "title" Dec.string)
    (Dec.field "description" Dec.string)
    (Dec.maybe (Dec.field "parameters" (
        Dec.keyValuePairs Dec.string
        |>  Dec.andThen (
            Helper.resultList (\(key, description) (others, dict) -> Math.parse key
                |> Result.andThen (treeToMatcher_ functions constants)
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
            (Dec.field "from" (expressionDecoder_ functions constants))
            (Dec.field "to" (expressionDecoder_ functions constants))
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

expressionDecoder_: Dict.Dict String FunctionProperties_ -> Set.Set String -> Dec.Decoder Expression_
expressionDecoder_ functions constants =
    Dec.andThen
    (\str -> case Math.parse str |> Result.andThen (treeToMatcher_ functions constants) of
        Err errStr -> Dec.fail ("'" ++ str ++ "' is not a valid expression: " ++ errStr)
        Ok (matcher, tokens) -> Dec.succeed {name = str, root = matcher, tokens = tokens}
    )
    Dec.string

treeToMatcher_: Dict.Dict String FunctionProperties_ -> Set.Set String -> Math.Tree () -> Result String (Matcher.Matcher, Dict.Dict String Token_)
treeToMatcher_ functions constants root =
    let
        processChildren = Helper.resultList (\child (list, tokens) -> treeToMatcher_ functions constants child
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
        Math.VariableNode s -> if Set.member s.name constants
            then Ok (Matcher.ExactMatcher {name = s.name, arguments = []}, Dict.empty)
            else Ok (Matcher.AnyMatcher {name = s.name, arguments = []}, Dict.singleton s.name AnyToken)
        Math.UnaryNode s -> treeToMatcher_ functions constants s.child
            |> Result.map (\(childMatcher, tokens) -> (Matcher.ExactMatcher {name = s.name, arguments = [childMatcher]}, tokens))
        Math.BinaryNode s -> processChildren s.children
            |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children}, tokens)) -- TODO: Switch to CommutativeAssociativeMatcher
        Math.DeclarativeNode s -> processChildren s.children
            |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children}, tokens))
        Math.GenericNode s -> case Dict.get s.name functions of
            Nothing -> processChildren s.children
                |> Result.map (\(children, tokens) ->
                    (Matcher.AnyMatcher {name = s.name, arguments = children}, Dict.insert s.name (List.length children |> FunctionToken) tokens)
                )
            Just prop -> if prop.associative || prop.commutative
                then processChildren s.children
                    |> Result.map (\(children, tokens) -> (Matcher.CommutativeMatcher {name = s.name, arguments = children}, tokens))
                else processChildren s.children
                    |> Result.map (\(children, tokens) -> (Matcher.ExactMatcher {name = s.name, arguments = children}, tokens))
