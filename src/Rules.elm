module Rules exposing (Model, Rule, Event, init, update, view, function)

import Dict
import Html exposing (Html, div)
import Json.Decode as Dec
import Set
import Math

{-
## Modeling rules
-}

type alias FunctionProperties_ =
    {   args: Int -- Number of arguments it takes
    ,   parameters: Int -- Number of parameters it takes
    ,   associative: Bool -- Allows processing in different order
    ,   commutative: Bool -- Allows swapping argument ordering
    ,   linear: Bool -- Allow processing individual terms in a sum, and then summing them back up
    }

type Token_ =
    FunctionToken FunctionProperties_
    | AnyToken

type alias Expression_ =
    {   root: Math.Tree ()
    ,   tokens: Dict.Dict String Token_
    }

type alias Rule =
    {   title: String
    ,   description: String
    ,   lhs: Expression_
    ,   rhs: Expression_
    ,   parameters: List (Expression_, String)
    }

type alias Topic =
    {   name: String
    ,   rules: List Rule
    ,   functions: Set.Set String -- all functions that are referenced in the rules, doesn't need to be stored
    }

{-
## Elm-y bits
-}
type alias Model =
    {   functions: Dict.Dict String FunctionProperties_
    ,   globalVariables: Dict.Dict String Float -- Like e or pi
    ,   topics: List Topic
    ,   createMode: Bool
    }

type Event =
    Click

init: Model
init = {rules = [], createMode = False}

update: Event -> Model -> (Model, Cmd Event)
update _ model = (model, Cmd.none)

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attrs model = div (attrs ++ []) []

function: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
function converter attrs model = div (attrs ++ []) []

{-
## Parser
-}

-- TODO: Validate all functions in rules are declared in functions
topicDecoder: Dec.Decoder (String, Dict.Dict String FunctionProperties_, Topic)
topicDecoder = Dec.map3 (\name functions rules -> (name, functions, rules))
    (Dec.field "name" Dec.string)
    (   Dec.field "functions"
        <| Dec.dict
        <| Dec.map4 FunctionProperties_
            (Dec.field "args" Dec.int)
            (Dec.field "associative" Dec.bool)
            (Dec.field "commutative" Dec.bool)
            (Dec.field "linear" Dec.bool)
    )
    (   Dec.field "equations"
        <| Dec.list ruleDecoder_
    )

-- TODO: Validate the equality (variables / functions)
ruleDecoder_: Dec.Decoder Rule
ruleDecoder_ = Dec.map4 Rule
    (Dec.field "title" Dec.string)
    (Dec.field "description" Dec.string)
    (Dec.field "lhs" sideDecoder_)
    (Dec.field "rhs" sideDecoder_)


-- TODO: Validate the expression
expressionDecoder_: Dec.Decoder Expression_
expressionDecoder_ = Dec.string
    |> Dec.andThen (\str -> case Math.parse str of
        Err errStr -> Dec.fail "'" ++ str ++ "' is not a valid expression: " ++ errStr
        Ok root -> let in
    )

tokenExtractor_: Math.Processor () (Result String (Dict.Dict String Token))
tokenExtractor =
    {   function: (\recurse dict (_, props) ->
    
            (global, Maybe (Tree state))
        )
    ,   var: (\res (_, name) -> case res of
            Err _ -> (res, Just (Math.Variable () name))
            Just dict -> case Dict.get name dict of
                Nothing -> (Ok (Dict.insert name AnyToken dict), Just (Math.Variable () name))
                Just tok -> case tok of
                    AnyToken -> (Ok dict, Just (Math.Variable () name))
                    FunctionToken -> (Err (name ++ " is shown as both a variable and a function"), Just (Math.Variable () name))
        )
    ,   real: (\res (_, val) -> (res, Just (Math.Real () val))) -- Numbers don't need to be tracked
    }