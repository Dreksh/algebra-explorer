module Math exposing (Tree(..), parse)
import Array exposing (Array)

type Tree state =
    Add state (List (Tree state))                   -- Unordered args
    | Multiply state (List (Tree state))            -- Unordered args
    | Equal state (List (Tree state))               -- Unordered args
    | Negative state (Tree state)                   -- Single arg
    | Reciprocal state (Tree state)                 -- Single arg
    | Function state String (Array (Tree state))    -- Name & Number of args, args are ordered
    | Variable state String                         -- No args
    | Real state Float                              -- No args
    | Collapsed state String                        -- String value, No Args

parse: String -> Result String (Tree ())
parse = parserParse

{-
1. Use the ./grammar-parser app to generate the transition table.
1. Paste it from "-- BEGIN AUTO GENERATED" onwards. It should override everything else

You will need to define the following:
    - type Parser<Grammar token name>
    - parserRule<rule number> -> ... -> Result String Parser<Grammar token name>), empty rules can be ignored
    - parserError<rule number> -> Int (Current Position) -> ParserToken (Received token) -> String

Helper functions provided:
    - parserTokenName: ParserToken -> String (to get the name as defined in grammar.txt)
    - parserGrammarString: String (content in grammar.txt)
    - parserParse: String -> Result () Parser<First Rule's Token Name>
-}

type alias ParserEquation = Tree ()
type alias ParserExpression = Tree ()
type alias ParserMultiple = Tree ()
type alias ParserNegatable = Tree ()
type alias ParserTerm = Tree ()
type alias ParserSpace = ()
type alias ParserArgs = List (Tree ())

parserRule1: ParserEquation -> ParserSpace -> String -> ParserExpression -> Result String ParserEquation
parserRule1 original _ _ expression = case original of
    Equal _ list -> Result.Ok (Equal () (List.append list [expression]))
    _ -> Result.Err "Unexpected top level token while parsing rule 1"

parserError1: Int -> ParserToken -> String
parserError1 num _ = case num of
    1 -> "Expecting Equation as the 1st element to rule 1"
    2 -> "Expecting a Space as the 2nd element to rule 1"
    3 -> "Expecting a '=' as the 3rd element to rule 1"
    4 -> "Expecting an Expression as the 4th element to rule 1"
    _ -> "Too many arguments to rule 1"

parserRule2: ParserExpression -> ParserSpace -> String -> ParserExpression -> Result String ParserEquation
parserRule2 original _ _ expression = Result.Ok (Equal () [original, expression])

parserError2: Int -> ParserToken -> String
parserError2 num _ = case num of
    1 -> "Expecting Expression as the 1st element to rule 2"
    2 -> "Expecting a Space as the 2nd element to rule 2"
    3 -> "Expecting a '=' as the 3rd element to rule 2"
    4 -> "Expecting an Expression as the 4th element to rule 2"
    _ -> "Too many arguments to rule 2"

parserRule3: ParserExpression -> ParserSpace -> String -> ParserMultiple -> Result String ParserExpression
parserRule3 original _ _ expression = case original of
    Add _ list -> Result.Ok (Add () (List.append list [expression]))
    _ -> Result.Ok (Add () [original, expression])

parserError3: Int -> ParserToken -> String
parserError3 num _ = case num of
    1 -> "Expecting Expression as the 1st element to rule 3"
    2 -> "Expecting a Space as the 2nd element to rule 3"
    3 -> "Expecting a '+' as the 3rd element to rule 3"
    4 -> "Expecting an Expression as the 4th element to rule 3"
    _ -> "Too many arguments to rule 3"

parserRule4: ParserExpression -> ParserSpace -> String -> ParserMultiple -> Result String ParserExpression
parserRule4 original _ _ expression = case original of
    Add _ list -> Result.Ok (Add () (List.append list [Negative () expression]))
    _ -> Result.Ok (Add () [original, (Negative () expression)])

parserError4: Int -> ParserToken -> String
parserError4 num _ = case num of
    1 -> "Expecting Expression as the 1st element to rule 4"
    2 -> "Expecting a Space as the 2nd element to rule 4"
    3 -> "Expecting a '-' as the 3rd element to rule 4"
    4 -> "Expecting an Expression as the 4th element to rule 4"
    _ -> "Too many arguments to rule 4"

parserRule5: ParserMultiple -> Result String ParserExpression
parserRule5 = Result.Ok

parserError5: Int -> ParserToken -> String
parserError5 _ _ = "surprised this errored: rule 5"

parserRule6: ParserMultiple -> ParserSpace -> String -> ParserNegatable -> Result String ParserMultiple
parserRule6 original _ _ expression = case original of
    Multiply _ list -> Result.Ok (Multiply () (List.append list [expression]))
    _ -> Result.Ok (Multiply () [original, expression])

parserError6: Int -> ParserToken -> String
parserError6 num _ = case num of
    1 -> "Expecting Expression as the 1st element to rule 6"
    2 -> "Expecting a Space as the 2nd element to rule 6"
    3 -> "Expecting a '*' as the 3rd element to rule 6"
    4 -> "Expecting an Expression as the 4th element to rule 6"
    _ -> "Too many arguments to rule 6"

parserRule7: ParserMultiple -> ParserSpace -> String -> ParserNegatable -> Result String ParserMultiple
parserRule7 original _ _ expression = case original of
    Multiply _ list -> Result.Ok (Multiply () (List.append list [Reciprocal () expression]))
    _ -> Result.Ok (Multiply () [original, (Reciprocal () expression)])

parserError7: Int -> ParserToken -> String
parserError7 num _ = case num of
    1 -> "Expecting Expression as the 1st element to rule 7"
    2 -> "Expecting a Space as the 2nd element to rule 7"
    3 -> "Expecting a '/' as the 3rd element to rule 7"
    4 -> "Expecting an Expression as the 4th element to rule 7"
    _ -> "Too many arguments to rule 7"

parserRule8: ParserMultiple -> Result String ParserExpression
parserRule8 = Result.Ok

parserError8: Int -> ParserToken -> String
parserError8 _ _ = "surprised this errored: rule 8"

parserRule9: ParserSpace -> String -> ParserNegatable -> Result String ParserNegatable
parserRule9 _ _ child = Result.Ok (Negative () child)

parserError9: Int -> ParserToken -> String
parserError9 _ _ = "surprised this errored: rule 9"

parserRule10: ParserSpace -> ParserTerm -> Result String ParserNegatable
parserRule10 _ term = Result.Ok term

parserError10: Int -> ParserToken -> String
parserError10 _ _ = "surprised this errored: rule 10"

parserRule11: String -> String -> ParserArgs -> String -> Result String ParserTerm
parserRule11 name _ args _ = Result.Ok (Function () name (Array.fromList args))

parserError11: Int -> ParserToken -> String
parserError11 _ _ = "surprised this errored: rule 11"

parserRule12: String -> Result String ParserTerm
parserRule12 name = Result.Ok (Variable () name)

parserError12: Int -> ParserToken -> String
parserError12 _ _ = "surprised this errored: rule 12"

parserRule13: String -> ParserExpression -> ParserSpace -> String -> Result String ParserTerm
parserRule13 _ child _ _ = Result.Ok child

parserError13: Int -> ParserToken -> String
parserError13 _ _ = "surprised this errored: rule 13"

parserRule14: String -> Result String ParserSpace
parserRule14 _ = Result.Ok ()

parserError14: Int -> ParserToken -> String
parserError14 _ _ = "surprised this errored: rule 14"

parserRule16: ParserArgs -> ParserSpace -> String -> ParserExpression -> Result String ParserArgs
parserRule16 original _ _ next = Result.Ok (List.append original [next])

parserError16: Int -> ParserToken -> String
parserError16 _ _ = "surprised this errored: rule 16"

parserRule17: ParserExpression -> Result String ParserArgs
parserRule17 next = Result.Ok [next]

parserError17: Int -> ParserToken -> String
parserError17 _ _ = "surprised this errored: rule 17"

-- BEGIN AUTO GENERATED
-- Grammar input
parserGrammer: String
parserGrammer =
    """
    Equation    ->  Equation Space "=" Expression
    Equation    ->  Expression Space "=" Expression
    Expression  ->  Expression Space "\\+" Multiple
    Expression  ->  Expression Space "-" Multiple
    Expression  ->  Multiple
    Multiple    ->  Multiple Space "\\*" Negatable
    Multiple    ->  Multiple Space "/" Negatable
    Multiple    ->  Negatable
    Negatable   ->  Space "-" Negatable
    Negatable   ->  Space Term
    Term        ->  "\\o[0-9\\o]*" "(" Args ")"
    Term        ->  "\\o[0-9\\o]*"
    Term        ->  "(" Expression Space ")"
    Space       ->  " +"
    Space       ->
    Args        ->  Args Space "," Expression
    Args        ->  Expression
    Args        ->
    """

-- Tokens
type ParserToken =
    ParserTokenEnd
    | ParserTokenEquation ParserEquation
    | ParserTokenExpression ParserExpression
    | ParserTokenMultiple ParserMultiple
    | ParserTokenNegatable ParserNegatable
    | ParserTokenTerm ParserTerm
    | ParserTokenSpace ParserSpace
    | ParserTokenArgs ParserArgs
    | ParserToken1 String
    | ParserToken2 String
    | ParserToken3 String
    | ParserToken4 String
    | ParserToken5 String
    | ParserToken6 String
    | ParserToken7 String
    | ParserToken8 String
    | ParserToken9 String
    | ParserToken10 String

parserTokenName: ParserToken -> String
parserTokenName token = case token of
    ParserTokenEnd -> "$"
    ParserTokenEquation _ -> "Equation"
    ParserTokenExpression _ -> "Expression"
    ParserTokenMultiple _ -> "Multiple"
    ParserTokenNegatable _ -> "Negatable"
    ParserTokenTerm _ -> "Term"
    ParserTokenSpace _ -> "Space"
    ParserTokenArgs _ -> "Args"
    ParserToken1 _ -> "\"=\""
    ParserToken2 _ -> "\"\\+\""
    ParserToken3 _ -> "\"-\""
    ParserToken4 _ -> "\"\\*\""
    ParserToken5 _ -> "\"/\""
    ParserToken6 _ -> "\"\\o[0-9\\o]*\""
    ParserToken7 _ -> "\"(\""
    ParserToken8 _ -> "\")\""
    ParserToken9 _ -> "\" +\""
    ParserToken10 _ -> "\",\""

parserExpectTokenEquation: ParserToken -> Result () ParserEquation
parserExpectTokenEquation token = case token of
    ParserTokenEquation e -> Result.Ok e
    _ -> Result.Err ()

parserExpectTokenExpression: ParserToken -> Result () ParserExpression
parserExpectTokenExpression token = case token of
    ParserTokenExpression e -> Result.Ok e
    _ -> Result.Err ()

parserExpectTokenMultiple: ParserToken -> Result () ParserMultiple
parserExpectTokenMultiple token = case token of
    ParserTokenMultiple e -> Result.Ok e
    _ -> Result.Err ()

parserExpectTokenNegatable: ParserToken -> Result () ParserNegatable
parserExpectTokenNegatable token = case token of
    ParserTokenNegatable e -> Result.Ok e
    _ -> Result.Err ()

parserExpectTokenTerm: ParserToken -> Result () ParserTerm
parserExpectTokenTerm token = case token of
    ParserTokenTerm e -> Result.Ok e
    _ -> Result.Err ()

parserExpectTokenSpace: ParserToken -> Result () ParserSpace
parserExpectTokenSpace token = case token of
    ParserTokenSpace e -> Result.Ok e
    _ -> Result.Err ()

parserExpectTokenArgs: ParserToken -> Result () ParserArgs
parserExpectTokenArgs token = case token of
    ParserTokenArgs e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken1: ParserToken -> Result () String
parserExpectToken1 token = case token of
    ParserToken1 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken2: ParserToken -> Result () String
parserExpectToken2 token = case token of
    ParserToken2 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken3: ParserToken -> Result () String
parserExpectToken3 token = case token of
    ParserToken3 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken4: ParserToken -> Result () String
parserExpectToken4 token = case token of
    ParserToken4 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken5: ParserToken -> Result () String
parserExpectToken5 token = case token of
    ParserToken5 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken6: ParserToken -> Result () String
parserExpectToken6 token = case token of
    ParserToken6 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken7: ParserToken -> Result () String
parserExpectToken7 token = case token of
    ParserToken7 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken8: ParserToken -> Result () String
parserExpectToken8 token = case token of
    ParserToken8 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken9: ParserToken -> Result () String
parserExpectToken9 token = case token of
    ParserToken9 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken10: ParserToken -> Result () String
parserExpectToken10 token = case token of
    ParserToken10 e -> Result.Ok e
    _ -> Result.Err ()

-- TokenState
type alias ParserStateToken =
    {   seen: String
    ,   state: Int
    }

parserProcessChar: ParserStateToken -> String -> (ParserStateToken, Maybe ParserToken)
parserProcessChar state char = case state.state of
    0 -> if String.contains char "0123456789"
        then let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Nothing)
        else if String.contains char " "
        then ({state | seen = state.seen ++ char, state = 2}, Nothing)
        else if String.contains char "("
        then ({state | seen = state.seen ++ char, state = 3}, Nothing)
        else if String.contains char ")"
        then ({state | seen = state.seen ++ char, state = 4}, Nothing)
        else if String.contains char "*"
        then ({state | seen = state.seen ++ char, state = 5}, Nothing)
        else if String.contains char "+"
        then ({state | seen = state.seen ++ char, state = 6}, Nothing)
        else if String.contains char ","
        then ({state | seen = state.seen ++ char, state = 7}, Nothing)
        else if String.contains char "-"
        then ({state | seen = state.seen ++ char, state = 8}, Nothing)
        else if String.contains char "/"
        then ({state | seen = state.seen ++ char, state = 9}, Nothing)
        else if String.contains char "="
        then ({state | seen = state.seen ++ char, state = 10}, Nothing)
        else ({state | seen = state.seen ++ char, state = 1}, Nothing)
    1 -> if String.contains char " ()*+,-/="
        then let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken6 state.seen))
        else ({state | seen = state.seen ++ char, state = 11}, Nothing)
    2 -> if String.contains char " "
        then ({state | seen = state.seen ++ char, state = 2}, Nothing)
        else let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken9 state.seen))
    3 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken7 state.seen))
    4 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken8 state.seen))
    5 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken4 state.seen))
    6 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken2 state.seen))
    7 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken10 state.seen))
    8 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken3 state.seen))
    9 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken5 state.seen))
    10 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken1 state.seen))
    11 -> if String.contains char " ()*+,-/="
        then let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken6 state.seen))
        else ({state | seen = state.seen ++ char, state = 11}, Nothing)
    _ -> ({seen = "", state = 0}, Nothing)

parserProcessEnd: ParserStateToken -> Maybe ParserToken
parserProcessEnd state = case state.state of
    1 -> Just (ParserToken6 state.seen)
    2 -> Just (ParserToken9 state.seen)
    3 -> Just (ParserToken7 state.seen)
    4 -> Just (ParserToken8 state.seen)
    5 -> Just (ParserToken4 state.seen)
    6 -> Just (ParserToken2 state.seen)
    7 -> Just (ParserToken10 state.seen)
    8 -> Just (ParserToken3 state.seen)
    9 -> Just (ParserToken5 state.seen)
    10 -> Just (ParserToken1 state.seen)
    11 -> Just (ParserToken6 state.seen)
    _ -> Nothing

-- Transitions
parserParse: String -> Result String ParserExpression
parserParse input =
    input
    |>  String.foldl
        (\char result -> case result of
            Result.Err _ -> result
            Result.Ok (tokenState, parseState) ->
                String.fromChar char
                |> parserProcessChar tokenState
                |> (\(newState, token) ->
                    case token of
                        Nothing -> Result.Ok (newState, parseState)
                        Just t -> parserProcessToken t parseState
                            |> Result.map (\pState -> (newState, pState))
                )
        )
        (Result.Ok ({seen="", state=0}, {stack=[], states=[0]}))
    |>  Result.andThen (\(tokenState, parseState) ->
            parserProcessEnd tokenState
            |> Maybe.map (\tok -> parserProcessToken tok parseState )
            |> Maybe.withDefault (Result.Ok parseState)
        )
    |>  Result.andThen (\pState ->
        pState
        |> parserProcessToken ParserTokenEnd
        |> Result.andThen (\state -> case state.stack of
            [e] -> parserExpectTokenExpression e |> Result.mapError (\_ -> "Parser result has incorrect type")
            [] -> Result.Err "Parser result is missing"
            _ -> Result.Err "Parser did not complete"
        )
    )

type alias ParserState =
    {   stack: List ParserToken
    ,   states: List Int
    }

parserExtractLastN: Int -> (ParserState, List ParserToken) -> Result () (ParserState, List ParserToken)
parserExtractLastN remaining (state, stack) = case remaining of
    0 -> Result.Ok (state, stack)
    r -> case parserExtractLastN ( r - 1 ) (state,stack) of
        Result.Ok (newState, newStack) -> case newState.stack of
            (e::others) -> Result.Ok ({newState | stack = others}, (e::newStack))
            [] -> Result.Err ()
        Result.Err () -> Result.Err ()

parserExtract: (ParserToken -> Result () token) -> (Int -> ParserToken -> String) -> Int -> Result String (List ParserToken, (token -> func)) -> Result String (List ParserToken, func)
parserExtract extract errFunc stepNum state =
    Result.andThen
    ( \(s, f) -> case s of
        (t::others) -> extract t
            |> Result.andThen (\tok -> Result.Ok (others, f tok))
            |> Result.mapError (\_ -> errFunc stepNum t)
        [] -> Result.Err "Temporary stack is missing tokens to process the rule"
    )
    state

parserProcessRule1: ParserState -> Result String ParserState
parserProcessRule1 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule1)
        |> parserExtract parserExpectTokenEquation parserError1 1
        |> parserExtract parserExpectTokenSpace parserError1 2
        |> parserExtract parserExpectToken1 parserError1 3
        |> parserExtract parserExpectTokenExpression parserError1 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenEquation o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule2: ParserState -> Result String ParserState
parserProcessRule2 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule2)
        |> parserExtract parserExpectTokenExpression parserError2 1
        |> parserExtract parserExpectTokenSpace parserError2 2
        |> parserExtract parserExpectToken1 parserError2 3
        |> parserExtract parserExpectTokenExpression parserError2 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenEquation o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule3: ParserState -> Result String ParserState
parserProcessRule3 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule3)
        |> parserExtract parserExpectTokenExpression parserError3 1
        |> parserExtract parserExpectTokenSpace parserError3 2
        |> parserExtract parserExpectToken2 parserError3 3
        |> parserExtract parserExpectTokenMultiple parserError3 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenExpression o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule4: ParserState -> Result String ParserState
parserProcessRule4 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule4)
        |> parserExtract parserExpectTokenExpression parserError4 1
        |> parserExtract parserExpectTokenSpace parserError4 2
        |> parserExtract parserExpectToken3 parserError4 3
        |> parserExtract parserExpectTokenMultiple parserError4 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenExpression o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule5: ParserState -> Result String ParserState
parserProcessRule5 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule5)
        |> parserExtract parserExpectTokenMultiple parserError5 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenExpression o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule6: ParserState -> Result String ParserState
parserProcessRule6 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule6)
        |> parserExtract parserExpectTokenMultiple parserError6 1
        |> parserExtract parserExpectTokenSpace parserError6 2
        |> parserExtract parserExpectToken4 parserError6 3
        |> parserExtract parserExpectTokenNegatable parserError6 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenMultiple o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule7: ParserState -> Result String ParserState
parserProcessRule7 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule7)
        |> parserExtract parserExpectTokenMultiple parserError7 1
        |> parserExtract parserExpectTokenSpace parserError7 2
        |> parserExtract parserExpectToken5 parserError7 3
        |> parserExtract parserExpectTokenNegatable parserError7 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenMultiple o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule8: ParserState -> Result String ParserState
parserProcessRule8 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule8)
        |> parserExtract parserExpectTokenNegatable parserError8 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenMultiple o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule9: ParserState -> Result String ParserState
parserProcessRule9 state = case parserExtractLastN 3 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule9)
        |> parserExtract parserExpectTokenSpace parserError9 1
        |> parserExtract parserExpectToken3 parserError9 2
        |> parserExtract parserExpectTokenNegatable parserError9 3
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenNegatable o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule10: ParserState -> Result String ParserState
parserProcessRule10 state = case parserExtractLastN 2 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule10)
        |> parserExtract parserExpectTokenSpace parserError10 1
        |> parserExtract parserExpectTokenTerm parserError10 2
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenNegatable o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule11: ParserState -> Result String ParserState
parserProcessRule11 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule11)
        |> parserExtract parserExpectToken6 parserError11 1
        |> parserExtract parserExpectToken7 parserError11 2
        |> parserExtract parserExpectTokenArgs parserError11 3
        |> parserExtract parserExpectToken8 parserError11 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule12: ParserState -> Result String ParserState
parserProcessRule12 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule12)
        |> parserExtract parserExpectToken6 parserError12 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule13: ParserState -> Result String ParserState
parserProcessRule13 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule13)
        |> parserExtract parserExpectToken7 parserError13 1
        |> parserExtract parserExpectTokenExpression parserError13 2
        |> parserExtract parserExpectTokenSpace parserError13 3
        |> parserExtract parserExpectToken8 parserError13 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule14: ParserState -> Result String ParserState
parserProcessRule14 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule14)
        |> parserExtract parserExpectToken9 parserError14 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenSpace o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule16: ParserState -> Result String ParserState
parserProcessRule16 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule16)
        |> parserExtract parserExpectTokenArgs parserError16 1
        |> parserExtract parserExpectTokenSpace parserError16 2
        |> parserExtract parserExpectToken10 parserError16 3
        |> parserExtract parserExpectTokenExpression parserError16 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenArgs o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule17: ParserState -> Result String ParserState
parserProcessRule17 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule17)
        |> parserExtract parserExpectTokenExpression parserError17 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenArgs o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessToken: ParserToken -> ParserState -> Result String ParserState
parserProcessToken token state = case state.states of
    [] -> Result.Err "Parser has completed"
    (0::_) -> case token of
        ParserTokenEquation _ -> Result.Ok {state | states = (1::state.states), stack=(token::state.stack) }
        ParserTokenExpression _ -> Result.Ok {state | states = (2::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError1 1 other)
    (1::_) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken1 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError1 2 other)
    (2::_) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (13::state.states), stack=(token::state.stack) }
        ParserToken1 _ -> Result.Ok {state | states = (14::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError2 2 other)
    (3::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken4 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken5 _ -> Result.Ok {state | states = (19::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> parserProcessRule5 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (4::states) -> case token of
        other -> parserProcessRule8 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (5::states) -> case token of
        other -> parserProcessRule10 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (6::_) -> case token of
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError9 2 other)
    (7::_) -> case token of
        ParserTokenNegatable _ -> Result.Ok {state | states = (20::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError9 3 other)
    (8::states) -> case token of
        ParserToken7 _ -> Result.Ok {state | states = (21::state.states), stack=(token::state.stack) }
        other -> parserProcessRule12 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (9::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (22::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 1 other)
    (10::states) -> case token of
        other -> parserProcessRule14 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (11::_) -> case token of
        ParserToken1 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError1 3 other)
    (12::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (23::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError1 4 other)
    (13::_) -> case token of
        ParserToken1 _ -> Result.Ok {state | states = (14::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError2 3 other)
    (14::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (24::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError2 4 other)
    (15::_) -> case token of
        ParserTokenMultiple _ -> Result.Ok {state | states = (25::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 4 other)
    (16::_) -> case token of
        ParserTokenMultiple _ -> Result.Ok {state | states = (26::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError4 4 other)
    (17::_) -> case token of
        ParserToken4 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken5 _ -> Result.Ok {state | states = (19::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError6 3 other)
    (18::_) -> case token of
        ParserTokenNegatable _ -> Result.Ok {state | states = (27::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError6 4 other)
    (19::_) -> case token of
        ParserTokenNegatable _ -> Result.Ok {state | states = (28::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError7 4 other)
    (20::states) -> case token of
        other -> parserProcessRule9 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (21::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (29::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (30::state.states), stack=(token::state.stack) }
        ParserTokenArgs _ -> Result.Ok {state | states = (31::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (32::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken10 _ -> Result.Ok {state | states = (33::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 1 other)
    (22::_) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (34::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (35::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 2 other)
    (23::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (36::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> parserProcessRule1 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (24::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (36::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> parserProcessRule2 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (25::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken4 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken5 _ -> Result.Ok {state | states = (19::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> parserProcessRule3 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (26::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken4 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken5 _ -> Result.Ok {state | states = (19::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> parserProcessRule4 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (27::states) -> case token of
        other -> parserProcessRule6 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (28::states) -> case token of
        other -> parserProcessRule7 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (29::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (36::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> parserProcessRule17 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (30::_) -> case token of
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken10 _ -> Result.Ok {state | states = (33::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError9 2 other)
    (31::_) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (37::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (32::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken10 _ -> Result.Ok {state | states = (33::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError11 4 other)
    (32::states) -> case token of
        other -> parserProcessRule11 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (33::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (38::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken7 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 1 other)
    (34::_) -> case token of
        ParserToken2 _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (35::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 3 other)
    (35::states) -> case token of
        other -> parserProcessRule13 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (36::_) -> case token of
        ParserToken2 _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 3 other)
    (37::_) -> case token of
        ParserToken10 _ -> Result.Ok {state | states = (33::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError16 3 other)
    (38::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (36::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        other -> parserProcessRule16 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (_::_) -> Result.Err "Unknown state reached"

