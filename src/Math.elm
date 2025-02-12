module Math exposing (Tree(..), notation, parse, toString)

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
    | Collapsed state (Tree state)                  -- No args, simply marks the state for how to preview

parse: String -> Result String (Tree ())
parse = parserParse

toString: Tree state -> String
toString root = case root of
    Reciprocal _ _ -> "1" ++ toStringRecursive root
    Collapsed _ child -> toStringRecursive child
    _ -> toStringRecursive root

toStringRecursive: Tree state -> String
toStringRecursive root = case root of
    Add _ list -> list
        |> List.foldl (\elem result ->
            if result == "" then toStringRecursive elem
            else case elem of
                Multiply _ _ -> result ++ "+" ++ toStringRecursive elem
                Negative _ _ -> result ++ toStringRecursive elem
                Reciprocal _ _ -> result ++ "+1" ++ toStringRecursive elem
                Function _ _ _ -> result ++ "+" ++ toStringRecursive elem
                Variable _ _ -> result ++ "+" ++ toStringRecursive elem
                Real _ _ -> result ++ "+" ++ toStringRecursive elem
                _ -> result ++ "+(" ++ (toStringRecursive elem) ++ ")"
        ) ""
    Multiply _ list -> list
        |> List.foldl (\elem (prev, result) -> case prev of
            Nothing -> case elem of
                Add _ _ -> (Just elem, "(" ++ toStringRecursive elem ++ ")")
                Reciprocal _ _ -> (Just elem, "1" ++ toStringRecursive elem)
                _ -> (Just elem, toStringRecursive elem)
            Just (Variable _ name) -> if String.length name == 1
                then case elem of
                    Function _ _ _ -> (Just elem, result ++ toStringRecursive elem)
                    Variable _ _ -> (Just elem, result ++ toStringRecursive elem)
                    Real _ _ -> (Just elem, result ++ "*" ++ toStringRecursive elem)
                    _ -> (Just elem, result ++ "(" ++ toStringRecursive elem ++ ")")
                else case elem of
                    Function _ _ _ -> (Just elem, result ++ toStringRecursive elem)
                    Variable _ nextName -> if String.length nextName == 1
                        then (Just elem, result ++ "*" ++ toStringRecursive elem)
                        else (Just elem, result ++ toStringRecursive elem)
                    Real _ _ -> (Just elem, result ++ "*" ++ toStringRecursive elem)
                    _ -> (Just elem, result ++ "*(" ++ toStringRecursive elem ++ ")")
            _ -> case elem of
                Function _ _ _ -> (Just elem, result ++ toStringRecursive elem)
                Variable _ _ -> (Just elem, result ++ toStringRecursive elem)
                Real _ _ -> (Just elem, result ++ "*" ++ toStringRecursive elem)
                _ -> (Just elem, result ++ "(" ++ toStringRecursive elem ++ ")")
        ) (Nothing, "")
        |> Tuple.second
    Equal _ list -> list
        |> List.foldl (\elem result -> 
            if result == "" then toString elem
            else result ++ "=" ++ toString elem
        ) ""
    Negative _ child -> case child of
        Reciprocal _ _ -> "-1" ++ toStringRecursive child
        Multiply _ _ -> "-" ++ toStringRecursive child
        Function _ _ _ -> "-" ++ toStringRecursive child
        Variable _ _ -> "-" ++ toStringRecursive child
        Real _ _ -> "-" ++ toStringRecursive child
        _ -> "-(" ++ toStringRecursive child ++ ")"
    Reciprocal _ child -> case child of
        Negative _ _ -> "/" ++ toStringRecursive child
        Function _ _ _ -> "/" ++ toStringRecursive child
        Variable _ _ -> "/" ++ toStringRecursive child
        Real _ _ -> "/" ++ toStringRecursive child
        _ -> "/(" ++ toStringRecursive child ++ ")"
    Function _ name list -> list
        |> Array.foldl (\elem result -> 
            if result == "" then toStringRecursive elem
            else result ++ "," ++ toStringRecursive elem
        ) ""
        |> (\inside -> "\\" ++ name ++ "(" ++ inside ++ ")")
    Variable _ name -> if String.length name == 1 then name else "\\" ++ name
    Real _ val -> String.fromFloat val
    Collapsed _ child -> "(" ++ toStringRecursive child ++ ")"

notation: String
notation = """
a - a variable called "a"
ab - a variable "a" multiplied by a variable "b"
\\ab - a variable with a long name called "ab"
\\a(x,y;z) - a function called "a" that takes in x and y, and uses z
"""

-- Parser implementation

type alias ParserEquation = Tree ()
type alias ParserExpression = Tree ()
type alias ParserMultiple = Tree ()
type alias ParserNegatable = Tree ()
type alias ParserTerm = Tree ()
type alias ParserSpace = ()
type alias ParserArgs = List (Tree ())

-- Equation -> Equation Space "=" Expression
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

-- Equation -> Expression Space "=" Expression
parserRule2: ParserExpression -> ParserSpace -> String -> ParserExpression -> Result String ParserEquation
parserRule2 original _ _ expression = Result.Ok (Equal () [original, expression])

parserError2: Int -> ParserToken -> String
parserError2 num _ = case num of
    1 -> "Expecting Expression as the 1st element to rule 2"
    2 -> "Expecting a Space as the 2nd element to rule 2"
    3 -> "Expecting a '=' as the 3rd element to rule 2"
    4 -> "Expecting an Expression as the 4th element to rule 2"
    _ -> "Too many arguments to rule 2"

-- Expression -> Expression Space "\+" Expression
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

-- Expression -> Expression Space "-" Expression
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

-- Expression -> Multiple
parserRule5: ParserMultiple -> Result String ParserExpression
parserRule5 = Result.Ok

parserError5: Int -> ParserToken -> String
parserError5 _ _ = "surprised this errored: rule 5"

-- Multiple -> Multiple Space "'*" Negatable
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

-- Multiple Space "/" Negatable
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

-- Multiple -> Multiple Space Term
parserRule8: ParserMultiple -> ParserSpace -> ParserTerm -> Result String ParserMultiple
parserRule8 original _ next = case original of
    Multiply _ list -> Result.Ok (Multiply () (List.append list [next]))
    _ -> Result.Ok (Multiply () [original, next])

parserError8: Int -> ParserToken -> String
parserError8 _ _ = "blah: rule 8"

-- Multiple -> Negatable
parserRule9: ParserMultiple -> Result String ParserExpression
parserRule9 = Result.Ok

parserError9: Int -> ParserToken -> String
parserError9 _ _ = "surprised this errored: rule 9"

-- Negatable -> Space "-" Negatable
parserRule10: ParserSpace -> String -> ParserNegatable -> Result String ParserNegatable
parserRule10 _ _ child = Result.Ok (Negative () child)

parserError10: Int -> ParserToken -> String
parserError10 _ _ = "surprised this errored: rule 10"

-- Negatable -> Space Term
parserRule11: ParserSpace -> ParserTerm -> Result String ParserNegatable
parserRule11 _ term = Result.Ok term

parserError11: Int -> ParserToken -> String
parserError11 _ _ = "surprised this errored: rule 11"

-- Term -> "\\" "\o[0-9\o]+"
parserRule12: String -> String -> Result String ParserTerm
parserRule12 _ name = Result.Ok (Variable () name)

parserError12: Int -> ParserToken -> String
parserError12 _ _ = "surprised this errored: rule 12"

-- Term -> "\\" "\o"
parserRule13: String -> String -> Result String ParserTerm
parserRule13 = parserRule12

parserError13: Int -> ParserToken -> String
parserError13 _ _ = "surprised this errored: rule 13"

-- Term -> "\\" "\o[0-9\o]+" "(" Args ")"
parserRule14: String -> String -> String -> ParserArgs -> String -> Result String ParserTerm
parserRule14 _ name _ args _ = Result.Ok (Function () name (Array.fromList args))

parserError14: Int -> ParserToken -> String
parserError14 _ _ = "surprised this errored: rule 14"

-- Term -> "\\" "\o" "(" Args ")"
parserRule15: String -> String -> String -> ParserArgs -> String -> Result String ParserTerm
parserRule15 = parserRule14

parserError15: Int -> ParserToken -> String
parserError15 _ _ = "surprised this errored: rule 15"

-- Term -> "\o"
parserRule16: String -> Result String ParserTerm
parserRule16 name = Result.Ok (Variable () name)

parserError16: Int -> ParserToken -> String
parserError16 _ _ = "surprised this errored: rule 16"

-- Term -> "[0-9]+" "\." "[0-9]+"
parserRule17: String -> String -> String -> Result String ParserTerm
parserRule17 number _ decimal = case number ++ "." ++ decimal |> String.toFloat of
    Just val -> Result.Ok ( Real () val)
    Nothing -> Result.Err ("invalid float: " ++ number ++ "." ++ decimal)

parserError17: Int -> ParserToken -> String
parserError17 _ _ = "surprised this errored: rule 17"

-- Term -> "[0-9]+"
parserRule18: String -> Result String ParserTerm
parserRule18 str = case String.toFloat str of
    Just val -> Result.Ok (Real () val)
    Nothing -> Result.Err ("invalid float: " ++ str)

parserError18: Int -> ParserToken -> String
parserError18 _ _ = "surprised this errored: rule 18"

-- Term -> "(" Expression Space ")"
parserRule19: String -> ParserExpression -> ParserSpace -> String -> Result String ParserTerm
parserRule19 _ child _ _ = Result.Ok child

parserError19: Int -> ParserToken -> String
parserError19 _ _ = "surprised this errored: rule 19"

-- Args -> Args Space "," Expression
parserRule20: ParserArgs -> ParserSpace -> String -> ParserExpression -> Result String ParserArgs
parserRule20 original _ _ next = Result.Ok (List.append original [next])

parserError20: Int -> ParserToken -> String
parserError20 _ _ = "surprised this errored: rule 20"

-- Args -> Expression
parserRule21: ParserExpression -> Result String ParserArgs
parserRule21 next = Result.Ok [next]

parserError21: Int -> ParserToken -> String
parserError21 _ _ = "surprised this errored: rule 21"

-- Space -> " +"
parserRule23: String -> Result String ParserSpace
parserRule23 _ = Result.Ok ()

parserError23: Int -> ParserToken -> String
parserError23 _ _ = "surprised this errored: rule 23"

-- BEGIN AUTO GENERATED
{-
Equation    ->  Equation Space "=" Expression
Equation    ->  Expression Space "=" Expression
Expression  ->  Expression Space "\+" Multiple
Expression  ->  Expression Space "-" Multiple
Expression  ->  Multiple
Multiple    ->  Multiple Space "\*" Negatable
Multiple    ->  Multiple Space "/" Negatable
Multiple    ->  Multiple Space Term
Multiple    ->  Negatable
Negatable   ->  Space "-" Negatable
Negatable   ->  Space Term
Term        ->  "\\" "\o[0-9\o]+"
Term        ->  "\\" "\o"
Term        ->  "\\" "\o[0-9\o]+" "(" Args ")"
Term        ->  "\\" "\o" "(" Args ")"
Term        ->  "\o"
Term        -> "[0-9]+" "\." "[0-9]+"
Term        -> "[0-9]+"
Term        ->  "(" Expression Space ")"
Args        ->  Args Space "," Expression
Args        ->  Expression
Args        ->
Space       ->  " +"
Space       ->
-}

-- Tokens
type ParserToken =
    ParserTokenEnd
    | ParserTokenEquation ParserEquation
    | ParserTokenExpression ParserExpression
    | ParserTokenMultiple ParserMultiple
    | ParserTokenNegatable ParserNegatable
    | ParserTokenTerm ParserTerm
    | ParserTokenArgs ParserArgs
    | ParserTokenSpace ParserSpace
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
    | ParserToken11 String
    | ParserToken12 String
    | ParserToken13 String
    | ParserToken14 String

parserTokenName: ParserToken -> String
parserTokenName token = case token of
    ParserTokenEnd -> "$"
    ParserTokenEquation _ -> "Equation"
    ParserTokenExpression _ -> "Expression"
    ParserTokenMultiple _ -> "Multiple"
    ParserTokenNegatable _ -> "Negatable"
    ParserTokenTerm _ -> "Term"
    ParserTokenArgs _ -> "Args"
    ParserTokenSpace _ -> "Space"
    ParserToken1 _ -> "\"=\""
    ParserToken2 _ -> "\"\\+\""
    ParserToken3 _ -> "\"-\""
    ParserToken4 _ -> "\"\\*\""
    ParserToken5 _ -> "\"/\""
    ParserToken6 _ -> "\"\\\\\""
    ParserToken7 _ -> "\"\\o[0-9\\o]+\""
    ParserToken8 _ -> "\"\\o\""
    ParserToken9 _ -> "\"(\""
    ParserToken10 _ -> "\")\""
    ParserToken11 _ -> "\"[0-9]+\""
    ParserToken12 _ -> "\"\\.\""
    ParserToken13 _ -> "\",\""
    ParserToken14 _ -> "\" +\""

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

parserExpectTokenArgs: ParserToken -> Result () ParserArgs
parserExpectTokenArgs token = case token of
    ParserTokenArgs e -> Result.Ok e
    _ -> Result.Err ()

parserExpectTokenSpace: ParserToken -> Result () ParserSpace
parserExpectTokenSpace token = case token of
    ParserTokenSpace e -> Result.Ok e
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

parserExpectToken11: ParserToken -> Result () String
parserExpectToken11 token = case token of
    ParserToken11 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken12: ParserToken -> Result () String
parserExpectToken12 token = case token of
    ParserToken12 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken13: ParserToken -> Result () String
parserExpectToken13 token = case token of
    ParserToken13 e -> Result.Ok e
    _ -> Result.Err ()

parserExpectToken14: ParserToken -> Result () String
parserExpectToken14 token = case token of
    ParserToken14 e -> Result.Ok e
    _ -> Result.Err ()

-- TokenState
type alias ParserStateToken =
    {   seen: String
    ,   state: Int
    }

parserProcessChar: ParserStateToken -> String -> (ParserStateToken, Maybe ParserToken)
parserProcessChar state char = case state.state of
    0 -> if String.contains char " "
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
        else if String.contains char "."
        then ({state | seen = state.seen ++ char, state = 9}, Nothing)
        else if String.contains char "/"
        then ({state | seen = state.seen ++ char, state = 10}, Nothing)
        else if String.contains char "0123456789"
        then ({state | seen = state.seen ++ char, state = 11}, Nothing)
        else if String.contains char "="
        then ({state | seen = state.seen ++ char, state = 12}, Nothing)
        else if String.contains char "\\"
        then ({state | seen = state.seen ++ char, state = 13}, Nothing)
        else ({state | seen = state.seen ++ char, state = 1}, Nothing)
    1 -> if String.contains char " ()*+,-./=\\"
        then let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken8 state.seen))
        else ({state | seen = state.seen ++ char, state = 14}, Nothing)
    2 -> if String.contains char " "
        then ({state | seen = state.seen ++ char, state = 2}, Nothing)
        else let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken14 state.seen))
    3 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken9 state.seen))
    4 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken10 state.seen))
    5 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken4 state.seen))
    6 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken2 state.seen))
    7 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken13 state.seen))
    8 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken3 state.seen))
    9 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken12 state.seen))
    10 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken5 state.seen))
    11 -> if String.contains char "0123456789"
        then ({state | seen = state.seen ++ char, state = 11}, Nothing)
        else let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken11 state.seen))
    12 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken1 state.seen))
    13 -> let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken6 state.seen))
    14 -> if String.contains char " ()*+,-./=\\"
        then let (newState, _) = parserProcessChar {seen = "", state=0} char in (newState, Just (ParserToken7 state.seen))
        else ({state | seen = state.seen ++ char, state = 14}, Nothing)
    _ -> ({seen = "", state = 0}, Nothing)

parserProcessEnd: ParserStateToken -> Maybe ParserToken
parserProcessEnd state = case state.state of
    1 -> Just (ParserToken8 state.seen)
    2 -> Just (ParserToken14 state.seen)
    3 -> Just (ParserToken9 state.seen)
    4 -> Just (ParserToken10 state.seen)
    5 -> Just (ParserToken4 state.seen)
    6 -> Just (ParserToken2 state.seen)
    7 -> Just (ParserToken13 state.seen)
    8 -> Just (ParserToken3 state.seen)
    9 -> Just (ParserToken12 state.seen)
    10 -> Just (ParserToken5 state.seen)
    11 -> Just (ParserToken11 state.seen)
    12 -> Just (ParserToken1 state.seen)
    13 -> Just (ParserToken6 state.seen)
    14 -> Just (ParserToken7 state.seen)
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
parserProcessRule8 state = case parserExtractLastN 3 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule8)
        |> parserExtract parserExpectTokenMultiple parserError8 1
        |> parserExtract parserExpectTokenSpace parserError8 2
        |> parserExtract parserExpectTokenTerm parserError8 3
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenMultiple o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule9: ParserState -> Result String ParserState
parserProcessRule9 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule9)
        |> parserExtract parserExpectTokenNegatable parserError9 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenMultiple o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule10: ParserState -> Result String ParserState
parserProcessRule10 state = case parserExtractLastN 3 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule10)
        |> parserExtract parserExpectTokenSpace parserError10 1
        |> parserExtract parserExpectToken3 parserError10 2
        |> parserExtract parserExpectTokenNegatable parserError10 3
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenNegatable o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule11: ParserState -> Result String ParserState
parserProcessRule11 state = case parserExtractLastN 2 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule11)
        |> parserExtract parserExpectTokenSpace parserError11 1
        |> parserExtract parserExpectTokenTerm parserError11 2
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenNegatable o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule12: ParserState -> Result String ParserState
parserProcessRule12 state = case parserExtractLastN 2 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule12)
        |> parserExtract parserExpectToken6 parserError12 1
        |> parserExtract parserExpectToken7 parserError12 2
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule13: ParserState -> Result String ParserState
parserProcessRule13 state = case parserExtractLastN 2 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule13)
        |> parserExtract parserExpectToken6 parserError13 1
        |> parserExtract parserExpectToken8 parserError13 2
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule14: ParserState -> Result String ParserState
parserProcessRule14 state = case parserExtractLastN 5 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule14)
        |> parserExtract parserExpectToken6 parserError14 1
        |> parserExtract parserExpectToken7 parserError14 2
        |> parserExtract parserExpectToken9 parserError14 3
        |> parserExtract parserExpectTokenArgs parserError14 4
        |> parserExtract parserExpectToken10 parserError14 5
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule15: ParserState -> Result String ParserState
parserProcessRule15 state = case parserExtractLastN 5 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule15)
        |> parserExtract parserExpectToken6 parserError15 1
        |> parserExtract parserExpectToken8 parserError15 2
        |> parserExtract parserExpectToken9 parserError15 3
        |> parserExtract parserExpectTokenArgs parserError15 4
        |> parserExtract parserExpectToken10 parserError15 5
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule16: ParserState -> Result String ParserState
parserProcessRule16 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule16)
        |> parserExtract parserExpectToken8 parserError16 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule17: ParserState -> Result String ParserState
parserProcessRule17 state = case parserExtractLastN 3 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule17)
        |> parserExtract parserExpectToken11 parserError17 1
        |> parserExtract parserExpectToken12 parserError17 2
        |> parserExtract parserExpectToken11 parserError17 3
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule18: ParserState -> Result String ParserState
parserProcessRule18 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule18)
        |> parserExtract parserExpectToken11 parserError18 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule19: ParserState -> Result String ParserState
parserProcessRule19 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule19)
        |> parserExtract parserExpectToken9 parserError19 1
        |> parserExtract parserExpectTokenExpression parserError19 2
        |> parserExtract parserExpectTokenSpace parserError19 3
        |> parserExtract parserExpectToken10 parserError19 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenTerm o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule20: ParserState -> Result String ParserState
parserProcessRule20 state = case parserExtractLastN 4 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule20)
        |> parserExtract parserExpectTokenArgs parserError20 1
        |> parserExtract parserExpectTokenSpace parserError20 2
        |> parserExtract parserExpectToken13 parserError20 3
        |> parserExtract parserExpectTokenExpression parserError20 4
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenArgs o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule21: ParserState -> Result String ParserState
parserProcessRule21 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule21)
        |> parserExtract parserExpectTokenExpression parserError21 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenArgs o)::newState.stack}
            Result.Err e -> Result.Err e
        )

parserProcessRule23: ParserState -> Result String ParserState
parserProcessRule23 state = case parserExtractLastN 1 (state,[]) of
    Result.Err _ -> Result.Err "Parser is missing tokens to process the rule"
    Result.Ok (newState, stack) ->
        Result.Ok (stack, parserRule23)
        |> parserExtract parserExpectToken14 parserError23 1
        |> Result.andThen ( \(_, output) -> case output of
            Result.Ok o -> Result.Ok {newState | stack = (ParserTokenSpace o)::newState.stack}
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
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError1 1 other)
    (1::_) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (13::state.states), stack=(token::state.stack) }
        ParserToken1 _ -> Result.Ok {state | states = (14::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError1 2 other)
    (2::_) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (15::state.states), stack=(token::state.stack) }
        ParserToken1 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError2 2 other)
    (3::states) -> case token of
        ParserTokenTerm _ -> Result.Ok {state | states = (19::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (20::state.states), stack=(token::state.stack) }
        ParserToken4 _ -> Result.Ok {state | states = (21::state.states), stack=(token::state.stack) }
        ParserToken5 _ -> Result.Ok {state | states = (22::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> parserProcessRule5 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (4::states) -> case token of
        other -> parserProcessRule9 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (5::states) -> case token of
        other -> parserProcessRule11 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (6::_) -> case token of
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError10 2 other)
    (7::_) -> case token of
        ParserTokenNegatable _ -> Result.Ok {state | states = (23::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError10 3 other)
    (8::_) -> case token of
        ParserToken7 _ -> Result.Ok {state | states = (24::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (25::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError12 2 other)
    (9::states) -> case token of
        other -> parserProcessRule16 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (10::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (26::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 1 other)
    (11::states) -> case token of
        ParserToken12 _ -> Result.Ok {state | states = (27::state.states), stack=(token::state.stack) }
        other -> parserProcessRule18 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (12::states) -> case token of
        other -> parserProcessRule23 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (13::_) -> case token of
        ParserToken1 _ -> Result.Ok {state | states = (14::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError1 3 other)
    (14::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (28::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError1 4 other)
    (15::_) -> case token of
        ParserToken1 _ -> Result.Ok {state | states = (16::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError2 3 other)
    (16::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (29::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError2 4 other)
    (17::_) -> case token of
        ParserTokenMultiple _ -> Result.Ok {state | states = (30::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 4 other)
    (18::_) -> case token of
        ParserTokenMultiple _ -> Result.Ok {state | states = (31::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError4 4 other)
    (19::states) -> case token of
        other -> parserProcessRule8 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (20::_) -> case token of
        ParserTokenTerm _ -> Result.Ok {state | states = (19::state.states), stack=(token::state.stack) }
        ParserToken4 _ -> Result.Ok {state | states = (21::state.states), stack=(token::state.stack) }
        ParserToken5 _ -> Result.Ok {state | states = (22::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError6 3 other)
    (21::_) -> case token of
        ParserTokenNegatable _ -> Result.Ok {state | states = (32::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError6 4 other)
    (22::_) -> case token of
        ParserTokenNegatable _ -> Result.Ok {state | states = (33::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError7 4 other)
    (23::states) -> case token of
        other -> parserProcessRule10 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (24::states) -> case token of
        ParserToken9 _ -> Result.Ok {state | states = (34::state.states), stack=(token::state.stack) }
        other -> parserProcessRule12 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (25::states) -> case token of
        ParserToken9 _ -> Result.Ok {state | states = (35::state.states), stack=(token::state.stack) }
        other -> parserProcessRule13 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (26::_) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (36::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken10 _ -> Result.Ok {state | states = (37::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 2 other)
    (27::_) -> case token of
        ParserToken11 _ -> Result.Ok {state | states = (38::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError17 3 other)
    (28::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (39::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> parserProcessRule1 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (29::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (39::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> parserProcessRule2 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (30::states) -> case token of
        ParserTokenTerm _ -> Result.Ok {state | states = (19::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (20::state.states), stack=(token::state.stack) }
        ParserToken4 _ -> Result.Ok {state | states = (21::state.states), stack=(token::state.stack) }
        ParserToken5 _ -> Result.Ok {state | states = (22::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> parserProcessRule3 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (31::states) -> case token of
        ParserTokenTerm _ -> Result.Ok {state | states = (19::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (20::state.states), stack=(token::state.stack) }
        ParserToken4 _ -> Result.Ok {state | states = (21::state.states), stack=(token::state.stack) }
        ParserToken5 _ -> Result.Ok {state | states = (22::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> parserProcessRule4 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (32::states) -> case token of
        other -> parserProcessRule6 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (33::states) -> case token of
        other -> parserProcessRule7 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (34::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (40::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenArgs _ -> Result.Ok {state | states = (41::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (42::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken10 _ -> Result.Ok {state | states = (43::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken13 _ -> Result.Ok {state | states = (44::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 1 other)
    (35::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (40::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenArgs _ -> Result.Ok {state | states = (45::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (42::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken10 _ -> Result.Ok {state | states = (46::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken13 _ -> Result.Ok {state | states = (44::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 1 other)
    (36::_) -> case token of
        ParserToken2 _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken10 _ -> Result.Ok {state | states = (37::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 3 other)
    (37::states) -> case token of
        other -> parserProcessRule19 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (38::states) -> case token of
        other -> parserProcessRule17 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (39::_) -> case token of
        ParserToken2 _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 3 other)
    (40::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (39::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> parserProcessRule21 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (41::_) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (47::state.states), stack=(token::state.stack) }
        ParserToken10 _ -> Result.Ok {state | states = (43::state.states), stack=(token::state.stack) }
        ParserToken13 _ -> Result.Ok {state | states = (44::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError14 5 other)
    (42::_) -> case token of
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken13 _ -> Result.Ok {state | states = (44::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError10 2 other)
    (43::states) -> case token of
        other -> parserProcessRule14 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (44::_) -> case token of
        ParserTokenExpression _ -> Result.Ok {state | states = (48::state.states), stack=(token::state.stack) }
        ParserTokenMultiple _ -> Result.Ok {state | states = (3::state.states), stack=(token::state.stack) }
        ParserTokenNegatable _ -> Result.Ok {state | states = (4::state.states), stack=(token::state.stack) }
        ParserTokenTerm _ -> Result.Ok {state | states = (5::state.states), stack=(token::state.stack) }
        ParserTokenSpace _ -> Result.Ok {state | states = (6::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (7::state.states), stack=(token::state.stack) }
        ParserToken6 _ -> Result.Ok {state | states = (8::state.states), stack=(token::state.stack) }
        ParserToken8 _ -> Result.Ok {state | states = (9::state.states), stack=(token::state.stack) }
        ParserToken9 _ -> Result.Ok {state | states = (10::state.states), stack=(token::state.stack) }
        ParserToken11 _ -> Result.Ok {state | states = (11::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError3 1 other)
    (45::_) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (47::state.states), stack=(token::state.stack) }
        ParserToken10 _ -> Result.Ok {state | states = (46::state.states), stack=(token::state.stack) }
        ParserToken13 _ -> Result.Ok {state | states = (44::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError15 5 other)
    (46::states) -> case token of
        other -> parserProcessRule15 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (47::_) -> case token of
        ParserToken13 _ -> Result.Ok {state | states = (44::state.states), stack=(token::state.stack) }
        other -> Result.Err (parserError20 3 other)
    (48::states) -> case token of
        ParserTokenSpace _ -> Result.Ok {state | states = (39::state.states), stack=(token::state.stack) }
        ParserToken2 _ -> Result.Ok {state | states = (17::state.states), stack=(token::state.stack) }
        ParserToken3 _ -> Result.Ok {state | states = (18::state.states), stack=(token::state.stack) }
        ParserToken14 _ -> Result.Ok {state | states = (12::state.states), stack=(token::state.stack) }
        other -> parserProcessRule20 {state | states=states} |> Result.andThen (\s -> parserProcessToken other s)
    (_::_) -> Result.Err "Unknown state reached"

