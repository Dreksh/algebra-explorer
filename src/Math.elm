module Math exposing (Tree(..), Symbol(..), getChildren, getState, notation, parse, symbolicate)

import Parser exposing ((|.), (|=))
import Set

type Tree s =
    RealNode {state: s, value: Float}
    | VariableNode {state: s, name: String}
    | UnaryNode {state: s, name: String, child: Tree s}
    | BinaryNode {state: s, name: String, associative: Bool, commutative: Bool, children: List (Tree s)}
    | GenericNode {state: s, name: String, children: List (Tree s)} -- Order matters

getState: Tree state -> state
getState node = case node of
    RealNode s -> s.state
    VariableNode s -> s.state
    UnaryNode s -> s.state
    BinaryNode s -> s.state
    GenericNode s -> s.state

getChildren: Tree state -> List (Tree state)
getChildren node = case node of
    RealNode _ -> []
    VariableNode _ -> []
    UnaryNode s -> [s.child]
    BinaryNode s -> s.children
    GenericNode s -> s.children

type Symbol s =
    Text String
    | Node {state: s, children: List (Symbol s)}

symbolicate: Tree state -> Symbol state
symbolicate root = symbolicateRecursive_ Nothing True root

functionName_: Tree state -> String
functionName_ node = case node of
    RealNode _ -> ""
    VariableNode _ -> ""
    UnaryNode s -> s.name
    BinaryNode s -> s.name
    GenericNode s -> s.name

functionPrecedence_: Tree state -> Int -- Higher Int has higher precedence
functionPrecedence_ node = case functionName_ node of
    -- multiplicative
    "*" -> 2
    "/" -> 2
    -- additive
    "+" -> 1
    "-" -> 1
    -- inequalities
    "=" -> 0
    -- Others (variables, function calls)
    _ -> -1

symbolicateRecursive_: Maybe (Tree state) -> Bool -> Tree state -> Symbol state
symbolicateRecursive_ parent multiplicativeFirst root = (
    case root of
        RealNode s -> [String.fromFloat s.value |> Text]
        VariableNode s -> if String.length s.name == 1 then [Text s.name] else [Text ("\\" ++ s.name)]
        UnaryNode s -> if s.name == "/"
            then if multiplicativeFirst
                then [Text "1/", symbolicateRecursive_ (Just root) False s.child]
                else [Text "/", symbolicateRecursive_ (Just root) False s.child]
            else [Text s.name, symbolicateRecursive_ (Just root) True s.child]
        BinaryNode s -> s.children
            |> List.foldl (\child result -> if List.isEmpty result
                then [symbolicateRecursive_ (Just root) True child]
                else case child of
                    UnaryNode c -> case (s.name, c.name) of
                        ("+", "-") -> result ++ [symbolicateRecursive_ (Just root) True child]
                        ("*", "/") -> result ++ [symbolicateRecursive_ (Just root) (List.isEmpty result) child]
                        _ -> result ++ [Text s.name, symbolicateRecursive_ (Just root) True child]
                    _ -> result ++ [Text s.name, symbolicateRecursive_ (Just root) True child]
                )
                []
        GenericNode s -> s.children
            |> List.map (symbolicateRecursive_ (Just root) True)
            |> List.intersperse (Text ",")
            |> (\arguments -> Text ("\\" ++ s.name ++ "(")::arguments ++ [Text ")"] )
    )
    |> (\tokens -> case (parent, root) of
        -- Ignore when top-level
        (Nothing, _) -> Node {state = getState root, children = tokens}
        -- Ignore if variable, number or the traditional way of expressing functions
        (_, RealNode _) -> Node {state = getState root, children = tokens}
        (_, VariableNode _) -> Node {state = getState root, children = tokens}
        (_, GenericNode _) -> Node {state = getState root, children = tokens}
        (Just p, _) ->
            if (functionPrecedence_ p) > (functionPrecedence_ root)
            then Node {state = getState root, children= Text "("::tokens ++ [Text ")"]}
            else Node {state = getState root, children = tokens}
    )

notation: String
notation = """
a - a variable called "a"
ab - a variable "a" multiplied by a variable "b"
\\ab - a variable with a long name called "ab"
\\a(x,y;z) - a function called "a" that takes in x and y, and uses z
"""

-- Parser implementation
parse: String -> Result String (Tree ())
parse input = Parser.run equation_ input |> Result.mapError (createErrorMessage_ input)

equation_: Parser.Parser (Tree ())
equation_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |. Parser.spaces |= expression_ |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. Parser.symbol "="
                |= expression_
                |. Parser.spaces
            ,   Parser.succeed()
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            ]
    )
    |> Parser.map (\children -> case children of
        [x] -> x
        _ -> BinaryNode {state = (), name = "=", associative = True, commutative = True, children = children}
    )

expression_: Parser.Parser (Tree ())
expression_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= multiple_ |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. Parser.symbol "+"
                |= multiple_
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop ((UnaryNode {state = (), name = "-", child = elem}) :: list))
                |. Parser.symbol "-"
                |= multiple_
                |. Parser.spaces
            ,   Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            ]
    ) |> Parser.map (\children -> case children of
        [x] -> x
        _ -> BinaryNode {state = (), name = "+", associative = True, commutative = True, children = children}
    )

multiple_: Parser.Parser (Tree ())
multiple_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= negatable_ |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. Parser.symbol "*"
                |= negatable_
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop (UnaryNode {state = (), name = "/", child = elem} :: list))
                |. Parser.symbol "/"
                |= negatable_
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |= term_
                |. Parser.spaces
            ,   Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            ]
    )
    |> Parser.map (\children -> case children of
        [x] -> x
        _ -> BinaryNode {state = (), name = "*", associative = True, commutative = True, children = children}
    )

negatable_: Parser.Parser (Tree ())
negatable_ = Parser.oneOf
    [   Parser.succeed (\x -> UnaryNode {state = (), name="-", child = x}) |. Parser.symbol "-" |. Parser.spaces |= Parser.lazy (\_ -> negatable_)
    ,   Parser.succeed identity |= term_
    ]

term_: Parser.Parser (Tree ())
term_ = Parser.oneOf
    [   Parser.succeed identity |. Parser.symbol "(" |= expression_ |. Parser.spaces |. Parser.symbol ")"
    ,   tokenNumber_
    ,   Parser.succeed (\a b -> (a, b)) |. Parser.symbol "\\" |= tokenLongName_ |= varOrFunc_
        |> Parser.map (\(name, props) -> case props of
            Nothing -> VariableNode {state = (), name = name}
            Just children -> GenericNode {state = (), name = name, children = children}
        )
    ,   Parser.succeed (\name -> VariableNode {state = (), name = name}) |= tokenShortName_
    ]

varOrFunc_: Parser.Parser (Maybe (List (Tree ())))
varOrFunc_ = Parser.oneOf
    [   Parser.succeed Just |. Parser.symbol "(" |= argsList_ |. Parser.symbol ")"
    ,   Parser.succeed Nothing
    ]

argsList_: Parser.Parser (List (Tree ()))
argsList_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= expression_ |. Parser.spaces
        else Parser.oneOf
        [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
            |. Parser.symbol ","
            |= expression_
            |. Parser.spaces
        ,   Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse list))
        ]
    )

tokenNumber_: Parser.Parser (Tree ())
tokenNumber_ = Parser.succeed (\a b -> a ++ b) |= tokenDigit_ |= Parser.oneOf [Parser.succeed (\b -> "." ++ b) |. Parser.symbol "." |= tokenDigit_ , Parser.succeed ""]
    |> Parser.andThen (\str -> case String.toFloat str of
        Nothing -> Parser.problem (str ++ " is not a valid number")
        Just f -> Parser.succeed (RealNode {state = (), value = f})
    )

tokenDigit_: Parser.Parser String
tokenDigit_ = Parser.variable
    {   start = Char.isDigit
    ,   inner = Char.isDigit
    ,   reserved = Set.empty
    }

validVarStart_: Char -> Bool
validVarStart_ char = not (String.contains (String.fromChar char) " ~+-=*/\\.()[],;%!:;<>0123456789^&|$↔→")

tokenLongName_: Parser.Parser String
tokenLongName_ = Parser.variable
    {   start = validVarStart_
    ,   inner = (\c -> validVarStart_ c || Char.isAlphaNum c )
    ,   reserved = Set.empty
    }

tokenShortName_: Parser.Parser String
tokenShortName_ = Parser.variable
    {   start = validVarStart_
    ,   inner = (\_ -> False)
    ,   reserved = Set.empty
    }

createErrorMessage_: String -> List Parser.DeadEnd -> String
createErrorMessage_ str err = "Error parsing \"" ++ str ++ "\":\n" ++ deadEndToString_ err

deadEndToString_: List Parser.DeadEnd -> String
deadEndToString_ = List.map
    (\deadEnd ->
        (   case deadEnd.problem of
                Parser.Expecting str -> "Expecting '" ++ str ++ "'"
                Parser.ExpectingInt -> "Expecting a whole number"
                Parser.ExpectingHex -> "Expecting a hex number"
                Parser.ExpectingOctal -> "Expecting an octal number"
                Parser.ExpectingBinary -> "Expecting a binary number"
                Parser.ExpectingFloat -> "Expecting a decimal number"
                Parser.ExpectingNumber -> "Expecting a number"
                Parser.ExpectingVariable -> "Expecting a variable"
                Parser.ExpectingSymbol str -> "Expecting '" ++ str ++ "'"
                Parser.ExpectingKeyword str -> "Expecting '" ++ str ++ "'"
                Parser.ExpectingEnd -> "Expecting no more characters"
                Parser.UnexpectedChar -> "Unknown symbol"
                Parser.Problem str -> str
                Parser.BadRepeat -> "Bad repeat"
        )
        |> (\str -> str ++ ", at position: " ++ String.fromInt deadEnd.col)
    )
    >> String.join "\n"