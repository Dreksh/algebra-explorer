module Math exposing (Function, Tree(..), Processor, Symbol(..), getChildren, getState, notation, parse, process, symbolicate)

import Parser exposing ((|.), (|=))
import Set

type Tree state =
    FunctionNode state (Function state)
    | VariableNode state String
    | RealNode state Float

type alias Function state =
    {   name: String
    ,   args: List (Tree state)
    ,   parameters: List (Tree state)
    }

-- First argument to all potentially recursive processing is
-- the recursive call to process, using the same Processor properties
type alias Processor prevState state global =
    {   function: (global -> Tree prevState -> (global, Maybe (Tree state))) -> global -> (prevState, Function prevState) -> (global, Maybe (Tree state))
    ,   var: global -> (prevState, String) -> (global, Maybe (Tree state))
    ,   real: global -> (prevState, Float) -> (global, Maybe (Tree state))
    }

process: Processor prevState state global -> global -> Tree prevState -> (global, Maybe (Tree state))
process processor g root = case root of
    FunctionNode s children -> processor.function (process processor) g (s, children)
    VariableNode s name -> processor.var g (s, name)
    RealNode s val -> processor.real g (s, val)

getState: Tree state -> state
getState node = case node of
    FunctionNode s _ -> s
    VariableNode s _ -> s
    RealNode s _ -> s

getChildren: Tree state -> List (Tree state)
getChildren node = case node of
    FunctionNode _ s -> s.args ++ s.parameters
    _ -> []

type Symbol state =
    Text String
    | Node state (List (Symbol state))

symbolicate: Tree state -> Symbol state
symbolicate root = symbolicateRecursive_ Nothing True root

functionName_: Tree state -> String
functionName_ node = case node of
    FunctionNode _ children -> children.name
    _ -> ""

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
symbolicateRecursive_ parent multiplicativeFirst root = case root of
    FunctionNode s children -> (
        case children.name of
            "=" -> List.map (symbolicateRecursive_ (Just root) True) children.args |> List.intersperse (Text "=")
            "+" -> children.args
                |> List.foldl (\elem result ->
                    if List.isEmpty result then [symbolicateRecursive_ (Just root) True elem]
                    else if (functionName_ elem) == "-" then result ++ [symbolicateRecursive_ (Just root) True elem]
                    else result ++ [Text "+", symbolicateRecursive_ (Just root) True elem]
                )
                []
            "-" -> children.args
                |> List.foldl (\elem result -> result ++ [symbolicateRecursive_ (Just root) True elem]) [Text "-"]
            "*" -> children.args
                |> List.foldl (\elem result ->
                    if List.isEmpty result then [symbolicateRecursive_ (Just root) True elem]
                    else if (functionName_ elem) == "/" then result ++ [symbolicateRecursive_ (Just root) False elem]
                    else result ++ [Text "*", symbolicateRecursive_ (Just root) False elem] -- (Could we do something smarter here?)
                )
                []
            "/" -> children.args
                |> List.foldl (\elem result -> result ++ [symbolicateRecursive_ (Just root) True elem]) [Text (if multiplicativeFirst then "1/" else "/")]
            _ ->
                let
                    input = List.map (symbolicateRecursive_ (Just root) True) children.args |> List.intersperse (Text ",")
                    params = List.map (symbolicateRecursive_ (Just root) True) children.parameters |> List.intersperse (Text ",")
                    args = if List.isEmpty params then input else input ++ (Text ";"::params)
                in
                    Text ("\\" ++ children.name ++ "(")::args ++ [Text ")"]
        )
        |> (\tokens -> case parent of
            Nothing -> Node s tokens
            Just p ->
                if (functionPrecedence_ p) > (functionPrecedence_ root)
                then Node s ((Text "(")::tokens ++ [Text ")"])
                else Node s tokens
        )
    VariableNode s name -> if String.length name == 1 then Node s [Text name] else Node s [Text ("\\" ++ name)]
    RealNode s val -> Node s [String.fromFloat val |> Text]

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
        _ -> FunctionNode () {name = "=", parameters =[], args = children}
    )

expression_: Parser.Parser (Tree ())
expression_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= multiple_ |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. Parser.symbol "+"
                |= multiple_
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop ((FunctionNode () {name = "-", parameters = [], args = [elem]}) :: list))
                |. Parser.symbol "-"
                |= multiple_
                |. Parser.spaces
            ,   Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            ]
    ) |> Parser.map (\children -> case children of
        [x] -> x
        _ -> FunctionNode () {name = "+", parameters =[], args = children}
    )

multiple_: Parser.Parser (Tree ())
multiple_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= negatable_ |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. Parser.symbol "*"
                |= negatable_
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop (FunctionNode () {name = "/", parameters = [], args = [elem]} :: list))
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
        _ -> FunctionNode () {name = "*", parameters =[], args = children}
    )

negatable_: Parser.Parser (Tree ())
negatable_ = Parser.oneOf
    [   Parser.succeed (\x -> FunctionNode () {name="-", parameters=[], args = [x]}) |. Parser.symbol "-" |. Parser.spaces |= Parser.lazy (\_ -> negatable_)
    ,   Parser.succeed identity |= term_
    ]

term_: Parser.Parser (Tree ())
term_ = Parser.oneOf
    [   Parser.succeed identity |. Parser.symbol "(" |= expression_ |. Parser.spaces |. Parser.symbol ")"
    ,   tokenNumber_
    ,   Parser.succeed (\a b -> (a, b)) |. Parser.symbol "\\" |= tokenLongName_ |= varOrFunc_
        |> Parser.map (\(name, props) -> case props of
            Nothing -> VariableNode () name
            Just p -> FunctionNode () {p | name = name}
        )
    ,   Parser.succeed (VariableNode ()) |= tokenShortName_
    ]

varOrFunc_: Parser.Parser (Maybe (Function ()))
varOrFunc_ = Parser.oneOf
    [   Parser.succeed Just |. Parser.symbol "(" |= args_ |. Parser.symbol ")"
    ,   Parser.succeed Nothing
    ]

args_: Parser.Parser (Function ())
args_ = Parser.succeed (\a b -> (a, b)) |= argsList_ |= Parser.oneOf
    [   Parser.succeed identity |. Parser.symbol ";" |= argsList_
    ,   Parser.succeed []
    ]
    |> Parser.map (\(args, parameters) ->
        {   name = ""
        ,   args = args
        ,   parameters = parameters
        }
    )

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
        Just f -> Parser.succeed (RealNode () f)
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