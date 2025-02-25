module Math exposing (Children, Tree(..), Processor, Symbol(..), getChildren, notation, parse, process, processState, symbolicate)

import Parser exposing ((|.), (|=))
import Set

type Tree state =
    Function state (Children state)
    | Variable state String
    | Real state Float

type alias Children state =
    {   name: String
    ,   args: List (Tree state)
    ,   parameters: List (Tree state)
    }

-- First argument to all potentially recursive processing is
-- the recursive call to process, using the same Processor properties
type alias Processor prevState state global =
    {   function: (global -> Tree prevState -> (global, Maybe (Tree state))) -> global -> Children prevState  -> (global, Maybe (Children state))
    ,   var: global -> String -> (global, Maybe String)
    ,   real: global -> Float -> (global, Maybe Float)
    ,   finalize: global -> global -> prevState -> (global, state) -- Original + Updated global states
    }

process: Processor prevState state global -> global -> Tree prevState -> (global, Maybe (Tree state))
process processor g root = case root of
    Function s children -> processor.function (process processor) g children
        |> processStep_ Function processor.finalize g s
    Variable s name -> processor.var g name
        |> processStep_ Variable processor.finalize g s
    Real s val -> processor.real g val
        |> processStep_ Real processor.finalize g s

processStep_: (state -> result -> Tree state) -> (global -> global -> prevState -> (global, state)) -> global -> prevState -> (global, Maybe result) -> (global, Maybe (Tree state))
processStep_ nodeType finalize oldG s (newG, res) = case res of
    Nothing -> (newG, Nothing)
    Just newRes -> finalize oldG newG s
        |> (\(newerG, newState) -> (newerG, Just (nodeType newState newRes)))

processState: (state -> result) -> Tree state -> result
processState p node =
    (   case node of
            Function s _ -> s
            Variable s _ -> s
            Real s _ -> s
    ) |> p

getChildren: Tree state -> List (Tree state)
getChildren root = case root of
    Function _ children -> children.args ++ children.parameters
    Variable _ _ -> []
    Real _ _ -> []

type Symbol state =
    Text String
    | Node state (List (Symbol state))

symbolicate: Tree state -> Symbol state
symbolicate root = symbolicateRecursive_ Nothing True root

functionName_: Tree state -> String
functionName_ node = case node of
    Function _ children -> children.name
    _ -> ""

functionPrecedence_: Tree state -> Int -- Lower Int has higher precedence, this is specifically from math notation
functionPrecedence_ node = case functionName_ node of
    -- multiplicative
    "*" -> 1
    "/" -> 1
    -- additive
    "+" -> 2
    "-" -> 2
    -- inequalities
    "=" -> 3
    -- Others (variables, function calls)
    _ -> 0

symbolicateRecursive_: Maybe (Tree state) -> Bool -> Tree state -> Symbol state
symbolicateRecursive_ parent multiplicativeFirst root = case root of
    Function s children -> (
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
                if (functionPrecedence_ p) < (functionPrecedence_ root)
                then Node s ((Text "(")::tokens ++ [Text ")"])
                else Node s tokens
        )
    Variable s name -> if String.length name == 1 then Node s [Text name] else Node s [Text ("\\" ++ name)]
    Real s val -> Node s [String.fromFloat val |> Text]

notation: String
notation = """
a - a variable called "a"
ab - a variable "a" multiplied by a variable "b"
\\ab - a variable with a long name called "ab"
\\a(x,y;z) - a function called "a" that takes in x and y, and uses z
"""

-- Parser implementation
parse: String -> Result (List Parser.DeadEnd) (Tree ())
parse = Parser.run equation_

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
        _ -> Function () {name = "=", parameters =[], args = children}
    )

expression_: Parser.Parser (Tree ())
expression_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= multiple_ |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. Parser.symbol "+"
                |= multiple_
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop ((Function () {name = "-", parameters = [], args = [elem]}) :: list))
                |. Parser.symbol "-"
                |= multiple_
                |. Parser.spaces
            ,   Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            ]
    ) |> Parser.map (\children -> case children of
        [x] -> x
        _ -> Function () {name = "+", parameters =[], args = children}
    )

multiple_: Parser.Parser (Tree ())
multiple_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= negatable_ |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. Parser.symbol "*"
                |= negatable_
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop (Function () {name = "/", parameters = [], args = [elem]} :: list))
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
        _ -> Function () {name = "*", parameters =[], args = children}
    )

negatable_: Parser.Parser (Tree ())
negatable_ = Parser.oneOf
    [   Parser.succeed (\x -> Function () {name="-", parameters=[], args = [x]}) |. Parser.symbol "-" |. Parser.spaces |= Parser.lazy (\_ -> negatable_)
    ,   Parser.succeed identity |= term_
    ]

term_: Parser.Parser (Tree ())
term_ = Parser.oneOf
    [   Parser.succeed identity |. Parser.symbol "(" |= expression_ |. Parser.spaces |. Parser.symbol ")"
    ,   tokenNumber_
    ,   Parser.succeed (\a b -> (a, b)) |. Parser.symbol "\\" |= tokenLongName_ |= varOrFunc_
        |> Parser.map (\(name, props) -> case props of
            Nothing -> Variable () name
            Just p -> Function () {p | name = name}
        )
    ,   Parser.succeed (Variable ()) |= tokenShortName_
    ]

varOrFunc_: Parser.Parser (Maybe (Children ()))
varOrFunc_ = Parser.oneOf
    [   Parser.succeed Just |. Parser.symbol "(" |= args_ |. Parser.symbol ")"
    ,   Parser.succeed Nothing
    ]

args_: Parser.Parser (Children ())
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
        Just f -> Parser.succeed (Real () f)
    )

tokenDigit_: Parser.Parser String
tokenDigit_ = Parser.variable
    {   start = Char.isDigit
    ,   inner = Char.isDigit
    ,   reserved = Set.empty
    }

validVarStart_: Char -> Bool
validVarStart_ char = not (String.contains (String.fromChar char) " ~+-=*/\\.()[],;%!:;<>0123456789^&|$")

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
    