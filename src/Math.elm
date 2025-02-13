module Math exposing (Tree(..), Properties, notation, parse, toString)

import Parser exposing ((|.), (|=))
import Set

type Tree state =
    Add state (List (Tree state))
    | Multiply state (List (Tree state))
    | Equal state (List (Tree state))
    | Negative state (Tree state)
    | Reciprocal state (Tree state)
    | Function state (Properties state)
    | Variable state String
    | Real state Float
    | Collapsed state (Tree state)

type alias Properties state =
    {   name: String
    ,   args: List (Tree state)
    ,   parameters: List (Tree state)
    -- Settable by rules, so that substitution / expansion works well
    ,   unary: Bool                                 -- Only allows 1 arg
    ,   associative: Bool                           -- Can combine nodes, allowing N-children
    ,   commutative: Bool                           -- Args can be reordered
    ,   linear: Bool                                -- If addition can be split out
    }

toString: Tree state -> String
toString root = case root of
    Reciprocal _ _ -> "1" ++ toStringRecursive root
    Collapsed _ child -> toString child
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
                Function _ _ -> result ++ "+" ++ toStringRecursive elem
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
                    Function _ _ -> (Just elem, result ++ toStringRecursive elem)
                    Variable _ _ -> (Just elem, result ++ toStringRecursive elem)
                    Real _ _ -> (Just elem, result ++ "*" ++ toStringRecursive elem)
                    _ -> (Just elem, result ++ "(" ++ toStringRecursive elem ++ ")")
                else case elem of
                    Function _ _ -> (Just elem, result ++ toStringRecursive elem)
                    Variable _ nextName -> if String.length nextName == 1
                        then (Just elem, result ++ "*" ++ toStringRecursive elem)
                        else (Just elem, result ++ toStringRecursive elem)
                    Real _ _ -> (Just elem, result ++ "*" ++ toStringRecursive elem)
                    _ -> (Just elem, result ++ "*(" ++ toStringRecursive elem ++ ")")
            _ -> case elem of
                Function _ _ -> (Just elem, result ++ toStringRecursive elem)
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
        Function _ _ -> "-" ++ toStringRecursive child
        Variable _ _ -> "-" ++ toStringRecursive child
        Real _ _ -> "-" ++ toStringRecursive child
        _ -> "-(" ++ toStringRecursive child ++ ")"
    Reciprocal _ child -> case child of
        Negative _ _ -> "/" ++ toStringRecursive child
        Function _ _ -> "/" ++ toStringRecursive child
        Variable _ _ -> "/" ++ toStringRecursive child
        Real _ _ -> "/" ++ toStringRecursive child
        _ -> "/(" ++ toStringRecursive child ++ ")"
    Function _ properties ->
        let
            input = List.foldl (\elem result -> if result == "" then toStringRecursive elem else result ++ "," ++ toStringRecursive elem) "" properties.args
            params = List.foldl (\elem result -> if result == "" then toStringRecursive elem else result ++ "," ++ toStringRecursive elem) "" properties.parameters
            args = if String.isEmpty params then input else input ++ ";" ++ params
        in
            "\\" ++ properties.name ++ "(" ++ args ++ ")"
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
    |> Parser.map (Equal ()) 

expression_: Parser.Parser (Tree ())
expression_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= multiple_ |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. Parser.symbol "+"
                |= multiple_
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop ((Negative () elem) :: list))
                |. Parser.symbol "-"
                |= multiple_
                |. Parser.spaces
            ,   Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            ]
    ) |> Parser.map (Add ())

multiple_: Parser.Parser (Tree ())
multiple_ = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= negatable_ |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. Parser.symbol "*"
                |= negatable_
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop (Reciprocal () elem :: list))
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
    |> Parser.map (Multiply ())

negatable_: Parser.Parser (Tree ())
negatable_ = Parser.oneOf
    [   Parser.succeed (Negative ()) |. Parser.symbol "-" |. Parser.spaces |= Parser.lazy (\_ -> negatable_)
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

varOrFunc_: Parser.Parser (Maybe (Properties()))
varOrFunc_ = Parser.oneOf
    [   Parser.succeed Just |. Parser.symbol "(" |= args_ |. Parser.symbol ")"
    ,   Parser.succeed Nothing
    ]

args_: Parser.Parser (Properties ())
args_ = Parser.succeed (\a b -> (a, b)) |= argsList_ |= Parser.oneOf
    [   Parser.succeed identity |. Parser.symbol ";" |= argsList_
    ,   Parser.succeed []
    ]
    |> Parser.map (\(args, parameters) ->
        {   name = ""
        ,   args = args
        ,   parameters = parameters
        -- Assume none of the following properties
        ,   unary = False
        ,   associative = False
        ,   commutative = False
        ,   linear = False
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
tokenNumber_ = Parser.number
    {   int = Just ( toFloat >> Real ())
    ,   hex = Nothing
    ,   octal = Nothing
    ,   binary = Nothing
    ,   float = Just (Real ())
    }

validVarStart_: Char -> Bool
validVarStart_ char = not (String.contains (String.fromChar char) " ~+-*/\\.()[],;%!:;<>0123456789^&|$")

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
    