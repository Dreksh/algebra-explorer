module Math exposing (Tree(..), notation, parse, toString)

import Array exposing (Array)
import Parser exposing ((|.), (|=))
import Set

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
        |> Parser.map (\(name, args) -> case args of
            Nothing -> Variable () name
            Just list -> Function () name list
        )
    ,   Parser.succeed (Variable ()) |= tokenShortName_
    ]

varOrFunc_: Parser.Parser (Maybe (Array (Tree ())))
varOrFunc_ = Parser.oneOf
    [   Parser.succeed (\elem -> Just elem) |. Parser.symbol "(" |= args_ |. Parser.symbol ")"
    ,   Parser.succeed Nothing
    ]

args_: Parser.Parser (Array (Tree ()))
args_ = Parser.loop []
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
    |> Parser.map Array.fromList

tokenNumber_: Parser.Parser (Tree ())
tokenNumber_ = Parser.number
    {   int = Just ( toFloat >> Real ())
    ,   hex = Nothing
    ,   octal = Nothing
    ,   binary = Nothing
    ,   float = Just (Real ())
    }

validVarStart_: Char -> Bool
validVarStart_ char = not (String.contains (String.fromChar char) "+-*/\\.()[],;%!:;<>0123456789^&")

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
    