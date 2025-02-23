module Math exposing (Tree(..), Processor, Properties, Symbol(..), getChildren, notation, parse, process, processState, symbolicate)

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

-- First argument to all potentially recursive processing is
-- the recursive call to process, using the same Processor properties
type alias Processor prevState state global =
    {   children: (global -> Tree prevState -> (global, Maybe (Tree state))) -> global -> List (Tree prevState) -> (global, Maybe (List (Tree state)))
    ,   function: (global -> Tree prevState -> (global, Maybe (Tree state))) -> global -> Properties prevState -> (global, Maybe (Properties state))
    ,   child: (global -> Tree prevState -> (global, Maybe (Tree state))) -> global -> Tree prevState -> (global, Maybe (Tree state))
    ,   var: global -> String -> (global, Maybe String)
    ,   real: global -> Float -> (global, Maybe Float)
    ,   finalize: global -> global -> prevState -> (global, state) -- Original + Updated global states
    }

process: Processor prevState state global -> global -> Tree prevState -> (global, Maybe (Tree state))
process processor g root = case root of
    Add s children -> processor.children (process processor) g children
        |> processStep_ Add processor.finalize g s
    Multiply s children -> processor.children (process processor) g children
        |> processStep_ Multiply processor.finalize g s
    Equal s children -> processor.children (process processor) g children
        |> processStep_ Equal processor.finalize g s
    Negative s child ->  processor.child (process processor) g child
        |> processStep_ Negative processor.finalize g s
    Reciprocal s child -> processor.child (process processor) g child
        |> processStep_ Reciprocal processor.finalize g s
    Collapsed s child -> processor.child (process processor) g child
        |> processStep_ Collapsed processor.finalize g s
    Function s properties -> processor.function (process processor) g properties
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
            Add s _ -> s
            Multiply s _ -> s
            Equal s _ -> s
            Negative s _ -> s
            Reciprocal s _ -> s
            Function s _ -> s
            Variable s _ -> s
            Real s _ -> s
            Collapsed s _ -> s
    ) |> p

getChildren: Tree state -> List (Tree state)
getChildren root = case root of
    Add _ children -> children
    Multiply _ children -> children
    Equal _ children -> children
    Negative _ child -> [child]
    Reciprocal _ child -> [child]
    Function _ prop -> prop.args ++ prop.parameters
    Variable _ _ -> []
    Real _ _ -> []
    Collapsed _ child -> [child]

type Symbol state =
    Text String
    | Node state (List (Symbol state))

symbolicate: Tree state -> Symbol state
symbolicate root = symbolicateRecursive_ Nothing Nothing root

symbolicateRecursive_: Maybe (Tree state) -> Maybe (Tree state) -> Tree state -> Symbol state
symbolicateRecursive_ parent sibling root = case root of
    Add s list -> list
        |> List.foldl (\elem (prev, result) ->
            if List.isEmpty result then (Just elem, [symbolicateRecursive_ (Just root) prev elem])
            else case elem of
                Negative _ _ -> (Just elem, result ++ [symbolicateRecursive_ (Just root) prev elem])
                _ -> (Just elem, result ++ [Text "+", symbolicateRecursive_ (Just root) prev elem])
        ) (Nothing, [])
        |> (\(_, tokens) -> case parent of
            Nothing -> Node s tokens
            Just (Equal _ _) -> Node s tokens
            Just (Function _ _) -> Node s tokens
            _ -> Node s ((Text "(")::tokens ++ [Text ")"])
        )
    Multiply s list -> list
        |> List.foldl (\elem (prev, result) ->
            if List.isEmpty result then (Just elem, [symbolicateRecursive_ (Just root) prev elem])
            else case elem of
                Reciprocal _ _ -> (Just elem, result ++ [symbolicateRecursive_ (Just root) prev elem])
                _ -> (Just elem, result ++ [Text "*", symbolicateRecursive_ (Just root) prev elem])
        ) (Nothing, [])
        |> (\(_, tokens) -> case parent of
            Just (Multiply _ _) -> Node s (Text "("::tokens ++ [Text ")"])
            Just (Reciprocal _ _) -> Node s (Text "("::tokens ++ [Text ")"])
            _ -> Node s tokens
        )
    Equal s list -> list
        |> List.map (symbolicateRecursive_ (Just root) Nothing)
        |> List.intersperse (Text "=")
        |> Node s
    Negative s child -> Node s [Text "-", symbolicateRecursive_ (Just root) Nothing child]
    Reciprocal s child -> case sibling of
        Nothing -> Node s [Text "1/", symbolicateRecursive_ (Just root) Nothing child]
        Just _ -> Node s [Text "/", symbolicateRecursive_ (Just root) Nothing child]
    Function s properties ->
        let
            input = List.map (symbolicateRecursive_ (Just root) Nothing) properties.args |> List.intersperse (Text ",")
            params = List.map (symbolicateRecursive_ (Just root) Nothing) properties.parameters |> List.intersperse (Text ",")
            args = if List.isEmpty params then input else input ++ (Text ";"::params)
        in
            Node s (Text ("\\" ++ properties.name ++ "(")::args ++ [Text ")"])
    Variable s name -> if String.length name == 1 then Node s [Text name] else Node s [Text ("\\" ++ name)]
    Real s val -> Node s [String.fromFloat val |> Text]
    Collapsed _ child -> symbolicateRecursive_ parent sibling child -- This was a flag for ourselves to determine the scope of displaying

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
        _ -> Equal () children
    )

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
    ) |> Parser.map (\children -> case children of
        [x] -> x
        _ -> Add () children
    )

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
    |> Parser.map (\children -> case children of
        [x] -> x
        _ -> Multiply () children
    )

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
    