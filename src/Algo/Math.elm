module Algo.Math exposing (Tree(..), Symbol(..),
    equal, validVariable,
    getChildren, getState, getName, map,
    notation, parse, symbolicate, toString,
    encode, decoder
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=))
import Set
-- Ours
import Algo.Backtrack as Backtrack

type Tree s =
    RealNode {state: s, value: Float}
    | VariableNode {state: s, name: String}
    | UnaryNode {state: s, name: String, child: Tree s}
    | BinaryNode {state: s, name: String, associative: Bool, commutative: Bool, children: List (Tree s)}
    | GenericNode {state: s, name: String, children: List (Tree s)} -- Order matters
    | DeclarativeNode {state: s, name: String, children: List (Tree s)} -- Can be strung together, but cannot be nested, assumed to be commutative for now

getState: Tree state -> state
getState node = case node of
    RealNode s -> s.state
    VariableNode s -> s.state
    UnaryNode s -> s.state
    BinaryNode s -> s.state
    GenericNode s -> s.state
    DeclarativeNode s -> s.state

getChildren: Tree state -> List (Tree state)
getChildren node = case node of
    RealNode _ -> []
    VariableNode _ -> []
    UnaryNode s -> [s.child]
    BinaryNode s -> s.children
    GenericNode s -> s.children
    DeclarativeNode s -> s.children

getName: Tree state -> String
getName root = case root of
    RealNode n -> String.fromFloat n.value
    VariableNode n -> n.name
    UnaryNode n -> n.name
    BinaryNode n -> n.name
    GenericNode n -> n.name
    DeclarativeNode n -> n.name

map: (Maybe newState -> Tree state -> global -> (newState, global)) -> global -> Tree state -> (Tree newState, global)
map transition glob root = map_ transition Nothing glob root

map_: (Maybe newState -> Tree state -> global -> (newState, global)) -> Maybe newState -> global -> Tree state -> (Tree newState, global)
map_ transition parent glob root =
    let
        (finS, nextG) = transition parent root glob
        processChildren nextGlob = List.foldl (\child (list, g) -> map_ transition (Just finS) g child |> \(c, finG) -> (c::list, finG)) ([], nextGlob)
    in
        case root of
            RealNode n -> (RealNode {state = finS, value = n.value}, nextG)
            VariableNode n -> (VariableNode {state = finS, name = n.name}, nextG)
            UnaryNode n -> map_ transition (Just finS) nextG n.child
                |> \(finChild, finGlob) -> (UnaryNode {state = finS, name = n.name, child = finChild}, finGlob)
            BinaryNode n -> processChildren nextG n.children
                |> \(finChildren, finGlob) -> (BinaryNode {state = finS, name = n.name, associative = n.associative, commutative = n.commutative, children = List.reverse finChildren}, finGlob)
            GenericNode n -> processChildren nextG n.children
                |> \(finChildren, finGlob) -> (GenericNode {state = finS, name = n.name, children = List.reverse finChildren}, finGlob)
            DeclarativeNode n -> processChildren nextG n.children
                |> \(finChildren, finGlob) -> (DeclarativeNode {state = finS, name = n.name, children = List.reverse finChildren}, finGlob)

equal: (a -> b -> Bool) -> Tree a -> Tree b -> Bool
equal check left right = case (left, right) of
    (RealNode l, RealNode r) -> check l.state r.state && l.value == r.value
    (VariableNode l, VariableNode r) -> check l.state r.state && l.name == r.name
    (UnaryNode l, UnaryNode r) -> check l.state r.state && l.name == r.name
        && equal check l.child r.child
    (BinaryNode l, BinaryNode r) -> check l.state r.state && l.name == r.name
        && l.associative == r.associative && l.commutative == r.commutative
        && List.length l.children == List.length r.children
        &&  (   Backtrack.run
                (   Backtrack.unorderedStack
                    (\lhs rhs result -> if equal check lhs rhs then Backtrack.return Just result else Backtrack.fail)
                    l.children
                ) r.children (Backtrack.init True)
            |> Maybe.andThen Backtrack.getState
            |> Maybe.withDefault False
            )
    (GenericNode l, GenericNode r) -> check l.state r.state && l.name == r.name
        && List.length l.children == List.length r.children
        && (List.map2 (equal check) l.children r.children |> List.foldl (&&) True)
    (DeclarativeNode l, DeclarativeNode r) -> check l.state r.state && l.name == r.name
        && List.length l.children == List.length r.children
        && (List.map2 (equal check) l.children r.children |> List.foldl (&&) True)
    _ -> False

type Symbol s =
    Text String
    | Node {state: s, children: List (Symbol s)}

symbolicate: Tree state -> Symbol state
symbolicate root = symbolicateRecursive_ Nothing True root

toString: Tree state -> String
toString root = symbolicate root |> eqToString_

eqToString_: Symbol msg -> String
eqToString_ root = case root of
    Text str -> str
    Node s -> List.map eqToString_ s.children |> String.join ""

functionPrecedence_: Tree state -> Int -- Higher Int has higher precedence
functionPrecedence_ node = case getName node of
    -- modifier
    "/" -> 3
    "-" -> 3
    -- multiplicative
    "*" -> 2
    -- additive
    "+" -> 1
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
        DeclarativeNode s -> s.children
            |> List.map (symbolicateRecursive_ (Just root) True)
            |> List.intersperse (Text s.name)
    )
    |> (\tokens -> case (parent, root) of
        -- Ignore when top-level
        (Nothing, _) -> Node {state = getState root, children = tokens}
        -- Ignore if variable, number or the traditional way of expressing functions
        (_, RealNode s) -> Node {state = s.state, children = tokens}
        (_, VariableNode s) -> Node {state = s.state, children = tokens}
        (_, GenericNode s) -> Node {state = s.state, children = tokens}
        (_, DeclarativeNode s) -> Node {state = s.state, children = tokens}
        (Just p, _) ->
            if (functionPrecedence_ p) >= (functionPrecedence_ root)
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
parse input = Parser.run (equation_ |. Parser.end) input |> Result.mapError (createErrorMessage_ input)

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
        _ -> DeclarativeNode {state = (), name = "=", children = children}
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

validVariable: String -> Result String String
validVariable str = Parser.run
    (Parser.oneOf [Parser.succeed identity |. Parser.token "\\" |= tokenLongName_, tokenShortName_])
    str
    |> Result.mapError (\_ -> "Inavlid variable name")

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

encode: (state -> Encode.Value) -> Tree state -> Encode.Value
encode converter root = case root of
    RealNode s -> Encode.object
        [("state", converter s.state),("value", Encode.float s.value),("type", Encode.string "real")]
    VariableNode s -> Encode.object
        [("state", converter s.state),("name", Encode.string s.name),("type", Encode.string "variable")]
    UnaryNode s -> Encode.object
        [("state", converter s.state),("name", Encode.string s.name),("child", encode converter s.child),("type", Encode.string "unary")]
    GenericNode s -> Encode.object
        [("state", converter s.state),("name", Encode.string s.name),("children", Encode.list (encode converter) s.children),("type", Encode.string "generic")]
    DeclarativeNode s -> Encode.object
        [("state", converter s.state),("name", Encode.string s.name),("children", Encode.list (encode converter) s.children),("type", Encode.string "declarative")]
    BinaryNode s -> Encode.object
        [   ("state", converter s.state)
        ,   ("name", Encode.string s.name)
        ,   ("associative", Encode.bool s.associative)
        ,   ("commutative", Encode.bool s.commutative)
        ,   ("children", Encode.list (encode converter) s.children)
        ,   ("type", Encode.string "binary")
        ]

decoder: Decode.Decoder state -> Decode.Decoder (Tree state)
decoder stateDecoder = Decode.field "type" Decode.string
    |> Decode.andThen (\t -> let sDec = Decode.field "state" stateDecoder in
        case t of
            "real" -> Decode.map2 (\s v -> RealNode {state = s, value = v}) sDec (Decode.field "value" Decode.float)
            "variable" -> Decode.map2 (\s n -> VariableNode {state = s, name = n}) sDec (Decode.field "name" Decode.string)
            "unary" -> Decode.map3 (\s n c -> UnaryNode {state = s, name = n, child = c})
                sDec (Decode.field "name" Decode.string) (Decode.field "child" <| Decode.lazy (\_ -> decoder stateDecoder))
            "binary" -> Decode.map5 (\s n a c children -> BinaryNode {state = s, name = n, associative = a, commutative = c, children = children})
                sDec (Decode.field "name" Decode.string) (Decode.field "associative" Decode.bool)
                (Decode.field "commutative" Decode.bool) (Decode.field "children" <| Decode.list <| Decode.lazy (\_ -> decoder stateDecoder))
            "generic" -> Decode.map3 (\s n c -> GenericNode {state = s, name = n, children = c} )
                sDec (Decode.field "name" Decode.string) (Decode.field "children" <| Decode.list <| Decode.lazy (\_ -> decoder stateDecoder))
            "declarative" -> Decode.map3 (\s n c -> DeclarativeNode {state = s, name = n, children = c} )
                sDec (Decode.field "name" Decode.string) (Decode.field "children" <| Decode.list <| Decode.lazy (\_ -> decoder stateDecoder))
            _ -> Decode.fail ("Unexpected type of node: " ++ t)
    )