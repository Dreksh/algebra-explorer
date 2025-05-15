module Algo.Math exposing (
    Tree(..), equal, getChildren, getState, getName, map,
    Symbol(..),symbolicate, toString,
    FunctionProperty, FunctionProperties, functionPropertyDecoder, addConstant, encodeFunctionProperty,
    notation, parse,validVariable,
    encode, decoder
    )

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Parser.Advanced as Parser exposing ((|.), (|=))
import Set
-- Ours
import Algo.Backtrack as Backtrack

type Tree s =
    RealNode {state: s, value: Float}
    | VariableNode {state: s, constant: Bool, name: String}
    | UnaryNode {state: s, name: String, child: Tree s}
    | BinaryNode {state: s, name: String, associative: Bool, commutative: Bool, identity: Float, children: List (Tree s)}
    | GenericNode {state: s, name: String, arguments: Maybe Int, children: List (Tree s)} -- Order matters
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
            VariableNode n -> (VariableNode {state = finS, constant = n.constant, name = n.name}, nextG)
            UnaryNode n -> map_ transition (Just finS) nextG n.child
                |> \(finChild, finGlob) -> (UnaryNode {state = finS, name = n.name, child = finChild}, finGlob)
            BinaryNode n -> processChildren nextG n.children
                |> \(finChildren, finGlob) -> (BinaryNode {state = finS, name = n.name, associative = n.associative, commutative = n.commutative, identity = n.identity, children = List.reverse finChildren}, finGlob)
            GenericNode n -> processChildren nextG n.children
                |> \(finChildren, finGlob) -> (GenericNode {state = finS, name = n.name, arguments = n.arguments, children = List.reverse finChildren}, finGlob)
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

type alias FunctionProperty = Tree ()
type alias FunctionProperties = Dict.Dict String FunctionProperty

functionPropertyDecoder: Decode.Decoder FunctionProperty
functionPropertyDecoder = Decode.field "arguments" Decode.int
    |> Decode.andThen (\arguments -> if arguments <= 0
        then Decode.fail "Functions must contain a positive number of integers"
        else if arguments == 1 then Decode.succeed (UnaryNode {state = (), name = "", child = RealNode {state = (), value = 0}})
        else if arguments == 2 then Decode.map3 (\c a i -> (Maybe.withDefault False c, Maybe.withDefault False a, i))
            (Decode.field "commutative" Decode.bool |> Decode.maybe)
            (Decode.field "associative" Decode.bool |> Decode.maybe)
            (Decode.field "identity" Decode.float |> Decode.maybe)
            |> Decode.andThen (\(c, a, i) ->
                if not c && not a
                then Decode.succeed (GenericNode {state = (), name = "", arguments = Just arguments, children = []})
                else case i of
                    Nothing -> Decode.fail "Missing identity for binary function"
                    Just iden -> Decode.succeed (BinaryNode {state = (), name="", associative = a, commutative = c, identity = iden, children = []})
            )
        else Decode.succeed (GenericNode {state = (), name = "", arguments = Just arguments, children = []})
    )

encodeFunctionProperty: FunctionProperty -> List (String, Encode.Value)
encodeFunctionProperty fp = case fp of
    UnaryNode _ -> [("arguments", Encode.int 1)]
    BinaryNode n ->
        [   ("arguments", Encode.int 2)
        ,   ("commutative", Encode.bool n.commutative)
        ,   ("associative", Encode.bool n.associative)
        ,   ("identity", Encode.float n.identity)
        ]
    GenericNode n ->
        [   ("arguments", case n.arguments of
            Nothing -> Encode.int 0
            Just num -> Encode.int num
            )
        ]
    _ -> []

addConstant: String -> FunctionProperties -> FunctionProperties
addConstant key = Dict.insert key (VariableNode {state = (), name = key, constant = True})

notation: String
notation = """
a - a variable called "a"
ab - a variable "a" multiplied by a variable "b"
\\ab - a variable with a long name called "ab"
\\a(x,y;z) - a function called "a" that takes in x and y, and uses z
"""

-- Parser implementation

type Context_ =
    PrevStr_ String
type Problem_ =
    EOF_
    | ArgMismatch_ {name: String, got: Int, expect: Int}
    | UnknownArguments_ String
    | ExpectingVariableName_
    | ExpectingFunction_ String
    | ExpectingConstant_ String
    | ExpectingDigits_

type alias Parser_ a = Parser.Parser Context_ Problem_ a

parserErrorToText: String -> List (Parser.DeadEnd Context_ Problem_) -> String
parserErrorToText input stack =
    case List.head stack of
        Nothing -> "No error found, unable to determine the cause"
        Just end -> case end.problem of
            EOF_ -> case List.head end.contextStack of
                Nothing -> case String.dropLeft (end.col - 1) input |> String.left 1 of
                    "+" -> "Cannot use '+' without having an expression to the left"
                    "/" -> "Cannot use '/' without having an expression to the left"
                    "*" -> "Cannot use '*' without having an expression to the left"
                    "=" -> "Cannot use '=' without having an expression to the left"
                    n -> "Symbol is not allowed: '" ++ n ++ "'"
                Just ctx -> case ctx.context of
                    PrevStr_ "+" -> "Got a dangling '+', remove it or fill in the ▒ in: " ++ inputExtract_ end.col input
                    PrevStr_ "-" -> "Got a dangling '-', remove it or fill in the ▒ in: " ++ inputExtract_ end.col input
                    PrevStr_ "*" -> "Got a dangling '*', remove it or fill in the ▒ in: " ++ inputExtract_ end.col input
                    PrevStr_ "/" -> "Got a dangling '/', remove it or fill in the ▒ in: " ++ inputExtract_ end.col input
                    PrevStr_ "(" -> "Got a dangling '(', remove it or fill in the ▒ in: " ++ inputExtract_ end.col input
                    _ -> "No idea what happened!"
            ArgMismatch_ a -> "Function '\\" ++ a.name ++ "' is defined with " ++ String.fromInt a.expect ++ " arguments, got " ++ String.fromInt a.got
            UnknownArguments_ name -> "Function '\\" ++ name ++ "' did not specify the number of arguments"
            ExpectingVariableName_ -> "Expecting a valid variable name at: " ++ inputExtract_ end.col input
            ExpectingFunction_ name -> name ++ " is a function. Cannot be used as a constant"
            ExpectingConstant_ name -> name ++ " is a constant. Cannot be used as a function"
            ExpectingDigits_ -> "Expecting digits at: " ++ inputExtract_ end.col input

-- Show 6 before (or ellipses & 5) + 3 after (or ellipses & 2)
inputExtract_: Int -> String -> String
inputExtract_ col input =
    let

        right = (if String.length input <= col + 2 then input else String.left (col+1) input ++ "…")
            |> String.dropLeft (col - 1)
    in

        String.left (col-1) input
        |> (\str -> if col > 7 then "…" ++ String.dropLeft (col-6) str else str)
        |> \final -> final ++ "▒" ++ right

parse: FunctionProperties -> String -> Result String (Tree ())
parse funcProps input = Parser.run (equation_ funcProps |. Parser.end EOF_) input |> Result.mapError (parserErrorToText input)

expectSymbol_: String -> Parser_ ()
expectSymbol_ name = Parser.symbol (Parser.Token name EOF_)

equation_: FunctionProperties -> Parser_ (Tree ())
equation_ funcProps = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |. Parser.spaces |= expression_ funcProps |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. expectSymbol_ "="
                |= (expression_ funcProps |> Parser.inContext (PrevStr_ "="))
                |. Parser.spaces
            ,   Parser.succeed()
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            ]
    )
    |> Parser.map (\children -> case children of
        [x] -> x
        _ -> DeclarativeNode {state = (), name = "=", children = children}
    )

expression_: FunctionProperties -> Parser_ (Tree ())
expression_ funcProps = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= multiple_ funcProps |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. expectSymbol_ "+"
                |= (multiple_ funcProps |> Parser.inContext (PrevStr_ "+"))
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop ((UnaryNode {state = (), name = "-", child = elem}) :: list))
                |. expectSymbol_ "-"
                |= (multiple_ funcProps |> Parser.inContext (PrevStr_ "-"))
                |. Parser.spaces
            ,   Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            ]
    ) |> Parser.map (\children -> case children of
        [x] -> x
        _ -> BinaryNode {state = (), name = "+", associative = True, commutative = True, identity = 0, children = children}
    )

multiple_: FunctionProperties -> Parser_ (Tree ())
multiple_ funcProps = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= negatable_ funcProps |. Parser.spaces
        else Parser.oneOf
            [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |. expectSymbol_ "*"
                |= (negatable_ funcProps |> Parser.inContext (PrevStr_ "*"))
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop (UnaryNode {state = (), name = "/", child = elem} :: list))
                |. expectSymbol_ "/"
                |= (negatable_ funcProps |> Parser.inContext (PrevStr_ "/"))
                |. Parser.spaces
            ,   Parser.succeed (\elem -> Parser.Loop (elem :: list))
                |= term_ funcProps
                |. Parser.spaces
            ,   Parser.succeed ()
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            ]
    )
    |> Parser.map (\children -> case children of
        [x] -> x
        _ -> BinaryNode {state = (), name = "*", associative = True, commutative = True, identity = 1, children = children}
    )

negatable_: FunctionProperties -> Parser_ (Tree ())
negatable_ funcProps = Parser.oneOf
    [   Parser.succeed (\x -> UnaryNode {state = (), name="-", child = x}) |. expectSymbol_ "-" |. Parser.spaces |= Parser.lazy (\_ -> negatable_ funcProps |> Parser.inContext (PrevStr_ "-"))
    ,   Parser.succeed identity |= term_ funcProps
    ]

term_: FunctionProperties -> Parser_ (Tree ())
term_ funcProps = Parser.oneOf
    [   Parser.succeed identity |. expectSymbol_ "(" |= (expression_ funcProps |> Parser.inContext (PrevStr_ "("))|. Parser.spaces |. expectSymbol_ ")"
    ,   tokenNumber_
    ,   Parser.succeed (\a b -> (a, b)) |. expectSymbol_ "\\" |= tokenLongName_ |= varOrFunc_ funcProps
        |> Parser.andThen (\(name, props) -> case (Dict.get name funcProps, props) of
            (Nothing, Nothing) -> VariableNode {state = (), constant = False, name = name} |> Parser.succeed
            (Nothing, Just children) -> GenericNode {state = (), name = name, arguments = Nothing, children = children} |> Parser.succeed
            (Just p, Nothing) -> case p of
                VariableNode _ -> VariableNode {state = (), constant = True, name = name} |> Parser.succeed
                _ -> Parser.problem (ExpectingFunction_ name)
            (Just p, Just children) -> case p of
                UnaryNode _ -> case children of
                    [child] -> Parser.succeed (UnaryNode {state = (), name = name, child = child})
                    _ -> Parser.problem (ArgMismatch_ {name = name, got = List.length children, expect = 1})
                GenericNode n -> case n.arguments of
                    Nothing -> Parser.problem (UnknownArguments_ name)
                    Just args -> if args /= List.length children then Parser.problem (ArgMismatch_ {name = name, got = List.length children, expect = args})
                        else Parser.succeed (GenericNode {state = (), name = name, arguments = Just args, children = children})
                BinaryNode n -> if List.length children < 2 then Parser.problem (ArgMismatch_ {name = name, got = List.length children, expect = 2})
                    else Parser.succeed (BinaryNode {state = (), name = name, associative = n.associative, commutative = n.commutative, identity = n.identity, children = children})
                _ -> Parser.problem (ExpectingConstant_ name)
        )
    ,   Parser.andThen (\name -> case Dict.get name funcProps of
            Nothing -> Parser.succeed (VariableNode {state = (), name = name, constant = False})
            Just (VariableNode _) -> Parser.succeed (VariableNode {state = (), name = name, constant = True})
            _ -> Parser.problem (ExpectingFunction_ name)
        ) tokenShortName_
    ]

varOrFunc_: FunctionProperties -> Parser_ (Maybe (List (Tree ())))
varOrFunc_ funcProps = Parser.oneOf
    [   Parser.succeed Just |. expectSymbol_ "(" |= argsList_ funcProps |. expectSymbol_ ")"
    ,   Parser.succeed Nothing
    ]

argsList_: FunctionProperties -> Parser_ (List (Tree ()))
argsList_ funcProps = Parser.loop []
    (\list -> if List.isEmpty list then Parser.succeed (List.singleton >> Parser.Loop) |= (expression_ funcProps |> Parser.inContext (PrevStr_ "(")) |. Parser.spaces
        else Parser.oneOf
        [   Parser.succeed (\elem -> Parser.Loop (elem :: list))
            |. expectSymbol_ ","
            |= (expression_ funcProps |> Parser.inContext (PrevStr_ ","))
            |. Parser.spaces
        ,   Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse list))
        ]
    )

tokenNumber_: Parser_ (Tree ())
tokenNumber_ = Parser.succeed (\a b -> a ++ b) |= tokenDigit_ |= Parser.oneOf [Parser.succeed (\b -> "." ++ b) |. expectSymbol_ "." |= tokenDigit_ , Parser.succeed ""]
    |> Parser.andThen (\str -> case String.toFloat str of
        Nothing -> Parser.problem ExpectingDigits_
        Just f -> Parser.succeed (RealNode {state = (), value = f})
    )

tokenDigit_: Parser_ String
tokenDigit_ = Parser.variable
    {   start = Char.isDigit
    ,   inner = Char.isDigit
    ,   reserved = Set.empty
    ,   expecting = ExpectingDigits_
    }

validVariable: String -> Result String String
validVariable str = Parser.run
    (Parser.oneOf [Parser.succeed identity |. expectSymbol_ "\\" |= tokenLongName_, tokenShortName_])
    str
    |> Result.mapError (\_ -> "Inavlid variable name")

validVarStart_: Char -> Bool
validVarStart_ char = not (String.contains (String.fromChar char) " ~+-=*/\\.()[],;%!:;<>0123456789^&|$↔→")

tokenLongName_: Parser_ String
tokenLongName_ = Parser.variable
    {   start = validVarStart_
    ,   inner = (\c -> validVarStart_ c || Char.isAlphaNum c )
    ,   reserved = Set.empty
    ,   expecting = ExpectingVariableName_
    }

tokenShortName_: Parser_ String
tokenShortName_ = Parser.variable
    {   start = validVarStart_
    ,   inner = (\_ -> False)
    ,   reserved = Set.empty
    ,   expecting = ExpectingVariableName_
    }

encode: (state -> Encode.Value) -> Tree state -> Encode.Value
encode converter root = case root of
    RealNode s -> Encode.object
        [("state", converter s.state),("value", Encode.float s.value),("type", Encode.string "real")]
    VariableNode s -> Encode.object
        [("state", converter s.state),("name", Encode.string s.name),("type", Encode.string "variable"),("constant", Encode.bool s.constant)]
    UnaryNode s -> Encode.object
        [("state", converter s.state),("name", Encode.string s.name),("child", encode converter s.child),("type", Encode.string "unary")]
    GenericNode s -> Encode.object
        [("state", converter s.state),("name", Encode.string s.name),("children", Encode.list (encode converter) s.children),("type", Encode.string "generic")
        , ("arguments", case s.arguments of
            Nothing -> Encode.null
            Just num -> Encode.int num
        )
        ]
    DeclarativeNode s -> Encode.object
        [("state", converter s.state),("name", Encode.string s.name),("children", Encode.list (encode converter) s.children),("type", Encode.string "declarative")]
    BinaryNode s -> Encode.object
        [   ("state", converter s.state)
        ,   ("name", Encode.string s.name)
        ,   ("associative", Encode.bool s.associative)
        ,   ("commutative", Encode.bool s.commutative)
        ,   ("identity", Encode.float s.identity)
        ,   ("children", Encode.list (encode converter) s.children)
        ,   ("type", Encode.string "binary")
        ]

decoder: Decode.Decoder state -> Decode.Decoder (Tree state)
decoder stateDecoder = Decode.field "type" Decode.string
    |> Decode.andThen (\t -> let sDec = Decode.field "state" stateDecoder in
        case t of
            "real" -> Decode.map2 (\s v -> RealNode {state = s, value = v}) sDec (Decode.field "value" Decode.float)
            "variable" -> Decode.map3 (\s n c -> VariableNode {state = s, constant = c, name = n}) sDec (Decode.field "name" Decode.string) (Decode.field "constant" Decode.bool)
            "unary" -> Decode.map3 (\s n c -> UnaryNode {state = s, name = n, child = c})
                sDec (Decode.field "name" Decode.string) (Decode.field "child" <| Decode.lazy (\_ -> decoder stateDecoder))
            "binary" -> Decode.map6 (\s n a c i children -> BinaryNode {state = s, name = n, associative = a, commutative = c, identity = i, children = children})
                sDec (Decode.field "name" Decode.string) (Decode.field "associative" Decode.bool)
                (Decode.field "commutative" Decode.bool) (Decode.field "identity" Decode.float)
                (Decode.field "children" <| Decode.list <| Decode.lazy (\_ -> decoder stateDecoder))
            "generic" -> Decode.map4 (\s n c a -> GenericNode {state = s, name = n, arguments = a, children = c} )
                sDec (Decode.field "name" Decode.string) (Decode.field "children" <| Decode.list <| Decode.lazy (\_ -> decoder stateDecoder))
                (Decode.maybe (Decode.field "arguments" Decode.int))
            "declarative" -> Decode.map3 (\s n c -> DeclarativeNode {state = s, name = n, children = c} )
                sDec (Decode.field "name" Decode.string) (Decode.field "children" <| Decode.list <| Decode.lazy (\_ -> decoder stateDecoder))
            _ -> Decode.fail ("Unexpected type of node: " ++ t)
    )