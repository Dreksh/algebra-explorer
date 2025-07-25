module Components.Latex exposing (Model, Part(..), Symbol(..), map, parse, unparse, symbolToStr, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=))
import Set
-- Ours
import Helper

type alias Model elem = List (Part elem)

type Part elem =
    Fraction elem (Model elem) (Model elem)
    | Text elem String
    | SymbolPart elem Symbol
    | Superscript elem (Model elem)
    | Subscript elem (Model elem)
    | Bracket elem (Model elem) -- Round brackets that wrap around some content
    | Sqrt elem (Model elem)
    | Argument elem Int -- Our custom insertion, for adding
    | Param elem Int

type Symbol =
    AlphaLower
    | BetaLower
    | CrossMultiplcation
    | Division
    | Integration

map: (a -> b) -> Model a -> Model b
map convert = List.map (\root -> case root of
        Fraction e top bot -> Fraction (convert e) (map convert top) (map convert bot)
        Text e str -> Text (convert e) str
        SymbolPart e symbol -> SymbolPart (convert e) symbol
        Superscript e inner -> Superscript (convert e) (map convert inner)
        Subscript e inner -> Subscript (convert e) (map convert inner)
        Bracket e inner -> Bracket (convert e) (map convert inner)
        Sqrt e inner -> Sqrt (convert e) (map convert inner)
        Argument e int -> Argument (convert e) int
        Param e int -> Param (convert e) int
    )

decoder: Decode.Decoder a -> Decode.Decoder (Model a)
decoder inner = Decode.field "type" Decode.string
    |> Decode.andThen (\str -> case str of
        "fraction" -> Decode.map3 Fraction
            (Decode.field "state" inner)
            (Decode.field "top" (Decode.lazy (\_ -> decoder inner)))
            (Decode.field "bot" (Decode.lazy (\_ -> decoder inner)))
        "text" -> Decode.map2 Text (Decode.field "state" inner) (Decode.field "text" Decode.string)
        "symbol" -> Decode.map2 SymbolPart (Decode.field "state" inner) (Decode.field "symbol" symbolDecoder_)
        "sup" -> Decode.map2 Superscript (Decode.field "state" inner) (Decode.field "inner" (Decode.lazy (\_ -> decoder inner)))
        "sub" -> Decode.map2 Subscript (Decode.field "state" inner) (Decode.field "inner" (Decode.lazy (\_ -> decoder inner)))
        "bracket" -> Decode.map2 Bracket (Decode.field "state" inner) (Decode.field "inner" (Decode.lazy (\_ -> decoder inner)))
        "sqrt" -> Decode.map2 Sqrt (Decode.field "state" inner) (Decode.field "inner" (Decode.lazy (\_ -> decoder inner)))
        "arg" -> Decode.map2 Argument (Decode.field "state" inner) (Decode.field "num" Decode.int)
        _ -> Decode.fail ("unknown type: '" ++ str ++ "'")
    )
    |> Decode.list

encode: (s -> Encode.Value) -> Model s -> Encode.Value
encode convert = Encode.list (\n -> case n of
        Fraction e top bot -> Encode.object [("type",Encode.string "fraction"), ("state",convert e),("top",encode convert top),("bot",encode convert bot)]
        Text e str -> Encode.object [("type",Encode.string "text"),("state", convert e),("text",Encode.string str)]
        SymbolPart e symbol -> Encode.object [("type",Encode.string "symbol"),("state", convert e),("symbol",encodeSymbol_ symbol)]
        Superscript e inner -> Encode.object [("type",Encode.string "sup"),("state", convert e),("inner",encode convert inner)]
        Subscript e inner -> Encode.object [("type",Encode.string "sub"),("state", convert e),("inner",encode convert inner)]
        Bracket e inner -> Encode.object [("type",Encode.string "bracket"),("state", convert e),("inner",encode convert inner)]
        Sqrt e inner -> Encode.object [("type",Encode.string "sqrt"),("state", convert e),("inner",encode convert inner)]
        Argument e int -> Encode.object [("type",Encode.string "arg"),("state", convert e),("num", Encode.int int)]
        Param e int -> Encode.object [("type",Encode.string "param"),("state", convert e),("num", Encode.int int)]
    )

symbolDecoder_: Decode.Decoder Symbol
symbolDecoder_ = Decode.string
    |> Decode.andThen (strToSymbol_ >> Helper.resultToDecoder)

strToSymbol_: String -> Result String Symbol
strToSymbol_ str = case str of
    "alpha" -> Ok AlphaLower
    "beta" -> Ok BetaLower
    "cross" -> Ok CrossMultiplcation
    "div" -> Ok Division
    "int" -> Ok Integration
    _ -> Err ("symbol not found: '" ++ str ++ "'")

symbolToStr: Symbol -> String
symbolToStr s = case s of
    AlphaLower -> "alpha"
    BetaLower -> "beta"
    CrossMultiplcation -> "cross"
    Division -> "div"
    Integration -> "int"

encodeSymbol_: Symbol -> Encode.Value
encodeSymbol_ = symbolToStr >> Encode.string

unparse: Model s -> String
unparse = List.map (\token -> case token of
    Fraction _ up down -> "\\frac{" ++ unparse up ++ "}{" ++ unparse down ++ "}"
    Text _ t -> t ++ " "
    SymbolPart _ s -> "\\" ++ symbolToStr s ++ " "
    Superscript _ inner -> "_{" ++ unparse inner ++ "}"
    Subscript _ inner -> "^{" ++ unparse inner ++ "}"
    Bracket _ inner -> "(" ++ unparse inner ++ ")"
    Sqrt _ inner -> "{" ++ unparse inner ++ "}"
    Argument _ num -> "\\arg{" ++ String.fromInt num ++ "}"
    Param _ num -> "\\param{" ++ String.fromInt num ++ "}"
    )
    >> String.join ""

-- https://www.overleaf.com/learn/latex/List_of_Greek_letters_and_math_symbols
-- The set is only for symbols, not functional stuff like \frac (that is done separately)
parse: String -> Result String (Model ())
parse str = case Parser.run (modelParser_ |. Parser.end) str of
    Err _ -> Err "TODO"
    Ok model -> extractArgs_ (Set.empty, 0) model
        |> Result.andThen ( \(set, max) -> if Set.size set == max then Ok model
            else Err "Not all arguments are present in the representation"
        )

extractArgs_: (Set.Set Int, Int) -> Model () -> Result String (Set.Set Int, Int)
extractArgs_ = Helper.resultList (\elem (found, m) -> case elem of
        Fraction _ top bot -> extractArgs_ (found, m) top
            |> Result.andThen (\b -> extractArgs_ b bot)
        Text _ _ -> Ok (found, m)
        SymbolPart _ _ -> Ok (found, m)
        Superscript _ n -> extractArgs_ (found, m) n
        Subscript _ n -> extractArgs_ (found, m) n
        Bracket _ _ -> Ok (found, m)
        Sqrt _ n -> extractArgs_ (found, m) n
        Argument _ arg -> if Set.member arg found
            then Err ("Arg number " ++ String.fromInt arg ++ " is repeated")
            else Ok (Set.insert arg found, max m arg)
        Param _ arg -> Ok (Set.insert arg found, max m arg)
    )

modelParser_: Parser.Parser (Model ())
modelParser_ = Parser.loop []
    (\list -> if List.isEmpty list then bracketOrSingle_ |. Parser.spaces |> Parser.map Parser.Loop else
        Parser.oneOf
        [   bracketOrSingle_ |> Parser.map (\new -> Parser.Loop (list ++ new))
        ,   Parser.succeed (\n -> list ++ [Subscript () n] |> Parser.Loop) |. Parser.token "_" |= bracketOrSingle_
        ,   Parser.succeed (\n -> list ++ [Superscript () n] |> Parser.Loop) |. Parser.token "^" |= bracketOrSingle_
        ,   Parser.succeed (\n -> n :: list |> Parser.Loop) |= argumentsParser_
        ,   Parser.succeed (Parser.Done list)
        ]
        |. Parser.spaces
    )

argumentsParser_: Parser.Parser (Part ())
argumentsParser_ = Parser.succeed (\n -> Bracket () n)
    |. Parser.token "("
    |= Parser.loop [] (\list -> if List.isEmpty list then modelParser_ |> Parser.map Parser.Loop
        else Parser.oneOf
            [   Parser.succeed ((::) (Text () ",") >> (++) list >> Parser.Loop) |. Parser.token "," |. Parser.spaces |= modelParser_
            ,   Parser.succeed (Parser.Done list)
            ]
    )
    |. Parser.token ")"

valueParser_: Parser.Parser (Part ())
valueParser_ = Parser.oneOf
    [   Parser.succeed identity
        |. Parser.token "\\"
        |= Parser.oneOf
            [   Parser.succeed (Fraction ()) |. Parser.keyword "frac" |= bracketParser_ |= bracketParser_
            ,   Parser.succeed (Sqrt ()) |. Parser.keyword "sqrt" |= bracketParser_
            ,   Parser.succeed (Argument ()) |. Parser.keyword "arg" |. Parser.token "{" |= numParser_ |. Parser.token "}"
            ,   Parser.succeed (Param ()) |. Parser.keyword "param" |. Parser.token "{" |= numParser_ |. Parser.token "}"
            ,   Parser.succeed (SymbolPart ()) |= wordParser_
            ]
    ,   Parser.succeed (Bracket ()) |. Parser.token "(" |. Parser.spaces |= modelParser_ |. Parser.token ")"
    ,   Parser.succeed (Text ()) |= letterParser_
    ]
    |. Parser.spaces

bracketOrSingle_: Parser.Parser (Model ())
bracketOrSingle_ = Parser.oneOf
    [   bracketParser_
    ,   Parser.succeed List.singleton |= valueParser_
    ]

bracketParser_: Parser.Parser (Model ())
bracketParser_ = Parser.succeed identity |. Parser.token "{" |= modelParser_ |. Parser.token "}"

numParser_: Parser.Parser Int
numParser_ = Parser.variable
    {   start = Char.isDigit
    ,   inner = Char.isDigit
    ,   reserved = Set.empty
    }
    |> Parser.andThen (\str -> case String.toInt str of
        Nothing -> Parser.problem "Cannot convert to integer"
        Just num -> Parser.succeed num
    )

letterParser_: Parser.Parser String
letterParser_ = Parser.variable
    {   start = notText_ >> not
    ,   inner = notText_ >> not
    ,   reserved = Set.empty
    }

notText_: Char.Char -> Bool
notText_ c = String.contains (String.fromChar c) " \n\t{}\\0123456789()"

wordParser_: Parser.Parser Symbol
wordParser_ = Parser.variable
    {   start = Char.isAlpha
    ,   inner = Char.isAlpha
    ,   reserved = Set.empty
    }
    |> Parser.andThen (\str -> case strToSymbol_ str of
        Ok symbol -> Parser.succeed symbol
        Err err -> Parser.problem err
    )