module Components.Latex exposing (
    Model, Style(..), Part(..), Symbol(..), map, replace, getParamIndexes,
    symbolToStr, extractArgs, greekLetters,
    parse, unparse, decoder, encode
    )

import Array
import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=))
import Set
-- Ours
import Helper

type alias Model elem = List (Part elem)
type Style =
    Faded
    | Emphasis
type Part elem =
    Fraction {state: elem, style: Maybe Style} (Model elem) (Model elem)
    | Text {state: elem, style: Maybe Style} String
    | SymbolPart {state: elem, style: Maybe Style} Symbol
    | Superscript {state: elem, style: Maybe Style} (Model elem)
    | Subscript {state: elem, style: Maybe Style} (Model elem)
    | Bracket {state: elem, style: Maybe Style} (Model elem) -- Round brackets that wrap around some content
    | Sqrt {state: elem, style: Maybe Style} (Model elem)
    | Argument {state: elem, style: Maybe Style} Int -- Our custom insertion, for adding
    | Param {state: elem, style: Maybe Style} Int
    -- Specifically for input
    | Caret {state: elem, style: Maybe Style}
    | Border {state: elem, style: Maybe Style} (Model elem) --

type Symbol =
    -- Greek
    AlphaLower
    | BetaLower
    | ChiLower
    | DeltaLower
    | EpsilonLower
    | EpsilonVarLower
    | EtaLower
    | GammaLower
    | IotaLower
    | KappaLower
    | LambdaLower
    | MuLower
    | NuLower
    | OmegaLower
    | PhiLower
    | PhiVarLower
    | PiLower
    | PsiLower
    | RhoLower
    | SigmaLower
    | TauLower
    | ThetaLower
    | UpsilonLower
    | XiLower
    | ZetaLower
    | DeltaUpper
    | GammaUpper
    | LambdaUpper
    | OmegaUpper
    | PhiUpper
    | PiUpper
    | PsiUpper
    | SigmaUpper
    | ThetaUpper
    | UpsilonUpper
    | XiUpper
    -- Symbols
    | Infinity
    | Circ
    | RightArrow
    -- Operators
    | CrossMultiplcation
    | Division
    | Integration

{- ## General processing of Latex.Model -}

map: (a -> b) -> Model a -> Model b
map convert = let inConv e = {style = e.style, state = convert e.state} in
    List.map (\root -> case root of
        Fraction e top bot -> Fraction (inConv e) (map convert top) (map convert bot)
        Text e str -> Text (inConv e) str
        SymbolPart e symbol -> SymbolPart (inConv e) symbol
        Superscript e inner -> Superscript (inConv e) (map convert inner)
        Subscript e inner -> Subscript (inConv e) (map convert inner)
        Bracket e inner -> Bracket (inConv e) (map convert inner)
        Sqrt e inner -> Sqrt (inConv e) (map convert inner)
        Argument e int -> Argument (inConv e) int
        Param e int -> Param (inConv e) int
        Caret e -> Caret (inConv e)
        Border e inner -> Border (inConv e) (map convert inner)
    )

replace: Array.Array (Model state) -> Model state -> Model state
replace params = List.concatMap (\part -> case part of
        Fraction e top bot -> [Fraction e (replace params top) (replace params bot)]
        Text e str -> [Text e str]
        SymbolPart e symbol -> [SymbolPart e symbol]
        Superscript e inner -> [Superscript e (replace params inner)]
        Subscript e inner -> [Subscript e (replace params inner)]
        Bracket e inner -> [Bracket e (replace params inner)]
        Sqrt e inner -> [Sqrt e (replace params inner)]
        Argument e num -> case Array.get (num - 1) params of
            Nothing -> [Argument e num]
            Just model -> model
        Param e num -> case Array.get (num - 1) params of
            Nothing -> [Param e num]
            Just model -> model
        Caret e -> [Caret e]
        Border e inner -> [Border e (replace params inner)]
    )

getParamIndexes: Model a -> Set.Set Int
getParamIndexes latex =
    let
        recursive m s = List.foldl (\part set -> case part of
            Fraction _ top bot -> recursive top set |> recursive bot
            Text _ _ -> set
            SymbolPart _ _ -> set
            Superscript _ inner -> recursive inner set
            Subscript _ inner -> recursive inner set
            Bracket _ inner -> recursive inner set
            Sqrt _ inner -> recursive inner set
            Argument _ _ -> set
            Param _ num -> Set.insert num set
            Caret _ -> set
            Border _ inner -> recursive inner set
            ) s m
    in
        recursive latex Set.empty

{- ## Encoding, Decoding: to and from the written form of the tree structure -}

decoder: Decode.Decoder a -> Decode.Decoder (Model a)
decoder inner =
    let
        stateDec = Decode.map2 (\a b -> {style = a, state = b})
            (Decode.field "style" styleDecoder_ |> Decode.maybe)
            (Decode.field "state" inner)
    in Decode.field "type" Decode.string
    |> Decode.andThen (\str -> case str of
        "fraction" -> Decode.map3 Fraction
            (Decode.field "state" stateDec)
            (Decode.field "top" (Decode.lazy (\_ -> decoder inner)))
            (Decode.field "bot" (Decode.lazy (\_ -> decoder inner)))
        "text" -> Decode.map2 Text (Decode.field "state" stateDec) (Decode.field "text" Decode.string)
        "symbol" -> Decode.map2 SymbolPart (Decode.field "state" stateDec) (Decode.field "symbol" symbolDecoder_)
        "sup" -> Decode.map2 Superscript (Decode.field "state" stateDec) (Decode.field "inner" (Decode.lazy (\_ -> decoder inner)))
        "sub" -> Decode.map2 Subscript (Decode.field "state" stateDec) (Decode.field "inner" (Decode.lazy (\_ -> decoder inner)))
        "bracket" -> Decode.map2 Bracket (Decode.field "state" stateDec) (Decode.field "inner" (Decode.lazy (\_ -> decoder inner)))
        "sqrt" -> Decode.map2 Sqrt (Decode.field "state" stateDec) (Decode.field "inner" (Decode.lazy (\_ -> decoder inner)))
        "arg" -> Decode.map2 Argument (Decode.field "state" stateDec) (Decode.field "num" Decode.int)
        _ -> Decode.fail ("unknown type: '" ++ str ++ "'")
    )
    |> Decode.list

encode: (s -> Encode.Value) -> Model s -> Encode.Value
encode convert =
    let inConv e = Encode.object [("state", convert e.state), ("style", Maybe.map (styleToStr_ >> Encode.string) e.style |> Maybe.withDefault Encode.null)] in
    Encode.list (\n -> case n of
        Fraction e top bot -> Encode.object
            [("type",Encode.string "fraction"),("top",encode convert top),("bot",encode convert bot),("state",inConv e)]
        Text e str -> Encode.object [("type",Encode.string "text"),("state", inConv e),("text",Encode.string str)]
        SymbolPart e symbol -> Encode.object [("type",Encode.string "symbol"),("state", inConv e),("symbol",encodeSymbol_ symbol)]
        Superscript e inner -> Encode.object [("type",Encode.string "sup"),("state", inConv e),("inner",encode convert inner)]
        Subscript e inner -> Encode.object [("type",Encode.string "sub"),("state", inConv e),("inner",encode convert inner)]
        Bracket e inner -> Encode.object [("type",Encode.string "bracket"),("state", inConv e),("inner",encode convert inner)]
        Sqrt e inner -> Encode.object [("type",Encode.string "sqrt"),("state", inConv e),("inner",encode convert inner)]
        Argument e int -> Encode.object [("type",Encode.string "arg"),("state", inConv e),("num", Encode.int int)]
        Param e int -> Encode.object [("type",Encode.string "param"),("state", inConv e),("num", Encode.int int)]
        Caret e -> Encode.null
        Border e inner -> Encode.null
    )

symbolDecoder_: Decode.Decoder Symbol
symbolDecoder_ = Decode.string
    |> Decode.andThen (strToSymbol_ >> Helper.resultToDecoder)

styleDecoder_: Decode.Decoder Style
styleDecoder_ = Decode.string
    |> Decode.andThen (\str -> case str of
        "faded" -> Decode.succeed Faded
        "emphasis" -> Decode.succeed Emphasis
        _ -> Decode.fail "unknown style"
    )

styleToStr_: Style -> String
styleToStr_ style = case style of
    Faded -> "faded"
    Emphasis -> "emphasis"

strToSymbol_: String -> Result String Symbol
strToSymbol_ str = case str of
    "alpha" -> Ok AlphaLower
    "beta" -> Ok BetaLower
    "chi" -> Ok ChiLower
    "delta" -> Ok DeltaLower
    "epsilon" -> Ok EpsilonLower
    "varepsilon" -> Ok EpsilonVarLower
    "eta" -> Ok EtaLower
    "gamma" -> Ok GammaLower
    "iota" -> Ok IotaLower
    "kappa" -> Ok KappaLower
    "lambda" -> Ok LambdaLower
    "mu" -> Ok MuLower
    "nu" -> Ok NuLower
    "omega" -> Ok OmegaLower
    "phi" -> Ok PhiLower
    "varphi" -> Ok PhiVarLower
    "pi" -> Ok PiLower
    "psi" -> Ok PsiLower
    "rho" -> Ok RhoLower
    "sigma" -> Ok SigmaLower
    "tau" -> Ok TauLower
    "theta" -> Ok ThetaLower
    "upsilon" -> Ok UpsilonLower
    "xi" -> Ok XiLower
    "zeta" -> Ok ZetaLower
    "Delta" -> Ok DeltaUpper
    "Gamma" -> Ok GammaUpper
    "Lambda" -> Ok LambdaUpper
    "Omega" -> Ok OmegaUpper
    "Phi" -> Ok PhiUpper
    "Pi" -> Ok PiUpper
    "Psi" -> Ok PsiUpper
    "Sigma" -> Ok SigmaUpper
    "Theta" -> Ok ThetaUpper
    "Upsilon" -> Ok UpsilonUpper
    "Xi" -> Ok XiUpper
    "infty" -> Ok Infinity
    "circ" -> Ok Circ
    "rightarrow" -> Ok RightArrow
    "times" -> Ok CrossMultiplcation
    "div" -> Ok Division
    "int" -> Ok Integration
    _ -> Err ("symbol not found: '" ++ str ++ "'")

symbolToStr: Symbol -> String
symbolToStr s = case s of
    AlphaLower -> "alpha"
    BetaLower -> "beta"
    ChiLower -> "chi"
    DeltaLower -> "delta"
    EpsilonLower -> "epsilon"
    EpsilonVarLower -> "varepsilon"
    EtaLower -> "eta"
    GammaLower -> "gamma"
    IotaLower -> "iota"
    KappaLower -> "kappa"
    LambdaLower -> "lambda"
    MuLower -> "mu"
    NuLower -> "nu"
    OmegaLower -> "omega"
    PhiLower -> "phi"
    PhiVarLower -> "varphi"
    PiLower -> "pi"
    PsiLower -> "psi"
    RhoLower -> "rho"
    SigmaLower -> "sigma"
    TauLower -> "tau"
    ThetaLower -> "theta"
    UpsilonLower -> "upsilon"
    XiLower -> "xi"
    ZetaLower -> "zeta"
    DeltaUpper -> "Delta"
    GammaUpper -> "Gamma"
    LambdaUpper -> "Lambda"
    OmegaUpper -> "Omega"
    PhiUpper -> "Phi"
    PiUpper -> "Pi"
    PsiUpper -> "Psi"
    SigmaUpper -> "Sigma"
    ThetaUpper -> "Theta"
    UpsilonUpper -> "Upsilon"
    XiUpper -> "Xi"
    Infinity -> "infty"
    Circ -> "circ"
    RightArrow -> "rightarrow"
    CrossMultiplcation -> "times"
    Division -> "div"
    Integration -> "int"

greekLetters: Dict.Dict String Symbol
greekLetters =
    [ AlphaLower, BetaLower, ChiLower, DeltaLower, EpsilonLower, EpsilonVarLower, EtaLower
    , GammaLower, IotaLower, KappaLower, LambdaLower, MuLower, NuLower, OmegaLower, PhiLower
    , PhiVarLower, PiLower, PsiLower, RhoLower, SigmaLower, TauLower, ThetaLower, UpsilonLower
    , XiLower, ZetaLower, DeltaUpper, GammaUpper, LambdaUpper, OmegaUpper, PhiUpper, PiUpper
    , PsiUpper, SigmaUpper, ThetaUpper, UpsilonUpper, XiUpper, Infinity
    ]
    |> List.map (\s -> (symbolToStr s,s))
    |> Dict.fromList

encodeSymbol_: Symbol -> Encode.Value
encodeSymbol_ = symbolToStr >> Encode.string

{- Parsing and Unparsing: To and from the written latex script (partial implementaiton) -}

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
    Caret _ -> ""
    Border _ inner -> unparse inner
    )
    >> String.join ""

-- https://www.overleaf.com/learn/latex/List_of_Greek_letters_and_math_symbols
-- The set is only for symbols, not functional stuff like \frac (that is done separately)
parse: String -> Result String (Model ())
parse str = case Parser.run (modelParser_ |. Parser.end) str of
    Err _ -> Err "TODO"
    Ok model -> extractArgs model |> Result.map (\_ -> model)

extractArgs: Model a -> Result String (Dict.Dict Int {style: Maybe Style, state: a})
extractArgs =
    let
        recursive initial = Helper.resultList (\elem (found, m) -> case elem of
                Fraction _ top bot -> recursive (found, m) top
                    |> Result.andThen (\b -> recursive b bot)
                Text _ _ -> Ok (found, m)
                SymbolPart _ _ -> Ok (found, m)
                Superscript _ n -> recursive (found, m) n
                Subscript _ n -> recursive (found, m) n
                Bracket _ _ -> Ok (found, m)
                Sqrt _ n -> recursive (found, m) n
                Argument s arg -> if Dict.member arg found
                    then Err ("Arg number " ++ String.fromInt arg ++ " is repeated")
                    else Ok (Dict.insert arg s found, max m arg)
                Param s arg -> if Dict.member arg found
                    then Err ("Param number " ++ String.fromInt arg ++ " is repeated")
                    else Ok (Dict.insert arg s found, max m arg)
                Caret _ -> Ok (found, m)
                Border _ n -> Ok (found, m)
            ) initial
    in
        recursive (Dict.empty, 0)
        >> Result.andThen (\(dict, maxNum) -> if Dict.size dict == maxNum then Ok dict
            else Err "Not all arguments are present in the representation"
        )

defaultState_: {state: (), style: Maybe Style}
defaultState_ = {state = (), style = Nothing}

modelParser_: Parser.Parser (Model ())
modelParser_ = Parser.loop []
    (\list -> if List.isEmpty list then bracketOrSingle_ |. Parser.spaces |> Parser.map Parser.Loop else
        Parser.oneOf
        [   Parser.succeed (\n -> list ++ [Subscript defaultState_ n] |> Parser.Loop) |. Parser.token "_" |= bracketOrSingle_
        ,   Parser.succeed (\n -> list ++ [Superscript defaultState_ n] |> Parser.Loop) |. Parser.token "^" |= bracketOrSingle_
        ,   Parser.succeed (\n -> list ++ [n] |> Parser.Loop) |= argumentsParser_
        ,   bracketOrSingle_ |> Parser.map (\new -> Parser.Loop (list ++ new))
        ,   Parser.succeed (Parser.Done list)
        ]
        |. Parser.spaces
    )

argumentsParser_: Parser.Parser (Part ())
argumentsParser_ = Parser.succeed (\n -> Bracket defaultState_ n)
    |. Parser.token "("
    |= Parser.loop [] (\list -> if List.isEmpty list then modelParser_ |> Parser.map Parser.Loop
        else Parser.oneOf
            [   Parser.succeed ((::) (Text defaultState_ ",") >> (++) list >> Parser.Loop) |. Parser.token "," |. Parser.spaces |= modelParser_
            ,   Parser.succeed (Parser.Done list)
            ]
    )
    |. Parser.token ")"

valueParser_: Parser.Parser (Part ())
valueParser_ = Parser.oneOf
    [   Parser.succeed identity
        |. Parser.token "\\"
        |= Parser.oneOf
            [   Parser.succeed (Fraction defaultState_)  |. Parser.keyword "frac" |= bracketParser_ |= bracketParser_
            ,   Parser.succeed (Sqrt defaultState_) |. Parser.keyword "sqrt" |= bracketParser_
            ,   Parser.succeed (Argument defaultState_) |. Parser.keyword "arg" |. Parser.token "{" |= numParser_ |. Parser.token "}"
            ,   Parser.succeed (Param defaultState_) |. Parser.keyword "param" |. Parser.token "{" |= numParser_ |. Parser.token "}"
            ,   Parser.succeed (SymbolPart defaultState_) |= wordParser_
            ]
    ,   Parser.succeed (Bracket defaultState_) |. Parser.token "(" |. Parser.spaces |= modelParser_ |. Parser.token ")"
    ,   Parser.succeed (Text defaultState_) |= letterParser_
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
notText_ c = String.contains (String.fromChar c) " \n\t{}\\0123456789()_^"

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