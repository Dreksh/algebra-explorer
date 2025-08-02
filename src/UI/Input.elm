module UI.Input exposing (Model, Event(..), Entry, baseState, init, update, clear, set, view, toTree)

import Array
import Dict
import Html
import Html.Attributes exposing (class, id, name, style, type_)
import Html.Keyed
import Json.Encode as Encode
-- Ours
import Helper
import Algo.Matcher as Matcher
import Algo.Math as Math
import Components.Latex as Latex
import Components.Rules as Rules
import UI.Animation as Animation
import UI.Display as Display
import UI.HtmlEvent as HtmlEvent
import UI.MathIcon as MathIcon
import UI.SvgDrag as SvgDrag

-- a scope is which function + parameter number they belong to
-- root is always (0, Just 1)
-- a Nothing parameter means it's part of the function's written form
type alias EntryState_ =
    {   function: Int
    ,   argument: Maybe Int
    ,   fixedArgs: Bool
    }
baseState: EntryState_
baseState = {function = 0, argument = Just 1, fixedArgs = True}

type alias Entry =
    -- All entries are implied as a `Latex.Scope baseState model`, but only the model is stored.
    {   latex: Latex.Model EntryState_
    ,   funcName: Dict.Dict Int String
    ,   nextFunc: Int
    }

type alias Model msg =
    {   entry: Entry
    ,   functionInput: Maybe String
    ,   cursor: Latex.CaretPosition
    ,   showCursor: Bool
    ,   holderID: String
    ,   mouseCmd: Encode.Value -> (Float, Float) -> Cmd msg
    ,   focusCmd: String -> Cmd msg
    }
type Event =
    Key (String, Bool, Bool)
    | ShowCursor
    | HideCursor
    | MouseDown Encode.Value (Float, Float)
    | Shift SvgDrag.Event
    | FunctionChoice String Bool (Latex.Model ()) -- The bool is for whether the args are fixed
    | SymbolChoice Latex.Symbol
    | HelperInput String
    | HelperClear

init: (Encode.Value -> (Float, Float) -> Cmd msg) -> (String -> Cmd msg) -> String -> Model msg
init mouseCmd focusCmd holderID =
    {   entry = {latex = [], funcName = Dict.empty, nextFunc = 1}
    ,   functionInput = Nothing
    ,   cursor = [0]
    ,   holderID = holderID
    ,   showCursor = False
    ,   mouseCmd = mouseCmd
    ,   focusCmd = focusCmd
    }

set: Entry -> Model msg -> Model msg
set entry model =
    {   model
    |   entry = entry
    ,   functionInput = Nothing
    ,   cursor = [List.length entry.latex]
    ,   showCursor = False
    }
clear: Model msg -> Model msg
clear model =
    {   model
    |   entry = {latex = [], funcName = Dict.empty, nextFunc = 1}
    ,   functionInput = Nothing
    ,   cursor = [0]
    ,   showCursor = False
    }

view: (Event -> msg) -> Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> List (Html.Attribute msg) -> Model msg -> Html.Html msg
view convert functions attr model =
    Html.div [id model.holderID, class "mathInput"]
    (   [   Html.input
            (   [   type_ "text"
                ,   id (model.holderID ++ "-input")
                ,   HtmlEvent.onKeyDown (Key >> convert)
                ,   HtmlEvent.onFocus (convert ShowCursor)
                ,   HtmlEvent.onBlur (convert HideCursor)
                ,   HtmlEvent.onPointerCapture convert MouseDown
                ]
            ++ attr
            )
            []
        ,   MathIcon.staticWithCursor [style "pointer-events" "none"]
            (if model.showCursor then model.cursor else []) model.entry.latex
        ]
    |> Helper.maybeAppend (model.functionInput
            -- Has to be maybe, so that only one input will be on screen at any point
            |> Maybe.map (\str -> displaySuggestions_ functions str
                |> \selectable -> Html.div
                    [class "popup"]
                    [   Html.input [type_ "text", id "inputHelper", HtmlEvent.onKeyChange HelperInput, HtmlEvent.onBlur HelperClear] []
                    ,   Html.Keyed.node "div" [] selectable
                    ]
                |> Html.map convert
            )
        )
    )

{- ## Updates -}

update: Event -> Model msg -> (Model msg, String, Cmd msg)
update event model = case event of
    Key e -> let entry = model.entry in
        case e |> Debug.log "key" of
        ("z", False, True) -> (model, "", Cmd.none) -- Undo
        ("Z", True, True) -> (model, "", Cmd.none) -- Redo
        ("Backspace", _, _) -> (model, "", Cmd.none)
        ("Delete", _, _) -> (model, "", Cmd.none)
        ("ArrowUp", False, _) -> ({model | cursor = model.cursor}, "", Cmd.none)
        ("ArrowDown", False, _) -> ({model | cursor = model.cursor}, "", Cmd.none)
        ("ArrowLeft", False, _) -> ({model | cursor = cursorNext_ False model.entry.latex model.cursor}, "", Cmd.none)
        ("ArrowRight", False, _) -> ({model | cursor = cursorNext_ True model.entry.latex model.cursor |> Debug.log "right"}, "", Cmd.none)
        (c, _, False) -> if String.length c == 1
            then case String.uncons c of
                Nothing -> (model, "", Cmd.none)
                Just (char, _) -> if Char.isAlphaNum char || char == ' ' || char == '.'
                    then case insertChar_ char model of
                        Err str -> (model, str, Cmd.none)
                        Ok (newModel) -> (newModel, "", Cmd.none)
                    else case char of
                        '(' -> (model, "", Cmd.none)
                        '+' -> (model, "", Cmd.none)
                        '*' -> (model, "", Cmd.none)
                        '/' -> (model, "", Cmd.none)
                        '=' -> (model, "", Cmd.none)
                        ',' -> (model, "", Cmd.none)
                        '\\' -> ({model | functionInput = Just ""}, "", model.focusCmd "inputHelper")
                        _ -> (model, "'" ++ c ++ "' is not an allowed input", Cmd.none)
            else (model, "", Cmd.none)
        _ -> (model, "", Cmd.none)
    ShowCursor -> ({model | showCursor = True}, "", Cmd.none)
    HideCursor -> ({model | showCursor = False}, "", Cmd.none)
    MouseDown val point -> (model, "", model.mouseCmd val point)
    Shift e -> case e of
        SvgDrag.End _ _ -> (model, "", Cmd.none)
        _ -> (model, "", Cmd.none) -- TODO: Start & Move for selecting text
    FunctionChoice name fixed latex -> case insertLatex_ (\_ -> rewriteLatex_ model.entry.nextFunc fixed latex) model of
        Err str -> (model, str, Cmd.none)
        Ok (newModel) -> let entry = newModel.entry in
            (   {   newModel
                |   functionInput = Nothing
                ,   entry =
                    {   entry
                    |   funcName = Dict.insert model.entry.nextFunc name entry.funcName
                    ,   nextFunc = model.entry.nextFunc + 1
                    }
                }
            , "", model.focusCmd (model.holderID ++ "-input")
            )
    SymbolChoice symb -> case insertLatex_ (\s -> (Latex.SymbolPart s symb, [])) model of
        Err str -> (model, str, Cmd.none)
        Ok (newModel) -> ({newModel | functionInput = Nothing}, "", model.focusCmd (model.holderID ++ "-input"))
    HelperInput str -> ({model | functionInput = Just str}, "", Cmd.none)
    HelperClear -> ({model | functionInput = Nothing}, "", Cmd.none)

{- ## Cursor Movement -}

type CaretShiftResult =
    ShiftNext (Latex.Model EntryState_) -- I want to get the next value (i.e. [1,0,5] to [1,1])
    | ShiftPrevious (Latex.Model EntryState_) -- I want to get the previous value (i.e. [1,1] to [1,0,5]) (i.e. [1,0] to [1])

cursorNext_: Bool -> Latex.Model EntryState_ -> List Int -> List Int
cursorNext_ forwards latex current = Latex.modifyCaret
    (\_ input -> case input of
        Latex.EOF -> Latex.Processing (if forwards then ShiftNext [] else ShiftPrevious [])
        Latex.TextCase (state, str) others num -> let newNum = if forwards then num + 1 else num - 1 in
            if newNum <= 0 then Latex.Complete (Latex.Text state str :: others, [0])
            else if newNum >= String.length str
            then Latex.Complete (Latex.Text state str :: others, [1])
            else Latex.Complete (Latex.Text state str :: others, [0,newNum])
        Latex.IntermediateCase next -> if not forwards then Latex.Processing (ShiftPrevious next)
            else case cursorFirstOf_ next of
                Just c -> Latex.Complete (next, c)
                Nothing -> Latex.Processing (ShiftNext next)
    )
    (\part scope res -> case res of
        ShiftNext remaining -> Latex.Processing (ShiftNext (part::remaining)) -- already checked in preprocess
        ShiftPrevious remaining -> case cursorLastOf_ part of
            Just c -> Latex.Complete (part::remaining, c)
            Nothing -> case scope.argument of
                Nothing -> Latex.Processing (ShiftPrevious (part::remaining))
                Just _ -> Latex.Complete (part::remaining, [0])
    )
    (\combine s next parent res -> let newl children = combine s children::next in
        case res of
            ShiftNext children -> case parent.argument of
                Just _ -> Latex.Complete (newl children, [1]) -- Allow to append random stuff
                Nothing -> case cursorFirstOf_ next of
                    Just (c::other) -> Latex.Complete (newl children, c+1::other)
                    _ -> Latex.Processing (ShiftNext (newl children))
            ShiftPrevious children -> case parent.argument of
                Just _ -> Latex.Complete (newl children, [0]) -- Allow to prepend random stuff
                Nothing -> Latex.Processing (ShiftPrevious (newl children))
    )
    baseState latex current
    |> \res -> case res of
        Ok (Latex.Complete (_,c)) -> c
        _ -> current

cursorFirstOf_: Latex.Model EntryState_ -> Maybe (List Int)
cursorFirstOf_ =
    let
        check isSameGap = List.foldl (\part (result, childNum, isSame) -> (   case result of
                Nothing -> case part of
                    Latex.Fraction _ top bot ->
                        case  check False top of
                            Just r -> Just ([childNum, 0] ++ r)
                            Nothing -> check False bot |> Maybe.map ((++) [childNum, 1])
                    Latex.SymbolPart s _ -> s.argument |> Maybe.map (\_ -> [childNum+1])
                    Latex.Text s t -> s.argument |> Maybe.map (\_ -> if String.length t <= 1 then [childNum+1] else [childNum, 1])
                    Latex.Superscript _ inner -> check False inner |> Maybe.map ((::) childNum)
                    Latex.Subscript _ inner -> check False inner |> Maybe.map ((::) childNum)
                    Latex.Bracket _ inner -> check False inner |> Maybe.map ((::) childNum)
                    Latex.Sqrt _ inner -> check False inner |> Maybe.map ((::) childNum)
                    Latex.Scope s inner -> case (s.argument, isSame) of
                        (Nothing, _) -> check isSame inner |> Maybe.map ((::) childNum)
                        (Just _, False) -> Just [childNum]
                        (Just _, True) -> case check True inner of
                            Nothing -> Just [childNum+1]
                            Just c -> case List.head c of
                                Nothing -> Just [childNum]
                                Just num -> if num == List.length inner then Just [childNum+1] else Just (childNum :: c)
                    _ -> Nothing
                _ -> result
            ,   childNum + 1
            ,   False
            )
            ) (Nothing, 0, isSameGap)
            >> \(a,_,_) -> a
    in
        check True

cursorLastOf_: Latex.Part EntryState_ -> Maybe (List Int)
cursorLastOf_ =
    let
        recursive isSameGap model = List.foldr (\p (res,postIndex,isSame) -> case res of
                Just _ -> (res, postIndex, isSame)
                Nothing -> case checkPart isSame p of
                    Just (head::c) -> (Just (postIndex+head::c), postIndex, isSame)
                    Just [] -> (Just [], postIndex, isSame)
                    _ -> (Nothing, postIndex - 1, False)
            ) (Nothing,List.length model-1,isSameGap) model
            |> \(a,_,_) -> a
        checkPart isSameGap part =
            case part of
                Latex.Fraction _ top bot -> case recursive False top of
                    Just c -> Just (0 :: 0 :: c)
                    Nothing -> recursive False bot |> Maybe.map (\c -> 0 :: 1 :: c)
                Latex.SymbolPart _ _ -> Nothing
                Latex.Text s text -> s.argument |> Maybe.andThen (\_ -> let strLen = String.length text in
                    if strLen == 0 then Nothing
                    else if strLen == 1 then Just [0]
                    else Just [0,String.length text - 1])
                Latex.Superscript _ inner -> recursive False inner |> Maybe.map ((::) 0)
                Latex.Subscript _ inner -> recursive False inner |> Maybe.map ((::) 0)
                Latex.Bracket _ inner -> recursive False inner |> Maybe.map ((::) 0)
                Latex.Sqrt _ inner -> recursive False inner |> Maybe.map ((::) 0)
                Latex.Scope s inner -> case (isSameGap, s.argument) of
                    (False, Just _) -> Just [1]
                    _ -> recursive isSameGap inner |> Maybe.map ((::) 0)
                _ -> Nothing
    in
        checkPart True

{- ## Insertion -}

type InsertionResult =
    AppendOrCreate (Latex.Model EntryState_)
    | AppendError

insertChar_: Char -> Model msg -> Result String (Model msg)
insertChar_ char model = Latex.modifyCaret
    (\_ input -> case input of
        Latex.EOF -> Latex.Processing (AppendOrCreate [])
        Latex.TextCase (state, str) others num ->
            let newStr = (String.left num str) ++String.fromChar char ++ (String.dropLeft num str) in
            Latex.Complete (Latex.Text state newStr :: others, [0, num+1])
        Latex.IntermediateCase next -> case next of
            (Latex.Text state str :: others) -> case state.argument of
                Just _ -> Latex.Complete (Latex.Text state (String.cons char str)::others,[0,1])
                Nothing -> Latex.Processing (AppendOrCreate (Latex.Text state str :: others))
            (Latex.Scope state inner :: others) -> case state.argument of
                Just _ -> Latex.Complete (appendChar_ state inner next char , [1])
                Nothing -> Latex.Processing (AppendOrCreate next)
            _ -> Latex.Processing (AppendOrCreate next)
    )
    (\part parentState res -> case res of
        AppendOrCreate others -> case part of
            Latex.Text s str -> case s.argument of
                Nothing -> Latex.Processing AppendError |> Debug.log "test3"
                Just _ -> Latex.Complete (Latex.Text s (str ++ String.fromChar char) :: others, [1])
            Latex.Scope s inner -> case s.argument of
                Nothing -> case parentState.argument of
                    Nothing -> Latex.Processing AppendError
                    Just _ -> Latex.Complete
                        ([part, Latex.Text parentState (String.fromChar char)] ++ others, [2])
                Just _ -> Latex.Complete (appendChar_ s inner others char, [1])
            _ -> case parentState.argument of
                Nothing -> Latex.Processing AppendError |> Debug.log "test"
                Just _ -> Latex.Complete ([part, Latex.Text parentState (String.fromChar char)] ++ others, [2])
        AppendError -> Latex.Processing AppendError
    )
    (\combine s next _ res -> case res of
        AppendOrCreate others -> case s.argument of
            Nothing -> Latex.Processing AppendError |> Debug.log "test4"
            Just _ -> let newEntry = Latex.Text s (String.fromChar char) in
                Latex.Complete (combine s (newEntry::others)::next, [0,1])
        AppendError -> Latex.Processing AppendError
    )
    baseState model.entry.latex model.cursor
    |> \res -> let entry = model.entry in case res of
        Ok (Latex.Complete (l,c)) -> Ok {model | entry = {entry | latex = l}, cursor = c}
        Ok (Latex.Processing (AppendOrCreate _)) -> let newEntry = Latex.Text baseState (String.fromChar char) in
            Ok {model | entry = {entry | latex = newEntry::model.entry.latex}, cursor = [0,1]}
        Ok (Latex.Processing AppendError) -> Err "Shouldn't be allowed to insert text here"
        _ -> Err "Something went wrong"

appendChar_: EntryState_ -> Latex.Model EntryState_ -> Latex.Model EntryState_ -> Char -> Latex.Model EntryState_
appendChar_ s inner others char = let finalIndex = List.length inner - 1 in
    (   case List.drop finalIndex inner of
            [Latex.Text tState str] -> List.take finalIndex inner
                ++ [Latex.Text tState (str ++ String.fromChar char)]
            _ -> inner ++ [Latex.Text s (String.fromChar char)]
    )
    |> \newInner -> Latex.Scope s newInner :: others

updateCaretPosition_: Int -> Latex.CaretPosition -> Latex.CaretPosition
updateCaretPosition_ by list = case list of
    [] -> [by + 1]
    c -> by :: c

insertLatex_: (EntryState_ -> (Latex.Part EntryState_, Latex.CaretPosition)) -> Model msg -> Result String (Model msg)
insertLatex_ creator model = Latex.modifyCaret
    (\parentState input -> let (symPart, newCaret) = creator parentState in
        case input of
        Latex.EOF -> case parentState.argument of
            Nothing -> Latex.Processing (AppendOrCreate [])
            Just _ -> Latex.Complete ([symPart], updateCaretPosition_ 0 newCaret)
        Latex.TextCase (state, str) others num -> case parentState.argument of
            Nothing -> Latex.Processing AppendError
            Just _ -> if num == 0 then Latex.Complete (symPart :: Latex.Text state str :: others, updateCaretPosition_ 0 newCaret)
                else Latex.Complete
                    (   Latex.Text state (String.left num str):: symPart::Latex.Text state (String.dropLeft num str)::others
                    ,   updateCaretPosition_ 1 newCaret
                    )
        Latex.IntermediateCase next -> case next of
            (Latex.Scope state inner :: others) -> case state.argument of
                Nothing -> Latex.Processing (AppendOrCreate next)
                Just _ -> Latex.Complete (Latex.Scope state (symPart::inner) :: others, 0 :: updateCaretPosition_ 0 newCaret)
            _ -> case parentState.argument of
                Nothing -> Latex.Processing (AppendOrCreate next)
                Just _ -> Latex.Complete (symPart :: next, updateCaretPosition_ 0 newCaret)
    )
    (\part parent res -> case res of
        AppendOrCreate others -> case parent.argument of
            Just _ -> let (symPart, newCaret) = creator parent in
                Latex.Complete (part :: symPart :: others, updateCaretPosition_ 1 newCaret)
            Nothing -> case part of
                Latex.Scope s inner -> case s.argument of
                    Nothing -> Latex.Processing AppendError
                    Just _ -> let (symPart, newCaret) = creator s in
                        Latex.Complete (Latex.Scope s (inner ++ [symPart])::others, 0 :: updateCaretPosition_ (List.length inner) newCaret)
                _ -> Latex.Processing AppendError
        AppendError -> Latex.Processing AppendError
    )
    (\combine s next _ res -> case res of
        AppendOrCreate others -> case s.argument of
            Nothing -> Latex.Processing AppendError
            Just _ -> let (newEntry, newCaret) = creator s in
                Latex.Complete (combine s (newEntry::others)::next, 0 :: updateCaretPosition_ 0 newCaret)
        AppendError -> Latex.Processing AppendError
    )
    baseState model.entry.latex model.cursor
    |> \res -> let entry = model.entry in case res of
        Ok (Latex.Complete (l,c)) -> Ok {model | entry = {entry | latex = l |> Debug.log "latex"}, cursor = c}
        Ok (Latex.Processing (AppendOrCreate _)) -> let (newEntry, newCaret) = creator baseState in
            Ok {model | entry = {entry | latex = newEntry::model.entry.latex}, cursor = 0 :: updateCaretPosition_ 0 newCaret}
        _ -> Err "Something went wrong"

rewriteLatex_: Int -> Bool -> Latex.Model () -> (Latex.Part EntryState_, Latex.CaretPosition)
rewriteLatex_ scopeNum fixed latex = let newState = {function = scopeNum, argument = Nothing, fixedArgs = fixed} in
    rewriteLatexRecursive_ newState latex
    |> \(model, caret) -> (Latex.Scope newState model, caret)

rewriteLatexRecursive_: EntryState_ -> Latex.Model () -> (Latex.Model EntryState_, Latex.CaretPosition)
rewriteLatexRecursive_ newState latex = List.foldr (\part (newModel, caret, index) -> case part of
        Latex.Fraction _ top bot ->
            let
                (newTop, topCar) = rewriteLatexRecursive_ newState top
                (newBot, botCar) = rewriteLatexRecursive_ newState bot
            in
                (   Latex.Fraction newState newTop newBot :: newModel
                ,   if List.isEmpty topCar
                    then if List.isEmpty botCar then caret
                        else (index::1::botCar)
                    else (index::0::topCar)
                ,   index - 1
                )
        Latex.Text _ str -> (Latex.Text newState str::newModel, caret, index - 1)
        Latex.SymbolPart _ symb -> (Latex.SymbolPart newState symb::newModel, caret, index - 1)
        Latex.Superscript _ inner -> let (newL, car) = rewriteLatexRecursive_ newState inner in
            (Latex.Superscript newState newL :: newModel, if List.isEmpty car then caret else index :: car, index - 1)
        Latex.Subscript _ inner -> let (newL, car) = rewriteLatexRecursive_ newState inner in
            (Latex.Subscript newState newL :: newModel, if List.isEmpty car then caret else index :: car, index - 1)
        Latex.Bracket _ inner -> let (newL, car) = rewriteLatexRecursive_ newState inner in
            (Latex.Bracket newState newL :: newModel, if List.isEmpty car then caret else index :: car, index - 1)
        Latex.Sqrt _ inner -> let (newL, car) = rewriteLatexRecursive_ newState inner in
            (Latex.Sqrt newState newL :: newModel, if List.isEmpty car then caret else index :: car, index - 1)
        Latex.Argument _ num ->
            (   Latex.Scope {newState | argument = (Just num)} [] :: newModel
            ,   if num == 1 then [index+1] else caret
            ,   index - 1
            )
        Latex.Param _ num ->
            (   Latex.Scope {newState | argument = (Just num)} [] :: newModel
            ,   if num == 1 then [index+1] else caret
            ,   index-1
            )
        Latex.Scope _ inner -> rewriteLatexRecursive_ newState inner
            |> \(a,b) -> (a,b,index - 1)
    ) ([], [], List.length latex - 1 ) latex
    |> \(a,b, _) -> (a, b)

{- ## Extraction -}

toTree: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Model msg -> Result String Display.FullEquation
toTree dict model = toEntryString_ model.entry.funcName model.entry.latex
    |> Result.andThen (Matcher.parseEquation dict Animation.stateOps)

-- Argument scope
toEntryString_: Dict.Dict Int String -> Latex.Model EntryState_ ->  Result String String
toEntryString_ funcName model = Helper.resultList (\symbol prev -> case symbol of
        Latex.Text elem text -> Ok (prev++text)
        Latex.SymbolPart elem symb -> Ok (prev++"\\" ++ Latex.symbolToStr symb)
        Latex.Bracket _ inner -> toEntryString_ funcName inner
            |> Result.map (\str -> prev++ "(" ++ str ++ ")")
        Latex.Scope elem inner -> case Dict.get elem.function funcName of
            Nothing -> Err "Function not found"
            Just name -> toEntryMap_ funcName Dict.empty inner
                |> Result.andThen (\dict -> Helper.resultDict (\key value (str, index) ->
                    if key /= index then Err "Missing an argument"
                    else if value == "" then Err "An input is empty"
                    else Ok
                        (   if String.isEmpty str then value else str++","++value
                        ,   index + 1
                        )
                    ) ("", 1) dict
                    |> Result.map (\(res,_) -> prev ++ "\\" ++ name ++ "(" ++ res ++ ")")
                )
        _ -> Err "Unable to stringify the latex parts"
    ) "" model

-- Non-argument scope
toEntryMap_: Dict.Dict Int String -> Dict.Dict Int String -> Latex.Model EntryState_ ->  Result String (Dict.Dict Int String)
toEntryMap_ funcName resDict model = Helper.resultList (\part res -> case part of
        Latex.Fraction _ top bot -> toEntryMap_ funcName res top
            |> Result.andThen (\newRes -> toEntryMap_ funcName newRes bot)
        Latex.Superscript _ inner -> toEntryMap_ funcName res inner
        Latex.Subscript _ inner -> toEntryMap_ funcName res inner
        Latex.Sqrt _ inner -> toEntryMap_ funcName res inner
        Latex.Bracket _ inner -> toEntryMap_ funcName res inner
        Latex.Scope elem inner -> case elem.argument of
            Nothing -> toEntryMap_ funcName res inner
            Just arg -> toEntryString_ funcName inner
                |> Result.andThen (\str -> if String.isEmpty str then Err "Argument is blank"
                    else Ok (Dict.insert arg str res)
                )
        _ -> Ok res
    ) resDict model

{- ## Suggestion -}

displaySuggestions_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String -> List (String, Html.Html Event)
displaySuggestions_ functions input =
    let
        inputOrder = letterOrder_ input
        createEntry key event latex = Just
            (   cosineCorrelation_ inputOrder (letterOrder_ key)
            ,   (   key
                ,   Html.a [class "clickable", HtmlEvent.onPointerCapture identity (\_ _ -> event)] -- don't use onClick, because some onBlurs get triggered first
                    [MathIcon.static [] latex]
                )
            )
        createLatex name args =
            [   Latex.Text () name
            ,   Latex.Bracket ()
                (List.range 1 args |> List.map (Latex.Argument ()) |> List.intersperse (Latex.Text () ","))
            ]
        greekSymbols = Dict.toList Latex.greekLetters
            |> List.filterMap (\(name, s) -> createEntry name (SymbolChoice s) [Latex.SymbolPart () s])
        functionSymbols = Dict.toList functions
            |> List.filterMap (\(key, value) ->
                case value.property of
                    Math.VariableNode n -> let latex = [Latex.Text () key] in createEntry key (FunctionChoice key True latex) latex
                    Math.UnaryNode n -> case n.state.latex of
                        Just l -> createEntry key (FunctionChoice key True l) l
                        Nothing -> let l = createLatex key 1 in createEntry key (FunctionChoice key True l) l
                    Math.BinaryNode n -> case n.state.latex of
                        Just l -> createEntry key (FunctionChoice key (not n.associative) l) l
                        Nothing -> if n.associative
                            then let l = createLatex key 1 in createEntry key (FunctionChoice key False l) l
                            else let l = createLatex key 2 in createEntry key (FunctionChoice key True l) l
                    Math.GenericNode n -> case n.state.latex of
                        Just l -> createEntry key (FunctionChoice key True l) l
                        Nothing -> let l = createLatex key (Maybe.withDefault 0 n.arguments) in createEntry key (FunctionChoice key True l) l
                    _ -> Nothing
            )
    in
        List.sortBy (\(corr, _) -> -corr) (functionSymbols ++ greekSymbols)
        |> List.map Tuple.second

-- 36 of them, a-z + 0-9
letterOrder_: String -> Array.Array Float
letterOrder_ = String.toLower
    >> String.foldl (\char (arr, sum, rank) ->
        (   if char >= 'a' && char <= 'z'
            then (Char.toCode char) - 97 |> Just
            else if char >= '0' && char <= '9'
            then (Char.toCode char) - 22 |> Just
            else Nothing
        )
        |> \index -> case (index, Maybe.andThen (\i -> Array.get i arr) index) of
            (Just i, Just r) -> if r == 0 then (Array.set i rank arr, sum + rank, rank - 1)
                else (arr, sum, rank - 1)
            _ -> (arr, sum, rank - 1)
    ) (Array.repeat 36 0, 0, 36)
    >> \(arr, sum, _) -> let shift = sum / 6 in -- for removing the [1,1,1,...,1] vector, equivalent to shifting
        Array.map (\elem -> elem - shift) arr

-- Assumes the length of the 2 arrays are 36
cosineCorrelation_: Array.Array Float -> Array.Array Float -> Float
cosineCorrelation_ left right = List.range 0 35
    |> List.foldl (\index (cross, l, r) -> case (Array.get index left, Array.get index right) of
        (Just a, Just b) -> (cross + a*b, l + a*a, r + b*b)
        _ -> (cross, l, r)
    ) (0, 0, 0)
    |> \(cross, l, r) -> if l == 0 || r == 0 then -1
        else cross / sqrt (l * r)