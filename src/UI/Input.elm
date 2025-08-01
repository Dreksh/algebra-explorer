module UI.Input exposing (Model, Event(..), Entry, init, update, clear, set, view, toTree)

import Array
import Dict
import Html
import Html.Attributes exposing (class, id, name, style, type_)
import Html.Keyed
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
type alias Scope_ =
    {   function: Int
    ,   argument: Maybe Int
    ,   fixedArgs: Bool
    }
type alias EntryState_ = {immutable: Bool, scope: Scope_}
baseState_: EntryState_
baseState_ = {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}}

type alias Entry =
    -- All Latex.Model has the first entry as either the model's scope, or a constant
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
    ,   mouseCmd: (Float, Float) -> Cmd msg
    ,   focusCmd: String -> Cmd msg
    }
type Event =
    Key (String, Bool, Bool)
    | ShowCursor
    | HideCursor
    | MouseDown (Float, Float)
    | Shift SvgDrag.Event
    | FunctionChoice String Bool (Latex.Model ()) -- The bool is for whether the args are fixed
    | SymbolChoice Latex.Symbol
    | HelperInput String
    | HelperClear

init: ((Float, Float) -> Cmd msg) -> (String -> Cmd msg) -> String -> Model msg
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
            (   [   type_ "textarea"
                ,   id (model.holderID ++ "-input")
                ,   HtmlEvent.onKeyDown (Key >> convert)
                ,   HtmlEvent.onFocus (convert ShowCursor)
                ,   HtmlEvent.onBlur (convert HideCursor)
                ,   HtmlEvent.onMouseDown (MouseDown >> convert)
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
        case e of
        ("z", False, True) -> (model, "", Cmd.none) -- Undo
        ("Z", True, True) -> (model, "", Cmd.none) -- Redo
        ("Backspace", _, _) -> (model, "", Cmd.none)
        ("Delete", _, _) -> (model, "", Cmd.none)
        ("ArrowUp", _, _) -> ({model | cursor = model.cursor}, "", Cmd.none)
        ("ArrowDown", _, _) -> ({model | cursor = model.cursor}, "", Cmd.none)
        ("ArrowLeft", _, _) -> ({model | cursor = cursorNext_ False model.entry.latex model.cursor |> Debug.log "left"}, "", Cmd.none)
        ("ArrowRight", _, _) -> ({model | cursor = cursorNext_ True model.entry.latex model.cursor |> Debug.log "right"}, "", Cmd.none)
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
    MouseDown point -> (model, "", model.mouseCmd point)
    Shift e -> case e of
        SvgDrag.End _ _ -> (model, "", Cmd.none)
        _ -> (model, "", Cmd.none) -- TODO: Start & Move for selecting text
    FunctionChoice name fixed latex -> case insertLatex_ name (rewriteLatex_ model.entry.nextFunc fixed latex) model of
        Err str -> (model, str, Cmd.none)
        Ok (newModel) -> ({newModel | functionInput = Nothing}, "", model.focusCmd (model.holderID ++ "-input"))
    SymbolChoice symb -> case insertSymbol_ symb model of
        Err str -> (model, str, Cmd.none)
        Ok (newModel) -> ({newModel | functionInput = Nothing}, "", model.focusCmd (model.holderID ++ "-input"))
    HelperInput str -> ({model | functionInput = Just str}, "", Cmd.none)
    HelperClear -> ({model | functionInput = Nothing}, "", Cmd.none)

{- ## Cursor Movement -}

type CaretShiftResult =
    GetNext (Latex.Model EntryState_) -- I want to get the next value (i.e. [1,0,5] to [1,1])
    | GetPrevious (Latex.Model EntryState_) -- I want to get the previous value (i.e. [1,1] to [1,0,5], or [1,0] to [1])

cursorNext_: Bool -> Latex.Model EntryState_ -> List Int -> List Int
cursorNext_ forwards latex current = Latex.modifyCaret
    (\_ input -> case input of
        Latex.EOF -> Latex.Processing (if forwards then GetNext [] else GetPrevious [])
        Latex.TextCase (state, str) others num -> let newNum = if forwards then num + 1 else num - 1 in
            if newNum < 0 then Latex.Complete (Latex.Text state str :: others, [0])
            else if newNum >= String.length str
            then Latex.Complete (Latex.Text state str :: others, [1])  |> Debug.log "test"
            else Latex.Complete (Latex.Text state str :: others, [0,newNum]) |> Debug.log "blah"
        Latex.SymbolCase (state, symb) others -> if forwards
            then Latex.Complete (Latex.SymbolPart state symb :: others, [1])
            else Latex.Processing (GetPrevious (Latex.SymbolPart state symb :: others))
        Latex.IntermediateCase next -> if not forwards then Latex.Processing (GetPrevious next)
            else Latex.Complete (next, cursorFirstOf_ next)
        Latex.FractionCase (topState, top) (botState, bot) others -> let fulllist = Latex.Fraction topState top botState bot::others in
            if not forwards
            then Latex.Complete (fulllist, [0])
            else case (topState.immutable, botState.immutable) of
                (False, _) -> Latex.Complete (fulllist, [0,0] ++ cursorFirstOf_ top) |> Debug.log "blah"
                (True, False) -> Latex.Complete (fulllist, [0,1] ++ cursorFirstOf_ bot)
                (True, True) -> Latex.Complete (fulllist, cursorFirstOf_ fulllist)
    )
    (\part _ res -> case res of
        GetNext remaining -> Latex.Processing (GetNext (part::remaining))
        GetPrevious remaining -> case cursorLastOf_ part of
            Just c -> Latex.Complete (part::remaining, c)
            Nothing -> Latex.Processing (GetPrevious (part::remaining))
    )
    (\combine s next res -> case res of
        GetNext children -> Latex.Complete (combine s children::next, [1])
        GetPrevious children -> Latex.Complete (combine s children::next, [0])
    )
    baseState_ latex current
    |> \res -> case res of
        Ok (Latex.Complete (_,c)) -> c
        Ok (Latex.Processing (GetPrevious _)) -> [0]
        _ -> [List.length latex]

cursorFirstOf_: Latex.Model EntryState_ -> List Int
cursorFirstOf_ = List.foldl (\part (result, childNum) ->
        (   case result of
                Nothing -> case part of
                    Latex.Fraction topState top botState bot -> case (topState.immutable, botState.immutable) of -- Prefer top over bottom
                        (False, _) -> Just ([childNum,0] ++ cursorFirstOf_ top)
                        (True, False) -> Just ([childNum,1] ++ cursorFirstOf_ bot)
                        (True, True) -> Nothing
                    Latex.SymbolPart s _ -> if s.immutable then Nothing else Just [childNum+1]
                    Latex.Text s t -> if s.immutable then Nothing else
                        if String.isEmpty t then Just [childNum+1] else Just [childNum, 0]
                    Latex.Superscript s inner -> if s.immutable then Nothing else Just [childNum, 0]
                    Latex.Subscript s inner -> if s.immutable then Nothing else Just [childNum, 0]
                    Latex.Bracket s inner -> if s.immutable then Nothing else Just [childNum, 0]
                    Latex.Sqrt s inner -> if s.immutable then Nothing else Just [childNum, 0]
                    _ -> Nothing
                _ -> result
        ,   childNum + 1
        )
    ) (Nothing, 0)
    >> \(res, count) -> case res of
        Nothing -> [count]
        Just c -> c

cursorLastOf_: Latex.Part EntryState_ -> Maybe (List Int)
cursorLastOf_ part = case part of
    Latex.Fraction topState top botState bot -> case (topState.immutable, botState.immutable) of
        (False, _) -> Just [0,0,List.length top]
        (True, False) -> Just [0,1,List.length bot]
        (True, True) -> Nothing
    Latex.SymbolPart s _ -> if s.immutable then Nothing else Just [0]
    Latex.Text s text -> if s.immutable then Nothing else let newIndex = String.length text - 1 in
        if newIndex < 0 then Nothing else Just [0,newIndex]
    Latex.Superscript s inner -> if s.immutable then Nothing else Just [0,List.length inner]
    Latex.Subscript s inner -> if s.immutable then Nothing else Just [0,List.length inner]
    Latex.Bracket s inner -> if s.immutable then Nothing else Just [0,List.length inner]
    Latex.Sqrt s inner -> if s.immutable then Nothing else Just [0,List.length inner]
    _ -> Nothing

{- ## Insertion -}

type InsertionResult =
    AppendOrCreate (Latex.Model EntryState_)

insertChar_: Char -> Model msg -> Result String (Model msg)
insertChar_ char model = Latex.modifyCaret
    (\_ input -> case input of
        Latex.EOF -> Latex.Processing (AppendOrCreate [])
        Latex.TextCase (state, str) others num ->
            let newStr = (String.left num str) ++String.fromChar char ++ (String.dropLeft num str) in
            Latex.Complete (Latex.Text state newStr :: others, [0, num+1])
        Latex.SymbolCase (state, symb) others -> Latex.Processing (AppendOrCreate others)
        Latex.IntermediateCase next -> case next of
            (Latex.Text state str :: others) ->
                Latex.Complete (Latex.Text state (String.cons char str)::others,[0,1])
            _ -> Latex.Processing (AppendOrCreate next)
        Latex.FractionCase (topState, top) (botState, bot) others ->
            Latex.Processing (AppendOrCreate (Latex.Fraction topState top botState bot::others))
    )
    (\part parentState res -> case res of
        AppendOrCreate others -> case part of
            Latex.Text s str -> if s.immutable
                then Latex.Complete ([part, Latex.Text parentState (String.fromChar char)] ++ others, [2])
                else Latex.Complete (Latex.Text s (str ++ String.fromChar char) :: others, [1])
            _ -> Latex.Complete ([part, Latex.Text parentState (String.fromChar char)] ++ others, [2])
    )
    (\combine s next res -> case res of
        AppendOrCreate others -> let newEntry = Latex.Text s (String.fromChar char) in
            Latex.Complete (combine s (newEntry::others)::next, [0,1])
    )
    baseState_ model.entry.latex model.cursor
    |> \res -> let entry = model.entry in case res of
        Ok (Latex.Complete (l,c)) -> Ok {model | entry = {entry | latex = l}, cursor = c}
        Ok (Latex.Processing (AppendOrCreate _)) -> let newEntry = Latex.Text baseState_ (String.fromChar char) in
            Ok {model | entry = {entry | latex = newEntry::model.entry.latex}, cursor = [0,1]}
        _ -> Err "Something went wrong"

insertSymbol_: Latex.Symbol -> Model msg -> Result String (Model msg)
insertSymbol_ symb model = Latex.modifyCaretSimple
    (\parentState input -> let symPart = Latex.SymbolPart parentState symb in
        case input of
        Latex.EOF -> ([symPart],[1])
        Latex.TextCase (state, str) others num ->
            if num == 0 then (symPart :: Latex.Text state str :: others, [1])
            else (Latex.Text state (String.left num str):: symPart::Latex.Text state (String.dropLeft num str)::others,[2])
        Latex.SymbolCase (state, symb2) others -> (symPart::Latex.SymbolPart state symb2::others, [1])
        Latex.IntermediateCase next -> (symPart :: next, [1])
        Latex.FractionCase (topState, top) (botState, bot) others ->
            (symPart:: Latex.Fraction topState top botState bot :: others, [1])
    ) baseState_ model.entry.latex model.cursor
    |> \res -> let entry = model.entry in case res of
        Ok (Latex.Complete (l,c)) -> Ok {model | entry = {entry | latex = l}, cursor = c}
        _ -> Err "Something went wrong"

insertLatex_: String -> (Latex.Model EntryState_, Latex.CaretPosition) -> Model msg -> Result String (Model msg)
insertLatex_ name (latex, caret) model = Latex.modifyCaretSimple
    (\_ input -> case input of
        Latex.EOF -> (latex, caret)
        Latex.TextCase (state, str) others num ->
            if num == 0 then (latex ++ (Latex.Text state str :: others), caret)
            else (  (Latex.Text state (String.left num str):: latex) ++ (Latex.Text state (String.dropLeft num str)::others)
                ,   case caret of
                    (x::next) -> (x+1::next)
                    [] -> []
                )
        Latex.SymbolCase (state, symb) others -> (latex++(Latex.SymbolPart state symb::others), caret)
        Latex.IntermediateCase next -> (latex ++ next, caret)
        Latex.FractionCase (topState, top) (botState, bot) others ->
            (latex ++ (Latex.Fraction topState top botState bot :: others), caret)
    ) baseState_ model.entry.latex model.cursor
    |> \res -> let entry = model.entry in case res of
        Ok (Latex.Complete (l,c)) ->
            Ok  {   model
                |   entry =
                    {   entry
                    |   latex = l
                    ,   funcName = Dict.insert entry.nextFunc name entry.funcName
                    ,   nextFunc = entry.nextFunc + 1
                    }
                ,   cursor = c
                }
        _ -> Err "Something went wrong"

rewriteLatex_: Int -> Bool -> Latex.Model () -> (Latex.Model EntryState_, Latex.CaretPosition)
rewriteLatex_ scopeNum fixed =
    rewriteLatexRecursive_ {function = scopeNum, argument = Nothing, fixedArgs = fixed}
    >> \(a,b,_) -> (a,b)

rewriteLatexRecursive_: Scope_ -> Latex.Model () -> (Latex.Model EntryState_, Latex.CaretPosition, Bool)
rewriteLatexRecursive_ scope =
    let
        createState arg immutable = case arg of
            Just _ -> {immutable = False, scope = {scope | argument = arg}}
            Nothing -> {immutable = immutable, scope = scope}
    in
    List.foldl (\part (newModel, caret, (index, immutable)) -> case part of
        Latex.Fraction _ top _ bot ->
            let
                (newTop, topCar, topImm) = rewriteLatexRecursive_ scope top
                (newBot, botCar, botImm) = rewriteLatexRecursive_ scope bot
            in
                (   Latex.Fraction (createState Nothing topImm) newTop (createState Nothing topImm) newBot :: newModel
                ,   if List.isEmpty topCar
                    then if List.isEmpty botCar then caret
                        else (index::1::botCar)
                    else (index::0::topCar)
                ,   (index + 1, topImm && botImm)
                )
        Latex.Text _ str -> (Latex.Text (createState Nothing True) str::newModel, caret, (index+1, True))
        Latex.SymbolPart _ symb -> (Latex.SymbolPart (createState Nothing True) symb::newModel, caret, (index+1, True))
        Latex.Superscript _ inner -> let (newL, car, imm) = rewriteLatexRecursive_ scope inner in
            (Latex.Superscript (createState Nothing imm) newL :: newModel, if List.isEmpty car then caret else index :: car, (index + 1, imm))
        Latex.Subscript _ inner -> let (newL, car, imm) = rewriteLatexRecursive_ scope inner in
            (Latex.Subscript (createState Nothing imm) newL :: newModel, if List.isEmpty car then caret else index :: car, (index + 1, imm))
        Latex.Bracket _ inner -> let (newL, car, imm) = rewriteLatexRecursive_ scope inner in
            (Latex.Bracket (createState Nothing imm) newL :: newModel, if List.isEmpty car then caret else index :: car, (index + 1, imm))
        Latex.Sqrt _ inner -> let (newL, car, imm) = rewriteLatexRecursive_ scope inner in
            (Latex.Sqrt (createState Nothing imm) newL :: newModel, if List.isEmpty car then caret else index :: car, (index + 1, imm))
        Latex.Argument _ num ->
            (   Latex.Text (createState (Just num) False) "" :: newModel
            ,   if num == 1 then [index+1] else caret
            ,   (index + 1, False)
            )
        Latex.Param _ num ->
            (   Latex.Text (createState (Just num) False) "" :: newModel
            ,   if num == 1 then [index+1] else caret
            ,   (index + 1, False)
            )
    ) ([], [], (0,True))
    >> \(a,b,(_,c)) -> (List.reverse a, b, c)

{- ## Extraction -}

toTree: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Model msg -> Result String Display.FullEquation
toTree dict model = toStringDict_ model.entry.funcName 0 model.entry.latex
    |> Result.andThen (Dict.get 1 >> Result.fromMaybe "Unable to convert to a string")
    |> Result.andThen (Matcher.parseEquation dict Animation.stateOps)

type alias ProcessState_ =
    {   currentParam: Int
    ,   functionEntry: Dict.Dict Int String
    ,   subscope: Maybe (Int, Dict.Dict Int String)
    }

-- Append scopes of the same function number to the assigned argument
-- if scopes do not share the same function number, assume they are smaller functions in the scope,
-- so they will be appended onto whatever the previous argument is (as a function)
-- with the exception when the currentParam is 0, where they will be prepended onto the next argument, when seen
toStringDict_: Dict.Dict Int String -> Int -> Latex.Model {immutable: Bool, scope: Scope_} ->  Result String (Dict.Dict Int String)
toStringDict_ funcName function model =
    let
        appendString key str dict = case Dict.get key dict of
            Nothing -> Dict.insert key str dict
            Just prev -> Dict.insert key (prev++str) dict

        flush state = case state.subscope of
            Nothing -> Ok state
            Just (funcNum, argDict) -> case Dict.get funcNum funcName of
                Nothing -> Err ("Cannot find the function name for the " ++ String.fromInt funcNum ++ " function")
                Just name -> Helper.resultDict (\key str (prev, total) ->
                        if prev + 1 /= key
                        then Err ("function '" ++ name ++ "' is missing argument " ++ String.fromInt key)
                        else if prev == 0 then Ok (key, str) else Ok (key, total ++ "," ++ str)
                    ) (0, "") argDict
                    |> Result.map (\(_, innerStr) ->
                        {   state
                        |   subscope = Nothing
                        ,   functionEntry = appendString state.currentParam ("\\" ++ name ++ "(" ++ innerStr ++ ")") state.functionEntry
                        }
                    )
        addText state elem text = case elem.scope.argument of
            -- Part of the function's layout, not considered actual text
            Nothing -> Ok state
            Just arg -> if elem.scope.function == function
                then flush state
                    |> Result.map (\newState -> case Dict.get 0 newState.functionEntry of
                        Nothing -> { newState | currentParam = arg , functionEntry = appendString arg text newState.functionEntry }
                        Just t -> -- Case if other scope had text before this own scope
                            {   newState
                            |   currentParam = arg
                            ,   functionEntry = appendString arg (t ++ text) newState.functionEntry
                                |> Dict.remove 0
                            }
                    )
                else case state.subscope of
                    Nothing -> Ok {state | subscope = Just (elem.scope.function, Dict.singleton arg text)}
                    Just (subFunc, subDict) -> if subFunc == elem.scope.function
                        then Ok {state | subscope = Just (subFunc,appendString arg text subDict)}
                        else flush state
                            |> Result.map (\newState -> {newState | subscope = Just (elem.scope.function, Dict.singleton arg text)})
        addDict state funcNum dict = case state.subscope of
            Nothing -> Ok {state | subscope = Just (funcNum, dict)}
            Just (subFunc, subDict) -> if subFunc == funcNum
                then Ok {state | subscope = Just (subFunc, Dict.union dict subDict)}
                else flush state
                    |> Result.map (\newState -> {newState | subscope = Just (funcNum, dict)})
    in
    Helper.resultList (\symbol state -> case symbol of
        Latex.Fraction topElem top botElem bot -> toStringDict_ funcName topElem.scope.function top
            |> Result.andThen (\extractedTop -> toStringDict_ funcName botElem.scope.function bot
                |> Result.andThen (\extractedBot -> addDict state topElem.scope.function (Dict.union extractedTop extractedBot))
            )
        Latex.Superscript elem inner -> toStringDict_ funcName elem.scope.function inner
            |> Result.andThen (addDict state elem.scope.function)
        Latex.Subscript elem inner -> toStringDict_ funcName elem.scope.function inner
            |> Result.andThen (addDict state elem.scope.function)
        Latex.Sqrt elem inner -> toStringDict_ funcName elem.scope.function inner
            |> Result.andThen (addDict state elem.scope.function)
        Latex.Bracket elem inner -> case elem.scope.argument of
            -- Just a bracket for indicating a function
            Nothing -> toStringDict_ funcName elem.scope.function inner
                |> Result.andThen (addDict state elem.scope.function)
            -- Part of the text
            Just arg -> toStringDict_ funcName elem.scope.function inner
                |> Result.andThen (\dict -> case Dict.get arg dict of
                    Nothing -> Err "text bracket contained random stuff"
                    Just text -> addText state elem ("(" ++ text ++")")
                )
        Latex.Text elem text -> addText state elem text
        Latex.SymbolPart elem symb -> addText state elem ("\\" ++ Latex.symbolToStr symb)
        Latex.Argument _ _ -> Err "Found latex argument in input"
        Latex.Param _ _ -> Err "Found latex parameter in input"
    ) (ProcessState_ 0 Dict.empty Nothing) model
    |> Result.map .functionEntry

{- ## Suggestion -}

displaySuggestions_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String -> List (String, Html.Html Event)
displaySuggestions_ functions input =
    let
        inputOrder = letterOrder_ input
        createEntry key event latex = Just
            (   cosineCorrelation_ inputOrder (letterOrder_ key)
            ,   (   key
                ,   Html.a [class "clickable", HtmlEvent.onMouseDown (\_ -> event)] -- don't use onClick, because some onBlurs get triggered first
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