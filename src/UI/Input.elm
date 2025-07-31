module UI.Input exposing (Model, Event(..), Entry, init, update, clear, set, view, toTree)

import Array
import Dict
import Html
import Html.Attributes exposing (class, id, name, style, type_)
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
type alias Entry =
    -- All Latex.Model has the first entry as either the model's scope, or a constant
    {   latex: Latex.Model EntryState_
    ,   funcName: Dict.Dict Int String
    ,   nextFunc: Int
    }
-- The leaf-node determines the position before the nth child
-- The intermediate-nodes (ones before the leaf) is the nth child of that layer
-- leaf-node cannot address the position after the last child (i.e. for list with l elements, the position cannot be l)
type alias CursorPosition = List Int

type alias Model =
    {   entry: Entry
    ,   functionInput: Maybe String
    ,   cursor: CursorPosition
    ,   showCursor: Bool
    ,   mouseCmd: (Float, Float) -> Cmd Event
    }
type Event =
    Key (String, Bool, Bool)
    | ShowCursor
    | HideCursor
    | MouseDown (Float, Float)
    | Shift SvgDrag.Event
    | FunctionChoice String (Latex.Model ())
    | SymbolChoice Latex.Symbol
    | HelperInput String
    | HelperClear

init: ((Float, Float) -> Cmd Event) -> Model
init mouseCmd =
    {   entry = {latex = [], funcName = Dict.empty, nextFunc = 1}
    ,   functionInput = Nothing
    ,   cursor = [0]
    ,   showCursor = False
    ,   mouseCmd = mouseCmd
    }

set: Entry -> Model -> Model
set entry model =
    {   model
    |   entry = entry
    ,   functionInput = Nothing
    ,   cursor = [List.length entry.latex]
    ,   showCursor = False
    }
clear: Model -> Model
clear model =
    {   model
    |   entry = {latex = [], funcName = Dict.empty, nextFunc = 1}
    ,   functionInput = Nothing
    ,   cursor = [0]
    ,   showCursor = False
    }

view: (Event -> msg) -> Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String -> List (Html.Attribute msg) -> Model -> Html.Html msg
view convert functions holderID attr model =
    Html.div [id holderID, class "mathInput"]
    (   [   Html.input
            (   [   type_ "textarea"
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
            |> Maybe.map (\str -> displaySuggestions_ functions str
                |> \selectable -> Html.div
                    [class "popup"]
                    [   Html.input [type_ "text", HtmlEvent.onKeyChange HelperInput, HtmlEvent.onBlur HelperClear] []
                    ,   Html.div [] selectable
                    ]
                |> Html.map convert
            )
        )
    )

{- ## Updates -}

update: Event -> Model -> (Model, Cmd Event)
update event model = case event of
    Key e -> let entry = model.entry in
        case e of
        ("z", False, True) -> (model, Cmd.none) -- Undo
        ("Z", True, True) -> (model, Cmd.none) -- Redo
        ("Backspace", _, _) -> (model, Cmd.none)
        ("Delete", _, _) -> (model, Cmd.none)
        ("ArrowUp", _, _) -> ({model | cursor = model.cursor}, Cmd.none)
        ("ArrowDown", _, _) -> ({model | cursor = model.cursor}, Cmd.none)
        ("ArrowLeft", _, _) -> ({model | cursor = cursorNext_ False model.entry.latex model.cursor}, Cmd.none)
        ("ArrowRight", _, _) -> ({model | cursor = cursorNext_ True model.entry.latex model.cursor}, Cmd.none)
        (c, _, False) -> if String.length c == 1
            then case String.uncons c of
                Nothing -> (model, Cmd.none)
                Just (char, _) -> if (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
                    then ({model | entry = {entry | latex = appendString_ c entry.latex} }, Cmd.none)
                    else if String.contains c "0123456789 ."
                    then ({model | entry = {entry | latex = appendString_ c entry.latex} }, Cmd.none)
                    else case char of
                        '(' -> (model, Cmd.none)
                        '+' -> (model, Cmd.none)
                        '*' -> (model, Cmd.none)
                        '/' -> (model, Cmd.none)
                        '=' -> (model, Cmd.none)
                        ',' -> (model, Cmd.none)
                        '\\' -> ({model | functionInput = Just ""}, Cmd.none)
                        _ -> (model, Cmd.none)
            else (model, Cmd.none)
        _ -> (model, Cmd.none)
    ShowCursor -> ({model | showCursor = True}, Cmd.none)
    HideCursor -> ({model | showCursor = False}, Cmd.none)
    MouseDown point -> (model, model.mouseCmd point)
    Shift e -> case e of
        SvgDrag.End _ _ -> (model, Cmd.none)
        _ -> (model, Cmd.none) -- TODO: Start & Move for selecting text
    FunctionChoice name latex -> ({model | functionInput = Nothing}, Cmd.none)
    SymbolChoice symb -> ({model | functionInput = Nothing}, Cmd.none)
    HelperInput str -> ({model | functionInput = Just str}, Cmd.none)
    HelperClear -> ({model | functionInput = Nothing}, Cmd.none)

{- ## Cursor Movement -}

type TreeIterationDecision =
    GetNext -- I want to get the next value (i.e. [1,0,5] to [1,1])
    | GetPrevious -- I want to get the previous value (i.e. [1,1] to [1,0,5])
    | RemoveChild -- I want to get the previous value (i.e. [1,0] to [1])
    | NewCursor (List Int) -- Result
    | None -- Error

-- Always takes the 'high road', i.e. top of fractions, superscripts (unless top is immutable, then bot will do)
-- for access to the others, use the down & up keys
cursorNext_: Bool -> Latex.Model EntryState_ -> List Int -> List Int
cursorNext_ forwards latex current = modifySelection_ (\res -> case res of
        Text text num -> let newNum = if forwards then num + 1 else num - 1 in
                if newNum < 0 then Exact RemoveChild
                else if newNum >= String.length text
                then Exact GetNext
                else Exact (NewCursor [newNum])
        IntermediateLevel children -> if not forwards then Exact GetPrevious
            else case cursorFirstOf_ children of
                NewCursor list -> Exact (NewCursor (0::list))
                r -> Exact r
        EOF -> Exact (if forwards then GetNext else GetPrevious)
        SameLevelTraversal head innerRes -> case innerRes of
            Exact GetPrevious -> case cursorLastOf_ head of
                NewCursor list -> Exact (NewCursor (0::list))
                other -> Exact other
            Exact (NewCursor (index::body)) -> Exact (NewCursor (index+1::body))
            _ -> innerRes
        NextLevelTraversal others innerRes -> case innerRes of
            Exact GetNext -> Exact (NewCursor [1])
            Exact GetPrevious -> Exact (NewCursor [0]) -- Behaves like RemoveChild if no more before this point
            Exact RemoveChild -> Exact (NewCursor [0])
            Exact (NewCursor list) -> Exact (NewCursor (0::list))
            _ -> innerRes
        InsertLevel level innerRes -> case innerRes of
            Exact (NewCursor list) -> Exact (NewCursor (level::list))
            _ -> innerRes
        _ -> res
    )
    (latex, current)
    |> \res -> case res of
        Exact (NewCursor c) -> c
        Exact GetPrevious -> [0]
        Exact RemoveChild -> [0]
        _ -> [List.length latex]

cursorFirstOf_: Latex.Model EntryState_ -> TreeIterationDecision
cursorFirstOf_ = List.foldl (\part result -> case result of
    GetNext -> case part of
        Latex.Fraction topState top botState bot -> case (topState.immutable, botState.immutable) of -- Prefer top over bottom
            (False, _) -> case cursorFirstOf_ top of
                NewCursor list -> NewCursor (0::list)
                other -> other
            (True, False) -> case cursorFirstOf_ bot of
                NewCursor list -> NewCursor (1::list)
                other -> other
            (True, True) -> GetNext
        Latex.SymbolPart s _ -> if s.immutable then GetNext else NewCursor []
        Latex.Text s _ -> if s.immutable then GetNext else NewCursor [0]
        Latex.Superscript s inner -> if s.immutable then GetNext else NewCursor [0]
        Latex.Subscript s inner -> if s.immutable then GetNext else NewCursor [0]
        Latex.Bracket s inner -> if s.immutable then GetNext else NewCursor [0]
        Latex.Sqrt s inner -> if s.immutable then GetNext else NewCursor [0]
        _ -> GetNext
    _ -> result
    ) GetNext

cursorLastOf_: Latex.Part EntryState_ -> TreeIterationDecision
cursorLastOf_ part = case part of
    Latex.Fraction topState top botState bot -> case (topState.immutable, botState.immutable) of
        (False, _) -> NewCursor [0,List.length top]
        (True, False) -> NewCursor [1,List.length bot]
        (True, True) -> GetPrevious
    Latex.SymbolPart s _ -> if s.immutable then GetPrevious else NewCursor [] -- No internal positions
    Latex.Text s text -> if s.immutable then GetPrevious else let newIndex = String.length text - 1 in
        if newIndex < 0 then GetPrevious else NewCursor [newIndex]
    Latex.Superscript s inner -> if s.immutable then GetPrevious else NewCursor [List.length inner]
    Latex.Subscript s inner -> if s.immutable then GetPrevious else NewCursor [List.length inner]
    Latex.Bracket s inner -> if s.immutable then GetPrevious else NewCursor [List.length inner]
    Latex.Sqrt s inner -> if s.immutable then GetPrevious else NewCursor [List.length inner]
    _ -> GetPrevious

{- ## Insertion -}

type IterationResult res =
    EOF -- End of row
    | BrokenCursor
    | BrokenLatex
    | SameLevelTraversal (Latex.Part EntryState_) (IterationResult res)
    | NextLevelTraversal (Latex.Model EntryState_) (IterationResult res)
    | InsertLevel Int (IterationResult res)
    | IntermediateLevel (Latex.Model EntryState_)
    | Exact res
    | Text String Int

exactMap_: (res -> res) -> IterationResult res -> IterationResult res
exactMap_ convert original = case original of
    Exact r -> Exact (convert r)
    _ -> original

modifySelection_: (IterationResult res -> IterationResult res) -> (Latex.Model EntryState_, CursorPosition) -> IterationResult res
modifySelection_ process initial =
    let
        scopeProcessor others inner children = if List.isEmpty children
            then IntermediateLevel inner |> process
            else modifySelection_ process (inner, children) |> NextLevelTraversal others |> process
    in
    (
    case initial of
        ([], _) -> EOF |> process
        (others, []) -> IntermediateLevel others |> process
        ((first::others),(x::children)) -> if x > 0
            then modifySelection_ process (others,(x-1::children)) |> SameLevelTraversal first |> process
            else (  case first of
                Latex.Fraction _ top _ bot -> case List.head children of
                    Just 0 -> modifySelection_ process (top, List.drop 1 children) |> InsertLevel 0 |> NextLevelTraversal others |> process
                    Just 1 -> modifySelection_ process (bot, List.drop 1 children) |> InsertLevel 1 |> NextLevelTraversal others |> process
                    Nothing -> IntermediateLevel top |> process |> InsertLevel 0 |> NextLevelTraversal others |> process
                    _ -> BrokenCursor
                Latex.Text _ text -> case List.head children of
                    Nothing -> IntermediateLevel [first] |> process
                    Just index -> Text text index |> process |> NextLevelTraversal others |> process
                Latex.SymbolPart _ _ -> IntermediateLevel [] |> process |> NextLevelTraversal others |> process
                Latex.Superscript _ inner -> scopeProcessor others inner children
                Latex.Subscript _ inner -> scopeProcessor others inner children
                Latex.Bracket _ inner -> scopeProcessor others inner children
                Latex.Sqrt _ inner -> scopeProcessor others inner children
                _ -> BrokenLatex
            )
    )

insertChar_: Char -> Model -> Model
insertChar_ char model = model


appendString_: String -> Latex.Model {immutable: Bool, scope: Scope_} -> Latex.Model {immutable: Bool, scope: Scope_}
appendString_ str latex =
    let
        lastIndex = List.length latex - 1
        (front,last) = (List.take lastIndex latex, List.drop lastIndex latex)
    in
        case last of
            [] -> front ++ [Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} str]
            (x::_) -> case x of
                Latex.Text e prev -> front ++ [Latex.Text e (prev ++ str)]
                _ -> latex ++ [Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} str]

{- ## Extraction -}

toTree: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Model -> Result String Display.FullEquation
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

displaySuggestions_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String -> List (Html.Html Event)
displaySuggestions_ functions input =
    let
        inputOrder = letterOrder_ input
        greekSymbols = Dict.toList Latex.greekLetters
            |> List.map (\(name, s) ->
                (   cosineCorrelation_ inputOrder (letterOrder_ name)
                ,   Html.a [class "clickable", HtmlEvent.onClick (SymbolChoice s)]
                    [MathIcon.static [] [Latex.SymbolPart () s]]
                )
            )
        functionSymbols = Dict.toList functions
            |> List.filterMap (\(key, value) -> value.property |> Math.getState |> .latex
                |> Maybe.map (\func ->
                    (   cosineCorrelation_ inputOrder (letterOrder_ key)
                    ,   Html.a [class "clickable", HtmlEvent.onClick (FunctionChoice key func)]
                        [MathIcon.static [] func]
                    )
                )
            )
    in
        List.sortBy (\(corr, _) -> -corr) (greekSymbols ++ functionSymbols)
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