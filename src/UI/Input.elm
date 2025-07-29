module UI.Input exposing (Model, Event(..), Entry, init, update, clear, set, view, toTree)

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

type alias Entry =
    -- All Latex.Model has the first entry as either the model's scope, or a constant
    {   latex: Latex.Model {immutable: Bool, scope: Scope_}
    ,   funcName: Dict.Dict Int String
    }
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

init: ((Float, Float) -> Cmd Event) -> Model
init mouseCmd =
    {   entry = {latex = [Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} ""], funcName = Dict.empty}
    ,   functionInput = Nothing
    ,   cursor = [0, 0]
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
    |   entry = {latex = [Latex.Text {immutable = False, scope = {function = 0, argument = Just 1, fixedArgs = True}} ""], funcName = Dict.empty}
    ,   functionInput = Nothing
    ,   cursor = [0, 0]
    ,   showCursor = False
    }

view: (Event -> msg) -> String -> List (Html.Attribute msg) -> Model -> Html.Html msg
view convert holderID attr model =
    Html.div [id holderID, class "mathInput"]
    [   Html.input
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
        ("ArrowLeft", _, _) -> case cursorNext_ False model.entry.latex model.cursor of
            Exact newCursor -> ({model | cursor = newCursor}, Cmd.none)
            None -> ({model | cursor = [List.length model.entry.latex]}, Cmd.none) -- reset to end
            _ -> (model, Cmd.none)
        ("ArrowRight", _, _) -> case cursorNext_ True model.entry.latex model.cursor of
            Exact newCursor -> ({model | cursor = newCursor}, Cmd.none)
            GetNext -> ({model | cursor = [List.length model.entry.latex]}, Cmd.none)
            None -> ({model | cursor = [List.length model.entry.latex]}, Cmd.none) -- reset to end
            _ -> (model, Cmd.none)
        (c, _, False) -> if String.length c == 1
            then case String.uncons c of
                Nothing -> (model, Cmd.none)
                Just (char, _) -> if (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
                    then ({model | entry = {entry | latex = appendString_ c entry.latex} }, Cmd.none)
                    else if String.contains c "0123456789 ."
                    then ({model | entry = {entry | latex = appendString_ c entry.latex} }, Cmd.none)
                    else (model, Cmd.none)
            else (model, Cmd.none)
        _ -> (model, Cmd.none)
    ShowCursor -> ({model | showCursor = True}, Cmd.none)
    HideCursor -> ({model | showCursor = False}, Cmd.none)
    MouseDown point -> (model, model.mouseCmd point)
    Shift e -> case e of
        SvgDrag.End _ _ -> (model, Cmd.none)
        _ -> (model, Cmd.none) -- Start & Move for selecting text

{- ## Cursor Movement -}

type TreeIterationDecision =
    GetNext
    | GetPrevious
    | Exact (List Int)
    | None

-- Always takes the 'high road', i.e. top of fractions, superscripts (unless top is immutable, then bot will do)
-- for access to the others, use the down & up keys
cursorNext_: Bool -> Latex.Model {immutable: Bool, scope: Scope_} -> List Int -> TreeIterationDecision
cursorNext_ forwards latex current =
    let
        incrementHead list = case list of
            [] -> []
            (x::rest) -> x+1::rest
        processPrevious headPart rest updateExact result = case result of
            GetPrevious -> case headPart of
                Nothing -> GetPrevious
                Just head -> case cursorLastOf_ head of
                    Exact list -> Exact (0::list)
                    other -> other
            GetNext -> case cursorFirstOf_ rest of
                Exact list -> incrementHead list |> Exact -- We start checking from the next, so it'll start from 1
                other -> other
            Exact list -> updateExact list |> Exact
            _ -> result
    in
    case (latex, current) of
        (_, []) -> None
        ([], _) -> if forwards then None else GetPrevious
        ((first::others),(x::children)) -> if x > 0
            then cursorNext_ forwards others (x-1::children)
                |> processPrevious (Just first) [] incrementHead -- pass empty 'next', since we didn't move a level, so they've already been checked
            else (  case first of
                Latex.Fraction _ top _ bot -> case List.head children of
                    Just 0 -> cursorNext_ forwards top (List.drop 1 children)
                    Just 1 -> cursorNext_ forwards bot (List.drop 1 children)
                    _ -> None
                Latex.Text _ text -> case List.head children of
                    Nothing -> None
                    Just num -> let newNum = if forwards then num + 1 else num - 1 in
                        if newNum < 0 then GetPrevious
                        else if newNum > String.length text then GetNext
                        else Exact [newNum]
                Latex.SymbolPart _ _ -> if forwards then GetNext else GetPrevious
                Latex.Superscript _ inner -> cursorNext_ forwards inner children
                Latex.Subscript _ inner -> cursorNext_ forwards inner children
                Latex.Bracket _ inner -> cursorNext_ forwards inner children
                Latex.Sqrt _ inner -> cursorNext_ forwards inner children
                _ -> None -- No params or arguments allowed
            ) |> processPrevious Nothing others ((::) 0) -- don't recheck first

cursorFirstOf_: Latex.Model {immutable: Bool, scope: Scope_} -> TreeIterationDecision
cursorFirstOf_ = List.foldl (\part result -> case result of
    GetNext -> case part of
        Latex.Fraction topState top botState bot -> case (topState.immutable, botState.immutable) of -- Prefer top over bottom
            (False, _) -> case cursorFirstOf_ top of
                Exact list -> Exact (0::list)
                other -> other
            (True, False) -> case cursorFirstOf_ bot of
                Exact list -> Exact (1::list)
                other -> other
            (True, True) -> GetNext
        Latex.Text s _ -> if s.immutable then GetNext else Exact [0]
        Latex.SymbolPart s _ -> if s.immutable then GetNext else Exact []
        Latex.Superscript s inner -> if s.immutable then GetNext else cursorFirstOf_ inner
        Latex.Subscript s inner -> if s.immutable then GetNext else cursorFirstOf_ inner
        Latex.Bracket s inner -> if s.immutable then GetNext else cursorFirstOf_ inner
        Latex.Sqrt s inner -> if s.immutable then GetNext else cursorFirstOf_ inner
        _ -> GetNext
    _ -> result
    ) GetNext

cursorLastOf_: Latex.Part {immutable: Bool, scope: Scope_} -> TreeIterationDecision
cursorLastOf_ part = case part of
    Latex.Fraction topState top botState bot -> case (topState.immutable, botState.immutable) of
        (False, _) -> Exact [0,List.length top]
        (True, False) -> Exact [1,List.length bot]
        (True, True) -> GetPrevious
    Latex.Text s text -> if s.immutable then GetPrevious else Exact [String.length text - 1]
    Latex.SymbolPart s _ -> if s.immutable then GetPrevious else Exact []
    Latex.Superscript s inner -> if s.immutable then GetPrevious else Exact [List.length inner]
    Latex.Subscript s inner -> if s.immutable then GetPrevious else Exact [List.length inner]
    Latex.Bracket s inner -> if s.immutable then GetPrevious else Exact [List.length inner]
    Latex.Sqrt s inner -> if s.immutable then GetPrevious else Exact [List.length inner]
    _ -> GetPrevious

{- ## Insertion -}

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
