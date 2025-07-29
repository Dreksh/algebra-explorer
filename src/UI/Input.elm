module UI.Input exposing (Model, Event, Entry, init, update, set, view, toTree)

import Dict
import Html
import Html.Attributes exposing (class, name, style, type_)
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
type Movement_ =
    Up
    | Down
    | Left
    | Right

type alias Model =
    {   entry: Entry
    ,   functionInput: Maybe String
    ,   cursor: CursorPosition
    }
type Event = Key (String, Bool, Bool)

init: Model
init =
    {   entry = {latex = [], funcName = Dict.empty}
    ,   functionInput = Nothing
    ,   cursor = []
    }

set: Entry -> Model
set entry =
    {   entry = entry
    ,   functionInput = Nothing
    ,   cursor = [List.length entry.latex - 1]
    }

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html.Html msg
view convert attr model = Html.div [class "mathInput"]
    [   Html.input
        (   [   type_ "textarea"
            ,   HtmlEvent.onKeyDown (Key >> convert)
            ]
        ++ attr
        )
        []
    ,   MathIcon.staticWithCursor [style "pointer-events" "none"] model.cursor model.entry.latex
    ]

{- ## Updates -}

update: Event -> Model -> Model
update event model = case event of
    Key e -> let entry = model.entry in
        let _ = Debug.log "keyPressed" e in case e of
        (c, _, False) -> if String.length c == 1
            then case String.uncons c of
                Nothing -> model
                Just (char, _) -> if (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
                    then {model | entry = {entry | latex = appendString_ c entry.latex} }
                    else if String.contains c "0123456789 ."
                    then {model | entry = {entry | latex = appendString_ c entry.latex} }
                    else model
            else model
        ("z", False, True) -> model -- Undo
        ("Z", True, True) -> model -- Redo
        ("Backspace", _, _) -> model
        ("Delete", _, _) -> model
        ("ArrowRight", _, _) -> moveCursor_ Right model
        ("ArrowDown", _, _) -> moveCursor_ Down model
        ("ArrowLeft", _, _) -> moveCursor_ Left model
        ("ArrowUp", _, _) -> moveCursor_ Up model
        _ -> model

moveCursor_: Movement_ -> Model -> Model
moveCursor_ movement model = model

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

{- ## For Interpreting the field -}

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
        Latex.Fraction elem top bot -> toStringDict_ funcName elem.scope.function top
            |> Result.andThen (\extractedTop -> toStringDict_ funcName elem.scope.function bot
                |> Result.andThen (\extractedBot -> addDict state elem.scope.function (Dict.union extractedTop extractedBot))
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
