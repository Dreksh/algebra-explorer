module UI.Input exposing (Model, Event(..), Scope(..), ScopeElement(..), Entry, init, update, clear, set, view, toLatex, toTree)

import Array
import Dict
import Html
import Html.Attributes exposing (class, id, name, style, type_)
import Html.Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Set
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

-- Scope is always editable and capable of inserting & deleting. But the scope itself cannot be deleted.
type Scope = Scope {fixed: Bool} (List ScopeElement)
type ScopeElement =
    StrElement String -- Must not be empty string, they should be handled by removing them
    | Fixed (Maybe Int) (Latex.Model ()) (Array.Array Scope) -- numFields & Only allow empty Latex.Text parts contain scope

type alias Entry =
    {   scope: Scope
    ,   funcName: Dict.Dict Int String
    ,   nextFunc: Int
    }

-- CaretPosition has 2 different meanings for leaf node vs intermediate node
-- intermediate node represents the zero-indexed nth ScopeElement in Scope, or the nth Scope in Fixed
-- lead node represents the position before the index in Scope & in StrElement.
-- However, StrElement does not have the 0th position or the length-th position, as they will be the ones in Scope
type alias CaretPosition = List Int


type alias Model msg =
    {   entry: Entry
    ,   functionInput: Maybe String
    ,   cursor: CaretPosition
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
    | LatexChoice String Bool (Latex.Model ())
    | HelperInput String
    | HelperClear

init: (Encode.Value -> (Float, Float) -> Cmd msg) -> (String -> Cmd msg) -> String -> Model msg
init mouseCmd focusCmd holderID =
    {   entry = {scope = Scope {fixed = True} [], funcName = Dict.empty, nextFunc = 1}
    ,   functionInput = Nothing
    ,   cursor = [0]
    ,   holderID = holderID
    ,   showCursor = False
    ,   mouseCmd = mouseCmd
    ,   focusCmd = focusCmd
    }

set: Entry -> Model msg -> Model msg
set entry model = let (Scope _ children) = entry.scope in
    {   model
    |   entry = entry
    ,   functionInput = Nothing
    ,   cursor = [List.length children]
    ,   showCursor = False
    }
clear: Model msg -> Model msg
clear model =
    {   model
    |   entry = {scope = Scope {fixed = True} [], funcName = Dict.empty, nextFunc = 1}
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
            (toLatex False (if model.showCursor then model.cursor else []) (model.entry.scope |> Debug.log "scope") |> Debug.log "latex")
        ]
    |> Helper.maybeAppend (model.functionInput
            -- Has to be maybe, so that only one input will be on screen at any point
            |> Maybe.map (\str -> displaySuggestions_ functions str
                |> \(selectable, eventCreator) -> Html.div
                    [class "popup"]
                    [   Html.form [HtmlEvent.onSubmitForm (formDecoder_ eventCreator)]
                        [   Html.input
                            [type_ "text", id "inputHelper", name "input", HtmlEvent.onKeyChange HelperInput, HtmlEvent.onBlur HelperClear]
                            []
                        ]
                    ,   Html.Keyed.node "div" [] selectable
                    ]
                |> Html.map convert
            )
        )
    )

formDecoder_: (String -> (String, Bool, Latex.Model ())) -> Decode.Decoder Event
formDecoder_ convert = Decode.field "input" (Decode.field "value" Decode.string)
    |> Decode.map (convert >> \(name, fixed, latex) -> LatexChoice name fixed latex)

toLatex: Bool -> CaretPosition -> Scope -> Latex.Model ()
toLatex border pos (Scope _ children) =
    let
        noCaret scopeElem = case scopeElem of
            StrElement text -> [Latex.Text () text]
            Fixed _ latex params -> Latex.replace (Array.map (toLatex True []) params) latex
    in
    List.foldl
    (\elem (model, index) ->
        (   model
            ++ case pos of
                [] -> noCaret elem
                [x] -> if x /= index then noCaret elem
                    else Latex.Caret () :: noCaret elem
                (x::y::other) -> case elem of
                    StrElement str -> if x /= index then noCaret elem
                        else
                            [   Latex.Text () (String.left y str)
                            ,   Latex.Caret ()
                            ,   Latex.Text () (String.dropLeft y str)
                            ]
                    Fixed _ latex params -> if x /= index then noCaret elem
                        else Latex.replace
                            (   Array.indexedMap
                                (\i -> toLatex True (if i /= y then [] else other))
                                params
                            )
                            latex
        ,   index + 1
        )
    ) ([], 0) children
    |> \(model, _) -> case (border, List.head pos |> Maybe.map ((==) (List.length children))) of
        (False, Just True) -> model ++ [Latex.Caret ()]
        (False, _) -> model
        (True, Just True) -> [Latex.Border () (model ++ [Latex.Caret ()])]
        (True, _) -> [Latex.Border () model]

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
        ("ArrowLeft", False, _) -> ({model | cursor = cursorNext_ False entry.scope model.cursor |> Debug.log "left"}, "", Cmd.none)
        ("ArrowRight", False, _) -> ({model | cursor = cursorNext_ True entry.scope model.cursor |> Debug.log "right"}, "", Cmd.none)
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
    LatexChoice name fixed latex ->
        latexArray_ fixed latex
        |> Result.andThen (\params ->
            insertLatex_ (if String.isEmpty name then Nothing else Just model.entry.nextFunc ,latex, params) model
        )
        |> \res -> case res of
            Err str -> (model, str, Cmd.none)
            Ok (newModel) -> let entry = newModel.entry in
                (   {   newModel
                    |   functionInput = Nothing
                    ,   entry = if String.isEmpty name then entry
                            else
                                {   entry
                                |   funcName = Dict.insert entry.nextFunc name entry.funcName
                                ,   nextFunc = entry.nextFunc + 1
                                }
                    }
                , "", model.focusCmd (model.holderID ++ "-input")
                )
    HelperInput str -> ({model | functionInput = Just str}, "", Cmd.none)
    HelperClear -> ({model | functionInput = Nothing}, "", Cmd.none)

{- ## Scope traversal -}

type TraversalCase_ =
    TextCase (List ScopeElement) String Int (List ScopeElement)
    | IntermediateCase (List ScopeElement) (List ScopeElement)
type TraversalResult_ =
    Done (List ScopeElement, CaretPosition)
    -- Next and Previous are specifically for moving the caret, inside Fixed ScopeElements
    | NextScope
    | PreviousScope
type TraversalError_  =
    BrokenCaret -- For when the position cannot be found

traverse: (TraversalCase_ -> TraversalResult_) -> Scope -> CaretPosition -> Result TraversalError_ TraversalResult_
traverse process (Scope _ children) caret = case caret of
    [] -> Err BrokenCaret
    [x] -> IntermediateCase (List.take x children) (List.drop x children) |> process |> Ok
    (x::y::others) -> let (prev, next) = (List.take x children, List.drop x children) in
        case next of
            (StrElement str :: after) -> TextCase prev str y after |> process |> Ok
            (Fixed funcNum latex params :: after) -> case Array.get y params of
                Nothing -> Err BrokenCaret
                Just (Scope detail inChildren) -> traverse process (Scope detail inChildren) others
                    |> Result.andThen (\res -> case res of
                            Done (newChildren, pos) ->
                                let newParams = Array.set y (Scope detail newChildren) params in
                                Ok (Done (prev ++ (Fixed funcNum latex newParams :: after), x::y::pos))
                            NextScope -> if y + 1 == Array.length params then Ok (Done (children, [x + 1]))
                                else Ok (Done (children, [x, y + 1, 0]))
                            PreviousScope -> if y == 0 then Ok (Done (children, [x]))
                                else case Array.get (y - 1) params of
                                    Nothing -> Err BrokenCaret
                                    Just (Scope _ prevChildren) -> Ok (Done (children, [x, y - 1, List.length prevChildren]))
                    )
            _ -> Err BrokenCaret

{- ## Cursor Movement -}

cursorNext_: Bool -> Scope -> List Int -> List Int
cursorNext_ forwards scope current = traverse
    (\input -> case input of
        TextCase prev str num next -> Done
            (   prev ++ (StrElement str :: next)
            ,   let newNum = if forwards then num + 1 else num - 1 in
                if newNum <= 0 then [List.length prev]
                else if newNum >= String.length str then [List.length prev + 1]
                else [List.length prev,newNum]
            )
        IntermediateCase prev next ->
            if forwards then case next of
                (head::after) -> Done
                    (   prev ++ next
                    ,   case head of
                        StrElement str -> if String.length str <= 1
                            then [List.length prev + 1]
                            else [List.length prev, 1]
                        Fixed _ _ params -> if Array.isEmpty params
                            then [List.length prev + 1]
                            else [List.length prev, 0, 0]
                    )
                _ -> NextScope
            else let beforeLength = List.length prev - 1 in
                case List.drop beforeLength prev of
                    [head] -> Done
                        (   prev ++ next
                        ,   case head of
                            StrElement str -> if String.length str <= 1
                                then [List.length prev - 1]
                                else [List.length prev - 1, String.length str - 1]
                            Fixed _ _ params -> if Array.isEmpty params
                                then [List.length prev - 1]
                                else let finalIndex = Array.length params - 1 in
                                    case Array.get finalIndex params of
                                        Nothing -> [List.length prev - 1, finalIndex, 0]
                                        Just (Scope _ p) ->
                                            [List.length prev - 1, finalIndex, List.length p]
                        )
                    _ -> PreviousScope
    )
    scope current
    |> \res -> case res of
        Ok (Done (_,c)) -> c
        _ -> current -- Either we have reached the ends, or something broke

{- ## Insertion -}

insertChar_: Char -> Model msg -> Result String (Model msg)
insertChar_ char model = traverse
    (\input -> case input of
        TextCase prev str num next ->
            let newStr = (String.left num str) ++String.fromChar char ++ (String.dropLeft num str) in
            Done (prev ++ (StrElement newStr :: next), [List.length prev, num+1])
        IntermediateCase prev next -> case next of
            (StrElement str::after) ->
                Done (prev ++ (StrElement (String.cons char str) :: after), [List.length prev, 1])
            _ -> let beforeLength = List.length prev - 1 in
                case List.drop beforeLength prev of
                    [StrElement str] -> Done
                        (   List.take beforeLength prev ++ (StrElement (str ++ String.fromChar char) :: next)
                        ,   [List.length prev]
                        )
                    _ -> Done
                        (   prev ++ (StrElement (String.fromChar char) :: next)
                        ,   [List.length prev + 1]
                        )
    )
    model.entry.scope model.cursor
    |> \res -> let entry = model.entry in case res of
        Ok (Done (l,c)) -> Ok {model | entry = {entry | scope = Scope {fixed = True} l}, cursor = c}
        _ -> Err "Something went wrong"

insertLatex_: (Maybe Int, Latex.Model (), Array.Array Scope) -> Model msg -> Result String (Model msg)
insertLatex_ (funcNum, latex, arr) model = traverse
    (\input -> case input of
        TextCase prev str num next -> Done
            (   prev
                ++  ( StrElement (String.left num str) :: Fixed funcNum latex arr
                    :: StrElement (String.dropLeft num str) :: next
                    )
            ,   if Array.isEmpty arr
                then [List.length prev + 2]
                else [List.length prev + 1, 0, 0]
            )
        IntermediateCase prev next -> Done
            (   prev ++ (Fixed funcNum latex arr :: next) |> Debug.log "insert"
            ,   if Array.isEmpty arr
                then [List.length prev + 1]
                else [List.length prev, 0, 0]
            )
    )
    model.entry.scope model.cursor
    |> \res -> let entry = model.entry in case res of
        Ok (Done (l,c)) -> Ok {model | entry = {entry | scope = Scope {fixed = True} l}, cursor = c}
        _ -> Err "Something went wrong"

latexArray_: Bool -> Latex.Model () -> Result String (Array.Array Scope)
latexArray_ fixed latex = Latex.extractArgs (Set.empty, 0) latex
    |> Result.map (\(_, count) -> if fixed
        then Array.repeat count (Scope {fixed = True} [])
        else Array.fromList [Scope {fixed = False} []]
    )

{- ## Extraction -}

toTree: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Model msg -> Result String Display.FullEquation
toTree dict model = toEntryString_ model.entry.funcName model.entry.scope
    |> Result.andThen (Matcher.parseEquation dict Animation.stateOps)

toEntryString_: Dict.Dict Int String -> Scope -> Result String String
toEntryString_ funcName (Scope _ children) = Helper.resultList (\child res -> case child of
        StrElement str -> Ok (res ++ str)
        Fixed funcNum latex params -> case funcNum of
            Nothing -> Ok (res ++ Latex.unparse latex) -- mainly symbols
            Just num -> case Dict.get num funcName of
                Nothing -> Err "Missing function name"
                Just name -> Helper.resultArray (\index innerScope innerRes ->
                        toEntryString_ funcName innerScope
                        |> Result.map (\str -> Array.set index str innerRes)
                    ) (Array.repeat (Array.length params) "") params
                    |> Result.map (\arr -> "\\" ++ name ++ "(" ++ (Array.toList arr |> String.join ",") ++ ")" )
    ) "" children

{- ## Suggestion -}

defaultOps: Set.Set String
defaultOps = Set.fromList ["+", "-", "*", "/", "="]

displaySuggestions_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String -> (List (String, Html.Html Event), String -> (String, Bool, Latex.Model ()))
displaySuggestions_ functions input =
    let
        inputOrder = letterOrder_ input
        createLatex name args =
            [   Latex.Text () name
            ,   Latex.Bracket ()
                (List.range 1 args |> List.map (Latex.Argument ()) |> List.intersperse (Latex.Text () ","))
            ]
        funcPropToLatex key value = case value.property of
            Math.VariableNode n -> case n.state.latex of
                Just l -> ("", True, l)
                Nothing -> let l = createLatex key 1 in (key, False, l)
            Math.UnaryNode n -> case n.state.latex of
                Just l -> (key, True, l)
                Nothing -> let l = createLatex key 1 in (key, True, l)
            Math.BinaryNode n -> case n.state.latex of
                Just l -> (key, (not n.associative), l)
                Nothing -> if n.associative
                    then let l = createLatex key 1 in (key, False, l)
                    else let l = createLatex key 2 in (key, True, l)
            Math.GenericNode n -> case n.state.latex of
                Just l -> (key, True, l)
                Nothing -> let l = createLatex key (Maybe.withDefault 0 n.arguments) in (key, True, l)
            _ -> let l = createLatex key 1 in (key, False, l)
        functionSymbols = functions
            |> Dict.filter (\key _ -> Set.member key defaultOps |> not)
            |> Dict.toList
            |> List.map (\(key, value) -> let (functionName, fixed, latex) = funcPropToLatex key value in
                (   cosineCorrelation_ inputOrder (letterOrder_ key)
                ,   (   key
                    -- don't use onClick, because some onBlurs get triggered first
                    ,   Html.a [class "clickable", HtmlEvent.onPointerCapture identity (\_ _ -> LatexChoice functionName fixed latex)]
                        [MathIcon.static [] latex]
                    )
                )
            )
    in
        List.sortBy (\(corr, _) -> -corr) functionSymbols
        |> \list ->
            (   List.map Tuple.second list
            ,   \str -> case Dict.get str functions of
                    Nothing -> (str, False, [Latex.Text () str, Latex.Bracket () [Latex.Argument () 1]])
                    Just value -> funcPropToLatex str value
            )

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