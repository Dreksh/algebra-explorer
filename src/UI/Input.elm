module UI.Input exposing (Model, Event(..), Scope(..), ScopeElement(..), defaultScopeDetail,
    init, initWithFixed, update, clear, set, current, view, toLatex, toTree, toString, fromString)

import Array
import Dict
import Html
import Html.Attributes exposing (class, id, name, style, type_)
import Html.Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|=), (|.))
import Set
-- Ours
import Helper
import Algo.History as History
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
type Scope = Scope ScopeDetail (List ScopeElement)
type alias ScopeDetail = {inseparable: Bool, immutable: Bool}
type ScopeElement =
    StrElement String -- Must not be empty string, they should be handled by removing them
    | Fixed
        {   text: String -- When put into string
        ,   latex: Latex.Model () -- When put into latex
        ,   params: Array.Array (Scope, DirectionOverride)
        ,   firstNode: Maybe Int
        ,   lastNode: Maybe Int
        }
    | Bracket (List ScopeElement)
    | InnerScope Scope
-- For Fixed models, the arguments can be in any order.
-- So the default behaviour is to jump out of the fixed scope.
-- If there is any overrides, it will use that to determine the next sibling to go to
type alias DirectionOverride = {up: Maybe Int, down: Maybe Int, left: Maybe Int, right: Maybe Int}
noOverride_: DirectionOverride
noOverride_ = {up = Nothing, down = Nothing, left = Nothing, right = Nothing}

-- CaretPosition has 2 different meanings for leaf node vs intermediate node
-- intermediate node represents the zero-indexed nth ScopeElement in Scope, or the nth Scope in Fixed
-- lead node represents the position before the index in Scope & in StrElement.
-- However, StrElement does not have the 0th position or the length-th position, as they will be the ones in Scope
type alias CaretPosition = List Int
-- PreviousInput checks whether the most recent action is an insertion event, or a deletion event
-- as well as when it happened, so that we can decide when to commit a change to its history
type alias PreviousInput =
    {   insertion: Bool
    ,   time: Float
    }

type alias Model msg =
    {   history: History.Model Scope
    ,   previousInput: Maybe PreviousInput
    ,   functionInput: Maybe String
    ,   cursor: CaretPosition
    ,   showCursor: Bool
    ,   holderID: String
    ,   mouseCmd: Encode.Value -> (Float, Float) -> Cmd msg
    ,   focusCmd: String -> Cmd msg
    }
type Event =
    Key {key: String, shift: Bool, meta: Bool, time: Float}
    | ShowCursor
    | HideCursor
    | MouseDown Encode.Value (Float, Float)
    | Shift SvgDrag.Event
    | InsertFixed ScopeElement
    | HelperInput String
    | HelperClear

defaultScopeDetail: ScopeDetail
defaultScopeDetail = {inseparable = True, immutable = False}

init: (Encode.Value -> (Float, Float) -> Cmd msg) -> (String -> Cmd msg) -> String -> Model msg
init mouseCmd focusCmd holderID =
    {   history = History.init (Scope defaultScopeDetail [])
    ,   previousInput = Nothing
    ,   functionInput = Nothing
    ,   cursor = [0]
    ,   holderID = holderID
    ,   showCursor = False
    ,   mouseCmd = mouseCmd
    ,   focusCmd = focusCmd
    }
initWithFixed: (Encode.Value -> (Float, Float) -> Cmd msg) -> (String -> Cmd msg) -> String
    -> List ScopeElement -> CaretPosition -> Model msg
initWithFixed mouseCmd focusCmd holderID elements pos =
    {   history = History.init (Scope {inseparable = True, immutable = True} elements)
    ,   previousInput = Nothing
    ,   functionInput = Nothing
    ,   cursor = pos
    ,   holderID = holderID
    ,   showCursor = False
    ,   mouseCmd = mouseCmd
    ,   focusCmd = focusCmd
    }

set: Scope -> Model msg -> Model msg
set (Scope detail children) model =
    {   model
    |   history = History.flushAndCommit (Scope detail children) model.history
    ,   previousInput = Nothing
    ,   functionInput = Nothing
    ,   cursor = [List.length children]
    ,   showCursor = False
    }
clear: Model msg -> Model msg
clear model =
    {   model
    |   history = History.init (Scope defaultScopeDetail [])
    ,   previousInput = Nothing
    ,   functionInput = Nothing
    ,   cursor = [0]
    ,   showCursor = False
    }

current: Model msg -> Scope
current model = History.next model.history

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
            (toLatex False [] (if model.showCursor then model.cursor else []) (current model))
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

formDecoder_: (String -> ScopeElement) -> Decode.Decoder Event
formDecoder_ convert = Decode.field "input" (Decode.field "value" Decode.string)
    |> Decode.map (convert >> InsertFixed)

toLatex: Bool -> List Int -> CaretPosition -> Scope -> Latex.Model (List Int)
toLatex border parentPos pos (Scope _ children) =
    let
        noCaret state scopeElem = case scopeElem of
            StrElement text -> [Latex.Text state text]
            Fixed n -> Latex.map (\_ -> state ) n.latex
                |> Latex.replace (Array.map (Tuple.first >> toLatex True state []) n.params)
            Bracket inner -> toLatex True state [] (Scope defaultScopeDetail inner)
                |> \newInner -> [Latex.Bracket state newInner]
            InnerScope inScope -> toLatex True state [] inScope
    in
    List.foldl
    (\elem (model, index) -> let childPos = parentPos ++ [index] in
        (   model
            ++ case pos of
                [] -> noCaret childPos elem
                [x] -> if x /= index then noCaret childPos elem
                    else Latex.Caret childPos :: noCaret childPos elem
                (x::y::other) -> case elem of
                    StrElement str -> if x /= index then noCaret childPos elem
                        else
                            [   Latex.Text childPos (String.left y str)
                            ,   Latex.Caret childPos
                            ,   Latex.Text childPos (String.dropLeft y str)
                            ]
                    Fixed n -> if x /= index then noCaret childPos elem
                        else Latex.map (\_ -> childPos) n.latex
                            |> Latex.replace
                            (   Array.indexedMap
                                (\i -> Tuple.first >> toLatex True (childPos ++ [i]) (if i /= y then [] else other))
                                n.params
                            )
                    Bracket inner -> if x /= index then noCaret childPos elem
                        else toLatex True (childPos ++ [0]) other (Scope defaultScopeDetail inner)
                            |> \newInner -> [ Latex.Bracket (childPos ++ [0]) newInner]
                    InnerScope inner -> if x /= index then noCaret childPos elem
                        else toLatex True (childPos ++ [0]) other inner
        ,   index + 1
        )
    ) ([], 0) children
    |> \(model, _) -> case (border, List.head pos |> Maybe.map ((==) (List.length children))) of
        (False, Just True) -> model ++ [Latex.Caret parentPos]
        (False, _) -> model
        (True, Just True) -> [Latex.Border parentPos (model ++ [Latex.Caret parentPos])]
        (True, _) -> [Latex.Border parentPos model]

{- ## Updates -}

allowedChars_: Set.Set Char
allowedChars_ = String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-.,= "
    |> Set.fromList

-- How long we will wait between character inputs where we assume they are within the same block of inputs
inputTimeout_: Float
inputTimeout_ = 750

batchFlushStage_: Float -> Bool -> Model msg -> Model msg
batchFlushStage_ currentTime insertion model = let newModel = {model | previousInput = Just {insertion = insertion, time = currentTime}} in
    case model.previousInput of
    Nothing -> newModel
    Just n -> if n.insertion /= insertion || currentTime - n.time > inputTimeout_
        then {newModel | history = History.commit model.history}
        else newModel

flushStage_: Model msg -> Model msg
flushStage_ model = {model | history = History.commit model.history, previousInput = Nothing}

update: Event -> Model msg -> (Model msg, String, Cmd msg)
update event model = case event of
    Key e -> let entry = current model in
        case (e.key, e.meta) of
        ("z", True) ->
            (   {model | history = History.commit model.history |> History.undo }
            , "", Cmd.none)
        ("Z", True) ->
            (   {model | history = History.commit model.history |> History.redo }
            , "", Cmd.none)
        ("Backspace", _) -> (batchFlushStage_ e.time False model |> delete_ False, "", Cmd.none)
        ("Delete", _) -> (batchFlushStage_ e.time False model |> delete_ True, "", Cmd.none)
        ("ArrowUp", _) -> (flushStage_ model |> cursorMove_ Up_, "", Cmd.none)
        ("ArrowDown", _) -> (flushStage_ model |> cursorMove_ Down_, "", Cmd.none)
        ("ArrowLeft", _) -> (flushStage_ model |> cursorMove_ Left_, "", Cmd.none)
        ("ArrowRight", _) -> (flushStage_ model |> cursorMove_ Right_, "", Cmd.none)
        (c, False) -> let updatedModel = batchFlushStage_ e.time True model in
            if String.length c == 1
            then case String.uncons c of
                Nothing -> (updatedModel, "", Cmd.none)
                Just (char, _) -> if Set.member char allowedChars_
                    then case insertChar_ char updatedModel of
                        Err str -> (updatedModel, str, Cmd.none)
                        Ok (newModel) -> (newModel, "", Cmd.none)
                    else case char of
                        '(' -> case insertScopeElement_ (Just 0) (Bracket []) updatedModel of
                            Err str -> (updatedModel, str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        '*' ->
                            let
                                detail = {text = "*", latex = [Latex.SymbolPart () Latex.CrossMultiplcation]
                                        , params = Array.empty, firstNode = Nothing, lastNode = Nothing }
                            in
                            case insertScopeElement_ Nothing (Fixed detail) updatedModel of
                            Err str -> (updatedModel, str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        '/' ->
                            let
                                detail = {text = "/", latex = [Latex.SymbolPart () Latex.Division]
                                        , params = Array.empty, firstNode = Nothing, lastNode = Nothing }
                            in
                            case insertScopeElement_ Nothing (Fixed detail) updatedModel of
                            Err str -> (updatedModel, str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        '\\' -> ({updatedModel | functionInput = Just ""}, "", updatedModel.focusCmd "inputHelper")
                        _ -> (updatedModel, "'" ++ c ++ "' is not an allowed input", Cmd.none)
            else (updatedModel, "", Cmd.none)
        _ -> (model, "", Cmd.none)
    ShowCursor -> ({model | showCursor = True}, "", Cmd.none)
    HideCursor -> ({model | history = History.commit model.history, showCursor = False}, "", Cmd.none)
    MouseDown val point -> (flushStage_ model, "", model.mouseCmd val point)
    Shift e -> case e of
        SvgDrag.End _ point -> (setCursor_ point model, "", Cmd.none)
        _ -> (model, "", Cmd.none) -- TODO: Start & Move for selecting text
    InsertFixed elem -> case elem of
        Fixed n -> case insertScopeElement_ n.firstNode elem model of
            Err str -> (flushStage_ model, str, Cmd.none)
            Ok finalModel -> (finalModel |> flushStage_, "", model.focusCmd (model.holderID ++ "-input"))
        _ -> (model, "unexpected fixed element to be inserted", Cmd.none)
    HelperInput str -> ({model | functionInput = Just str}, "", Cmd.none)
    HelperClear -> ({model | functionInput = Nothing}, "", Cmd.none)

{- ## Cursor Movement -}

type Direction_ = Up_ | Down_ | Left_ | Right_
type CursorDecision_ =
    FoundCursor CaretPosition
    | PreviousCursor | NextCursor | UpCursor | DownCursor

cursorMove_: Direction_ -> Model msg -> Model msg
cursorMove_ direction model = let (Scope d children) = current model in
    case cursorNext_ direction (Scope d children) model.cursor of
        FoundCursor newC -> {model | cursor = newC}
        _ -> model

cursorNext_: Direction_ -> Scope -> CaretPosition -> CursorDecision_
cursorNext_ direction (Scope detail children) caret =
    let
        getPrevious d c midPoint = if midPoint <= 0 then PreviousCursor
            else case List.drop (midPoint-1) c |> List.head of
                Nothing -> PreviousCursor
                Just (StrElement str) -> if d.immutable then getPrevious d c (midPoint - 1)
                    else FoundCursor (if String.length str <= 1 then [midPoint-1] else [midPoint-1, String.length str - 1])
                Just (Fixed f) -> case f.lastNode of
                    Nothing -> if d.immutable then getPrevious d c (midPoint - 1)
                        else FoundCursor [midPoint - 1]
                    Just n -> case Array.get n f.params of
                        Nothing -> FoundCursor [midPoint - 1, n, 0]
                        Just (Scope _ inS, _) -> FoundCursor [midPoint - 1, n, List.length inS]
                Just (Bracket inner) -> if d.immutable then getPrevious d c (midPoint - 1)
                    else FoundCursor [midPoint-1,0,List.length inner]
                Just (InnerScope (Scope inDetail inner)) -> if inDetail.immutable
                    then if d.immutable then getPrevious d c (midPoint - 1) else FoundCursor [midPoint - 1]
                    else FoundCursor [ midPoint - 1, 0, List.length inner]
        getNext d c midPoint = case List.drop midPoint children |> List.head of
            Nothing -> NextCursor
            Just (StrElement str) -> if d.immutable then getNext d c (midPoint + 1)
                else FoundCursor (if String.length str <= 1 then [midPoint+1] else [midPoint, 1])
            Just (Fixed f) -> case f.firstNode of
                Nothing -> if d.immutable then getNext d c (midPoint + 1) else FoundCursor [midPoint + 1]
                Just n -> FoundCursor [midPoint, n, 0]
            Just (Bracket _) -> if d.immutable then getNext d c (midPoint + 1)
                else FoundCursor [midPoint,0,0]
            Just (InnerScope (Scope inDetail _)) -> if inDetail.immutable
                then if d.immutable then getNext d c midPoint else FoundCursor [midPoint + 1]
                else FoundCursor [midPoint, 0, 0]
    in
    case caret of
    [] -> FoundCursor []
    [x] -> case direction of
        Up_ -> UpCursor
        Down_ -> DownCursor
        Left_ -> getPrevious detail children x
        Right_ -> getNext detail children x
    (x::y::others) ->
        case List.drop x children |> List.head of
            Just (StrElement str) ->
                case direction of
                Up_ -> UpCursor
                Down_ -> DownCursor
                Left_ -> if detail.immutable then getPrevious detail children x
                    else FoundCursor (if y == 1 then [x] else [x, y - 1])
                Right_ -> if detail.immutable then getNext detail children (x + 1)
                    else FoundCursor (if y + 1 == String.length str then [x + 1] else [x, y + 1])
            Just (Fixed f) -> case Array.get y f.params of
                Nothing -> if detail.immutable then getPrevious detail children x
                    else FoundCursor [x] -- reset to right before the fixed element
                Just (inScope, overrides) -> cursorNext_ direction inScope others
                    |> \res -> case res of
                        FoundCursor c -> FoundCursor (x::y::c)
                        UpCursor -> case overrides.up of
                            Nothing -> UpCursor
                            Just n -> FoundCursor [x, n, 0] -- TODO: Appraoch from % through the scope
                        DownCursor -> case overrides.down of
                            Nothing -> DownCursor
                            Just n -> FoundCursor [x, n, 0] -- TODO: Approach from % through the scope
                        PreviousCursor -> case overrides.left of
                            Nothing -> if detail.immutable then getPrevious detail children x else FoundCursor [x]
                            Just n -> case Array.get n f.params of
                                Nothing -> FoundCursor [x,n,0]
                                Just (Scope _ newC, _) -> FoundCursor [x,n,List.length newC]
                        NextCursor -> case overrides.right of
                            Nothing -> if detail.immutable then getNext detail children (x + 1) else FoundCursor [x+1]
                            Just n -> FoundCursor [x, n, 0]
            Just (Bracket inner) -> cursorNext_ direction (Scope detail inner) others
                |> \res -> case res of
                    FoundCursor c -> FoundCursor (x::y::c)
                    UpCursor -> UpCursor
                    DownCursor -> DownCursor
                    PreviousCursor -> if detail.immutable then getPrevious detail children x else FoundCursor [x]
                    NextCursor -> if detail.immutable then getNext detail children (x + 1) else FoundCursor [x+1]
            Just (InnerScope inner) -> cursorNext_ direction inner others
                |> \res -> case res of
                    FoundCursor c -> FoundCursor (x::y::c)
                    UpCursor -> UpCursor
                    DownCursor -> DownCursor
                    PreviousCursor -> if detail.immutable then getPrevious detail children x else FoundCursor [x]
                    NextCursor -> if detail.immutable then getNext detail children (x + 1) else FoundCursor [x+1]
            Nothing -> if detail.immutable then getPrevious detail children x else FoundCursor [x] -- reset to previous

{- ## Click -}

setCursor_: (Float, Float) -> Model msg -> Model msg
setCursor_ point model = toLatex False [] [] (current model)
    |> \latex -> MathIcon.closestFrame latex point
    |> \frameRes -> case frameRes of
        Nothing -> model
        Just (newC, diff) -> traverse (\_ input -> case input of
            TextCase prev str num next -> Ok (prev ++ (StrElement str :: next), [List.length prev, num])
            IntermediateCase prev next -> case List.head next of
                Nothing -> Ok (prev ++ next, [List.length prev + List.length next])
                Just n -> case n of
                    StrElement str -> MathIcon.closestChar str diff
                        |> \(index, vector) -> if Tuple.first vector > 0
                            then if index + 1 == String.length str
                                then Ok (prev ++ next, [List.length prev + 1])
                                else Ok (prev ++ next, [List.length prev, index + 1])
                            else if index == 0
                                then Ok (prev ++ next, [List.length prev])
                                else Ok (prev ++ next, [List.length prev, index])
                    _ -> if Tuple.first diff > 0
                        then Ok (prev ++ next, [List.length prev + 1])
                        else Ok (prev ++ next, [List.length prev])
            ) (current model) newC
            |> \res -> case res of
                Ok (_,c) -> {model | cursor = c}
                Err _ -> model

{- ## Scope traversal -}

type TraversalCase_ =
    TextCase (List ScopeElement) String Int (List ScopeElement)
    | IntermediateCase (List ScopeElement) (List ScopeElement)
type TraversalError_  =
    BrokenCaret -- For when the position cannot be found
    | CannotSplitArgument_
    | CannotModifySection_

traverse: (ScopeDetail -> TraversalCase_ -> Result TraversalError_ (List ScopeElement, CaretPosition))
    -> Scope -> CaretPosition -> Result TraversalError_ (List ScopeElement, CaretPosition)
traverse process (Scope detail children) caret = case caret of
    [] -> Err BrokenCaret
    [x] -> IntermediateCase (List.take x children) (List.drop x children) |> process detail
    (x::y::others) -> let (prev, next) = (List.take x children, List.drop x children) in
        case next of
            (StrElement str :: after) -> TextCase prev str y after |> process detail
            (Fixed f :: after) -> case Array.get y f.params of
                Nothing -> Err BrokenCaret
                Just (Scope inDetail inChildren, override) -> traverse process (Scope inDetail inChildren) others
                    |> Result.andThen (\(newChildren, pos) -> let newParams = Array.set y (Scope inDetail newChildren, override) f.params in
                        Ok (prev ++ (Fixed {f | params = newParams} :: after), x::y::pos)
                    )
            (Bracket kids :: after) -> traverse process (Scope detail kids) others
                |> Result.map (\(newChildren, pos) -> (prev ++ (Bracket newChildren) :: after, x::y::pos))
            (InnerScope (Scope inD inC) :: after) -> traverse process (Scope inD inC) others
                |> Result.map (\(newChildren, pos) -> (prev ++ (InnerScope (Scope inD newChildren)) :: after, x::y::pos))
            _ -> Err BrokenCaret

{- ## Insertion -}

insertChar_: Char -> Model msg -> Result String (Model msg)
insertChar_ char model = let (Scope detail children) = current model in
    traverse
    (\parent input -> if parent.inseparable && char == ',' then Err CannotSplitArgument_
        else Ok
        (   case input of
            TextCase prev str num next ->
                let newStr = (String.left num str) ++String.fromChar char ++ (String.dropLeft num str) in
                (prev ++ (StrElement newStr :: next), [List.length prev, num+1])
            IntermediateCase prev next -> case next of
                (StrElement str::after) ->
                    (prev ++ (StrElement (String.cons char str) :: after), [List.length prev, 1])
                _ -> let beforeLength = List.length prev - 1 in
                    case List.drop beforeLength prev of
                        [StrElement str] ->
                            (   List.take beforeLength prev ++ (StrElement (str ++ String.fromChar char) :: next)
                            ,   [List.length prev]
                            )
                        _ ->
                            (   prev ++ (StrElement (String.fromChar char) :: next)
                            ,   [List.length prev + 1]
                            )
        )
    )
    (Scope detail children) model.cursor
    |> \res -> case res of
        Ok (l,c) -> Ok {model | history = History.stage (Scope detail l) model.history, cursor = c}
        _ -> Err "Something went wrong"

insertScopeElement_: Maybe Int -> ScopeElement -> Model msg -> Result String (Model msg)
insertScopeElement_ firstNode element model = let (Scope detail children) = current model in
    traverse
    (\_ input -> Ok
        (   case input of
                TextCase prev str num next ->
                    (   prev
                        ++  ( StrElement (String.left num str) :: element
                            :: StrElement (String.dropLeft num str) :: next
                            )
                    ,   case firstNode of
                        Nothing -> [List.length prev + 2]
                        Just n -> [List.length prev + 1, n, 0]
                    )
                IntermediateCase prev next ->
                    (   prev ++ (element :: next)
                    ,   case firstNode of
                        Nothing -> [List.length prev + 1]
                        Just n -> [List.length prev, n, 0]
                    )
        )
    )
    (Scope detail children) model.cursor
    |> \res -> case res of
        Ok (l,c) -> Ok {model | history = History.stage (Scope detail l) model.history, cursor = c}
        _ -> Err "Something went wrong"

latexArray_: Bool -> Latex.Model DirectionOverride -> Result String (Array.Array (Scope, DirectionOverride))
latexArray_ fixed latex = Latex.extractArgs latex
    |> Result.map (\dict -> if fixed
        then Dict.foldl (\_ override -> Array.push (Scope defaultScopeDetail [], override)) Array.empty dict
        else Array.fromList [(Scope defaultScopeDetail [], noOverride_)]
    )

{- ## Deletion -}

delete_: Bool -> Model msg -> Model msg
delete_ forwards model = let (Scope detail children) = current model in
    traverse
    (\scopeDetail input -> if scopeDetail.immutable then Err CannotModifySection_
        else Ok (   case input of
            TextCase prev str num next -> if forwards
                then
                    (   prev ++ (StrElement (String.left num str ++ String.dropLeft (num+1) str) :: next)
                    ,   if num + 1 == String.length str then [List.length prev + 1] else [List.length prev, num]
                    )
                else
                    (   prev ++ (StrElement (String.left (num - 1) str ++ String.dropLeft num str) :: next)
                    ,   if num == 1 then [List.length prev] else [List.length prev, num - 1]
                    )
            IntermediateCase prev next -> if forwards
                then
                    (   case List.head next of
                        Nothing -> prev
                        Just (StrElement str) -> prev ++ (StrElement (String.dropLeft 1 str) :: List.drop 1 next)
                        Just (Fixed _) -> prev ++ List.drop 1 next
                        Just (Bracket _) -> prev ++ List.drop 1 next
                        Just (InnerScope _) -> prev ++ List.drop 1 next
                    ,   [List.length prev]
                    )
                else let beforeLength = List.length prev - 1 in
                    case List.drop beforeLength prev of
                        [StrElement str] ->
                            (   List.take beforeLength prev ++ (StrElement (String.dropRight 1 str)::next)
                            ,   [List.length prev]
                            )
                        [Fixed _] -> (List.take beforeLength prev ++ next, [beforeLength])
                        [Bracket _] -> (List.take beforeLength prev ++ next, [beforeLength])
                        _ -> (next, [0])
        )
    )
    (Scope detail children) model.cursor
    |> \res -> case res of
        Ok (l, c) -> {model | history = History.stage (Scope detail l) model.history, cursor = c}
        _ -> model

{- ## Extraction -}

toTree: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Model msg -> Result String Display.FullEquation
toTree dict model = toString model
    |> Result.andThen (Matcher.parseEquation dict Animation.stateOps)

toString: Model msg -> Result String String
toString model = toEntryString_ (current model)

toEntryString_: Scope -> Result String String
toEntryString_ (Scope _ children) = Helper.resultList (\child res -> case child of
        StrElement str -> Ok (res ++ str)
        Bracket inner -> toEntryString_ (Scope defaultScopeDetail inner)
            |> Result.map (\inStr -> res ++ "(" ++ inStr ++ ")")
        Fixed f -> if Array.isEmpty f.params
            then Ok (res ++ "\\" ++ f.text)
            else Helper.resultArray (\index (innerScope, _) innerRes ->
                toEntryString_ innerScope
                |> Result.map (\str -> Array.set index str innerRes)
            ) (Array.repeat (Array.length f.params) "") f.params
            |> Result.map (\arr -> res ++ "\\" ++ f.text ++ "(" ++ (Array.toList arr |> String.join ",") ++ ")" )
        InnerScope inner -> toEntryString_ inner
            |> Result.map (\inStr -> res ++ inStr)
    ) "" children

{- ## Suggestion -}

defaultOps: Set.Set String
defaultOps = Set.fromList ["+", "-", "*", "/", "="]

displaySuggestions_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String
    -> (List (String, Html.Html Event), String -> ScopeElement)
displaySuggestions_ functions input = let inputOrder = letterOrder_ input in
    Dict.keys functions
    |> List.filter (\key -> Set.member key defaultOps |> not)
    |> List.map (\key -> let (fixed, latex) = funcPropToLatex_ functions key in
        (   cosineCorrelation_ inputOrder (letterOrder_ key)
        ,   (   key
            -- don't use onClick, because some onBlurs get triggered first
            ,   Html.a
                [   class "clickable"
                ,   HtmlEvent.onPointerCapture identity
                    (\_ _ -> InsertFixed (fixedFrom_ key fixed Array.empty latex))
                ]
                [MathIcon.static [] latex]
            )
        )
    )
    |> List.sortBy (\(corr, _) -> -corr)
    |> \list ->
        (   List.map Tuple.second list
        ,   \str -> let (fixed, latex) = funcPropToLatex_ functions str in
                fixedFrom_ str fixed Array.empty latex
        )

funcPropToLatex_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String -> (Bool, Latex.Model ())
funcPropToLatex_ funcDict key = case Dict.get key funcDict of
    Nothing -> (False, [Latex.Text () key, Latex.Bracket () [Latex.Argument () 1]])
    Just value ->
        let
            createLatex name args =
                [   Latex.Text () name
                ,   Latex.Bracket ()
                    (List.range 1 args |> List.map (Latex.Argument ()) |> List.intersperse (Latex.Text () ","))
                ]
        in
        case value.property of
        Math.VariableNode n -> case n.state.latex of
            Just l -> (True, l)
            Nothing -> let l = createLatex key 1 in (False, l)
        Math.UnaryNode n -> case n.state.latex of
            Just l -> (True, l)
            Nothing -> let l = createLatex key 1 in (True, l)
        Math.BinaryNode n -> case n.state.latex of
            Just l -> (not n.associative, l)
            Nothing -> if n.associative
                then let l = createLatex key 1 in (False, l)
                else let l = createLatex key 2 in (True, l)
        Math.GenericNode n -> case n.state.latex of
            Just l -> (True, l)
            Nothing -> let l = createLatex key (Maybe.withDefault 0 n.arguments) in (True, l)
        _ -> let l = createLatex key 1 in (False, l)

fixedFrom_: String -> Bool -> Array.Array (List ScopeElement) -> Latex.Model () -> ScopeElement
fixedFrom_ text fixed args latex =
    let
        defaultStart =
            (   {topLast = Nothing, midLast = Just -1, botLast  = Nothing}
            ,   Dict.singleton -1 noOverride_
            )
        updateRight now (prev, dict) =
            (   {topLast = Nothing, midLast = Just now, botLast = Nothing}
            ,   case (prev.topLast, prev.midLast, prev.botLast) of
                (Nothing, Just mid, Nothing) -> Dict.insert now {noOverride_ | left = Just mid} dict
                    |> Dict.update mid (Maybe.map(\v -> {v | right = Just now}))
                (Nothing, Just mid, Just bot) -> Dict.insert now {noOverride_ | left = Just bot} dict
                    |> Dict.update mid (Maybe.map(\v -> {v | right = Just bot}))
                    |> Dict.update bot (Maybe.map(\v -> {v | right = Just now}))
                (Just top, Just mid, Nothing) ->  Dict.insert now {noOverride_ | left = Just top} dict
                    |> Dict.update mid (Maybe.map(\v -> {v | right = Just now}))
                    |> Dict.update top (Maybe.map(\v -> {v | right = Just now}))
                (Just top, Just mid, Just bot) ->  Dict.insert now {noOverride_ | left = Just top} dict
                    |> Dict.update mid (Maybe.map(\v -> {v | right = Just now}))
                    |> Dict.update top (Maybe.map(\v -> {v | right = Just now}))
                    |> Dict.update bot (Maybe.map(\v -> {v | right = Just now}))
                (Just top, Nothing, Just bot) ->  Dict.insert now {noOverride_ | left = Just top} dict
                    |> Dict.update top (Maybe.map(\v -> {v | right = Just now}))
                    |> Dict.update bot (Maybe.map(\v -> {v | right = Just now}))
                _ -> Dict.insert now noOverride_ dict
            )
        getLastest ref = case ref.topLast of
            Just _ -> ref.topLast
            Nothing -> case ref.botLast of
                Just _ -> ref.botLast
                Nothing -> ref.midLast
        getFirstAndUpdate newLeft dict =
            (   Dict.get -1 dict |> Maybe.andThen .right
            ,   Dict.remove -1 dict
                |> Dict.map (\_ v -> if v.left == Just -1 then {v | left = newLeft} else v)
            )
        recursive initial =  List.foldl (\part (ref, dict) -> case part of
            Latex.Fraction _ top bot ->
                let
                    (topRef, topDict) = recursive defaultStart top
                    (botRef, botDict) = recursive defaultStart bot
                    prev = getLastest ref
                    (topFirst, newTop) = getFirstAndUpdate prev topDict
                    (botFirst, newBot) = getFirstAndUpdate prev botDict
                    first = case topFirst of
                        Just _ -> topFirst
                        Nothing -> botFirst
                in
                (   {   topLast = getLastest topRef
                    ,   midLast = Nothing
                    ,   botLast = getLastest botRef
                    }
                ,   Maybe.map (\p -> Dict.update p (Maybe.map (\v -> {v | right = first})) dict) prev
                    |> Maybe.withDefault dict
                    |> \dict1 -> Dict.foldl (\k v -> Dict.insert k {v | down = botFirst}) dict1 newTop
                    |> \dict2 -> Dict.foldl (\k v -> Dict.insert k {v | up = topFirst}) dict2 newBot
                )
            Latex.Superscript _ inner -> let (newRef, newDict) = recursive (ref, dict) inner in
                ({ref | topLast = newRef.midLast}, newDict)
            Latex.Subscript _ inner -> let (newRef, newDict) = recursive (ref, dict) inner in
                ({ref | botLast = newRef.midLast}, newDict)
            Latex.Bracket _ inner -> recursive (ref, dict) inner
            Latex.Sqrt _ inner -> recursive (ref, dict) inner
            Latex.Argument _ num -> updateRight (num - 1) (ref, dict)
            Latex.Param _ num -> updateRight (num - 1) (ref, dict)
            _ -> (ref, dict)
            ) initial
    in
        recursive defaultStart latex
        |> \(ref, dict) -> let (first, dict1) = getFirstAndUpdate Nothing dict in
            Fixed
            {   text = text
            ,   latex = latex
            ,   params = Dict.toList dict1
                    |> List.map (\(i, override) ->
                        (   Scope {inseparable = fixed, immutable = False}
                            (Array.get i args |> Maybe.withDefault [])
                        ,   override
                        )
                    )
                    |> Array.fromList
            ,   firstNode = first
            ,   lastNode = getLastest ref
            }

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

{- ## conversions -}

fromString: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String -> Result String (List ScopeElement)
fromString funcDict str = Parser.run (scopeParser_ funcDict|. Parser.end) str
    |> \res -> case res of
        Err _ -> Err "Something went wrong parsing into scopes"
        Ok list -> Ok list


scopeParser_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Parser.Parser (List ScopeElement)
scopeParser_ funcDict = Parser.loop []
    (\prev -> Parser.oneOf
        [   textParser_ |> Parser.map (\elem -> Parser.Loop (elem :: prev))
        ,   Parser.succeed Bracket |. Parser.token "(" |= scopeParser_ funcDict |. Parser.token ")"
            |> Parser.map (\elem -> Parser.Loop (elem :: prev))
        ,   fixedParser_ funcDict |> Parser.map (\elem -> Parser.Loop (elem :: prev))
        ,   Parser.succeed (Parser.Done prev)
        ]
    )
    |> Parser.map List.reverse

textParser_: Parser.Parser ScopeElement
textParser_ = Parser.variable
    {   start = \c -> Set.member c allowedChars_
    ,   inner = \c -> Set.member c allowedChars_
    ,   reserved = Set.empty
    }
    |> Parser.map StrElement

nameParser_: Parser.Parser String
nameParser_ = Parser.variable
    {   start = Char.isAlphaNum
    ,   inner = Char.isAlphaNum
    ,   reserved = Set.empty
    }

fixedParser_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Parser.Parser ScopeElement
fixedParser_ funcDict = Parser.succeed
    (\name args -> let (fixed, latex) = funcPropToLatex_ funcDict name in
        fixedFrom_ name fixed
            ((if fixed then args else [List.intersperse [StrElement ","] args |> List.concat]) |> Array.fromList)
            latex
    )
    |. Parser.token "\\"
    |= nameParser_
    |= Parser.oneOf
        [   Parser.succeed identity
            |. Parser.token "("
            |= Parser.loop []
                (\prev -> Parser.oneOf
                    [   Parser.succeed
                        (\inner done -> if done then Parser.Done (List.reverse (inner :: prev)) else Parser.Loop (inner::prev))
                        |= scopeParser_ funcDict
                        |= Parser.oneOf
                            [   Parser.succeed True |. Parser.token ")"
                            ,   Parser.succeed False |. Parser.token ","
                            ]
                    ,   Parser.succeed (Parser.Done prev)
                    ]
                )
        ,   Parser.succeed []
        ]

