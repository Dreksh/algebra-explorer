module UI.Input exposing (Model, Event(..), Scope(..), ScopeElement(..), defaultScopeDetail,
    init, initWithFixed, advanceTime, update, clear, set, current, view, toLatex, toTree, toString, fromString)

import Array
import Dict
import Html
import Html.Attributes exposing (class, id, name, style, type_, placeholder)
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
import UI.BrickSvg as BrickSvg
import UI.Display as Display
import UI.HtmlEvent as HtmlEvent
import UI.MathIcon as MathIcon
import UI.SvgDrag as SvgDrag
import UI.BrickSvg as BrickSvg

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
    ,   previousInput: Maybe PreviousInput -- For checking when to commit / flush
    ,   suggestions: List (String, Html.Html Event) -- Cached suggestions list
    ,   cursor: CaretPosition
    ,   matchFoundText: Animation.EaseState Float
    ,   popupHeight: Animation.EaseState Float
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

defaultScopeDetail: ScopeDetail
defaultScopeDetail = {inseparable = True, immutable = False}

animationDuration_: Float
animationDuration_ = 400

delayDuration_: Float
delayDuration_ = 100

init: (Encode.Value -> (Float, Float) -> Cmd msg) -> (String -> Cmd msg) -> String -> Model msg
init mouseCmd focusCmd holderID =
    {   history = History.init (Scope defaultScopeDetail [])
    ,   previousInput = Nothing
    ,   suggestions = []
    ,   matchFoundText = Animation.newEaseFloat animationDuration_ 0 |> Animation.withDelay delayDuration_
    ,   cursor = [0]
    ,   holderID = holderID
    ,   popupHeight = Animation.newEaseFloat animationDuration_ 0 |> Animation.withDelay delayDuration_
    ,   mouseCmd = mouseCmd
    ,   focusCmd = focusCmd
    }
initWithFixed: (Encode.Value -> (Float, Float) -> Cmd msg) -> (String -> Cmd msg) -> String
    -> List ScopeElement -> CaretPosition -> Model msg
initWithFixed mouseCmd focusCmd holderID elements pos =
    {   history = History.init (Scope {inseparable = True, immutable = True} elements)
    ,   previousInput = Nothing
    ,   suggestions = []
    ,   matchFoundText = Animation.newEaseFloat animationDuration_ 0 |> Animation.withDelay delayDuration_
    ,   cursor = pos
    ,   holderID = holderID
    ,   popupHeight = Animation.newEaseFloat animationDuration_ 0 |> Animation.withDelay delayDuration_
    ,   mouseCmd = mouseCmd
    ,   focusCmd = focusCmd
    }

set: Animation.Tracker -> Scope -> Model msg -> (Model msg, Animation.Tracker)
set t (Scope detail children) model =
    let
        (newHeight, newT) = Animation.setEase t 0 model.popupHeight
        (newText, finalT) = Animation.setEase newT 0 model.matchFoundText
    in
    (   {   model
        |   history = History.flushAndCommit (Scope detail children) model.history
        ,   previousInput = Nothing
        ,   suggestions = []
        ,   matchFoundText = newText
        ,   cursor = [List.length children]
        ,   popupHeight = newHeight
        }
    ,   finalT
    )
clear: Animation.Tracker -> Model msg -> (Model msg, Animation.Tracker)
clear t model =
    let
        (newHeight, newT) = Animation.setEase t 0 model.popupHeight
        (newText, finalT) = Animation.setEase newT 0 model.matchFoundText
    in
    (   {   model
        |   history = History.init (Scope defaultScopeDetail [])
        ,   previousInput = Nothing
        ,   suggestions = []
        ,   matchFoundText = newText
        ,   cursor = [0]
        ,   popupHeight = newHeight
        }
    ,   finalT
    )

current: Model msg -> Scope
current model = History.next model.history

advanceTime: Float -> Model msg -> Model msg
advanceTime time model =
    {   model
    |   popupHeight = Animation.advance time model.popupHeight
    ,   matchFoundText = Animation.advance time model.matchFoundText
    }

view: (Event -> msg) -> List (Html.Attribute msg) -> Model msg -> Html.Html msg
view convert attr model = Html.div ([id model.holderID, class "mathInput"] ++ attr)
    (   [   Html.input
            [   type_ "text"
            ,   id (model.holderID ++ "-input")
            ,   HtmlEvent.onKeyDown (Key >> convert)
            ,   HtmlEvent.onFocus (convert ShowCursor)
            ,   HtmlEvent.onBlur (convert HideCursor)
            ,   HtmlEvent.onPointerCapture convert MouseDown
            ]
            []
        ,   Html.div [class "placeholder"] [Html.text (if showPlaceholder_ model then "Type an equation..." else "") ]
        ,   MathIcon.staticWithCursor [style "pointer-events" "none"]
            (toLatex False [] (if Animation.target model.popupHeight == 1 then model.cursor else []) (current model))
        ,   Html.p [class "mathHint", style "max-height" ( (Animation.current model.matchFoundText |> String.fromFloat) ++ "em")]
            [Html.text "Press [Space] to insert"]
        ,   Html.div [class "mathPopup", style "max-height" ( (Animation.current model.popupHeight |> String.fromFloat) ++ "em") ]
            [   Html.Keyed.node "div" [class "holder", class "hideScrollbar"] (model.suggestions ++ [("space_", Html.span [class "space"] [])])
                |> Html.map convert
            ,   Html.div [class "scrollMask"] []
            ]
        ]
    )

formDecoder_: (String -> ScopeElement) -> Decode.Decoder Event
formDecoder_ convert = Decode.field "input" (Decode.field "value" Decode.string)
    |> Decode.map (convert >> InsertFixed)

toLatex: Bool -> List Int -> CaretPosition -> Scope -> Latex.Model (List Int)
toLatex border parentPos pos (Scope _ children) =
    let
        noCaret state scopeElem = case scopeElem of
            StrElement text -> [Latex.Text state text]
            Fixed n -> Latex.map (\_ -> state.state ) n.latex
                |> Latex.replace (Array.indexedMap (\i (p, _) -> toLatex True (state.state ++ [i]) [] p) n.params)
            Bracket inner -> toLatex True state.state [] (Scope defaultScopeDetail inner)
                |> \newInner -> [Latex.Bracket state newInner]
            InnerScope inScope -> toLatex True state.state [] inScope
        appendIndex n state = {state | state = state.state ++ [n]}
    in
    List.foldl
    (\elem (model, index) -> let childPos = {state=parentPos ++ [index], style=Nothing} in
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
                        else Latex.map (\_ -> childPos.state) n.latex
                            |> Latex.replace
                            (   Array.indexedMap
                                (\i -> Tuple.first >> toLatex True (childPos.state ++ [i]) (if i /= y then [] else other))
                                n.params
                            )
                    Bracket inner -> if x /= index then noCaret (appendIndex 0 childPos) elem
                        else toLatex True (childPos.state ++ [0]) other (Scope defaultScopeDetail inner)
                            |> \newInner -> [ Latex.Bracket (appendIndex 0 childPos) newInner]
                    InnerScope inner -> if x /= index then noCaret (appendIndex 0 childPos) elem
                        else toLatex True (childPos.state ++ [0]) other inner
        ,   index + 1
        )
    ) ([], 0) children
    |> \(model, _) -> case (border, List.head pos |> Maybe.map ((==) (List.length children))) of
        (False, Just True) -> model ++ [Latex.Caret {state=parentPos, style=Nothing}]
        (False, _) -> model
        (True, Just True) -> [Latex.Border {state=parentPos, style=Nothing} (model ++ [Latex.Caret {state=parentPos, style=Nothing}])]
        (True, _) -> [Latex.Border {state=parentPos,style=Nothing} model]

showPlaceholder_: Model msg -> Bool
showPlaceholder_ model =
    let (Scope _ children) = current model
    in List.isEmpty children && Animation.target model.popupHeight == 0

{- ## Updates -}

allowedChars_: Set.Set Char
allowedChars_ = String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,"
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

addOp_: ScopeElement
addOp_ = Fixed {text = "+", latex = [Latex.Text {state=(), style=Nothing} "+"]
            , params = Array.empty, firstNode = Nothing, lastNode = Nothing }
equalOp_: ScopeElement
equalOp_ = Fixed {text = "=", latex = [Latex.Text {state=(), style=Nothing} "="]
            , params = Array.empty, firstNode = Nothing, lastNode = Nothing }
subOp_: ScopeElement
subOp_ = Fixed {text = "-", latex = [Latex.Text {state=(), style=Nothing} "-"]
            , params = Array.empty, firstNode = Nothing, lastNode = Nothing }
timesOp_: ScopeElement
timesOp_ = Fixed {text = "*", latex = [Latex.SymbolPart {state=(), style=Nothing} Latex.CrossMultiplcation]
            , params = Array.empty, firstNode = Nothing, lastNode = Nothing }
divideOp_: ScopeElement
divideOp_ = Fixed {text = "/", latex = [Latex.SymbolPart {state=(), style=Nothing} Latex.Division]
            , params = Array.empty, firstNode = Nothing, lastNode = Nothing }


update: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Animation.Tracker
    -> Event -> Model msg -> ((Model msg, Animation.Tracker), String, Cmd msg)
update funcProp t event model = case event of
    Key e -> let entry = current model in
        case (e.key, e.meta) of
        ("z", True) ->
            (({model | history = History.commit model.history |> History.undo }, t)
            , "", Cmd.none)
        ("Z", True) ->
            (({model | history = History.commit model.history |> History.redo }, t)
            , "", Cmd.none)
        ("Backspace", _) -> (batchFlushStage_ e.time False model |> delete_ False |> checkMatches_ funcProp t, "", Cmd.none)
        ("Delete", _) -> (batchFlushStage_ e.time False model |> delete_ True |> checkMatches_ funcProp t, "", Cmd.none)
        ("ArrowUp", _) -> (flushStage_ model |> cursorMove_ funcProp Up_ |> checkMatches_ funcProp t, "", Cmd.none)
        ("ArrowDown", _) -> (flushStage_ model |> cursorMove_ funcProp Down_ |> checkMatches_ funcProp t, "", Cmd.none)
        ("ArrowLeft", _) -> (flushStage_ model |> cursorMove_ funcProp Left_ |> checkMatches_ funcProp t, "", Cmd.none)
        ("ArrowRight", _) -> (flushStage_ model |> cursorMove_ funcProp Right_ |> checkMatches_ funcProp t, "", Cmd.none)
        (c, False) -> let updatedModel = batchFlushStage_ e.time True model in
            if String.length c == 1
            then case String.uncons c of
                Nothing -> ((updatedModel, t), "", Cmd.none)
                Just (char, _) -> if Set.member char allowedChars_
                    then case insertChar_ char updatedModel of
                        Err str -> ((updatedModel, t), str, Cmd.none)
                        Ok (newModel) ->
                            (   {newModel | suggestions = getSuggestions_ funcProp (current newModel) newModel.cursor}
                                |> checkMatches_ funcProp t
                            , "", Cmd.none
                            )
                    else case char of
                        '(' -> case insertScopeElement_ t (Just 0) (Bracket []) updatedModel of
                            Err str -> ((updatedModel, t), str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        ')' -> case exitBracket_ updatedModel of
                            Err str -> ((updatedModel, t), str, Cmd.none)
                            Ok newModel -> ((newModel, t), "", Cmd.none)
                        '+' -> case insertScopeElement_ t Nothing addOp_ updatedModel of
                            Err str -> ((updatedModel, t), str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        '-' -> case insertScopeElement_ t Nothing subOp_ updatedModel of
                            Err str -> ((updatedModel, t), str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        '=' -> case insertScopeElement_ t Nothing equalOp_ updatedModel of
                            Err str -> ((updatedModel, t), str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        '*' -> case insertScopeElement_ t Nothing timesOp_ updatedModel of
                            Err str -> ((updatedModel, t), str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        '/' -> case insertScopeElement_ t Nothing divideOp_ updatedModel of
                            Err str -> ((updatedModel, t), str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        ' ' -> case insertSpace_ funcProp t updatedModel of
                            Err str -> ((updatedModel, t), str, Cmd.none)
                            Ok newModel -> (newModel, "", Cmd.none)
                        _ -> ((updatedModel, t), "'" ++ c ++ "' is not an allowed input", Cmd.none)
            else ((updatedModel, t), "", Cmd.none)
        _ -> ((model, t), "", Cmd.none)
    ShowCursor -> let (newHeight, newT) = Animation.setEase t 1 model.popupHeight in
        (   {   model
            |   popupHeight = newHeight
            ,   suggestions = getSuggestions_ funcProp (current model) model.cursor
            }
            |> checkMatches_ funcProp newT
        , "", Cmd.none
        )
    HideCursor -> let (newHeight, newT) = Animation.setEase t 0 model.popupHeight in
        (({model | history = History.commit model.history, popupHeight = newHeight}, newT), "", Cmd.none)
    MouseDown val point -> ((flushStage_ model, t), "", model.mouseCmd val point)
    Shift e -> case e of
        SvgDrag.End _ point -> (setCursor_ point model |> checkMatches_ funcProp t, "", Cmd.none)
        _ -> ((model, t), "", Cmd.none) -- TODO: Start & Move for selecting text
    InsertFixed elem -> case elem of
        Fixed n -> case insertScopeElement_ t n.firstNode elem model of
            Err str -> ((flushStage_ model, t), str, Cmd.none)
            Ok (finalModel, newT) -> ((finalModel |> flushStage_, newT), "", model.focusCmd (model.holderID ++ "-input"))
        _ -> ((model, t), "unexpected fixed element to be inserted", Cmd.none)

{- ## Cursor Movement -}

type Direction_ = Up_ | Down_ | Left_ | Right_
type CursorDecision_ =
    FoundCursor CaretPosition
    | PreviousCursor | NextCursor | UpCursor | DownCursor

cursorMove_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Direction_ -> Model msg
    -> Model msg
cursorMove_ funcProp direction model = let (Scope d children) = current model in
    case cursorNext_ direction (Scope d children) model.cursor of
        FoundCursor newC ->
            {   model
            |   cursor = newC
            ,   suggestions = getSuggestions_ funcProp (Scope d children) newC
            }
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

exitBracket_: Model msg -> Result String (Model msg)
exitBracket_ model =
    let
        enter (Scope detail children) caret = case caret of
            (x::y::others) -> case List.drop x children |> List.head of
                Just (Fixed n) -> case Array.get y n.params of
                    Nothing -> if detail.immutable then Nothing else Just [x+1]
                    Just (inScope, _) -> case enter inScope others of
                        Nothing -> if detail.immutable then Nothing else Just [x+1]
                        Just pos -> Just (x::y::pos)
                Just (Bracket kids) -> case enter (Scope detail kids) others of
                    Nothing -> if detail.immutable then Nothing else Just [x+1]
                    Just pos -> Just (x::y::pos)
                Just (InnerScope inScope) -> enter inScope others
                    |> Maybe.map (\pos -> x::y::pos)
                _ -> Nothing
            _ -> Nothing
    in
        case enter (current model) model.cursor of
            Nothing -> Err "')' is not an allowed input"
            Just pos -> Ok {model | cursor = pos}

{- ## Click -}

setCursor_: (Float, Float) -> Model msg -> Model msg
setCursor_ point model = toLatex False [] [] (current model)
    |> \latex -> MathIcon.closestFrame latex point
    |> \frameRes -> case frameRes of
        Nothing -> model
        Just (newC, diff) -> traverse (\_ input -> case input of
            TextCase prev str num next -> Ok ((), Just (prev ++ (StrElement str :: next), [List.length prev, num]))
            IntermediateCase prev next -> case List.head next of
                Nothing -> Ok ((), Just (prev ++ next, [List.length prev + List.length next]))
                Just n -> case n of
                    StrElement str -> MathIcon.closestChar str diff
                        |> \(index, vector) -> if Tuple.first vector > 0
                            then if index + 1 == String.length str
                                then Ok ((), Just (prev ++ next, [List.length prev + 1]))
                                else Ok ((), Just (prev ++ next, [List.length prev, index + 1]))
                            else if index  == 0
                                then Ok ((), Just (prev ++ next, [List.length prev]))
                                else Ok ((), Just (prev ++ next, [List.length prev, index]))
                    _ -> if Tuple.first diff > 0
                        then Ok ((), Just (prev ++ next, [List.length prev + 1]))
                        else Ok ((), Just (prev ++ next, [List.length prev]))
            ) (current model) newC
            |> \res -> case res of
                Ok (_, Just (_,c)) -> {model | cursor = c}
                _ -> model

{- ## Scope traversal -}

type TraversalCase_ =
    TextCase (List ScopeElement) String Int (List ScopeElement)
    | IntermediateCase (List ScopeElement) (List ScopeElement)
type TraversalError_  =
    BrokenCaret -- For when the position cannot be found
    | CannotSplitArgument_
    | CannotModifySection_
    | NoTextToChange_

traverse: (ScopeDetail -> TraversalCase_ -> Result TraversalError_ (a, Maybe (List ScopeElement, CaretPosition)))
    -> Scope -> CaretPosition -> Result TraversalError_ (a, Maybe (List ScopeElement, CaretPosition))
traverse process (Scope detail children) caret = case caret of
    [] -> IntermediateCase [] children |> process detail
    [x] -> IntermediateCase (List.take x children) (List.drop x children) |> process detail
    (x::y::others) -> let (prev, next) = (List.take x children, List.drop x children) in
        case next of
            (StrElement str :: after) -> TextCase prev str y after |> process detail
            (Fixed f :: after) -> case Array.get y f.params of
                Nothing -> Err BrokenCaret
                Just (Scope inDetail inChildren, override) -> traverse process (Scope inDetail inChildren) others
                    |> Result.andThen (\res -> case res of
                        (r, Nothing) -> if detail.immutable then Ok (r, Just (children, caret))
                            else Ok (r, Just (prev ++ after, [x]))
                        (r, Just (newChildren, pos)) -> let newParams = Array.set y (Scope inDetail newChildren, override) f.params in
                            Ok (r, Just (prev ++ (Fixed {f | params = newParams} :: after), x::y::pos))
                    )
            (Bracket kids :: after) -> traverse process (Scope detail kids) others
                |> Result.map (\res -> case res of
                    (r, Nothing) -> (r, Just (prev ++ after, [x]))
                    (r, Just (newChildren, pos)) -> (r, Just (prev ++ (Bracket newChildren) :: after, x::y::pos))
                )
            (InnerScope (Scope inD inC) :: after) -> traverse process (Scope inD inC) others
                |> Result.map (\res -> case res of
                    (r, Nothing) -> (r, Just (children, caret)) -- InnerScopes cannot be deleted (they're for parameter inputs)
                    (r, Just (newChildren, pos)) -> (r, Just (prev ++ (InnerScope (Scope inD newChildren)) :: after, x::y::pos))
                )
            _ -> Err BrokenCaret

{- ## Check Matches -}

matchDict_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Dict.Dict (Int, String) (String, Math.FunctionProperty Rules.FunctionProp)
matchDict_ funcProp = Dict.toList funcProp
    |> List.concatMap (\(k, v) -> k :: (v.property |> Math.getState |> .alternativeNames)
        |> List.map (\match -> ((-(String.length match), match), (k, v.property)))
    )
    |> Dict.fromList

checkMatches_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Animation.Tracker -> Model msg -> (Model msg, Animation.Tracker)
checkMatches_ funcProp t model = let match word = matchDict_ funcProp |> Dict.foldl (\(_, w) _ res -> res || String.endsWith w word) False in
    traverse (\_ c -> case c of
        TextCase prev str num next -> Ok
            (match (String.left num str), Just (prev ++ (StrElement str :: next), [List.length prev, num]))
        IntermediateCase prev next -> let beforeLength = List.length prev - 1 in Ok
            (   if beforeLength < 0 then False
                else case List.drop beforeLength prev |> List.head of
                    Just (StrElement str) -> match str
                    _ -> False
            ,   Just (prev ++ next, [List.length prev])
            )
    )
    (current model) model.cursor
    |> \res -> case res of
        Ok (found, _) -> let (newHeight, newT) = Animation.setEase t (if found then 1.5 else 0) model.matchFoundText in
            ({model | matchFoundText = newHeight}, newT)
        _ -> (model, t)

{- ## Insertion -}

insertChar_: Char -> Model msg -> Result String (Model msg)
insertChar_ char model = let (Scope detail children) = current model in
    traverse
    (\parent input -> if parent.inseparable && char == ',' then Err CannotSplitArgument_
        else Ok
        (   ()
        ,   case input of
            TextCase prev str num next -> (String.left num str) ++String.fromChar char ++ (String.dropLeft num str)
                |> \newStr -> Just (prev ++ (StrElement newStr :: next), [List.length prev, num+1])
            IntermediateCase prev next -> case next of
                (StrElement str::after) -> Just (prev ++ (StrElement (String.cons char str) :: after), [List.length prev, 1])
                _ -> let beforeLength = List.length prev - 1 in
                    case List.drop beforeLength prev of
                        [StrElement str] -> str ++ String.fromChar char
                            |> \newStr -> Just (List.take beforeLength prev ++ (StrElement newStr :: next), [List.length prev])
                        _ -> Just (prev ++ (StrElement (String.fromChar char)::next), [List.length prev + 1])
        )
    )
    (Scope detail children) model.cursor
    |> \res -> case res of
        Ok (_, Just (l,c)) -> Ok {model | history = History.stage (Scope detail l) model.history, cursor = c}
        _ -> Err "Something went wrong"

insertSpace_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Animation.Tracker
    -> Model msg -> Result String (Model msg, Animation.Tracker)
insertSpace_ funcProp t model =
    let
        (Scope detail children) = current model
        updateIndex currentIndex index = case index of
            Nothing -> [currentIndex + 1]
            Just n -> [currentIndex, n, 0]
        updateString prev front back next = getSubstitution_ funcProp front
            |> Result.map (\res -> case res of
                ("", fixed, index) -> if back == ""
                    then ((), Just (prev ++ (fixed :: next), updateIndex (List.length prev) index))
                    else ((), Just (prev ++ (fixed :: StrElement back :: next), updateIndex (List.length prev) index))
                (before, fixed, index) -> if back == ""
                    then ((), Just (prev ++ (StrElement before :: fixed :: next), updateIndex (List.length prev + 1) index))
                    else ((), Just (prev ++ (StrElement before :: fixed :: StrElement back :: next), updateIndex (List.length prev + 1) index))
            )

    in
    traverse
    (\_ input -> case input of
        TextCase prev str num next -> updateString prev (String.left num str) (String.dropLeft num str) next
        IntermediateCase prev next -> let beforeLength = List.length prev - 1 in
            case List.drop beforeLength prev of
                [StrElement str] -> updateString (List.take beforeLength prev) str "" next
                _ -> Err NoTextToChange_
    )
    (Scope detail children) model.cursor
    |> \res -> case res of
        Ok (_, Just (l,c)) -> let (newHeight, newT) = Animation.setEase t 0 model.matchFoundText in
            Ok ({model | history = History.stage (Scope detail l) model.history, cursor = c, matchFoundText = newHeight}, newT)
        Err NoTextToChange_ -> Err "No text to convert to a symbol"
        _ -> Err "Something went wrong"

getSubstitution_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String -> Result TraversalError_ (String, ScopeElement, Maybe Int)
getSubstitution_ funcProp str = if String.isEmpty str then Err NoTextToChange_
    else matchDict_ funcProp
    |> Dict.foldl (\(_, match) (key, prop) res -> case res of
        Just _ -> res
        Nothing -> if String.endsWith match str
            then
                let
                    (fixed, latex, _) = funcPropToLatex_ key (Just {property=prop})
                    element = fixedFrom_ (if Set.member key defaultOps then key else "\\" ++ key) fixed Array.empty latex
                    first = case element of
                        Fixed n -> n.firstNode
                        _ -> Nothing
                in
                Just (String.left (String.length str - String.length match) str, element, first)
            else Nothing
    ) Nothing
    |> \res -> case res of
        Just r -> Ok r
        Nothing ->
            let
                funcName = String.right 1 str
                (_, latex, _) = funcPropToLatex_ funcName Nothing
            in
            Ok (String.dropRight 1 str, fixedFrom_ ("\\" ++ funcName) False Array.empty latex, Just 0)

insertScopeElement_: Animation.Tracker -> Maybe Int -> ScopeElement -> Model msg -> Result String (Model msg, Animation.Tracker)
insertScopeElement_ t firstNode element model = let (Scope detail children) = current model in
    traverse
    (\_ input -> Ok
        (   ()
        ,   case input of
                TextCase prev str num next -> Just
                    (   prev
                        ++  ( StrElement (String.left num str) :: element
                            :: StrElement (String.dropLeft num str) :: next
                            )
                    ,   case firstNode of
                        Nothing -> [List.length prev + 2]
                        Just n -> [List.length prev + 1, n, 0]
                    )
                IntermediateCase prev next -> Just
                    (   prev ++ (element :: next)
                    ,   case firstNode of
                        Nothing -> [List.length prev + 1]
                        Just n -> [List.length prev, n, 0]
                    )
        )
    )
    (Scope detail children) model.cursor
    |> \res -> case res of
        Ok (_, Just (l,c)) -> let (newHeight, newT) = Animation.setEase t 0 model.matchFoundText in
            Ok ({model | history = History.stage (Scope detail l) model.history, cursor = c, matchFoundText = newHeight}, newT)
        _ -> Err "Something went wrong"

latexArray_: Bool -> Latex.Model DirectionOverride -> Result String (Array.Array (Scope, DirectionOverride))
latexArray_ fixed latex = Latex.extractArgs latex
    |> Result.map (\dict -> if fixed
        then Dict.foldl (\_ override -> Array.push (Scope defaultScopeDetail [], override.state)) Array.empty dict
        else Array.fromList [(Scope defaultScopeDetail [], noOverride_)]
    )

{- ## Deletion -}

delete_: Bool -> Model msg -> Model msg
delete_ forwards model = let (Scope detail children) = current model in
    traverse
    (\scopeDetail input -> if scopeDetail.immutable then Err CannotModifySection_
        else Ok ( ()
        ,   case input of
            TextCase prev str num next -> if forwards
                then Just
                    (   prev ++ (StrElement (String.left num str ++ String.dropLeft (num+1) str) :: next)
                    ,   if num + 1 == String.length str then [List.length prev + 1] else [List.length prev, num]
                    )
                else Just
                    (   prev ++ (StrElement (String.left (num - 1) str ++ String.dropLeft num str) :: next)
                    ,   if num == 1 then [List.length prev] else [List.length prev, num - 1]
                    )
            IntermediateCase prev next -> if forwards
                then Just
                    (   case List.head next of
                        Nothing -> prev
                        Just (StrElement str) -> if String.length str == 1
                            then prev ++ List.drop 1 next
                            else prev ++ (StrElement (String.dropLeft 1 str) :: List.drop 1 next)
                        Just (Fixed _) -> prev ++ List.drop 1 next
                        Just (Bracket _) -> prev ++ List.drop 1 next
                        Just (InnerScope _) -> prev ++ List.drop 1 next
                    ,   [List.length prev]
                    )
                else let beforeLength = List.length prev - 1 in
                    case List.drop beforeLength prev of
                        [StrElement str] -> if String.length str == 1
                            then Just (List.take beforeLength prev ++ next, [beforeLength])
                            else Just (   List.take beforeLength prev ++ (StrElement (String.dropRight 1 str)::next)
                                ,   [List.length prev]
                                )
                        [Fixed _] -> Just (List.take beforeLength prev ++ next, [beforeLength])
                        [Bracket _] -> Just (List.take beforeLength prev ++ next, [beforeLength])
                        [InnerScope _] -> Just (List.take beforeLength prev ++ next, [beforeLength])
                        _ -> Nothing
        )
    )
    (Scope detail children) model.cursor
    |> \res -> case res of
        Ok (_, Just (l, c)) -> {model | history = History.stage (Scope detail l) model.history, cursor = c}
        Ok (_, Nothing) -> {model | history = History.flushAndCommit (Scope detail []) model.history ,cursor = [0]}
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
            then Ok (res ++ f.text)
            else Helper.resultArray (\index (innerScope, _) innerRes ->
                toEntryString_ innerScope
                |> Result.map (\str -> Array.set index str innerRes)
            ) (Array.repeat (Array.length f.params) "") f.params
            |> Result.map (\arr -> res ++ f.text ++ "(" ++ (Array.toList arr |> String.join ",") ++ ")" )
        InnerScope inner -> toEntryString_ inner
            |> Result.map (\inStr -> res ++ inStr)
    ) "" children

{- ## Suggestion -}

getSuggestions_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> Scope -> CaretPosition
    -> List (String, Html.Html Event)
getSuggestions_ dicts origin caret =
    let
        findString (Scope detail children) pos = case pos of
            [] -> ""
            [x] -> case List.drop x children |> List.head of
                Just (StrElement str) -> str
                _ -> case List.drop (x-1) children |> List.head of
                    Just (StrElement str) -> str
                    _ -> ""
            (x::y::others) -> case List.drop x children |> List.head of
                Nothing -> ""
                Just (StrElement str) -> str
                Just (Fixed f) -> case Array.get y f.params of
                    Nothing -> ""
                    Just (scope,_) -> findString scope others
                Just (Bracket kids) -> findString (Scope detail kids) others
                Just (InnerScope scope) -> findString scope others
    in
        findString origin caret
        |> displaySuggestions_ dicts

defaultOps: Set.Set String
defaultOps = Set.fromList ["+", "-", "*", "/", "="]

displaySuggestions_: Dict.Dict String {a | property: Math.FunctionProperty Rules.FunctionProp} -> String
    -> List (String, Html.Html Event)
displaySuggestions_ functions input = let inputOrder = letterOrder_ input in
    Dict.keys functions
    |> List.filter (\key -> Set.member key defaultOps |> not)
    |> List.map (\key -> let (fixed, latex, numArgs) = Dict.get key functions |> funcPropToLatex_ key in
        (   (   cosineCorrelation_ inputOrder (letterOrder_ key)
            ,   numArgs
            )
        ,   (   key
            -- don't use onClick, because some onBlurs get triggered first
            ,   Html.a
                [   class "clickable"
                ,   HtmlEvent.onPointerCapture identity
                    (\_ _ -> InsertFixed (fixedFrom_ ("\\" ++ key) fixed Array.empty latex))
                ]
                [toHtmlBlock_ latex]
            )
        )
    )
    |> List.sortBy (\((corr, args), _) -> (-corr, -args))
    |> List.map Tuple.second
    |> (++)
        [   (   "+"
            ,   Html.a [class "clickable", HtmlEvent.onPointerCapture identity (\_ _ -> InsertFixed addOp_)]
                [[Latex.Text {state=(), style=Nothing} "+"] |> toHtmlBlock_]
            )
        ,   (   "-"
            ,   Html.a [class "clickable", HtmlEvent.onPointerCapture identity (\_ _ -> InsertFixed subOp_)]
                [[Latex.Text {state=(), style=Nothing} "-"] |> toHtmlBlock_]
            )
        ,   (   "*"
            ,   Html.a [class "clickable", HtmlEvent.onPointerCapture identity (\_ _ -> InsertFixed timesOp_)]
                [[Latex.SymbolPart {state=(), style=Nothing} Latex.CrossMultiplcation] |> toHtmlBlock_]
            )
        ,   (   "/"
            ,   Html.a [class "clickable", HtmlEvent.onPointerCapture identity (\_ _ -> InsertFixed divideOp_) ]
                [[Latex.SymbolPart {state=(), style=Nothing} Latex.Division] |> toHtmlBlock_]
            )
        ,   (   "="
            ,   Html.a [class "clickable", HtmlEvent.onPointerCapture identity (\_ _ -> InsertFixed equalOp_)]
                [[Latex.Text {state=(), style=Nothing} "="] |> toHtmlBlock_]
            )
        ]

toHtmlBlock_: Latex.Model s -> Html.Html msg
toHtmlBlock_ latex = let frame = MathIcon.latexToFrames latex in
    let (scale, width) = BrickSvg.getScaleAndWidth latex in
    MathIcon.initStatic (Just scale) frame
    |> BrickSvg.brick 0 width 0 1 1 BrickSvg.Leaf True []
    |> \brick -> BrickSvg.bricks width 1 [brick]

funcPropToLatex_: String -> Maybe {a | property: Math.FunctionProperty Rules.FunctionProp} -> (Bool, Latex.Model (), Int)
funcPropToLatex_ key funcProp =
    let
        emState = {state = (), style=Just Latex.Emphasis}
        regularState = {state = (), style=Just Latex.Emphasis}
        createLatex fixed name args =
            [   Latex.Text emState name
            ,   Latex.Bracket emState
                (   List.range 1 args
                |>  List.map (Latex.Argument regularState)
                |>  List.intersperse (Latex.Text (if fixed then emState else regularState) ",")
                )
            ]
    in
    case funcProp of
    Nothing -> (False, [Latex.Text emState key, Latex.Bracket emState [Latex.Argument regularState 1]], 1)
    Just value -> case value.property of
        Math.VariableNode n -> case n.state.latex of
            Just l -> (True, l, 0)
            Nothing -> (True, [Latex.Text emState n.name], 0)
        Math.UnaryNode n -> case n.state.latex of
            Just l -> (True, l, 1)
            Nothing -> let l = createLatex False key 1 in (True, l, 1)
        Math.BinaryNode n -> case n.state.latex of
            Just l -> (n.associative == Nothing, l, 2)
            Nothing -> if n.associative == Nothing
                then let l = createLatex False key 2 in (True, l, 2)
                else let l = createLatex False key 1 in (False, l, 2)
        Math.GenericNode n -> let numArgs = Maybe.withDefault 0 n.arguments in
            case n.state.latex of
            Just l -> (True, l, numArgs)
            Nothing -> let l = createLatex True key numArgs in (True, l, numArgs)
        _ -> let l = createLatex False key 1 in (False, l, 1)

fixedFrom_: String -> Bool -> Array.Array (List ScopeElement) -> Latex.Model () -> ScopeElement
fixedFrom_ text fixed args latex =
    let
        defaultStart =
            (   {topLast = Nothing, midLast = Nothing, botLast  = Nothing}
            ,   Dict.singleton -1 noOverride_
            )
        updateRight now (prev, dict) =
            (   {topLast = Nothing, midLast = Just now, botLast = Nothing}
            ,   case (prev.topLast, prev.midLast, prev.botLast) of
                -- All in the middle
                (Nothing, Just mid, Nothing) -> Dict.insert now {noOverride_ | left = Just mid} dict
                    |> Dict.update mid (Maybe.map(\v -> {v | right = Just now}))
                -- subscript
                (Nothing, Just mid, Just bot) -> Dict.insert now {noOverride_ | left = Just bot} dict
                    |> Dict.update mid (Maybe.map(\v -> {v | right = Just bot}))
                    |> Dict.update bot (Maybe.map(\v -> {v | right = Just now}))
                -- superscript
                (Just top, Just mid, Nothing) ->  Dict.insert now {noOverride_ | left = Just top} dict
                    |> Dict.update mid (Maybe.map(\v -> {v | right = Just now}))
                    |> Dict.update top (Maybe.map(\v -> {v | right = Just now}))
                -- subscript and superscript
                (Just top, Just mid, Just bot) ->  Dict.insert now {noOverride_ | left = Just top} dict
                    |> Dict.update mid (Maybe.map(\v -> {v | right = Just now}))
                    |> Dict.update top (Maybe.map(\v -> {v | right = Just now}))
                    |> Dict.update bot (Maybe.map(\v -> {v | right = Just now}))
                -- fraction
                (Just top, Nothing, Just bot) ->  Dict.insert now {noOverride_ | left = Just top} dict
                    |> Dict.update top (Maybe.map(\v -> {v | right = Just now}))
                    |> Dict.update bot (Maybe.map(\v -> {v | right = Just now}))
                -- fraction with no argument on the bottom
                (Just top, Nothing, Nothing) ->  Dict.insert now {noOverride_ | left = Just top} dict
                    |> Dict.update top (Maybe.map(\v -> {v | right = Just now}))
                -- fraction with no argument on the top
                (Nothing, Nothing, Just bot) ->  Dict.insert now {noOverride_ | left = Just bot} dict
                    |> Dict.update bot (Maybe.map(\v -> {v | right = Just now}))
                -- start of the latex
                (Nothing, Nothing, Nothing) -> Dict.insert now {noOverride_ | left = Nothing} dict
                    |> Dict.update -1 (Maybe.map(\v -> {v | right = Just now}))
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
                    withFallback b a = case a of
                        Just _ -> a
                        Nothing -> b
                in
                (   {   topLast = getLastest topRef
                    ,   midLast = Nothing
                    ,   botLast = getLastest botRef
                    }
                ,   Dict.update (Maybe.withDefault -1 prev) (Maybe.map (\v -> {v | right = topFirst |> withFallback botFirst})) dict
                    |> \dict1 -> Dict.foldl (\k v -> Dict.insert k {v | down = v.down |> withFallback botFirst}) dict1 newTop
                    |> \dict2 -> Dict.foldl (\k v -> Dict.insert k {v | up = v.up |> withFallback topFirst}) dict2 newBot
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
    (\name args -> let (fixed, latex, _) = Dict.get name funcDict |> funcPropToLatex_ name in
        fixedFrom_ ("\\" ++ name) fixed
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

