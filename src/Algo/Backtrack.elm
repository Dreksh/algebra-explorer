module Algo.Backtrack exposing (Continuation, Evaluator,
    fail, init, return, map, getState, run,
    orderedStack, unorderedStack
    )
import Helper

-- For continuing from a previous attempt
type alias Continuation state =
    {   initial: state
    ,   progress: Maybe (state, Progress_ state)
    }

type Progress_ state =
    Match_                                  -- All is consumed
    | Choice_ Int (Continuation state)      -- Remove specific choice each time
    | List_ (List (Continuation state))     -- Track child Continuation through multiple steps

type alias Evaluator state choice = List choice -> Continuation state -> Maybe (Continuation state)

init: state -> Continuation state
init initial = Continuation initial Nothing

fail: Maybe (Continuation state)
fail = Nothing

-- Returns once only, no other choices
return: (state -> Maybe state) -> Continuation state -> Maybe (Continuation state)
return func token = case token.progress of
    Nothing -> func token.initial |> Maybe.map (\newS -> {token | progress = Just (newS, Match_)})
    _ -> Nothing

-- This only supplies additional information to the continuation, overwriting the initial state
map: (state -> state) -> Continuation state -> Continuation state
map func token = {token | initial = func token.initial}

getState: Continuation state -> Maybe state
getState c = Maybe.map Tuple.first c.progress

{-
## Run
-}

run: List (Evaluator state choice) -> List choice -> Continuation state -> Maybe (Continuation state)
run evals choices tok = case evals of
    [] -> case tok.progress of
        Nothing -> Just {tok | progress = Just (tok.initial, List_ [])}
        Just _ -> Nothing
    (eval::nextEvals) -> case tok.progress of
        Nothing -> processNext_ eval nextEvals choices tok
        Just (oldRes,List_ (currentTok::otherToks)) -> currentTok.progress
            |> Maybe.andThen (\(_, childP) -> removeSelectedChildren_ childP choices)
            |> Maybe.andThen (\newChoices -> case run nextEvals newChoices {tok | progress = Just (oldRes, List_ otherToks)} of
                Nothing -> processNext_ eval nextEvals choices currentTok
                Just newNext -> case newNext.progress of
                    Just (finalRes, List_ finalList) -> Just {tok | progress = Just (finalRes, List_ (currentTok::finalList))}
                    _ -> Nothing
            )
        _ -> Nothing

processNext_: Evaluator state choice -> List (Evaluator state choice) -> List choice -> Continuation state -> Maybe (Continuation state)
processNext_ eval nextEvals choices inputTok = eval choices inputTok
    |> Maybe.andThen (\childTok -> case childTok.progress of
        Just (res, p) -> removeSelectedChildren_ p choices
            |> Maybe.andThen (\newChoices -> case run nextEvals newChoices {initial = res, progress = Nothing} of
                Nothing -> processNext_ eval nextEvals choices childTok
                Just finalTok -> case finalTok.progress of
                    Just (finalRes, List_ list) -> Just {inputTok | progress = Just (finalRes, List_ (childTok::list))}
                    _ -> Nothing
            )
        _ -> Nothing -- evaluating should always produce a result
    )

removeSelectedChildren_: Progress_ state -> List choice -> Maybe (List choice)
removeSelectedChildren_ p c = case p of
    Match_ -> Just []
    Choice_ index _ -> if index < 0 || index >= List.length c then Nothing
        else Just ((List.take index c) ++ (List.drop (index+1) c))
    List_ list -> Helper.maybeList (\tok children -> tok.progress
        |> Maybe.andThen (\(_, childP) -> removeSelectedChildren_ childP children)
        )
        c list

{-
## Common list-iterating operations
-}

orderedStack: (slot -> choice -> Continuation state -> Maybe (Continuation state)) -> List slot -> List (Evaluator state choice)
orderedStack func = List.map (\s choices tok ->
    List.head choices
    |> Maybe.andThen (\c -> case tok.progress of
        Nothing -> func s c tok
        Just (_, Choice_ 0 childTok) -> func s c childTok
        _ -> Nothing
    )
    |> Maybe.andThen (\newChild ->
        getState newChild
        |> Maybe.map (\newRes -> {tok | progress = Just (newRes, Choice_ 0 newChild)})
    )
    )

unorderedStack: (slot -> choice -> Continuation state -> Maybe (Continuation state)) -> List slot -> List (Evaluator state choice)
unorderedStack func = List.map (\s -> checkOption_ func s 0)

checkOption_: (slot -> choice -> Continuation state -> Maybe (Continuation state)) -> slot -> Int -> List choice -> Continuation state -> Maybe (Continuation state)
checkOption_ func slot index choices tok = case choices of
    [] -> Nothing
    (choice::other) ->
        let
            testChoice token = case func slot choice token of
                Nothing -> checkOption_ func slot (index+1) other {tok | progress = Nothing} -- Try next
                Just child -> getState child |> Maybe.map (\res -> {tok | progress = Just (res, Choice_ index child)})
        in
            case tok.progress of
                Nothing -> testChoice tok
                Just (old, Choice_ num childTok) -> if num < 0 || num >= List.length choices then Nothing
                    else if num == 0 then testChoice childTok
                    else checkOption_ func slot num (List.drop num choices) {tok | progress = Just (old, Choice_ 0 childTok)}
                _ -> Nothing
