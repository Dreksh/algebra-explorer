module Backtrack exposing (Continuation, fail, init, searchOrdered, searchUnordered, return, toMaybe)
import Http exposing (Response(..))

-- For continuing from a previous attempt
type Continuation state =
    Start_ {initial: state}                                                                 -- Initial state
    | Done_                                                                                 -- Negative result / No more results
    | Select_ {initial: state, result: state}                                               -- Positive result (do not retry)
    | Ordered_ {initial: state, result: state, children: List (Continuation state)}         -- Marking for choice-child pairs (ordered match)
    | Unordered_ {initial: state, result: state, children: List (Int, Continuation state)}  -- Marking for choice made per child (unordered match)

init: state -> Continuation state
init initial = Start_ {initial = initial}

fail: Continuation state
fail = Done_

return: (state -> Maybe state) -> Continuation state -> Continuation state
return func initial = case initial of
    Start_ s -> case func s.initial of
        Just newS -> Select_ {initial = s.initial, result = newS}
        Nothing -> Done_
    _ -> Done_

toMaybe: Continuation state -> Maybe state
toMaybe c = case c of
    Select_ s -> Just s.result
    Ordered_ s -> Just s.result
    Unordered_ s -> Just s.result
    Start_ _ -> Nothing -- start is not valid, it's a blank initial state
    Done_ -> Nothing

searchOrdered: (slot -> choice -> Continuation state -> Continuation state) -> List slot -> List choice -> Continuation state -> Continuation state
searchOrdered func slots choices initial =
    let
        checkNext prev otherSlot otherChoice result = case toMaybe result |> Maybe.map (\s -> searchOrdered func otherSlot otherChoice (Start_ {initial = s}) ) of
            Just (Ordered_ newS) -> Ordered_ {newS | children = (result :: newS.children), initial = prev}
            _ -> Done_
    in
        if List.length slots /= List.length choices
        then Done_
        else case initial of
            Start_ s -> case (slots, choices) of
                ([], []) -> Ordered_ {initial = s.initial, result = s.initial, children = []}
                ((startSlot::otherSlot), (startChoice::otherChoice)) -> func startSlot startChoice initial
                    |> checkNext s.initial otherSlot otherChoice
                _ -> Done_
            Ordered_ s -> case (slots, choices, s.children) of
                ([currentSlot], [currentChoice], [currentToken]) -> func currentSlot currentChoice currentToken
                        |> checkNext s.initial [] []
                ((startSlot::otherSlot), (startChoice::otherChoice), (startToken::otherToken)) -> case searchOrdered func otherSlot otherChoice (Ordered_ {s | children = otherToken}) of
                    Ordered_ newS -> Ordered_ {newS | children = (startToken::newS.children), initial = s.initial}
                    _ -> func startSlot startChoice startToken
                        |> checkNext s.initial otherSlot otherChoice
                _ -> Done_
            _ -> Done_

searchUnordered: (slot -> choice -> Continuation state -> Continuation state) -> List slot -> List choice -> Continuation state -> Continuation state
searchUnordered func slots choices initial = unordered_ func slots [] choices initial

unordered_: (slot -> choice -> Continuation state -> Continuation state) -> List slot -> List choice -> List choice -> Continuation state -> Continuation state
unordered_ func slots prechoice choices initial =
    let
        checkNext prev nextSlot currentChoice nextChoice result = case toMaybe result |> Maybe.map (\s -> unordered_ func nextSlot [] (prechoice ++ nextChoice) (Start_ {initial = s})) of
            Nothing -> unordered_ func slots (prechoice ++ [currentChoice]) nextChoice (Start_ {initial = prev})
            Just (Unordered_ newS) -> Unordered_ {newS | children = (List.length prechoice, result)::newS.children, initial = prev }
            _ -> Done_
    in
        case initial of
            Start_ s -> case (slots, choices) of
                ([], _) -> Unordered_ {initial = s.initial, result = s.initial, children = []}
                (_, []) -> Done_
                ((currentSlot::nextSlot),(currentChoice::nextChoice)) -> func currentSlot currentChoice initial
                    |> checkNext s.initial nextSlot currentChoice nextChoice
            Unordered_ s -> case (slots, choices) of
                ([], _) -> Done_
                (_, []) -> Done_
                ((currentSlot::nextSlot),(currentChoice::nextChoice)) -> case s.children of
                    ((num, continuation)::others) -> if num > 0
                        then unordered_ func slots (prechoice ++ [currentChoice]) nextChoice (Unordered_ {s | children = (num - 1,continuation)::others})
                        else case unordered_ func nextSlot [] (prechoice++nextChoice) (Unordered_ {s | children = others}) of
                            Unordered_ newS -> Unordered_ {newS | children = (num, continuation)::newS.children, initial = s.initial}
                            _ -> func currentSlot currentChoice continuation
                                |> checkNext s.initial nextSlot currentChoice nextChoice
                    _ -> Done_
            _ -> Done_