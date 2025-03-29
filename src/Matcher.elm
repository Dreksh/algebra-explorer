module Matcher exposing (Equation, Matcher(..), MatchResult, State,
    getID, parseEquation, selectedSubtree,
    groupSubtree, ungroupSubtree,
    addMatch, matchNode, matchSubtree
    )

import Dict
import Math
import Set
-- Ours
import Backtrack
import Helper

type State state = State_ Int state

getID: State state -> Int
getID s = case s of
    State_ rootID _ -> rootID

type alias Tracker_ state =
    {   nextID: Int
    ,   parent: Dict.Dict Int Int
    ,   defaultState: state
    }

type alias Equation state =
    {   root: Math.Tree (State state)
    ,   tracker: Tracker_ state
    }

-- Ignore only Associativity for now, given that we don't have any functions that follow that for now
type Matcher =
    AnyMatcher {name: String, arguments: List Matcher} -- Unknown variables and functions
    | RealMatcher {value: Float} -- Numbers
    | ExactMatcher {name: String, arguments: List Matcher} -- Known functions or variables, exact arguments
    | CommutativeMatcher {name: String, arguments: List Matcher }  -- Unordered (set-based)
    | CommutativeAssociativeMatcher {name: String, arguments: List Matcher } -- Unordered & group-able

type alias MatchResult state =
    {   nodes: Dict.Dict Int (Math.Tree (State state))
    ,   matches: Dict.Dict String (Source_ state)
    }

type Source_ state =
    Internal_ (Set.Set Int)
    | External_ (Math.Tree ())

addMatch: String -> Math.Tree () -> MatchResult state -> MatchResult state
addMatch key value result = {result | matches = Dict.insert key (External_ value) result.matches}

-- ## parserEquation: Also assigned ID
parseEquation: state -> String -> Result String (Equation state)
parseEquation default input = Math.parse input
    |> Result.map (
        processID_ -1 {nextID = 0, parent = Dict.empty, defaultState = default}
        >> (\(newRoot, tracker) -> {root = newRoot, tracker = {tracker | parent = Dict.remove 0 tracker.parent}})
    )

processID_: Int -> Tracker_ state -> Math.Tree () -> (Math.Tree (State state), Tracker_ state)
processID_ parent tracker oldRoot =
    let
        id = tracker.nextID
        (state, newTracker) = addNode_ parent tracker
        processChildren = Helper.listMapWithState (processID_ id)
    in
    case oldRoot of
        Math.RealNode s -> (Math.RealNode {state = state, value = s.value}, newTracker)
        Math.VariableNode s -> (Math.VariableNode {state = state, name = s.name}, newTracker)
        Math.UnaryNode s -> let (newChild, finalT) = processID_ id newTracker s.child in
            (Math.UnaryNode {state = state, name = s.name, child = newChild}, finalT)
        Math.BinaryNode s -> let (newChildren, finalT) = processChildren newTracker s.children in
            (Math.BinaryNode {state = state, name = s.name, associative = s.associative, commutative = s.commutative, children = newChildren}, finalT)
        Math.GenericNode s -> let (newChildren, finalT) = processChildren newTracker s.children in
            (Math.GenericNode {state = state, name = s.name, children = newChildren}, finalT)
        Math.DeclarativeNode s -> let (newChildren, finalT) = processChildren newTracker s.children in
            (Math.DeclarativeNode {state = state, name = s.name, children = newChildren}, finalT)

addNode_: Int -> Tracker_ state -> (State state, Tracker_ state)
addNode_ parent tracker = (State_ tracker.nextID tracker.defaultState, {tracker | nextID = tracker.nextID + 1, parent = Dict.insert tracker.nextID parent tracker.parent})

-- ## selectSubtree: If there is a subtree, it returns the root node as well as the affected subtrees
selectedSubtree: Set.Set Int -> Equation state -> Result String (Math.Tree (State state), Int)
selectedSubtree ids eq = case affectedSubtree_ ids eq.tracker.parent of
    Nothing -> Err "Nodes not found"
    Just (id, nodes) ->
        processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq -> Ok (subEq.root, subEq)) eq
        |> Result.map (\(root, _) ->
            (   root
            ,   (   case root of
                Math.RealNode _ -> 0
                Math.VariableNode _ -> 0
                Math.UnaryNode _ -> 1
                Math.BinaryNode s -> s.children |> List.filter (\n -> Set.member (Math.getState n |> getID) nodes) |> List.length
                Math.GenericNode s -> s.children |> List.filter (\n -> Set.member (Math.getState n |> getID) nodes) |> List.length
                Math.DeclarativeNode s -> s.children |> List.filter (\n -> Set.member (Math.getState n |> getID) nodes) |> List.length
            )
            )
        )

toStatelessTree_: Math.Tree s -> Math.Tree ()
toStatelessTree_ root = case root of
    Math.RealNode s -> Math.RealNode {state = (), value = s.value}
    Math.VariableNode s -> Math.VariableNode {state = (), name = s.name}
    Math.UnaryNode s -> Math.UnaryNode {state = (), name = s.name, child = toStatelessTree_ s.child}
    Math.BinaryNode s -> Math.BinaryNode
        {state = (), name = s.name, associative = s.associative, commutative = s.commutative, children = List.map toStatelessTree_ s.children}
    Math.GenericNode s -> Math.GenericNode {state = (), name = s.name, children = List.map toStatelessTree_ s.children}
    Math.DeclarativeNode s -> Math.DeclarativeNode {state = (), name = s.name, children = List.map toStatelessTree_ s.children}

affectedSubtree_: Set.Set Int -> Dict.Dict Int Int -> Maybe (Int, Set.Set Int)
affectedSubtree_ nodes parent = case Set.toList nodes of
    [] -> Nothing
    (start::others) ->
        let
            increment id dict = case Dict.get id dict of
                Nothing -> Dict.insert id 1 dict
                Just c -> Dict.insert id (c+1) dict
            count id parentCount = case Dict.get id parent of
                Nothing -> increment id parentCount
                Just p -> count p parentCount |> increment id

        in
        List.foldl count Dict.empty others
        |> (\countDict ->
            let
                target = (List.length others) + 1
                search node dict = let newDict = increment node dict in
                    Dict.get node newDict
                    |> Maybe.andThen (\c -> if c == target then Just node else Nothing)
                    |> (\r -> case r of
                        Just n -> (n, newDict)
                        Nothing -> case Dict.get node parent of
                            Nothing -> (node, newDict)
                            Just p -> search p newDict
                    )
            in
            search start countDict
        )
        |> (\(root, newDict) -> Just (root, Dict.keys newDict |> Set.fromList))

searchPath_: Dict.Dict Int Int -> Int -> List Int
searchPath_ map id =
    let
        recursive input = case Dict.get input map of
            Nothing -> [input]
            Just a -> input :: recursive a
    in
    recursive id |> List.reverse

processSubtree_: List Int -> (Equation state -> Result String (result, Equation state)) -> Equation state -> Result String (result, Equation state)
processSubtree_ path processor eq =
    let
        processChildren p children = case children of
            [] -> Err "blah"
            (current::others) -> case processSubtree_ p processor {eq | root = current} of
                Err _ -> processChildren p others |> Result.map (\(r, newChildren, t) -> (r, current::newChildren, t))
                Ok (a, newEq) -> Ok (a, newEq.root::others, newEq.tracker)
        id = Math.getState eq.root |> getID
    in
    case path of
        [] -> Err "blah"
        [expected] -> if expected /= id then Err "blah"
            else processor eq
        (expected::next) -> if expected /= id then Err "blah"
            else case eq.root of
                Math.RealNode _ -> Err "blah"
                Math.VariableNode _ -> Err "blah"
                Math.UnaryNode s -> processSubtree_ next processor {eq | root = s.child} |> Result.map (\(r, newEq) -> (r, {newEq | root = Math.UnaryNode {s | child = newEq.root}}))
                Math.BinaryNode s -> processChildren next s.children |> Result.map (\(r, children, t) -> (r, {root = Math.BinaryNode {s | children = children}, tracker = t}))
                Math.GenericNode s -> processChildren next s.children |> Result.map (\(r, children, t) -> (r, {root = Math.GenericNode {s | children = children}, tracker = t}))
                Math.DeclarativeNode s -> processChildren next s.children |> Result.map (\(r, children, t) -> (r, {root = Math.DeclarativeNode {s | children = children}, tracker = t}))

matchNode: Matcher -> Math.Tree (State state) -> Bool
matchNode matcher root = case (matcher, root) of
    (AnyMatcher _, Math.DeclarativeNode _) -> False
    (AnyMatcher _, _) -> True
    (RealMatcher m, Math.RealNode n) -> m.value == n.value
    (ExactMatcher m, Math.VariableNode n) -> m.name == n.name && List.isEmpty m.arguments
    (ExactMatcher m, Math.GenericNode n) -> m.name == n.name && List.length m.arguments == List.length n.children
    (CommutativeMatcher m, Math.DeclarativeNode n) -> m.name == n.name
    (CommutativeMatcher m, Math.BinaryNode n) -> m.name == n.name -- TODO: switch to CommutativeAssociativeMatcher
    _ -> False

-- ## groupSubtree: make children go one more step down
groupSubtree: Set.Set Int -> Equation state -> Result String (Equation state)
groupSubtree ids eq = case affectedSubtree_ ids eq.tracker.parent of
    Nothing -> Err "Nodes not found"
    Just (id, nodes) -> processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq -> case subEq.root of
            Math.BinaryNode n -> if not n.associative then Err "Node is not associative"
                else if not n.commutative then Err "Not implemented for non-commutative"
                else let filtered = List.filter (\c -> Set.member (Math.getState c |> getID) nodes) n.children in
                    let unfiltered = List.filter (\c -> Set.member (Math.getState c |> getID) nodes |> not) n.children in
                    if List.isEmpty filtered || List.isEmpty unfiltered then Err "Grouping all or none does nothing"
                    else let (newS, newT) = addNode_ (getID n.state) subEq.tracker in
                        let newP = List.foldl (\c -> Dict.insert (Math.getState c |> getID) (getID newS)) newT.parent filtered in
                        Ok ((), {root = Math.BinaryNode {n | children = Math.BinaryNode {n | children = filtered, state = newS}::unfiltered}, tracker = {newT | parent = newP}})
            _ -> Err "Node is not associative"
        )
        eq
        |> Result.map Tuple.second

ungroupSubtree: Set.Set Int -> Equation state -> Result String (Equation state)
ungroupSubtree ids eq = let tracker = eq.tracker in
    case affectedSubtree_ ids tracker.parent of
        Nothing -> Err "Nodes not found"
        Just (id, nodes) -> processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq -> case subEq.root of
                Math.BinaryNode n -> if not n.associative then Err "Node is not associative"
                    else
                        let
                            updateParent s =
                                List.foldl
                                (\c -> Dict.insert (Math.getState c |> getID) id)
                                (Dict.remove (getID s.state) tracker.parent)
                                s.children
                            processChildren found children = case children of
                                [] -> Err "All children are ungrouped"
                                (c::other) -> case c of
                                    Math.BinaryNode m -> if m.name /= n.name then processChildren found other |> Result.map (\(list, t) -> (c::list, t))
                                        else if Set.member (getID m.state) nodes then Ok (m.children ++ other, {tracker | parent = updateParent m})
                                        else case processChildren found other of
                                            Ok (list, t) -> Ok (c::list, t)
                                            Err errStr -> if found then Err errStr
                                                else Ok (m.children ++ other, {tracker | parent = updateParent m})
                                    _ -> processChildren found other |> Result.map (\(list, t) -> (c::list, t))
                        in
                            processChildren False n.children
                            |> Result.map (\(list, t) -> ((), {root = Math.BinaryNode {n | children = list}, tracker= t}))
                _ -> Err "Node is not associative"
                )
                eq
            |> Result.map Tuple.second

-- ## matchSubtree: make sure the root is already been through "reduceNodes_"
matchSubtree: Matcher -> Math.Tree (State state) -> Maybe (MatchResult state)
matchSubtree matcher root =
    extractPattern_ matcher root (Backtrack.init {nodes = Dict.empty, matches = Dict.empty})
    |> Backtrack.toMaybe

extractPattern_: Matcher -> Math.Tree (State state) -> Backtrack.Continuation (MatchResult state) -> Backtrack.Continuation (MatchResult state)
extractPattern_ from root token = case from of
    RealMatcher s -> case root of
        Math.RealNode n -> if n.value /= s.value then Backtrack.fail
            else Backtrack.return Just token
        _ -> Backtrack.fail
    AnyMatcher s -> let id = Math.getState root |> getID in
        case root of
            Math.DeclarativeNode _ -> Backtrack.fail
            _ -> Backtrack.return (\result -> case Dict.get s.name result.matches of
                Just (Internal_ prevSet) -> prevSet
                    |> Set.filter (\elem -> case Dict.get elem result.nodes of
                        Nothing -> False
                        Just n -> Math.equal n root
                    )
                    |> (\newSet -> if Set.isEmpty newSet then Nothing
                        else Just {result | matches = Dict.insert s.name (Internal_ newSet) result.matches}
                    )
                _ -> Just
                    {  result
                    |   nodes = Dict.insert id root result.nodes
                    ,   matches = Dict.insert s.name (Set.singleton id |> Internal_) result.matches
                    }
                ) token
    ExactMatcher s -> if List.isEmpty s.arguments
        then case root of
            Math.VariableNode n -> if n.name /= s.name then Backtrack.fail
                else Backtrack.return Just token
            _ -> Backtrack.fail
        else case root of
            Math.GenericNode n -> if n.name /= s.name then Backtrack.fail
                else Backtrack.searchOrdered extractPattern_ s.arguments n.children token
            _ -> Backtrack.fail
    CommutativeMatcher s -> case root of
        Math.BinaryNode n -> if s.name /= n.name then Backtrack.fail
            else Backtrack.searchUnordered extractPattern_ s.arguments n.children token
        _ -> Backtrack.fail
    CommutativeAssociativeMatcher _ -> Backtrack.fail -- TODO:

-- Insertion in associative node:
-- If 2 into 1, Take the lower index
-- If 2 into 2, reuse the same indexes
-- If 2 into n, insert new nodes into the higher index