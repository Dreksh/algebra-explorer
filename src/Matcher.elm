module Matcher exposing (Equation, Matcher(..), MatchResult, State,
    getID, matchSubtree, parseEquation, selectedSubtree
    )

import Dict
import Math
import Set
-- Ours
import Backtrack
import Custom

type State state = State_ Int state

type alias Equation state =
    {   root: Math.Tree (State state)
    ,   tracker: Tracker_ state
    }

type alias Tracker_ state =
    {   nextID: Int
    ,   parent: Dict.Dict Int Int
    ,   defaultState: state
    }

-- Ignore only Associativity for now, given that we don't have any functions that follow that for now
type Matcher =
    AnyMatcher {name: String, arguments: List Matcher} -- Unknown variables and functions
    | RealMatcher {value: Float} -- Numbers
    | ExactMatcher {name: String, arguments: List Matcher} -- Known functions or variables, exact arguments
    | CommutativeMatcher {name: String, arguments: List Matcher }  -- Unordered (set-based)

getID: State state -> Int
getID s = case s of
    State_ rootID _ -> rootID

-- ## parserEquation: Also assigned ID
parseEquation: state -> String -> Result String (Equation state)
parseEquation default input = Math.parse input
    |> Result.map (
        processID_ -1 {nextID = 0, parent = Dict.empty, defaultState = default}
        >> (\(tracker, newRoot) -> {root = newRoot, tracker = {tracker | parent = Dict.remove 0 tracker.parent}})
    )

processID_: Int -> Tracker_ state -> Math.Tree () -> (Tracker_ state, Math.Tree (State state))
processID_ parent tracker oldRoot =
    let
        id = tracker.nextID
        (state, newTracker) = addNode_ parent tracker
        processChildren p childTracker = List.foldl
            (\elem (nextEq, list) -> let (finalEq, child) = processID_ p nextEq elem in (finalEq, list ++ [child]) )
            (childTracker, [])
    in
    case oldRoot of
        Math.RealNode s -> (newTracker, Math.RealNode {state = state, value = s.value})
        Math.VariableNode s -> (newTracker, Math.VariableNode {state = state, name = s.name})
        Math.UnaryNode s -> let (finalEq, newChild) = processID_ id newTracker s.child in
            (finalEq, Math.UnaryNode {state = state, name = s.name, child = newChild})
        Math.BinaryNode s -> let (finalEq, newChildren) = processChildren id newTracker s.children in
            (finalEq, Math.BinaryNode {state = state, name = s.name, associative = s.associative, commutative = s.commutative, children = newChildren})
        Math.GenericNode s -> let (finalEq, newChildren) = processChildren id newTracker s.children in
            (finalEq, Math.GenericNode {state = state, name = s.name, children = newChildren})

addNode_: Int -> Tracker_ state -> (State state, Tracker_ state)
addNode_ parent tracker = (State_ tracker.nextID tracker.defaultState, {tracker | nextID = tracker.nextID + 1, parent = Dict.insert tracker.nextID parent tracker.parent})

-- ## selectSubtree: If there is a subtree, it returns the root node as well as the affected subtrees
selectedSubtree: Set.Set Int -> Equation state -> Maybe (Math.Tree (State state), Set.Set Int)
selectedSubtree ids eq = affectedSubtree_ ids eq.tracker.parent
        |> Maybe.andThen (\(id, nodes) ->
            processSubtree_ (searchPath_ eq.tracker.parent id) (\node -> (Just node, node)) eq.root
            |> (\(root, _) -> Maybe.map (\r -> (r, nodes)) root)
        )

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
searchPath_ map id = case Dict.get id map of
    Nothing -> [id]
    Just a -> searchPath_ map a ++ [id]

processSubtree_: List Int -> (Math.Tree (State state) -> (Maybe result, Math.Tree (State state))) -> Math.Tree (State state) -> (Maybe result, Math.Tree (State state))
processSubtree_ path processor node =
    let
        processChildren p children = case children of
            [] -> (Nothing, [])
            (current::others) -> case processSubtree_ p processor current of
                (Nothing, _) -> processChildren p others |> (\(r, newChildren) -> (r, current::newChildren))
                (Just a, newNode) -> (Just a, newNode::others)
        id = Math.getState node |> getID
    in
    case path of
        [] -> (Nothing, node)
        [expected] -> if expected /= id then (Nothing, node)
            else processor node
        (expected::next) -> if expected /= id then (Nothing, node)
            else case node of
                Math.RealNode _ -> (Nothing, node)
                Math.VariableNode _ -> (Nothing, node)
                Math.UnaryNode s -> processSubtree_ next processor s.child |> (\(r, child) -> (r, Math.UnaryNode {s | child = child}))
                Math.BinaryNode s -> let (r, children) = processChildren next s.children in
                    case children of
                        [child] -> (r, child)
                        _ -> if not s.associative then (r, Math.BinaryNode {s | children = children})
                            else List.foldl (\child list -> case child of
                                    Math.BinaryNode childS -> if childS.name == s.name then list ++ childS.children
                                        else list ++ [child]
                                    _ -> list ++ [child]
                                )
                                []
                                children
                                |> (\c -> (r, Math.BinaryNode {s | children = c}))
                Math.GenericNode s -> processChildren next s.children |> (\(r, children) -> (r, Math.GenericNode {s | children = children}))

-- ## matchSubtree: make sure the root is already been through "reduceNodes_"
matchSubtree: Matcher -> Math.Tree (State state) -> Maybe (MatchResult state)
matchSubtree matcher root =
    extractPattern_ matcher root (Backtrack.init {nodes = Dict.empty, matches = Dict.empty})
    |> Backtrack.toMaybe

type alias MatchResult state =
    {   nodes: Dict.Dict Int (Math.Tree (State state))
    ,   matches: Dict.Dict String (Set.Set Int)
    }

extractPattern_: Matcher -> Math.Tree (State state) -> Backtrack.Continuation (MatchResult state) -> Backtrack.Continuation (MatchResult state)
extractPattern_ from root token = case from of
    RealMatcher s -> case root of
        Math.RealNode n -> if n.value /= s.value then Backtrack.fail
            else Backtrack.return Just token
        _ -> Backtrack.fail
    AnyMatcher s -> let id = Math.getState root |> getID in
        Backtrack.return (\result -> case Dict.get s.name result.matches of
            Nothing -> Just
                {  result
                |   nodes = Dict.insert id root result.nodes
                ,   matches = Dict.insert s.name (Set.singleton id) result.matches
                }
            Just prevSet -> prevSet
                |> Set.filter (\elem -> case Dict.get elem result.nodes of
                    Nothing -> False
                    Just n -> Math.equal n root
                )
                |> (\newSet -> if Set.isEmpty newSet then Nothing
                    else Just {result | matches = Dict.insert s.name newSet result.matches}
                )
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

-- Insertion in associative node:
-- If 2 into 1, Take the lower index
-- If 2 into 2, reuse the same indexes
-- If 2 into n, insert new nodes into the higher index