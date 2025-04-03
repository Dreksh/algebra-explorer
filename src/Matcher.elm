module Matcher exposing (Equation, Matcher(..), MatchResult, State,
    getID, getName, countChildren, parseEquation, selectedSubtree, variableArgsOnly,
    groupSubtree, ungroupSubtree,
    addMatch, matchNode, matchSubtree, replaceSubtree
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

getState: State state -> state
getState s = case s of
    State_ _ newS -> newS

type alias Tracker_ state =
    {   nextID: Int
    ,   parent: Dict.Dict Int Int
    ,   newState: Int -> state
    ,   copyState: State state -> Int -> state
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
    | DeclarativeMatcher {name: String, arguments: List Matcher, commutative: Bool} -- Specifically for Declarative statements
    | CommutativeAssociativeMatcher {name: String, arguments: List Matcher } -- Unordered & group-able

getName: Matcher -> String
getName m = case m of
    AnyMatcher s -> s.name
    RealMatcher s -> String.fromFloat s.value
    ExactMatcher s -> s.name
    CommutativeMatcher s -> s.name
    DeclarativeMatcher s -> s.name
    CommutativeAssociativeMatcher s -> s.name

countChildren: Matcher -> Int
countChildren m = case m of
    AnyMatcher s -> List.length s.arguments
    RealMatcher s -> 0
    ExactMatcher s -> List.length s.arguments
    CommutativeMatcher s -> List.length s.arguments
    DeclarativeMatcher s -> List.length s.arguments
    CommutativeAssociativeMatcher s -> List.length s.arguments

type alias MatchResult state =
    {   nodes: Dict.Dict Int (Math.Tree (State state))
    ,   matches: Dict.Dict String (Value_ state)
    }

type alias ReplacementState_ state =
    {   original: Maybe (State state)
    ,   argument: Maybe Int
    }

type alias Value_ state =
    {   subtree: Math.Tree (ReplacementState_ state)
    ,   example: List Int -- List is only useful if there are no arguments
    }

addMatch: String -> Dict.Dict String Int -> Math.Tree () -> MatchResult state -> MatchResult state
addMatch key args value result = {result | matches = Dict.insert key {subtree = toValue_ (\_ -> Nothing) args value, example = []} result.matches}

variableArgsOnly: List Matcher -> Result String (Dict.Dict String Int)
variableArgsOnly = List.indexedMap Tuple.pair
    >> Helper.resultList (\(index, elem) dict ->
        case elem of
            AnyMatcher s -> if List.isEmpty s.arguments then Ok (Dict.insert s.name index dict)
                else Err "Composite functions not implemented yet"
            _ -> Err "non-variables not allowed as function arguments"
    )
    Dict.empty

-- ## parserEquation: Also assigned ID
parseEquation: (Int -> state) -> (State state -> Int -> state) -> String -> Result String (Equation state)
parseEquation newState copyState input = Math.parse input
    |> Result.map (
        processID_ -1 {nextID = 0, parent = Dict.empty, newState = newState, copyState = copyState}
        >> (\(newRoot, tracker) -> {root = newRoot, tracker = {tracker | parent = Dict.remove 0 tracker.parent}})
    )

processID_: Int -> Tracker_ state -> Math.Tree () -> (Math.Tree (State state), Tracker_ state)
processID_ parent tracker oldRoot =
    let
        id = tracker.nextID
        (state, newTracker) = addNode_ Nothing parent tracker
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

addNode_: Maybe (State state) -> Int -> Tracker_ state -> (State state, Tracker_ state)
addNode_ previous parent tracker =
    (   State_ tracker.nextID (case previous of
            Nothing -> tracker.newState tracker.nextID
            Just prev -> tracker.copyState prev tracker.nextID
        )
    ,   {tracker | nextID = tracker.nextID + 1, parent = Dict.insert tracker.nextID parent tracker.parent}
    )

toValue_: (orig -> Maybe (State state)) -> Dict.Dict String Int -> Math.Tree orig -> Math.Tree (ReplacementState_ state)
toValue_ converter varDict =
    let
        convert node = case node of
            Math.RealNode s -> Math.RealNode {state = {original = converter s.state, argument = Nothing}, value = s.value}
            Math.VariableNode s -> case Dict.get s.name varDict of
                Nothing -> Math.VariableNode {state = {original = converter s.state, argument = Nothing}, name = s.name}
                Just n -> Math.VariableNode {state = {original = converter s.state, argument = Just n}, name = ""}
            Math.UnaryNode s -> Math.UnaryNode {state = {original = converter s.state, argument = Nothing}, name = s.name, child = convert s.child}
            Math.BinaryNode s -> Math.BinaryNode
                {state = {original = converter s.state, argument = Nothing}, name = s.name, associative = s.associative, commutative = s.commutative, children = List.map convert s.children}
            Math.GenericNode s -> Math.GenericNode {state = {original = converter s.state, argument = Nothing}, name = s.name, children = List.map convert s.children}
            Math.DeclarativeNode s -> Math.DeclarativeNode {state = {original = converter s.state, argument = Nothing}, name = s.name, children = List.map convert s.children}
    in
        convert

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
    (DeclarativeMatcher m, Math.DeclarativeNode n) -> m.name == n.name
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
                    else let (newS, newT) = addNode_ Nothing (getID n.state) subEq.tracker in
                        let newP = List.foldl (\c -> Dict.insert (Math.getState c |> getID) (getID newS)) newT.parent filtered in
                        Ok ((), {root = Math.BinaryNode {n | children = Math.BinaryNode {n | children = filtered, state = newS}::unfiltered}, tracker = {newT | parent = newP}})
            _ -> Err "Node is not associative"
        )
        eq
        |> Result.map Tuple.second

ungroupSubtree: Set.Set Int -> Equation state -> Result String (Equation state)
ungroupSubtree ids eq =
    let
        tracker = eq.tracker
    in
    case affectedSubtree_ ids tracker.parent of
        Nothing -> Err "Nodes not found"
        Just (id, nodes) -> processSubtree_ (searchPath_ eq.tracker.parent id) (foo tracker id nodes) eq
            |> Result.map Tuple.second

foo: Tracker_ state -> Int -> Set.Set Int -> Equation state -> Result String ((), Equation state)
foo tracker id nodes subEq =
    case subEq.root of
        Math.BinaryNode n ->
            if not n.associative then Err "Node is not associative"
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
            _ -> Backtrack.return (\result -> variableArgsOnly s.arguments
                |> Result.toMaybe
                |> Maybe.andThen (\varDict -> let newSubtree = toValue_ Just varDict root in
                    case Dict.get s.name result.matches of
                        Just existing ->
                            if subtreeEqual_ newSubtree existing.subtree |> not then Nothing
                            else if List.isEmpty s.arguments |> not then Just result
                            else
                                Just
                                {  result
                                |   nodes = Dict.insert id root result.nodes
                                ,   matches = Dict.insert s.name {existing | example = id::existing.example} result.matches
                                }
                        _ ->
                            if List.isEmpty s.arguments |> not then
                                Just {  result
                                | matches = Dict.insert s.name {subtree = newSubtree, example = []} result.matches
                                }
                            else Just
                                {  result
                                |   nodes = Dict.insert id root result.nodes
                                ,   matches = Dict.insert s.name {subtree = newSubtree, example = [id]} result.matches
                                }
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
            Math.UnaryNode n -> if n.name /= s.name then Backtrack.fail
                else case s.arguments of
                    [arg] -> extractPattern_ arg n.child token
                    _ -> Backtrack.fail
            _ -> Backtrack.fail
    CommutativeMatcher s -> case root of
        Math.BinaryNode n -> if s.name /= n.name then Backtrack.fail
            else Backtrack.searchUnordered extractPattern_ s.arguments n.children token
        _ -> Backtrack.fail
    DeclarativeMatcher s -> case root of
        Math.DeclarativeNode n -> if s.name /= n.name then Backtrack.fail
            else if s.commutative then Backtrack.searchUnordered extractPattern_ s.arguments n.children token
            else Backtrack.searchOrdered extractPattern_ s.arguments n.children token
        _ -> Backtrack.fail
    CommutativeAssociativeMatcher _ -> Backtrack.fail -- TODO:

subtreeEqual_: Math.Tree (ReplacementState_ state) -> Math.Tree (ReplacementState_ state) -> Bool
subtreeEqual_ = Math.equal
    (\l r -> case (l.argument,r.argument) of
        (Nothing, Nothing) -> True
        (Just a, Just b) -> a == b
        _ -> False
    )

-- ## replaceSubtree
replaceSubtree: Set.Set Int -> Matcher -> MatchResult state -> Equation state -> Result String (Equation state)
replaceSubtree ids into with eq = case affectedSubtree_ ids eq.tracker.parent of
    Nothing -> Err "Unable to find selected nodes"
    Just (id, _) ->
        processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq ->
            let (newRoot, (newTracker, _)) = constructFromSource_ with subEq.tracker -1 into in
            let parent = Dict.get (Math.getState subEq.root |> getID) subEq.tracker.parent in
            Ok ((), {root = newRoot, tracker = setParent_ newRoot parent newTracker})
        )
        eq
        |> Result.map Tuple.second

setParent_: Math.Tree (State state) -> Maybe Int -> Tracker_ state -> Tracker_ state
setParent_ root parent tracker = let id = Math.getState root |> getID in
    case parent of
        Nothing -> {tracker | parent = Dict.remove id tracker.parent}
        Just p -> {tracker | parent = Dict.insert id p tracker.parent}

constructFromSource_: MatchResult state -> Tracker_ state -> Int -> Matcher -> (Math.Tree (State state), (Tracker_ state, MatchResult state))
constructFromSource_ result tracker p matcher =
    let
        id = tracker.nextID
        (s, t) = addNode_ Nothing p tracker
        constructChildren otherT = Helper.listMapWithState (\(oTracker, res) -> constructFromSource_ res oTracker id) (otherT, result)
    in
        case matcher of
            AnyMatcher m -> case Dict.get m.name result.matches of
                Just existing -> case existing.example of
                    (nodeID::other) -> case Dict.get nodeID result.nodes of
                        Just n -> (n, (setParent_ n (Just p) tracker, {result | matches = Dict.insert m.name {existing | example = other} result.matches}))
                        Nothing -> constructFromValue_ p result m.arguments existing.subtree tracker
                    _ -> constructFromValue_ p result m.arguments existing.subtree tracker
                _ -> (Math.VariableNode {state = s, name = "MISSING MATCH"}, (t, result)) -- TODO:
            RealMatcher m -> (Math.RealNode {state = s, value = m.value}, (t, result))
            ExactMatcher m -> case m.arguments of
                [] -> (Math.VariableNode {state = s, name = m.name}, (t, result))
                [child] -> constructFromSource_ result t id child
                    |> (\(c, final) -> (Math.UnaryNode {state = s, name = m.name, child = c}, final))
                _ -> constructChildren t m.arguments
                    |> (\(c, final) -> (Math.GenericNode {state = s, name = m.name, children = c}, final))
            DeclarativeMatcher m -> constructChildren t m.arguments
                |> (\(c, final) -> (Math.DeclarativeNode {state = s, name = m.name, children = c}, final))
            CommutativeMatcher m -> constructChildren t m.arguments
                |> (\(c, final) -> (Math.BinaryNode {state = s, name = m.name, associative = True, commutative = True, children = c}, final))
            CommutativeAssociativeMatcher _ -> (Math.VariableNode {state = s, name = "TODO"}, (t, result)) -- TODO:

-- TODO: If the rules match f(x) on an existing node, we shuld try to reuse the existing node ids
constructFromValue_: Int -> MatchResult state -> List Matcher -> Math.Tree (ReplacementState_ state) -> Tracker_ state -> (Math.Tree (State state), (Tracker_ state, MatchResult state))
constructFromValue_ parent result args existing tracker =
    let
        id = tracker.nextID
        (newS, newTracker) = addNode_ (Math.getState existing |> .original) parent tracker
        processChildren = Helper.listMapWithState (\(childTracker, r) matcher -> constructFromValue_ id r args matcher childTracker)
    in
    case existing of
        Math.RealNode s -> (Math.RealNode {state = newS, value = s.value}, (newTracker, result))
        Math.VariableNode s -> case Maybe.andThen (\n -> Helper.listIndex n args) s.state.argument of
            Nothing -> (Math.VariableNode {state = newS, name = s.name}, (newTracker, result))
            Just matcher -> constructFromSource_ result tracker parent matcher
        Math.UnaryNode s -> let (newChild, final) = constructFromValue_ id result args s.child newTracker in
            (Math.UnaryNode {state = newS, name = s.name, child = newChild}, final)
        Math.BinaryNode s -> let (newChildren, final) = processChildren (newTracker, result) s.children in
            (Math.BinaryNode {state = newS, children = newChildren, name = s.name, associative = s.associative, commutative = s.commutative}, final)
        Math.GenericNode s -> let (newChildren, final) = processChildren (newTracker, result) s.children in
            (Math.GenericNode {state = newS, name = s.name, children = newChildren}, final)
        Math.DeclarativeNode s -> let (newChildren, final) = processChildren (newTracker, result) s.children in
            (Math.DeclarativeNode {state = newS, name = s.name, children = newChildren}, final)