module Algo.Matcher exposing (Equation, Matcher(..), MatchResult, State,
    getID, getName, countChildren, parseEquation, selectedSubtree, variableArgsOnly,
    groupSubtree, ungroupSubtree,
    addMatch, matchNode, matchSubtree, replaceSubtree, replaceRealNode,
    encodeEquation, encodeMatcher, encodeMatchResult,
    equationDecoder, matcherDecoder, matchResultDecoder
    )

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Set
-- Ours
import Algo.Math as Math
import Algo.Backtrack as Backtrack
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
    | CommutativeMatcher {name: String, arguments: List Matcher, others: Maybe String}  -- Unordered (set-based)
    | DeclarativeMatcher {name: String, arguments: List Matcher, commutative: Bool} -- Specifically for Declarative statements

getName: Matcher -> String
getName m = case m of
    AnyMatcher s -> s.name
    RealMatcher s -> String.fromFloat s.value
    ExactMatcher s -> s.name
    CommutativeMatcher s -> s.name
    DeclarativeMatcher s -> s.name

countChildren: Matcher -> Int
countChildren m = case m of
    AnyMatcher s -> List.length s.arguments
    RealMatcher _ -> 0
    ExactMatcher s -> List.length s.arguments
    CommutativeMatcher s -> List.length s.arguments + (Maybe.map (\_ -> 1) s.others |> Maybe.withDefault 0)
    DeclarativeMatcher s -> List.length s.arguments

type alias MatchResult state =
    {   nodes: Dict.Dict Int (Math.Tree (State state))
    ,   matches: Dict.Dict String (Value_ state)
    }

type alias ReplacementState_ state =
    {   original: Maybe (State state)
    ,   argument: Maybe Int
    }

type Value_ state =
    SingleValue_ (Replacement_ state) -- List is only useful if there are no arguments
    | MultiValue_ {op: String, pre: List (Replacement_ state), post: List (Replacement_ state)} -- These should only be matched once, so it'll simply be moving them around

type alias Replacement_ state = {subtree: Math.Tree (ReplacementState_ state), example: List Int}

addMatch: String -> Dict.Dict String Int -> Math.Tree () -> MatchResult state -> MatchResult state
addMatch key args value result =
    {   result
    |   matches = Dict.insert key (SingleValue_ {subtree = toValue_ (\_ -> Nothing) args value, example = []}) result.matches
    }

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
selectedSubtree: Set.Set Int -> Equation state -> Result String (Math.Tree (State state), Set.Set Int, Int)
selectedSubtree ids eq = case affectedSubtree_ ids eq.tracker.parent of
    Nothing -> Err "Nodes not found"
    Just (id, nodes) ->
        processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq -> Ok (subEq.root, subEq)) eq
        |> Result.map (\(root, _) ->
            (   root
            ,   nodes
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
    (CommutativeMatcher m, Math.BinaryNode n) -> m.name == n.name
    _ -> False

-- ## groupSubtree: make children go one more step down
groupSubtree: Set.Set Int -> Equation state -> Result String (Equation state)
groupSubtree ids eq = case affectedSubtree_ ids eq.tracker.parent of
    Nothing -> Err "Nodes not found"
    Just (id, nodes) -> processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq -> case subEq.root of
            Math.BinaryNode n -> if not n.associative then Err "Node is not associative"
                else if not n.commutative then Err "Not implemented for non-commutative"
                else let (pre,group,post) = groupPartition_ (\c -> Set.member (Math.getState c |> getID) nodes) n.children in
                    if (List.length pre + List.length post == 0) || List.isEmpty group then Err "Grouping all or none does nothing"
                    else let (newS, newT) = addNode_ Nothing (getID n.state) subEq.tracker in
                        let newP = List.foldl (\c -> Dict.insert (Math.getState c |> getID) (getID newS)) newT.parent group in
                        Ok ((), {root = Math.BinaryNode
                            {   n
                            |   children = List.reverse pre++(Math.BinaryNode {n | children = List.reverse group, state = newS}::List.reverse post)
                            }
                            , tracker = {newT | parent = newP}
                            }
                        )
            _ -> Err "Node is not associative"
        )
        eq
        |> Result.map Tuple.second

groupPartition_: (Math.Tree (State state) -> Bool) -> List (Math.Tree (State state)) -> (List (Math.Tree (State state)),List (Math.Tree (State state)),List (Math.Tree (State state)))
groupPartition_ check = List.foldl
    (\elem (pre,group,post) -> if check elem then (pre, elem::group, post)
        else if List.isEmpty group then (elem::pre, group, post)
        else (pre, group, elem::post)
    )
    ([],[],[])

ungroupSubtree: Set.Set Int -> Equation state -> Result String (Equation state)
ungroupSubtree ids eq =
    let
        tracker = eq.tracker
    in
    case affectedSubtree_ ids tracker.parent of
        Nothing -> Err "Nodes not found"
        Just (id, nodes) -> processSubtree_ (searchPath_ eq.tracker.parent id) (ungroupChild_ tracker id nodes) eq
            |> Result.map Tuple.second

ungroupChild_: Tracker_ state -> Int -> Set.Set Int -> Equation state -> Result String ((), Equation state)
ungroupChild_ tracker id nodes subEq =
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
                    traverseRemaining unselectedFound children = case children of
                        [] -> Err "All children are ungrouped"
                        (c::other) -> case c of
                            Math.BinaryNode m -> if m.name /= n.name then traverseRemaining unselectedFound other |> Result.map (\(list, t) -> (c::list, t))
                                else if Set.member (getID m.state) nodes then Ok (m.children ++ other, {tracker | parent = updateParent m})
                                else case traverseRemaining True other of
                                    Ok (list, t) -> Ok (c::list, t)
                                    Err errStr -> if unselectedFound then Err errStr -- Keep propagating
                                        else Ok (m.children ++ other, {tracker | parent = updateParent m})
                            _ -> traverseRemaining unselectedFound other |> Result.map (\(list, t) -> (c::list, t))
                in
                    traverseRemaining False n.children
                    |> Result.map (\(list, t) -> ((), {root = Math.BinaryNode {n | children = list}, tracker= t}))
        _ -> Err "Node is not associative"

replaceRealNode: Set.Set Int -> Float -> Math.Tree () -> Equation state -> Result String (Equation state)
replaceRealNode ids target subtree eq = case affectedSubtree_ ids eq.tracker.parent of
    Nothing -> Err "Nodes not found"
    Just (id, _) -> processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq -> case subEq.root of
            Math.RealNode n -> if target /= n.value then Err "Expression does not equal to the node's value"
                else processID_ -1 subEq.tracker subtree
                |> (\(root, tracker) ->
                    let
                        parent = Dict.get id tracker.parent
                        nextTracker = {tracker | parent = Dict.remove (getID n.state) tracker.parent}
                    in
                        Ok ((), {root = root, tracker = setParent_ root parent nextTracker})
                )
            _ -> Err "Node is not a number"
        )
        eq
        |> Result.map Tuple.second

-- ## matchSubtree: make sure the root is already been through "reduceNodes_"
matchSubtree: Set.Set Int -> Matcher -> Math.Tree (State state) -> Maybe (MatchResult state)
matchSubtree priority matcher root =
    extractPattern_ priority matcher root (Backtrack.init {nodes = Dict.empty, matches = Dict.empty})
    |> Maybe.andThen Backtrack.getState

extractPattern_: Set.Set Int -> Matcher -> Math.Tree (State state) -> Backtrack.Continuation (MatchResult state) -> Maybe (Backtrack.Continuation (MatchResult state))
extractPattern_ priority from root token = case from of
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
                        Just (MultiValue_ _) -> Nothing
                        Just (SingleValue_ existing) ->
                            if subtreeEqual_ newSubtree existing.subtree |> not then Nothing
                            else if List.isEmpty s.arguments |> not then Just result
                            else
                                Just
                                {  result
                                |   nodes = Dict.insert id root result.nodes
                                ,   matches = Dict.insert s.name (SingleValue_ {existing | example = id::existing.example}) result.matches
                                }
                        _ ->
                            if List.isEmpty s.arguments |> not then
                                Just {  result
                                | matches = Dict.insert s.name (SingleValue_ {subtree = newSubtree, example = []}) result.matches
                                } -- TODO: Someway of preserving some of the functions
                            else Just
                                {  result
                                |   nodes = Dict.insert id root result.nodes
                                ,   matches = Dict.insert s.name (SingleValue_ {subtree = newSubtree, example = [id]}) result.matches
                                }
                    )
                ) token
    ExactMatcher s -> if List.isEmpty s.arguments
        then case root of
            Math.VariableNode n -> if n.name /= s.name then Backtrack.fail
                else Backtrack.return Just token
            _ -> Backtrack.fail
        else case root of
            Math.GenericNode n -> if n.name /= s.name || (List.length s.arguments /= List.length n.children)
                then Backtrack.fail
                else Backtrack.run (Backtrack.orderedStack (extractPattern_ priority) s.arguments) n.children token
            Math.UnaryNode n -> if n.name /= s.name then Backtrack.fail
                else case s.arguments of
                    [arg] -> extractPattern_ priority arg n.child token
                    _ -> Backtrack.fail
            _ -> Backtrack.fail
    CommutativeMatcher s -> case root of
        Math.BinaryNode n -> if s.name /= n.name then Backtrack.fail
            else case s.others of
                Nothing -> let priorityList = priorityList_ priority n.children in
                    if List.length s.arguments /= List.length priorityList then Backtrack.fail
                    else Backtrack.run (Backtrack.unorderedStack (extractPattern_ priority) s.arguments) priorityList token
                Just o -> List.indexedMap Tuple.pair n.children
                    |> List.partition (\(_, childN) -> Set.member (Math.getState childN |> getID) priority)
                    |> (\(a,b) -> Backtrack.run
                        (   Backtrack.unorderedStack (\slot (_, c) -> extractPattern_ priority slot c) s.arguments
                        ++  [ otherMatchEvaluator_ s.name o ]
                        )
                        (a++b) token
                    )
        _ -> Backtrack.fail
    DeclarativeMatcher s -> case root of
        Math.DeclarativeNode n -> if s.name /= n.name || (List.length s.arguments /= List.length n.children) then Backtrack.fail
            else let priorityList = priorityList_ priority n.children in
                if s.commutative then Backtrack.run (Backtrack.unorderedStack (extractPattern_ priority) s.arguments) priorityList token
                else Backtrack.run (Backtrack.orderedStack (extractPattern_ priority) s.arguments) priorityList token
        _ -> Backtrack.fail

priorityList_: Set.Set Int -> List (Math.Tree (State state)) -> List (Math.Tree (State state))
priorityList_ set = List.partition (\n -> Set.member (Math.getState n |> getID) set) >> (\(a,b) -> a++b)

otherMatchEvaluator_: String -> String -> Backtrack.Evaluator (MatchResult state) (Int, Math.Tree (State state))
otherMatchEvaluator_ op key nodes = Backtrack.return (\result -> if List.isEmpty nodes then Just result
    else if List.length nodes == 1 then List.head nodes
        |> Maybe.map (\(_, node) -> let id = Math.getState node |> getID in
            {   result
            |   nodes = Dict.insert id node result.nodes
            ,   matches = Dict.insert key (SingleValue_ {subtree = toValue_ Just Dict.empty node, example = [id]}) result.matches
            })
    else List.sortBy Tuple.first nodes
        |> List.indexedMap Tuple.pair
        |> List.partition (\(index, (preIndex, _)) -> index == preIndex)
        |> (\(a,b)-> let toReplacements = List.map (\(_, (_, n)) -> {subtree = toValue_ Just Dict.empty n, example = [Math.getState n |> getID]}) in
            Just {   result
            |   nodes = List.foldl (\(_, n) -> Dict.insert (Math.getState n |> getID) n) result.nodes nodes
            ,   matches = Dict.insert key (MultiValue_ {op = op, pre = toReplacements a, post = toReplacements b}) result.matches
            }
        )
        )

-- Splits
childrenCategories_: List (Math.Tree (State state)) -> Dict.Dict String (List (Math.Tree (State state)))
childrenCategories_ list = Dict.empty

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
            let
                (newRoot, (newTracker, _)) = constructFromSource_ with subEq.tracker -1 into
                pID = Dict.get (Math.getState subEq.root |> getID) subEq.tracker.parent
                finalTracker = Set.diff (getSubtreeIDs_ subEq.root) (getSubtreeIDs_ newRoot)
                    |> Set.foldl (\elem t -> {t | parent = Dict.remove elem t.parent}) (setParent_ newRoot pID newTracker)
            in
            Ok ((), {root = newRoot, tracker = finalTracker})
        )
        eq
        |> Result.map Tuple.second

setParent_: Math.Tree (State state) -> Maybe Int -> Tracker_ state -> Tracker_ state
setParent_ root parent tracker = let id = Math.getState root |> getID in
    case parent of
        Nothing -> {tracker | parent = Dict.remove id tracker.parent}
        Just p -> {tracker | parent = Dict.insert id p tracker.parent}

getSubtreeIDs_: Math.Tree (State state) -> Set.Set Int
getSubtreeIDs_ root = case root of
    Math.RealNode s -> Set.singleton (getID s.state)
    Math.VariableNode s -> Set.singleton (getID s.state)
    Math.UnaryNode s -> Set.insert (getID s.state) (getSubtreeIDs_ s.child)
    Math.BinaryNode s -> List.foldl (\e -> Set.union (getSubtreeIDs_ e)) (Set.singleton (getID s.state)) s.children
    Math.GenericNode s -> List.foldl (\e -> Set.union (getSubtreeIDs_ e)) (Set.singleton (getID s.state)) s.children
    Math.DeclarativeNode s -> List.foldl (\e -> Set.union (getSubtreeIDs_ e)) (Set.singleton (getID s.state)) s.children

constructFromSource_: MatchResult state -> Tracker_ state -> Int -> Matcher -> (Math.Tree (State state), (Tracker_ state, MatchResult state))
constructFromSource_ result tracker p matcher =
    let
        id = tracker.nextID
        (s, t) = addNode_ Nothing p tracker
        constructChildren otherT = Helper.listMapWithState (\(oTracker, res) -> constructFromSource_ res oTracker id) (otherT, result)
    in
        case matcher of
            AnyMatcher m -> case Dict.get m.name result.matches of
                Just (SingleValue_ existing) -> case existing.example of
                    (nodeID::other) -> case Dict.get nodeID result.nodes of
                        Just n -> (n, (setParent_ n (Just p) tracker, {result | matches = Dict.insert m.name (SingleValue_ {existing | example = other}) result.matches}))
                        Nothing -> constructFromValue_ p result m.arguments existing.subtree tracker
                    _ -> constructFromValue_ p result m.arguments existing.subtree tracker
                Just (MultiValue_ nodes) -> processMultiValue_ id result.nodes t nodes.pre
                    |> (\(preNodes, (newT, newPre)) -> processMultiValue_ id result.nodes newT nodes.post
                        |> (\(postNodes, (finalT, newPost)) ->
                            (   Math.BinaryNode {state = s, name = nodes.op, commutative = True, associative = True, children = preNodes ++ postNodes}
                            ,   (   finalT
                                ,   {result | matches = Dict.insert m.name (MultiValue_ {nodes | pre = newPre, post = newPost}) result.matches}
                                )
                            )
                        )
                    )
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
                |> (\(c, (nextT, nextRes)) -> case m.others of
                    Nothing -> (Math.BinaryNode {state = s, name = m.name, associative = True, commutative = True, children = c}, (nextT, nextRes))
                    Just o -> case Dict.get o result.matches of
                        Nothing ->(Math.BinaryNode {state = s, name = m.name, associative = True, commutative = True, children = c}, (nextT, nextRes)) -- no additional nodes matched
                        Just (MultiValue_ mv) -> if mv.op /= m.name
                            then constructFromSource_ nextRes nextT id (AnyMatcher {name = o, arguments = []})
                                |> (\(finalNode, final) -> (Math.BinaryNode {state = s, name = m.name, associative = True, commutative = True, children = c ++ [finalNode]}, final))
                            else processMultiValue_ id result.nodes nextT mv.pre
                                |> (\(preNodes, (newT, newPre)) -> processMultiValue_ id result.nodes newT mv.post
                                    |> (\(postNodes, (finalT, newPost)) ->
                                        (   Math.BinaryNode {state = s, name = mv.op, commutative = True, associative = True, children = preNodes ++ c ++ postNodes}
                                        ,   (   finalT
                                            ,   {nextRes | matches = Dict.insert m.name (MultiValue_ {mv | pre = newPre, post = newPost}) nextRes.matches}
                                            )
                                        )
                                    )
                                )
                        _ -> constructFromSource_ nextRes nextT id (AnyMatcher {name = o, arguments = []})
                            |> (\(finalNode, final) -> (Math.BinaryNode {state = s, name = m.name, associative = True, commutative = True, children = c ++ [finalNode]}, final))
                )

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

processMultiValue_: Int -> Dict.Dict Int (Math.Tree (State state)) -> Tracker_ state -> List (Replacement_ state) -> (List (Math.Tree (State state)), (Tracker_ state, List (Replacement_ state)))
processMultiValue_ parent dict tracker = Helper.listMapWithState (\(t,r) child -> case List.head child.example of
        Nothing -> constructFromValue_ parent {nodes = Dict.empty, matches = Dict.empty} [] child.subtree t
            |> \(node, (newT, _)) -> (node, (newT, r ++ [child]))
        Just nodeID -> case Dict.get nodeID dict of
            Nothing -> let (newS, newTracker) = addNode_ Nothing parent t in
                (Math.VariableNode {state = newS, name = "MISSING MATCH"},(newTracker, r ++ [{child | example = List.drop 1 child.example}]))
            Just node -> (node, (setParent_ node (Just parent) tracker, r ++ [{child | example = List.drop 1 child.example}]))
    )
    (tracker,[])

{-
## Encoding and Decoding
-}

encodeEquation: (state -> Encode.Value) -> Equation state -> Encode.Value
encodeEquation convert eq = Encode.object
    [   ("root", Math.encode (encodeState_ convert) eq.root)
    ,   ("tracker", Encode.object
        [   ("nextID", Encode.int eq.tracker.nextID)
        ,   ("parent", Encode.dict String.fromInt Encode.int eq.tracker.parent)
        ]
        )
    ]

encodeState_: (state -> Encode.Value) -> State state -> Encode.Value
encodeState_ convert s = case s of
    State_ id innerState -> Encode.object [("id", Encode.int id), ("state", convert innerState)]

stateDecoder_: Decode.Decoder state -> Decode.Decoder (State state)
stateDecoder_ innerDec = Decode.map2 State_ (Decode.field "id" Decode.int) (Decode.field "state" innerDec)

equationDecoder: (Int -> state) -> (State state -> Int -> state) -> Decode.Decoder state -> Decode.Decoder (Equation state)
equationDecoder newState copyState innerDec = Decode.map2 (\root tracker -> {root = root, tracker = tracker})
    (Decode.field "root" <| Math.decoder <| stateDecoder_ innerDec)
    (   Decode.field "tracker"
        <| Decode.map2 (\id p -> {nextID = id, parent = p, newState = newState, copyState = copyState})
            (Decode.field "nextID" Decode.int)
            (Decode.field "parent" <| Helper.intDictDecoder Decode.int)
    )

encodeMatcher: Matcher -> Encode.Value
encodeMatcher matcher = case matcher of
    AnyMatcher s -> Encode.object
        [("name", Encode.string s.name), ("arguments", Encode.list encodeMatcher s.arguments), ("type", Encode.string "any")]
    RealMatcher s -> Encode.object
        [("value", Encode.float s.value), ("type", Encode.string "real")]
    ExactMatcher s -> Encode.object
        [("name", Encode.string s.name), ("arguments", Encode.list encodeMatcher s.arguments), ("type", Encode.string "exact")]
    CommutativeMatcher s -> Encode.object
        [("name", Encode.string s.name), ("arguments", Encode.list encodeMatcher s.arguments),
        ("other", Maybe.map Encode.string s.others |> Maybe.withDefault Encode.null),("type", Encode.string "commutative")]
    DeclarativeMatcher s -> Encode.object
        [("name", Encode.string s.name), ("arguments", Encode.list encodeMatcher s.arguments)
        ,("commutative", Encode.bool s.commutative), ("type", Encode.string "declarative")]

matcherDecoder: Decode.Decoder Matcher
matcherDecoder = Decode.field "type" Decode.string
    |> Decode.andThen (\t -> case t of
        "any" -> Decode.map2 (\n a -> AnyMatcher {name = n, arguments = a})
            (Decode.field "name" Decode.string) (Decode.field "arguments" <| Decode.list <| Decode.lazy (\_ -> matcherDecoder))
        "real" -> Decode.map (\v -> RealMatcher {value = v}) (Decode.field "value" Decode.float)
        "exact" -> Decode.map2 (\n a -> ExactMatcher {name = n, arguments = a})
            (Decode.field "name" Decode.string) (Decode.field "arguments" <| Decode.list <| Decode.lazy (\_ -> matcherDecoder))
        "commutative" ->Decode.map3 (\n a o -> CommutativeMatcher {name = n, arguments = a, others = o})
            (Decode.field "name" Decode.string) (Decode.field "arguments" <| Decode.list <| Decode.lazy (\_ -> matcherDecoder))
            (Decode.maybe <| Decode.field "other" Decode.string)
        "declarative" ->Decode.map3 (\n a c -> DeclarativeMatcher {name = n, arguments = a, commutative = c})
            (Decode.field "name" Decode.string) (Decode.field "arguments" <| Decode.list <| Decode.lazy (\_ -> matcherDecoder))
            (Decode.field "commutative" <| Decode.oneOf [Decode.bool, Decode.succeed False])
        _ -> Decode.fail ("Unknown type of matcher: " ++ t)
    )

encodeMatchResult: (state -> Encode.Value) -> MatchResult state -> Encode.Value
encodeMatchResult convert result = Encode.object
    [   ("nodes", Encode.dict String.fromInt (Math.encode (encodeState_ convert)) result.nodes)
    ,   ("matches", Encode.dict identity (encodeValue_ convert) result.matches)
    ]

encodeValue_: (state -> Encode.Value) -> Value_ state -> Encode.Value
encodeValue_ convert v = case v of
    SingleValue_ s -> Encode.object [("type",Encode.string "single"),("child",encodeReplacement_ convert s)]
    MultiValue_ s -> Encode.object
        [("type",Encode.string "multi"),("op",Encode.string s.op)
        ,("pre",Encode.list (encodeReplacement_ convert) s.pre)
        ,("post", Encode.list (encodeReplacement_ convert) s.post)
        ]

encodeReplacement_: (state -> Encode.Value) -> Replacement_ state -> Encode.Value
encodeReplacement_ convert r = Encode.object
    [   ("subtree", Math.encode (\s -> Encode.object
            [ ("original", Maybe.map (encodeState_ convert) s.original |> Maybe.withDefault Encode.null)
            , ("argument", Maybe.map Encode.int s.argument |> Maybe.withDefault Encode.null)
            ]
        ) r.subtree)
    ,   ("example", Encode.list Encode.int r.example)
    ]

matchResultDecoder: Decode.Decoder state -> Decode.Decoder (MatchResult state)
matchResultDecoder innerDec = Decode.map2 (\n m -> {nodes = n, matches = m})
    (Decode.field "nodes" <| Helper.intDictDecoder (Math.decoder (stateDecoder_ innerDec)))
    (Decode.field "matches" <| Decode.dict (valueDecoder_ innerDec))

valueDecoder_: Decode.Decoder state -> Decode.Decoder (Value_ state)
valueDecoder_ innerDecode = Decode.field "type" Decode.string
    |> Decode.andThen (\t -> case t of
        "single" -> Decode.map SingleValue_ (Decode.field "child" (replacementDecoder_ innerDecode))
        "multi" -> Decode.map3 (\o pr po -> MultiValue_ {op = o, pre = pr, post = po})
            (Decode.field "op" Decode.string)
            (Decode.field "pre" (Decode.list (replacementDecoder_ innerDecode)))
            (Decode.field "post" (Decode.list (replacementDecoder_ innerDecode)))
        _ -> Decode.fail "unknown type of value"
    )

replacementDecoder_: Decode.Decoder state -> Decode.Decoder (Replacement_ state)
replacementDecoder_ innerDec = Decode.map2 (\s e -> {subtree = s, example = e})
    (Decode.field "subtree" <| Math.decoder (
        Decode.map2 (\o a -> {original = o, argument = a})
        (Decode.maybe <| Decode.field "original" <| stateDecoder_ innerDec)
        (Decode.maybe <| Decode.field "argument" <| Decode.int)
    ))
    (Decode.field "example" <| Decode.list Decode.int)