module Algo.Matcher exposing (
    State, getID, getName,
    Equation, parseEquation, encodeEquation, equationDecoder,
    Matcher(..), FunctionProperties, parseMatcher, countChildren, encodeMatcher, matcherDecoder,
    Replacement, toReplacement, encodeReplacement, replacementDecoder,
    MatchResult, addMatch, matchNode,
    groupSubtree, ungroupSubtree, selectedSubtree, matchSubtree, replaceSubtree, replaceRealNode,
    replaceAllOccurrences
    )

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Set
-- Ours
import Algo.Backtrack as Backtrack
import Algo.Math as Math
import Helper

{--
Equation, State and Tracker form equations for interacting
--}

type State state = State_ Int state

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

getID: State state -> Int
getID s = case s of
    State_ rootID _ -> rootID

getState: State state -> state
getState s = case s of
    State_ _ newS -> newS

parseEquation: (Int -> state) -> (State state -> Int -> state) -> Math.Tree a -> Equation state
parseEquation newState copyState = processID_ {nextID = 0, parent = Dict.empty, newState = newState, copyState = copyState}
    >> (\(newRoot, tracker) -> {root = newRoot, tracker = {tracker | parent = Dict.remove 0 tracker.parent}})

processID_: Tracker_ state -> Math.Tree a -> (Math.Tree (State state), Tracker_ state)
processID_ = Math.map (\p _ t -> case p of
    Nothing -> (State_ t.nextID (t.newState t.nextID), {t | nextID = t.nextID + 1})
    Just pState -> addNode_ Nothing (getID pState) t
    )

addNode_: Maybe (State state) -> Int -> Tracker_ state -> (State state, Tracker_ state)
addNode_ previous parent tracker =
    (   State_ tracker.nextID (case previous of
            Nothing -> tracker.newState tracker.nextID
            Just prev -> tracker.copyState prev tracker.nextID
        )
    ,   {tracker | nextID = tracker.nextID + 1, parent = Dict.insert tracker.nextID parent tracker.parent}
    )

{--
Matcher is for pattern matching, allowing variables to be extracted
--}

-- Ignore only Associativity for now, given that we don't have any functions that follow that for now
type Matcher =
    AnyMatcher {name: String, arguments: List String} -- Unknown variables and functions
    | RealMatcher {value: Float} -- Numbers
    | ExactMatcher {name: String, arguments: List Matcher} -- Known functions or variables, exact arguments
    | CommutativeMatcher {name: String, arguments: List Matcher, others: Maybe String}  -- Unordered (set-based)
    | DeclarativeMatcher {name: String, arguments: List Matcher, commutative: Bool} -- Specifically for Declarative statements

type alias FunctionProperties =
    {   arguments: Int
    ,   associative: Bool -- Allows processing in different order
    ,   commutative: Bool -- Allows swapping argument ordering
    }

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

parseMatcher: (String -> Int -> b -> Result String b) -> Dict.Dict String FunctionProperties -> b -> String -> Result String (Matcher, b)
parseMatcher checker knownProps state = Math.parse
    >> Result.andThen (treeToMatcher_ checker (KnownFuncs_ knownProps) state)

type SearchType_ =
    KnownFuncs_ (Dict.Dict String FunctionProperties)
    | KnownArgs_ (Dict.Dict String (Int, Int))

treeToMatcher_: (String -> Int -> b -> Result String b) -> SearchType_ -> b -> Math.Tree a -> Result String (Matcher, b)
treeToMatcher_ checker knownProps state root =
    let
        processChildren = Helper.resultList (\child (list, s) -> treeToMatcher_ checker knownProps s child
                |> Result.map (\(newChild, newS) -> (newChild :: list, newS))
            )
            ([], state)
    in
    case root of
        Math.RealNode s -> Ok (RealMatcher {value = s.value}, state)
        Math.VariableNode s -> case knownProps of
            KnownArgs_ argMap -> case Dict.get s.name argMap of
                Nothing -> Ok (ExactMatcher {name = s.name, arguments = []}, state)
                Just _ -> Ok (AnyMatcher {name = s.name, arguments = []}, state)
            KnownFuncs_ kp -> case Dict.get s.name kp of
                Nothing -> checker s.name 0 state
                    |> Result.map (\newS -> (AnyMatcher {name = s.name, arguments = []}, newS))
                Just p -> if p.arguments /= 0 then Err (s.name ++ " cannot be used as a variable")
                    else Ok (ExactMatcher {name = s.name, arguments = []}, state)
        Math.UnaryNode s -> treeToMatcher_ checker knownProps state s.child
            |> Result.map (\(childMatcher, newS) -> (ExactMatcher {name = s.name, arguments = [childMatcher]}, newS))
        Math.BinaryNode s -> if s.commutative
            then processChildren s.children
                |> Result.map (\(children, newS) -> (CommutativeMatcher {name = s.name, arguments = List.reverse children, others = Nothing}, newS))
            else processChildren s.children
                |> Result.map (\(children, newS) -> (ExactMatcher {name = s.name, arguments = List.reverse children}, newS))
        Math.DeclarativeNode s -> processChildren s.children
            |> Result.map (\(children, newS) -> (DeclarativeMatcher {name = s.name, commutative = True, arguments = List.reverse children}, newS)) -- TODO: Not all of them are commutative (like <)
        Math.GenericNode s -> case knownProps of
            KnownArgs_ _ -> processChildren s.children |> Result.map (\(children, newS) -> (ExactMatcher {name = s.name, arguments = children}, newS))
            KnownFuncs_ kp -> case Dict.get s.name kp of
                Nothing -> checker s.name (List.length s.children) state
                    |> Result.andThen (\newState -> Helper.resultList (\child (list, seen, newS) -> case child of
                            Math.VariableNode n -> if Set.member n.name seen then Err "Function argument can only match on unique variables"
                                else checker n.name 0 newS
                                    |> Result.map (\finalS -> (n.name :: list, Set.insert n.name seen, finalS))
                            _ -> Err "Only variables are supported in function arguments"
                        )
                        ([], Set.empty, newState)
                        s.children
                    )
                    |> Result.map (\(children, _, newState) -> (AnyMatcher {name = s.name, arguments = List.reverse children}, newState))
                Just prop -> if prop.arguments /= (List.length s.children) then Err (s.name ++ " has an unexpected function signature")
                    else if prop.commutative then processChildren s.children |> Result.map (\(children, newS) -> (CommutativeMatcher {name = s.name, arguments = children, others = Nothing}, newS))
                    else processChildren s.children |> Result.map (\(children, newS) -> (ExactMatcher {name = s.name, arguments = children}, newS))

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

{--
Replacement are bluerpints to reconstruct equations
--}

type alias Replacement = Math.Tree (Maybe Int)

toReplacement: Dict.Dict String FunctionProperties -> Dict.Dict String (Int, Int) -> String -> Result String Replacement
toReplacement funcProps argDict = Math.parse >> Result.andThen (toReplacement_ (Just funcProps) argDict)

toReplacement_: Maybe (Dict.Dict String FunctionProperties) -> Dict.Dict String (Int, Int) -> Math.Tree a -> Result String Replacement
toReplacement_ funcProps argDict =
    let
        getFuncProp name = case funcProps of
            Nothing -> Ok Nothing
            Just fp -> case Dict.get name fp of
                Nothing -> Err ("Unable to construct " ++ name)
                Just p -> Ok (Just p)

        -- Can't use Math.map since GenericNode can turn into BinaryNode
        convert node = case node of
            Math.RealNode s -> Ok (Math.RealNode {state = Nothing, value = s.value})
            Math.VariableNode s -> case Dict.get s.name argDict of
                Just (args, index) -> if args /= 0 then Err (s.name ++ " is not a variable")
                    else Ok (Math.VariableNode {state = Just index, name = ""})
                Nothing -> getFuncProp s.name
                    |> Result.andThen (\fp -> case fp of
                        Nothing -> Ok (Math.VariableNode {state = Nothing, name = s.name})
                        Just p -> if p.arguments /= 0 then Err (s.name ++ " is not a variable")
                            else Ok (Math.VariableNode {state = Nothing, name = s.name})
                    )
            Math.UnaryNode s -> convert s.child
                |> Result.map (\child -> Math.UnaryNode {state = Nothing, name = s.name, child = child})
            Math.BinaryNode s -> Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                |> Result.map (\children -> Math.BinaryNode {state = Nothing, name = s.name, associative = s.associative, commutative = s.commutative, children = List.reverse children})
            Math.GenericNode s -> case Dict.get s.name argDict of
                Just (args, index) -> if args /= List.length s.children then Err (s.name ++ " has the wrong number of inputs")
                    else Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                        |> Result.map (\children -> Math.GenericNode {state = Just index, name = "", children = children})
                Nothing -> getFuncProp s.name
                    |> Result.andThen (\fp -> case fp of
                        Nothing -> Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                            |> Result.map (\children -> Math.GenericNode {state = Nothing, name = s.name, children = List.reverse children})
                        Just p -> if p.arguments == 2 && (p.commutative || p.associative) then Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                                |> Result.map (\children -> Math.BinaryNode {state = Nothing, name=s.name, associative = p.associative, commutative = p.commutative, children = List.reverse children})
                            else if p.arguments /= List.length s.children then Err (s.name ++ " has the wrong number of inputs")
                            else Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                                |> Result.map (\children -> Math.GenericNode {state = Nothing, name = s.name, children = List.reverse children})
                    )
            Math.DeclarativeNode s -> Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                |> Result.map (\children -> Math.DeclarativeNode {state = Nothing, name = s.name, children = List.reverse children})
    in
        convert

{--
MatchResult is the result of extracting patterns using Matcher, or with replacements from external sources
--}

type alias MatchResult state = Dict.Dict String (Value_ state)
type alias InternalReplacement_ state = Math.Tree (State state, Maybe Int)

type Value_ state =
    ExternalValue_ Replacement
    | InternalValue_ (InternalReplacement_ state)
    | MultiValue_ {op: String, arguments: Int, associative: Bool, commutative: Bool, pre: List (InternalReplacement_ state), post: List (InternalReplacement_ state)}
    | AsIsValue_ Bool (Math.Tree (State state)) -- 'used' and then 'tree'

addMatch: String -> Replacement -> MatchResult state -> MatchResult state
addMatch key replacement = Dict.insert key (ExternalValue_ replacement)

toInternalReplacement_: Dict.Dict String Int -> Math.Tree (State state) -> InternalReplacement_ state
toInternalReplacement_ argDict = Math.map (\_ node _ -> case node of
        Math.VariableNode s -> case Dict.get s.name argDict of
            Nothing -> ((s.state, Nothing), ())
            Just n -> ((s.state, Just n), ())
        Math.GenericNode s -> case Dict.get s.name argDict of
            Nothing -> ((s.state, Nothing), ())
            Just n -> ((s.state, Just n), ())
        _ -> ((Math.getState node, Nothing), ())
    )
    ()
    >> Tuple.first

-- ## selectSubtree: If there is a subtree, it returns the root node as well as the affected subtrees
selectedSubtree: Set.Set Int -> Equation state -> Result String (Int, Set.Set Int, Math.Tree (State state))
selectedSubtree ids eq = case affectedSubtree_ ids eq.tracker.parent of
    Nothing -> Err "Nodes not found"
    Just (id, nodes) -> processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq -> Ok (subEq.root, subEq)) eq
        |> Result.map (\(root, _) -> (id, nodes, root))

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
            [] -> Err "Children not found"
            (current::others) -> case processSubtree_ p processor {eq | root = current} of
                Err _ -> processChildren p others |> Result.map (\(r, newChildren, t) -> (r, current::newChildren, t))
                Ok (a, newEq) -> Ok (a, newEq.root::others, newEq.tracker)
        id = Math.getState eq.root |> getID
    in
    case path of
        [] -> Err "Target node not found"
        [expected] -> if expected /= id then Err "Intermediary node not found"
            else processor eq
        (expected::next) -> if expected /= id then Err "Intermediary node not found"
            else case eq.root of
                Math.RealNode _ -> Err "Cannot find children in RealNode"
                Math.VariableNode _ -> Err "Cannot find children in VariableNode"
                Math.UnaryNode s -> processSubtree_ next processor {eq | root = s.child} |> Result.map (\(r, newEq) -> (r, {newEq | root = Math.UnaryNode {s | child = newEq.root}}))
                Math.BinaryNode s -> processChildren next s.children |> Result.map (\(r, children, t) -> (r, {root = Math.BinaryNode {s | children = children}, tracker = t}))
                Math.GenericNode s -> processChildren next s.children |> Result.map (\(r, children, t) -> (r, {root = Math.GenericNode {s | children = children}, tracker = t}))
                Math.DeclarativeNode s -> processChildren next s.children |> Result.map (\(r, children, t) -> (r, {root = Math.DeclarativeNode {s | children = children}, tracker = t}))

-- ## groupSubtree: make children go one more step down
groupSubtree: Int -> Set.Set Int -> Equation state -> Result String (Int, Equation state)
groupSubtree id nodes eq = processSubtree_ (searchPath_ eq.tracker.parent id)
    (\subEq -> case subEq.root of
        Math.BinaryNode n -> if not n.associative then Err "Node is not associative"
            else if not n.commutative then Err "Not implemented for non-commutative"
            else let (pre,group,post) = groupPartition_ (\c -> Set.member (Math.getState c |> getID) nodes) n.children in
                if (List.length pre + List.length post == 0) || List.isEmpty group then Err "Grouping all or none does nothing"
                else let (newS, newT) = addNode_ Nothing (getID n.state) subEq.tracker in
                    let newP = List.foldl (\c -> Dict.insert (Math.getState c |> getID) (getID newS)) newT.parent group in
                    Ok (getID newS, {root = Math.BinaryNode
                        {   n
                        |   children = List.reverse pre++(Math.BinaryNode {n | children = List.reverse group, state = newS}::List.reverse post)
                        }
                        , tracker = {newT | parent = newP}
                        }
                    )
        _ -> Err "Node is not associative"
    )
    eq

groupPartition_: (Math.Tree (State state) -> Bool) -> List (Math.Tree (State state)) -> (List (Math.Tree (State state)),List (Math.Tree (State state)),List (Math.Tree (State state)))
groupPartition_ check = List.foldl
    (\elem (pre,group,post) -> if check elem then (pre, elem::group, post)
        else if List.isEmpty group then (elem::pre, group, post)
        else (pre, group, elem::post)
    )
    ([],[],[])

-- ## ungroupSubtree: merge children back into the parent
ungroupSubtree: Int -> Set.Set Int -> Equation state -> Result String (Equation state)
ungroupSubtree id nodes eq = processSubtree_ (searchPath_ eq.tracker.parent id) (ungroupChild_ eq.tracker id nodes) eq
    |> Result.map Tuple.second

ungroupChild_: Tracker_ state -> Int -> Set.Set Int -> Equation state -> Result String ((), Equation state)
ungroupChild_ tracker id nodes subEq =
    case subEq.root of
        Math.BinaryNode n ->
            if not n.associative then Err "Node is not associative"
            else
                let
                    updateParent s = List.foldl
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

-- ## replaceRealNode: merge children back into the parent
replaceRealNode: Int -> Float -> Replacement -> Equation state -> Result String (Int, Equation state)
replaceRealNode id target subtree eq = processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq -> case subEq.root of
        Math.RealNode n -> if target /= n.value then Err "Expression does not equal to the node's value"
            else processID_ subEq.tracker subtree
            |> (\(root, tracker) ->
                let
                    parent = Dict.get id tracker.parent
                    nextTracker = {tracker | parent = Dict.remove (getID n.state) tracker.parent}
                in
                    Ok ((Math.getState root |> getID), {root = root, tracker = setParent_ root parent nextTracker})
            )
        _ -> Err "Node is not a number"
    )
    eq

-- ## matchSubtree: make sure the root is already been through "reduceNodes_"
matchSubtree: Set.Set Int -> Matcher -> Math.Tree (State state) -> Maybe (MatchResult state)
matchSubtree priority matcher root =
    extractPattern_ priority matcher root (Backtrack.init Dict.empty)
    |> Maybe.andThen Backtrack.getState

extractPattern_: Set.Set Int -> Matcher -> Math.Tree (State state) -> Backtrack.Continuation (MatchResult state) -> Maybe (Backtrack.Continuation (MatchResult state))
extractPattern_ priority from root token = case from of
    RealMatcher s -> case root of
        Math.RealNode n -> if n.value == s.value then Backtrack.return Just token else Backtrack.fail
        _ -> Backtrack.fail
    AnyMatcher s -> case root of
        Math.DeclarativeNode _ -> Backtrack.fail
        _ -> Backtrack.return (\result -> List.indexedMap (\a b -> (b, a)) s.arguments
            |> Dict.fromList
            |> (\varDict -> let newSubtree = toInternalReplacement_ varDict root in
                case Dict.get s.name result of
                    Just (InternalValue_ existing) -> if subtreeEqual_ newSubtree existing then Just result else Nothing
                    Nothing -> Just (Dict.insert s.name (InternalValue_ newSubtree) result)
                    _ -> Nothing
                )
            ) token
    ExactMatcher s -> if List.isEmpty s.arguments
        then case root of
            Math.VariableNode n -> if n.name == s.name then Backtrack.return Just token else Backtrack.fail
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
                        ++  [ otherMatchEvaluator_ s.name n.associative n.commutative o ]
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

otherMatchEvaluator_: String -> Bool -> Bool -> String -> Backtrack.Evaluator (MatchResult state) (Int, Math.Tree (State state))
otherMatchEvaluator_ op associative commutative key nodes = Backtrack.return (\result -> if List.isEmpty nodes then Just result
    else if List.length nodes == 1 then List.head nodes
        |> Maybe.map (\(_, node) -> Dict.insert key (toInternalReplacement_ Dict.empty node |> InternalValue_) result)
    else List.sortBy Tuple.first nodes
        |> List.indexedMap Tuple.pair
        |> List.partition (\(index, (preIndex, _)) -> index == preIndex)
        |> (\(a,b)-> let processChildren = List.map (\(_, (_, n)) -> toInternalReplacement_ Dict.empty n) in
            Just (Dict.insert key (MultiValue_ {arguments = 0, op = op, associative = associative, commutative = commutative, pre = processChildren a, post = processChildren b}) result)
        )
    )

subtreeEqual_: InternalReplacement_ state -> InternalReplacement_ state -> Bool
subtreeEqual_ = Math.equal
    (\l r -> case (l,r) of
        ((_, Nothing), (_, Nothing)) -> True
        ((_, Just a), (_, Just b)) -> a == b
        _ -> False
    )

-- ## replaceSubtree
replaceSubtree: Set.Set Int -> Replacement -> MatchResult state -> Equation state -> Result String (Int, Equation state)
replaceSubtree ids replacement with eq = case affectedSubtree_ ids eq.tracker.parent of
    Nothing -> Err "Unable to find selected nodes"
    Just (id, _) -> processSubtree_ (searchPath_ eq.tracker.parent id) (replaceSubtree_ replacement with) eq

replaceSubtree_: Replacement -> MatchResult state -> Equation state -> Result String (Int, Equation state)
replaceSubtree_ replacement result eq =
    let args = Dict.toList result |> List.indexedMap (\index (_, val) -> (index, val)) |> Dict.fromList in
    constructFromReplacement_ -1 eq.tracker args replacement
    |> Result.map (\(r, t, _) ->
        let
            (newRoot, newTracker) = replacedToTree_ 0 t r
            pID = Dict.get (Math.getState eq.root |> getID) eq.tracker.parent
            finalTracker = Set.diff (getSubtreeIDs_ eq.root) (getSubtreeIDs_ newRoot)
                |> Set.foldl (\elem childT -> {childT | parent = Dict.remove elem childT.parent}) (setParent_ newRoot pID newTracker)
        in
        ((Math.getState newRoot |> getID), {root = newRoot, tracker = finalTracker})
    )

type Replaced_ state =
    SingleNodeReplaced_ (Math.Tree (State state))
    | MultiNodeReplaced_ {name: String, associative: Bool, commutative: Bool} (List (Math.Tree (State state))) (List (Math.Tree (State state)))

replacedToTree_: Int -> Tracker_ state -> Replaced_ state -> (Math.Tree (State state), Tracker_ state)
replacedToTree_ p t r = case r of
    SingleNodeReplaced_ tree -> (tree, t)
    MultiNodeReplaced_ f pre post -> let (st, newT) = addNode_ Nothing p t in
        (   Math.BinaryNode {state = st, name = f.name, associative = f.associative, commutative = f.commutative, children = pre ++ post}
        ,   {newT | parent = List.foldl (\child -> Dict.insert (Math.getState child |> getID) t.nextID) newT.parent (pre ++ post) }
        )

type ReplacementList_ state =
    RList_ (List (Math.Tree (State state)))
    | FList_ {name: String, associative: Bool, commutative: Bool} (List (Math.Tree (State state))) (List (Math.Tree (State state)))

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

constructFromReplacement_: Int -> Tracker_ state -> Dict.Dict Int (Value_ state) -> Replacement -> Result String (Replaced_ state, Tracker_ state, Dict.Dict Int (Value_ state))
constructFromReplacement_ parent tracker arguments replaceNode =
    let
        id = tracker.nextID
        (s, t) = addNode_ Nothing parent tracker
        constructChildren name firstT = Helper.resultList (\child (list, oT, args) ->
                constructFromReplacement_ id oT args child
                |> Result.map (\(newChild, newT, newRes) -> (newChild :: list, newT, newRes))
            )
            ([], firstT, arguments)
            >> Result.andThen (\(children, newT, a) -> toReplacementList_ id name newT children
                |> Result.map (\(rList, finalT) -> (rList, finalT, a))
            )
    in
        case replaceNode of
            Math.RealNode n -> Ok (SingleNodeReplaced_ (Math.RealNode {state = s, value = n.value}), t, arguments)
            Math.UnaryNode n -> constructFromReplacement_ id t arguments n.child
                |> Result.map (\(newChild, newT, newArg) -> replacedToTree_ id newT newChild
                    |> \(tree, finalT) -> (SingleNodeReplaced_ (Math.UnaryNode {state = s, name = n.name, child = tree}), finalT, newArg)
                )
            Math.BinaryNode n -> constructChildren n.name t n.children
                |> Result.map (\(res, newT, newArg) ->
                    (   case res of
                        RList_ children -> SingleNodeReplaced_ (Math.BinaryNode {state = s, name = n.name, associative = n.associative, commutative = n.commutative, children = children})
                        FList_ _ pre post -> SingleNodeReplaced_ (Math.BinaryNode {state = s, name = n.name, associative = n.associative, commutative = n.commutative, children = pre ++ post})
                    ,   newT, newArg)
                )
            Math.DeclarativeNode n -> constructChildren n.name t n.children
                |> Result.map (\(res, newT, newArg) ->
                    (   case res of
                        RList_ children -> SingleNodeReplaced_ (Math.DeclarativeNode {state = s, name = n.name, children = children})
                        FList_ _ pre post -> SingleNodeReplaced_ (Math.DeclarativeNode {state = s, name = n.name, children = pre ++ post})
                    ,   newT, newArg)
                )
            Math.VariableNode n -> case n.state of
                Nothing -> Ok (SingleNodeReplaced_ (Math.VariableNode {state = s, name = n.name}), t, arguments)
                Just argNum -> case Dict.get argNum arguments of
                    Nothing -> Err "Couldn't find value to replace"
                    Just value -> constructFromValue_ parent tracker Dict.empty value
                        |> Result.map (\(replaced, newT, val) -> replacedToTree_ parent newT replaced
                            |> \(tree, finalT) -> (SingleNodeReplaced_ tree, finalT, Dict.insert argNum val arguments)
                        )
            Math.GenericNode n -> case n.state of
                Nothing -> constructChildren "" t n.children
                    |> Result.map (\(res, newT, newArg) ->
                        (   case res of
                                RList_ children -> SingleNodeReplaced_ (Math.GenericNode {state = s, name = n.name, children = children})
                                FList_ _ pre post -> SingleNodeReplaced_ (Math.GenericNode {state = s, name = n.name, children = pre ++ post})
                        ,   newT
                        ,   newArg
                        )
                    )
                Just argNum -> case Dict.get argNum arguments of
                    Nothing -> Err "Couldn't find the function to replace"
                    Just value -> constructChildren "" tracker n.children
                        |> Result.andThen (\(res, newT, newArg) -> case res of
                            FList_ _ _ _ -> Err "Something went wrong: Grouping detected"
                            RList_ children -> List.indexedMap (\num child -> (num, AsIsValue_ False child)) children
                                |> \childArgs -> constructFromValue_ parent newT (Dict.fromList childArgs) value
                                |> Result.map (\(finalNode, finalT, newVal) -> (finalNode, finalT, Dict.insert argNum newVal newArg))
                        )

constructFromInternalReplacement_: Int -> Tracker_ state -> Dict.Dict Int (Value_ state) -> InternalReplacement_ state -> Result String (Replaced_ state, Tracker_ state, Dict.Dict Int (Value_ state))
constructFromInternalReplacement_ parent tracker arguments replaceNode =
    let
        id = tracker.nextID
        (s, t) = addNode_ (Math.getState replaceNode |> Tuple.first |> Just) parent tracker
        constructChildren name firstT = Helper.resultList (\child (list, oT, args) ->
                constructFromInternalReplacement_ id oT args child
                |> Result.map (\(newChild, newT, newRes) -> (newChild :: list, newT, newRes))
            )
            ([], firstT, arguments)
            >> Result.andThen (\(children, newT, a) -> toReplacementList_ id name newT children
                |> Result.map (\(rList, finalT) -> (rList, finalT, a))
            )
    in
        case replaceNode of
            Math.RealNode n -> Ok (SingleNodeReplaced_ (Math.RealNode {state = s, value = n.value}), t, arguments)
            Math.UnaryNode n -> constructFromInternalReplacement_ id t arguments n.child
                |> Result.map (\(newChild, newT, newArg) -> replacedToTree_ id newT newChild
                    |> \(tree, finalT) -> (SingleNodeReplaced_ (Math.UnaryNode {state = s, name = n.name, child = tree}), finalT, newArg)
                )
            Math.BinaryNode n -> constructChildren n.name t n.children
                |> Result.map (\(res, newT, newArg) ->
                    (   case res of
                        RList_ children -> SingleNodeReplaced_ (Math.BinaryNode {state = s, name = n.name, associative = n.associative, commutative = n.commutative, children = children})
                        FList_ _ pre post -> SingleNodeReplaced_ (Math.BinaryNode {state = s, name = n.name, associative = n.associative, commutative = n.commutative, children = pre ++ post})
                    ,   newT, newArg)
                )
            Math.DeclarativeNode n -> constructChildren n.name t n.children
                |> Result.map (\(res, newT, newArg) ->
                    (   case res of
                        RList_ children -> SingleNodeReplaced_ (Math.DeclarativeNode {state = s, name = n.name, children = children})
                        FList_ _ pre post -> SingleNodeReplaced_ (Math.DeclarativeNode {state = s, name = n.name, children = pre ++ post})
                    ,   newT, newArg)
                )
            Math.VariableNode n -> case n.state of
                (oldS, Nothing) -> let newState = tracker.copyState oldS tracker.nextID in Ok
                    (    SingleNodeReplaced_ (Math.VariableNode {state = State_ tracker.nextID newState, name = n.name})
                    ,   {tracker | nextID = tracker.nextID + 1, parent = Dict.insert tracker.nextID parent tracker.parent}
                    ,   arguments
                    )
                (_, Just argNum) -> case Dict.get argNum arguments of
                    Nothing -> Err "Couldn't find value to replace"
                    Just value -> constructFromValue_ parent tracker Dict.empty value
                        |> Result.map (\(replaced, newT, newVal) -> replacedToTree_ parent newT replaced
                            |> \(tree, finalT) -> (SingleNodeReplaced_ tree, finalT, Dict.insert argNum newVal arguments)
                        )
            Math.GenericNode n -> case n.state of
                (oldS, Nothing) -> constructChildren "" t n.children
                    |> Result.map (\(res, newT, newArg) ->
                        (   case res of
                                RList_ children -> SingleNodeReplaced_ (Math.GenericNode {state = s, name = n.name, children = children})
                                FList_ _ pre post -> SingleNodeReplaced_ (Math.GenericNode {state = s, name = n.name, children = pre ++ post})
                        , newT, newArg
                        )
                    )
                (_, Just argNum) -> case Dict.get argNum arguments of
                    Nothing -> Err "Couldn't find the function to replace"
                    Just value -> constructChildren "" tracker n.children
                        |> Result.andThen (\(res, newT, newArg) -> case res of
                            FList_ _ _ _ -> Err "Something went wrong: Grouping detected"
                            RList_ children -> List.indexedMap (\num child -> (num, AsIsValue_ False child)) children
                                |> \childArgs -> constructFromValue_ parent newT (Dict.fromList childArgs) value
                                |> Result.map (\(finalNode, finalT, newVal) -> (finalNode, finalT, Dict.insert argNum newVal newArg))
                        )

toReplacementList_: Int -> String -> Tracker_ state -> List (Replaced_ state) -> Result String (ReplacementList_ state, Tracker_ state)
toReplacementList_ parent name tracker = List.foldl (\child (list, oT) -> case (list, child) of
        (Nothing, SingleNodeReplaced_ tree) -> (Just (RList_ [tree]), oT)
        (Nothing, MultiNodeReplaced_ p pre post) -> if p.name == name then (Just (FList_ p pre post), oT)
            else addNode_ Nothing parent oT
                |> \(s, t) -> (Just (RList_ [Math.BinaryNode {state = s, name = p.name, associative = p.associative, commutative = p.commutative, children = pre ++ post}]), t)
        (Just (RList_ n), SingleNodeReplaced_ tree) -> (Just (RList_ (tree :: n)), oT)
        (Just (RList_ n), MultiNodeReplaced_ p pre post) -> if p.name == name then (Just (FList_ p pre (post ++ n)), oT)
            else addNode_ Nothing parent oT
                |> \(s, t) -> (Just (RList_ (Math.BinaryNode {state = s, name = p.name, associative = p.associative, commutative = p.commutative, children = pre ++ post}::n)), t)
        (Just (FList_ n preOp postOp), SingleNodeReplaced_ tree) -> (Just (FList_ n preOp (tree :: postOp)), oT)
        (Just (FList_ n preOp postOp), MultiNodeReplaced_ p pre post) -> if p.name == name then (Just (FList_ n preOp (pre ++ post ++ postOp)), oT)
            else addNode_ Nothing parent oT
                |> \(s, t) -> (Just (FList_ n preOp (Math.BinaryNode {state = s, name = p.name, associative = p.associative, commutative = p.commutative, children = pre ++ post}:: postOp)), t)
    )
    (Nothing, tracker)
    >> (\(replacement, t) -> case replacement of
        Nothing -> Err "Missing children"
        Just r -> Ok (r, t)
    )

constructFromValue_: Int -> Tracker_ state -> Dict.Dict Int (Value_ state) -> Value_ state -> Result String (Replaced_ state, Tracker_ state, Value_ state)
constructFromValue_ parent tracker args value = case value of
    ExternalValue_ r -> constructFromReplacement_ parent tracker args r
        |> Result.map (\(newChild, newT, newArgs) -> (newChild, newT, value))
    InternalValue_ r -> constructFromInternalReplacement_ parent tracker args r
        |> Result.map (\(newChild, newT, newArgs) -> (newChild, newT, value))
    MultiValue_ p ->
        let
            processChildren internalRep (list, t, innerArgs) = constructFromInternalReplacement_ parent t innerArgs internalRep
                |> Result.map (\(replaced, newT, newArgs) -> replacedToTree_ parent newT replaced |> \(tree, finalT) -> (tree :: list, finalT, newArgs))
        in
            Helper.resultList processChildren ([], tracker, args) p.pre
            |> Result.andThen (\(newPre, nextT, nextArgs) -> Helper.resultList processChildren ([], nextT, nextArgs) p.post
                |> Result.map (\(newPost, finalT, _) -> (MultiNodeReplaced_ {name = p.op, associative = p.associative, commutative = p.commutative} (List.reverse newPre) (List.reverse newPost), finalT, value))
            )
    AsIsValue_ used tree -> if used then duplicateTree_ parent tracker tree |> \(newRoot, newTree) -> Ok (SingleNodeReplaced_ newRoot, newTree, value)
        else Ok (SingleNodeReplaced_ tree, {tracker | parent = Dict.insert (Math.getState tree |> getID) parent tracker.parent}, AsIsValue_ True tree)

duplicateTree_: Int -> Tracker_ state -> Math.Tree (State state) -> (Math.Tree (State state), Tracker_ state)
duplicateTree_ parent tracker root = Math.map (\p s t -> addNode_ (Math.getState s |> Just) (Maybe.map getID p |> Maybe.withDefault parent) t) tracker root
    |> \(finalRoot, finalT) -> (finalRoot, {finalT | parent = Dict.insert (Math.getState finalRoot |> getID) parent finalT.parent})

-- ## replaceAllOccurences: Using the Equation, switch between LHS<->RHS if it matches
replaceAllOccurrences: Dict.Dict String FunctionProperties -> Set.Set Int -> Equation state -> Equation state -> Result String (Set.Set Int, Equation state)
replaceAllOccurrences funcs roots with on = case with.root of
    Math.DeclarativeNode withN -> if withN.name /= "=" then Err "Chosen substitution equation is not an equation"
        else case withN.children of
            [left, right] -> createMatcherPair_ funcs left right
                |> Result.andThen (\pairs -> Set.foldl (replaceOnNode_ pairs) (Set.empty, on) roots
                    |> \(newRoots, newEq) -> if Set.isEmpty newRoots then Err "No selected nodes can be substituted"
                        else Ok (newRoots, newEq)
                )
            _ -> Err "Ambiguous equation. Ensure that it only equates 2 statements"
    _ -> Err "Chosen substitution equation is not an equation"

createMatcherPair_: Dict.Dict String FunctionProperties -> Math.Tree (State state) -> Math.Tree (State state) -> Result String (List (Matcher, Replacement))
createMatcherPair_ funcs left right =
    let
        toMatcherPair (from, to) = case treeToMatcher_ (\_ _ -> Ok) (KnownFuncs_ funcs) () from of
            Ok (AnyMatcher n, _) -> let args = List.indexedMap (\index var -> (var, (0, index))) n.arguments |> Dict.fromList in
                toReplacement_ Nothing args to
                |> Result.map (\toR -> treeToMatcher_ (\_ _ -> Ok) (KnownArgs_ args) () to
                    |> Result.andThen (\(toM, _) -> toReplacement_ Nothing args from
                        |> Result.map (\fromR -> [(toM, fromR)])
                    )
                    |> Result.toMaybe |> Maybe.withDefault []
                    |> (::) (ExactMatcher {name = n.name, arguments = List.map (\name -> AnyMatcher {name = name, arguments = []}) n.arguments}, toR)
                )
                |> Result.toMaybe |> Maybe.withDefault []
            _ -> []
    in
        List.concatMap toMatcherPair [(left, right), (right, left)]
        |> \res -> if List.isEmpty res then Err "Substitution requires the equation to be a variable or function definition"
            else Ok res

replaceOnNode_: List (Matcher, Replacement) -> Int -> (Set.Set Int, Equation state) -> (Set.Set Int, Equation state)
replaceOnNode_ matches id (selected, eq) = processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq ->
        List.foldl (\(matcher, replacement) result -> case result of
            Just _ -> result
            Nothing -> extractPattern_ Set.empty matcher subEq.root (Backtrack.init Dict.empty)
                |> Maybe.andThen Backtrack.getState
                |> Maybe.andThen (\matchRes -> replaceSubtree_ replacement matchRes subEq |> Result.toMaybe )
        ) Nothing matches
        |> Result.fromMaybe "No matches"
    )
    eq
    |> \res -> case res of
        Ok (newRoot, newEq) -> (Set.insert newRoot selected, newEq)
        _ -> (selected, eq)

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
        [("name", Encode.string s.name), ("arguments", Encode.list Encode.string s.arguments), ("type", Encode.string "any")]
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
            (Decode.field "name" Decode.string) (Decode.field "arguments" <| Decode.list Decode.string)
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

encodeReplacement: Replacement -> Encode.Value
encodeReplacement = Math.encode (\arg -> case arg of
        Nothing -> Encode.null
        Just n -> Encode.int n
    )

replacementDecoder: Decode.Decoder Replacement
replacementDecoder = Math.decoder (Decode.maybe Decode.int)
