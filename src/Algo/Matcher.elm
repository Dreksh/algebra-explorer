module Algo.Matcher exposing (
    State, StateOp, getID, getState, getName, encodeState, stateDecoder,
    Equation, parseEquation, getNode, encodeEquation, equationDecoder,
    Matcher(..), parseMatcher, countChildren, encodeMatcher, matcherDecoder,
    Replacement, toReplacement, encodeReplacement, replacementDecoder,
    MatchResult, newResult, addMatch, matchNode,
    groupSubtree, ungroupSubtree, setChildIndex, refreshFuncProp,
    selectedSubtree, matchSubtree, replaceSubtree, replaceRealNode, replaceAllOccurrences
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

type alias Tracker_ props state =
    {   nextID: Int
    ,   parent: Dict.Dict Int Int
    ,   ops: StateOp props state
    }
type alias StateOp props state =
    {   new: Maybe props -> Int -> state
    ,   copy: State state -> Int -> state
    ,   update: Maybe props -> state -> (state, Bool) -- bool refers to whether an update happened
    ,   extract: state -> Maybe props
    }

type alias Equation props state =
    {   root: Math.Tree (State state)
    ,   tracker: Tracker_ props state
    }

getID: State state -> Int
getID s = case s of
    State_ rootID _ -> rootID

getState: State state -> state
getState s = case s of
    State_ _ newS -> newS

parseEquation: Dict.Dict String {a | property: Math.FunctionProperty prop} -> StateOp prop state -> String -> Result String (Equation prop state)
parseEquation fp ops = Math.parse fp
    >> Result.map (
        processID_ {nextID = 0, parent = Dict.empty, ops = ops}
        >> \(newRoot, tracker) -> {root = newRoot, tracker = {tracker | parent = Dict.remove 0 tracker.parent}}
    )

processID_: Tracker_ prop state -> Math.Tree (Maybe prop) -> (Math.Tree (State state), Tracker_ prop state)
processID_ = Math.map (\parent current t -> case parent of
    Nothing -> (State_ t.nextID (t.ops.new (Math.getState current) t.nextID), {t | nextID = t.nextID + 1})
    Just pState -> addNode_ Nothing (Math.getState current) (getID pState) t
    )

addNode_: Maybe (State state) -> Maybe prop -> Int -> Tracker_ prop state -> (State state, Tracker_ prop state)
addNode_ previous property parent tracker =
    (   State_ tracker.nextID (case previous of
            Nothing -> tracker.ops.new property tracker.nextID
            Just prev -> tracker.ops.copy prev tracker.nextID
        )
    ,   {tracker | nextID = tracker.nextID + 1, parent = Dict.insert tracker.nextID parent tracker.parent}
    )

getNode: Int -> Equation prop state -> Maybe (Math.Tree (State state))
getNode num eq = processSubtree_ (searchPath_ eq.tracker.parent num) (\e -> Ok (e.root, e)) eq
    |> Result.map Tuple.first |> Result.toMaybe

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

parseMatcher: (String -> Int -> b -> Result String b) -> Dict.Dict String {a | property: Math.FunctionProperty c} -> b -> String -> Result String (Matcher, b)
parseMatcher checker knownProps state = Math.parse knownProps
    >> Result.andThen (treeToMatcher_ checker Dict.empty state)

treeToMatcher_: (String -> Int -> b -> Result String b) -> Dict.Dict String (Int, Int) -> b -> Math.Tree a -> Result String (Matcher, b)
treeToMatcher_ checker argMap state root =
    let
        processChildren = Helper.resultList (\child (list, s) -> treeToMatcher_ checker argMap s child
                |> Result.map (\(newChild, newS) -> (newChild :: list, newS))
            )
            ([], state)
    in
    case root of
        Math.RealNode s -> Ok (RealMatcher {value = s.value}, state)
        Math.VariableNode s -> case Dict.get s.name argMap of
            Nothing -> if s.constant then Ok (ExactMatcher {name = s.name, arguments = []}, state)
                else checker s.name 0 state
                    |> Result.map (\newS -> (AnyMatcher {name = s.name, arguments = []}, newS))
            Just _ -> Ok (AnyMatcher {name = s.name, arguments = []}, state)
        Math.UnaryNode s -> treeToMatcher_ checker argMap state s.child
            |> Result.map (\(childMatcher, newS) -> (ExactMatcher {name = s.name, arguments = [childMatcher]}, newS))
        Math.BinaryNode s -> if s.commutative
            then processChildren s.children
                |> Result.map (\(children, newS) -> (CommutativeMatcher {name = s.name, arguments = List.reverse children, others = Nothing}, newS))
            else processChildren s.children
                |> Result.map (\(children, newS) -> (ExactMatcher {name = s.name, arguments = List.reverse children}, newS))
        Math.DeclarativeNode s -> processChildren s.children
            |> Result.map (\(children, newS) -> (DeclarativeMatcher {name = s.name, commutative = True, arguments = List.reverse children}, newS)) -- TODO: Not all of them are commutative (like <)
        Math.GenericNode s -> case Dict.get s.name argMap of
            Just _ -> processChildren s.children |> Result.map (\(children, newS) -> (ExactMatcher {name = s.name, arguments = List.reverse children}, newS))
            Nothing -> case s.arguments of
                Nothing -> checker s.name (List.length s.children) state
                    |> Result.andThen (\newState -> Helper.resultList (\child (list, seen, newS) -> case child of
                            Math.VariableNode n -> if Set.member n.name seen then Err "Function argument can only match on unique variables"
                                else if n.constant then Err "Only variables are supported in function arguments. Constants are not allowed"
                                else checker n.name 0 newS
                                    |> Result.map (\finalS -> (n.name :: list, Set.insert n.name seen, finalS))
                            _ -> Err "Only variables are supported in function arguments"
                        )
                        ([], Set.empty, newState)
                        s.children
                    )
                    |> Result.map (\(children, _, newState) -> (AnyMatcher {name = s.name, arguments = List.reverse children}, newState))
                Just argNum -> if argNum /= List.length s.children then Err (s.name ++ " has an unexpected function signature")
                    else processChildren s.children |> Result.map (\(children, newS) -> (ExactMatcher {name = s.name, arguments = List.reverse children}, newS))

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
Replacement are blueprints to reconstruct equations
--}

type alias Replacement prop = Math.Tree (Maybe prop, Maybe Int)

toReplacement: Dict.Dict String {a | property: Math.FunctionProperty prop} -> Bool -> Dict.Dict String (Int, Int) -> String -> Result String (Replacement prop)
toReplacement funcProps strict argDict = Math.parse funcProps >> Result.andThen (toReplacement_ identity strict argDict)

toReplacement_: (a -> Maybe prop) -> Bool -> Dict.Dict String (Int, Int) -> Math.Tree a -> Result String (Replacement prop)
toReplacement_ converter strict argDict =
    let
        convert node = case node of
            Math.RealNode s -> Ok (Math.RealNode {state = (converter s.state, Nothing), value = s.value})
            Math.VariableNode s -> case Dict.get s.name argDict of
                Just (args, index) -> if args /= 0 then Err (s.name ++ " is not a variable")
                    else Ok (Math.VariableNode {state = (converter s.state, Just index), constant = False, name = ""})
                Nothing -> if s.constant || not strict then Ok (Math.VariableNode {state = (converter s.state, Nothing), name = s.name, constant = s.constant})
                    else Err ("Cannot construct the variable " ++ s.name)
            Math.UnaryNode s -> convert s.child
                |> Result.map (\child -> Math.UnaryNode {state = (converter s.state, Nothing), name = s.name, child = child})
            Math.BinaryNode s -> Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                |> Result.map (\children -> Math.BinaryNode {state = (converter s.state, Nothing), name = s.name, associative = s.associative, commutative = s.commutative, identity = s.identity, children = List.reverse children})
            Math.GenericNode s -> case Dict.get s.name argDict of
                Just (args, index) -> if args /= List.length s.children then Err (s.name ++ " has the wrong number of inputs")
                    else Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                        |> Result.map (\children -> Math.GenericNode {state = (Nothing, Just index), name = "", arguments = Nothing, children = children})
                Nothing -> case s.arguments of
                    Nothing -> if strict then Err ("Cannot construct the function " ++ s.name)
                        else Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                            |> Result.map (\children -> Math.GenericNode {state = (converter s.state, Nothing), name = s.name, arguments = s.arguments, children = List.reverse children})
                    Just _ -> Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                            |> Result.map (\children -> Math.GenericNode {state = (converter s.state, Nothing), name = s.name, arguments = s.arguments, children = List.reverse children})
            Math.DeclarativeNode s -> Helper.resultList (\child list -> convert child |> Result.map (\c -> c :: list)) [] s.children
                |> Result.map (\children -> Math.DeclarativeNode {state = (converter s.state, Nothing), name = s.name, children = List.reverse children})
    in
        convert

{--
MatchResult is the result of extracting patterns using Matcher, or with replacements from external sources
--}

type alias MatchResult prop state =
    {   var: Dict.Dict String (Value_ prop state) -- Stores values of the variables / function replacements
    ,   exact: Dict.Dict String (List (State state)) -- Stores operations that were observed, in reverse order
    }
type alias InternalReplacement_ state = Math.Tree (State state, Maybe Int)

type Value_ prop state =
    ExternalValue_ (Replacement prop)
    | InternalValue_ (InternalReplacement_ state)
    | MultiValue_ {op: String, arguments: Int, associative: Bool, commutative: Bool, identity: Float, state: State state, pre: List (InternalReplacement_ state), post: List (InternalReplacement_ state)}
    | AsIsValue_ Bool (Math.Tree (State state)) -- 'used' and then 'tree'

newResult: MatchResult prop state
newResult = MatchResult Dict.empty Dict.empty

addExactState_: {a | name: String, state: State s} -> MatchResult prop s -> MatchResult prop s
addExactState_ node res =
    {   res
    |   exact = Dict.update node.name (\prev -> case prev of
            Nothing -> Just [node.state]
            Just list -> Just (node.state :: list)
        )
        res.exact
    }

addMatch: String -> Replacement prop -> MatchResult prop state -> MatchResult prop state
addMatch key replacement result = {result | var = Dict.insert key (ExternalValue_ replacement) result.var}

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
selectedSubtree: Set.Set Int -> Equation prop state -> Result String (Int, Set.Set Int)
selectedSubtree ids eq = affectedSubtree_ ids eq.tracker.parent |> Result.fromMaybe "Nodes not found"

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

processSubtree_: List Int -> (Equation prop state -> Result String (result, Equation prop state)) -> Equation prop state -> Result String (result, Equation prop state)
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
groupSubtree: Int -> Set.Set Int -> Equation prop state -> Result String (Int, Equation prop state)
groupSubtree id nodes eq = processSubtree_ (searchPath_ eq.tracker.parent id)
    (\subEq -> case subEq.root of
        Math.BinaryNode n -> if not n.associative then Err "Node is not associative"
            else if not n.commutative then Err "Not implemented for non-commutative"
            else let (pre,group,post) = groupPartition_ (\c -> Set.member (Math.getState c |> getID) nodes) n.children in
                if (List.length pre + List.length post == 0) || List.isEmpty group then Err "Grouping all or none does nothing"
                else let (newS, newT) = addNode_ (Just n.state) Nothing (getID n.state) subEq.tracker in
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
ungroupSubtree: Int -> Equation prop state -> Result String (Int, Equation prop state)
ungroupSubtree id eq = processSubtree_
    (searchPath_ eq.tracker.parent id |> \list -> List.take (List.length list - 1) list)
    (ungroupChild_ eq.tracker id)
    eq

ungroupChild_: Tracker_ prop state -> Int -> Equation prop state -> Result String (Int, Equation prop state)
ungroupChild_ tracker id subEq = case subEq.root of
    Math.BinaryNode n ->
        if not n.associative then Err "Parent node is not associative"
        else
            let
                traverseRemaining next = case next of
                    [] -> Err "Node not found"
                    (c::other) -> if (Math.getState c |> getID) /= id
                        then traverseRemaining other |> Result.map (\(list, t) -> (c :: list, t))
                        else case c of
                            Math.BinaryNode m -> if m.name /= n.name then Err "Cannot ungroup into a different function"
                                else List.foldl (\node t->
                                        {t | parent = Dict.insert (Math.getState node |> getID) (getID n.state) t.parent}
                                    ) tracker m.children
                                    |> \t -> Ok (m.children ++ other, t)
                            _ -> Err "Node cannot be ungrouped"
            in
                traverseRemaining n.children
                |> Result.map (\(list, t) -> (getID n.state, {root = Math.BinaryNode {n | children = list}, tracker= t}))
    _ -> Err "Node is not associative"

-- ## setChildIndex: Inserting a dragged block into a specific position
setChildIndex: Int -> Int -> Equation prop state -> Result String (Equation prop state)
setChildIndex root newIndex eq = processSubtree_
    (searchPath_ eq.tracker.parent root |> \list -> List.take (List.length list - 1) list)
    (\subEq ->
        let
            extract next = case next of
                [] -> Err "Node not found"
                (current::other) -> if (Math.getState current |> getID) == root
                    then Ok (0, current, other)
                    else extract other |> Result.map (\(index, child, rest) -> (index+1, child, current::rest))
            insert index item list = if index == 0
                then Ok (item :: list)
                else case list of
                    [] -> Err "New index is out of bounds"
                    (x::next) -> insert (index - 1) item next |> Result.map ((::) x)
            processChildren finalize next = extract next
                |> Result.andThen (\(originalIndex, child, others) ->
                    if originalIndex == newIndex then Err "No change detected"
                    else insert newIndex child others
                        |> Result.map (\newChildren ->
                            ((), {root = finalize newChildren, tracker = subEq.tracker})
                        )
                )
        in
        case subEq.root of
            Math.BinaryNode n -> if not n.commutative then Err "Parent node is not commutative"
                else processChildren (\c -> Math.BinaryNode {n | children = c}) n.children
            Math.DeclarativeNode n -> processChildren (\c -> Math.DeclarativeNode {n | children =c}) n.children
            _ -> Err "Parent node is not commutative"
    )
    eq
    |> Result.map Tuple.second

-- ## refreshFuncProp: Updates the cached function properties, and only emits a new value if any of them has been updated
refreshFuncProp: Dict.Dict String {a | property: Math.FunctionProperty prop} -> Equation prop state -> Maybe (Equation prop state)
refreshFuncProp dict eq = let (newRoot, changed) = refreshFuncPropInTree_ dict eq.tracker.ops.update eq.root in
    if changed then Just {eq | root = newRoot} else Nothing

refreshFuncPropInTree_: Dict.Dict String {a | property: Math.FunctionProperty prop} -> (Maybe prop -> state -> (state, Bool)) -> Math.Tree (State state) -> (Math.Tree (State state), Bool)
refreshFuncPropInTree_ dict updater original =
    let
        updateChildren name (State_ num s) children = List.foldl (\child (list, bool) ->
                let (newChild, change) = refreshFuncPropInTree_ dict updater child in
                    (newChild :: list, change || bool)
            ) ([], False) children
            |> \(newChildren, change) -> let (newState, thisChange) = updater (Dict.get name dict |> Maybe.map (.property >> Math.getState)) s in
                (State_ num newState, List.reverse newChildren, change || thisChange)
    in
    case original of
        Math.RealNode _ -> (original, False)
        Math.VariableNode _ -> (original, False)
        Math.UnaryNode n -> let (child, change) = refreshFuncPropInTree_ dict updater n.child in
            case n.state of
                State_ num inner -> let (newState, thisChange) = updater (Dict.get n.name dict |> Maybe.map (.property >> Math.getState)) inner in
                    (Math.UnaryNode {n | state = State_ num newState, child = child}, change || thisChange)
        Math.BinaryNode n -> let (newState, children, change) = updateChildren n.name n.state n.children in
            (Math.BinaryNode {n | state = newState, children = children}, change)
        Math.GenericNode n -> let (newState, children, change) = updateChildren n.name n.state n.children in
            (Math.GenericNode {n | state = newState, children = children}, change)
        Math.DeclarativeNode n -> let (newState, children, change) = updateChildren n.name n.state n.children in
            (Math.DeclarativeNode {n | state = newState, children = children}, change)

-- ## replaceRealNode: merge children back into the parent
replaceRealNode: Int -> Float -> Math.Tree (Maybe prop) -> Equation prop state -> Result String (Int, Equation prop state)
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

matchSubtree: Set.Set Int -> Matcher -> Math.Tree (State state) -> Maybe (MatchResult prop state)
matchSubtree priority matcher root =
    extractPattern_ priority matcher root (Backtrack.init newResult)
    |> Maybe.andThen Backtrack.getState

extractPattern_: Set.Set Int -> Matcher -> Math.Tree (State state) -> Backtrack.Continuation (MatchResult prop state) -> Maybe (Backtrack.Continuation (MatchResult prop state))
extractPattern_ priority from root token = case from of
    RealMatcher s -> case root of
        Math.RealNode n -> if n.value == s.value then Backtrack.return (addExactState_ {name = String.fromFloat n.value, state = n.state} >> Just) token else Backtrack.fail
        _ -> Backtrack.fail
    AnyMatcher s -> case root of
        Math.DeclarativeNode _ -> Backtrack.fail
        _ -> Backtrack.return (\result -> List.indexedMap (\a b -> (b, a)) s.arguments
            |> Dict.fromList
            |> (\varDict -> let newSubtree = toInternalReplacement_ varDict root in
                case Dict.get s.name result.var of
                    Just (InternalValue_ existing) -> if subtreeEqual_ newSubtree existing then Just result else Nothing
                    Nothing -> Just {result | var = Dict.insert s.name (InternalValue_ newSubtree) result.var}
                    _ -> Nothing
                )
            ) token
    ExactMatcher s -> if List.isEmpty s.arguments
        then case root of
            Math.VariableNode n -> if n.name == s.name then Backtrack.return (addExactState_ n >> Just) token else Backtrack.fail
            _ -> Backtrack.fail
        else case root of
            Math.GenericNode n -> if n.name /= s.name || (List.length s.arguments /= List.length n.children)
                then Backtrack.fail
                else Backtrack.run (Backtrack.orderedStack (extractPattern_ priority) s.arguments) n.children (Backtrack.map (addExactState_ n) token)
            Math.UnaryNode n -> if n.name /= s.name then Backtrack.fail
                else case s.arguments of
                    [arg] -> extractPattern_ priority arg n.child (Backtrack.map (addExactState_ n) token)
                    _ -> Backtrack.fail
            _ -> Backtrack.fail
    CommutativeMatcher s -> case root of
        Math.BinaryNode n -> if s.name /= n.name then Backtrack.fail
            else case s.others of
                Nothing -> let priorityList = priorityList_ priority n.children in
                    if List.length s.arguments /= List.length priorityList then Backtrack.fail
                    else Backtrack.run (Backtrack.unorderedStack (extractPattern_ priority) s.arguments) priorityList (Backtrack.map (addExactState_ n) token)
                Just o -> List.indexedMap Tuple.pair n.children
                    |> List.partition (\(_, childN) -> Set.member (Math.getState childN |> getID) priority)
                    |> (\(a,b) -> Backtrack.run
                        (   Backtrack.unorderedStack (\slot (_, c) -> extractPattern_ priority slot c) s.arguments
                        ++  [ otherMatchEvaluator_ s.name n o ]
                        )
                        (a++b) (Backtrack.map (addExactState_ n) token)
                    )
        _ -> Backtrack.fail
    DeclarativeMatcher s -> case root of
        Math.DeclarativeNode n -> if s.name /= n.name || (List.length s.arguments /= List.length n.children) then Backtrack.fail
            else let priorityList = priorityList_ priority n.children in
                if s.commutative then Backtrack.run (Backtrack.unorderedStack (extractPattern_ priority) s.arguments) priorityList (Backtrack.map (addExactState_ n) token)
                else Backtrack.run (Backtrack.orderedStack (extractPattern_ priority) s.arguments) priorityList (Backtrack.map (addExactState_ n) token)
        _ -> Backtrack.fail

priorityList_: Set.Set Int -> List (Math.Tree (State state)) -> List (Math.Tree (State state))
priorityList_ set = List.partition (\n -> Set.member (Math.getState n |> getID) set) >> (\(a,b) -> a++b)

otherMatchEvaluator_: String -> {a | associative: Bool, commutative: Bool, identity: Float, state: State state} -> String -> Backtrack.Evaluator (MatchResult prop state) (Int, Math.Tree (State state))
otherMatchEvaluator_ op props key nodes = Backtrack.return (\result -> if List.isEmpty nodes
    then Just {result | var = Dict.insert key (Math.RealNode {value = props.identity, state = (Nothing, Nothing)}|> ExternalValue_) result.var}
    else if List.length nodes == 1 then List.head nodes
        |> Maybe.map (\(_, node) -> {result | var = Dict.insert key (toInternalReplacement_ Dict.empty node |> InternalValue_) result.var})
    else List.sortBy Tuple.first nodes
        |> List.indexedMap Tuple.pair
        |> List.partition (\(index, (preIndex, _)) -> index == preIndex)
        |> (\(a,b)-> let processChildren = List.map (\(_, (_, n)) -> toInternalReplacement_ Dict.empty n) in
            Just
            {   result
            |   var = Dict.insert key (MultiValue_ {arguments = 0, op = op, associative = props.associative, commutative = props.commutative, identity = props.identity, state = props.state, pre = processChildren a, post = processChildren b}) result.var
            }
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
replaceSubtree: Set.Set Int -> Replacement prop -> MatchResult prop state -> Equation prop state -> Result String (Int, Equation prop state)
replaceSubtree ids replacement with eq = case affectedSubtree_ ids eq.tracker.parent of
    Nothing -> Err "Unable to find selected nodes"
    Just (id, _) -> processSubtree_
        (searchPath_ eq.tracker.parent id)
        (replaceSubtree_ replacement {with | exact = Dict.map (\_ -> List.reverse) with.exact})
        eq

replaceSubtree_: Replacement prop -> MatchResult prop state -> Equation prop state -> Result String (Int, Equation prop state)
replaceSubtree_ replacement result eq =
    let args = Dict.toList result.var |> List.indexedMap (\index (_, val) -> (index, val)) |> Dict.fromList in
    constructFromReplacement_ -1 eq.tracker {args = args, exact = result.exact} replacement
    |> Result.map (\(r, t, _) ->
        let
            (newRoot, newTracker) = replacedToTree_ 0 t r
            pID = Dict.get (Math.getState eq.root |> getID) eq.tracker.parent
            finalTracker = Set.diff (getSubtreeIDs_ eq.root) (getSubtreeIDs_ newRoot)
                |> Set.foldl (\elem childT -> {childT | parent = Dict.remove elem childT.parent}) (setParent_ newRoot pID newTracker)
        in
        ((Math.getState newRoot |> getID), {root = newRoot, tracker = finalTracker})
    )

type alias ArgResult_ prop state =
    {   args: Dict.Dict Int (Value_ prop state) -- The position in the function to be replaced
    ,   exact: Dict.Dict String (List (State state)) -- The states for specific functions (to be reused)
    }
type Replaced_ state =
    SingleNodeReplaced_ (Math.Tree (State state))
    | MultiNodeReplaced_ {name: String, associative: Bool, commutative: Bool, identity: Float, state: State state} (List (Math.Tree (State state))) (List (Math.Tree (State state)))

replacedToTree_: Int -> Tracker_ prop state -> Replaced_ state -> (Math.Tree (State state), Tracker_ prop state)
replacedToTree_ parent tracker repl = case repl of
    SingleNodeReplaced_ tree -> (tree, tracker)
    MultiNodeReplaced_ f pre post -> let (st, newT) = addNode_ Nothing (f.state |> getState |> tracker.ops.extract) parent tracker in
        (   Math.BinaryNode {state = st, name = f.name, associative = f.associative, commutative = f.commutative, identity = f.identity, children = pre ++ post}
        ,   {newT | parent = List.foldl (\child -> Dict.insert (Math.getState child |> getID) tracker.nextID) newT.parent (pre ++ post) }
        )

type ReplacementList_ state =
    RList_ (List (Math.Tree (State state)))
    | FList_ {name: String, associative: Bool, commutative: Bool, identity: Float, state: State state} (List (Math.Tree (State state))) (List (Math.Tree (State state)))

setParent_: Math.Tree (State state) -> Maybe Int -> Tracker_ prop state -> Tracker_ prop state
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

constructFromReplacement_: Int -> Tracker_ prop state -> ArgResult_ prop state -> Replacement prop -> Result String (Replaced_ state, Tracker_ prop state, ArgResult_ prop state)
constructFromReplacement_ parent tracker arguments replaceNode =
    let
        id = tracker.nextID
        createState (current, _) name = case Dict.get name arguments.exact of
            Just [x] -> addNode_ (Just x) current parent tracker |> \(a,b) -> (a,b,arguments)
            Just (x::others) -> addNode_ (Just x) current parent tracker |> \(a,b) -> (a,b,{arguments | exact = Dict.insert name others arguments.exact})
            _ -> addNode_ Nothing current parent tracker |> \(a,b) -> (a,b,arguments)
        constructChildren name firstT firstArgs = Helper.resultList (\child (list, oT, args) ->
                constructFromReplacement_ id oT args child
                |> Result.map (\(newChild, newT, newRes) -> (newChild :: list, newT, newRes))
            )
            ([], firstT, firstArgs)
            >> Result.andThen (\(children, newT, a) -> toReplacementList_ id name newT children
                |> Result.map (\(rList, finalT) -> (rList, finalT, a))
            )
    in
        case replaceNode of
            Math.RealNode n -> let (s, t, a) = createState n.state (String.fromFloat n.value) in
                Ok (SingleNodeReplaced_ (Math.RealNode {state = s, value = n.value}), t, a)
            Math.UnaryNode n -> let (s, t, a) = createState n.state n.name in
                constructFromReplacement_ id t a n.child
                |> Result.map (\(newChild, newT, newArg) -> replacedToTree_ id newT newChild
                    |> \(tree, finalT) -> (SingleNodeReplaced_ (Math.UnaryNode {state = s, name = n.name, child = tree}), finalT, newArg)
                )
            Math.BinaryNode n -> let (s, t, a) = createState n.state n.name in
                constructChildren n.name t a n.children
                |> Result.map (\(res, newT, newArg) ->
                    (   case res of
                        RList_ children -> SingleNodeReplaced_ (Math.BinaryNode {state = s, name = n.name, associative = n.associative, commutative = n.commutative, identity = n.identity, children = children})
                        FList_ _ pre post -> SingleNodeReplaced_ (Math.BinaryNode {state = s, name = n.name, associative = n.associative, commutative = n.commutative, identity = n.identity, children = pre ++ post})
                    ,   newT, newArg)
                )
            Math.DeclarativeNode n -> let (s, t, a) = createState n.state n.name in
                constructChildren n.name t a n.children
                |> Result.map (\(res, newT, newArg) ->
                    (   case res of
                        RList_ children -> SingleNodeReplaced_ (Math.DeclarativeNode {state = s, name = n.name, children = children})
                        FList_ _ pre post -> SingleNodeReplaced_ (Math.DeclarativeNode {state = s, name = n.name, children = pre ++ post})
                    ,   newT, newArg)
                )
            Math.VariableNode n -> case n.state of
                (_, Nothing) -> let (s, t, a) = createState n.state n.name in
                    Ok (SingleNodeReplaced_ (Math.VariableNode {state = s, name = n.name, constant = n.constant}), t, a)
                (_, Just argNum) -> case Dict.get argNum arguments.args of
                    Nothing -> Err "Couldn't find value to replace"
                    Just value -> constructFromValue_ parent tracker {args = Dict.empty, exact = Dict.empty} value
                        |> Result.map (\(replaced, newT, val) -> replacedToTree_ parent newT replaced
                            |> \(tree, finalT) -> (SingleNodeReplaced_ tree, finalT, {arguments | args = Dict.insert argNum val arguments.args})
                        )
            Math.GenericNode n -> case n.state of
                (_, Nothing) -> let (s, t, a) = createState n.state n.name in
                    constructChildren "" t a n.children
                    |> Result.map (\(res, newT, newArg) ->
                        (   case res of
                                RList_ children -> SingleNodeReplaced_ (Math.GenericNode {state = s, name = n.name, arguments = n.arguments, children = children})
                                FList_ _ pre post -> SingleNodeReplaced_ (Math.GenericNode {state = s, name = n.name, arguments = n.arguments, children = pre ++ post})
                        ,   newT
                        ,   newArg
                        )
                    )
                (props, Just argNum) -> case Dict.get argNum arguments.args of
                    Nothing -> Err "Couldn't find the function to replace"
                    Just value -> constructChildren "" tracker arguments n.children
                        |> Result.andThen (\(res, newT, newArg) -> case res of
                            FList_ _ _ _ -> Err "Something went wrong: Grouping detected"
                            RList_ children -> List.indexedMap (\num child -> (num, AsIsValue_ False child)) children
                                |> \childArgs -> constructFromValue_ parent newT {args = Dict.fromList childArgs, exact = Dict.empty} value
                                |> Result.map (\(finalNode, finalT, newVal) -> (finalNode, finalT, {newArg | args = Dict.insert argNum newVal newArg.args}))
                        )

constructFromInternalReplacement_: Int -> Tracker_ prop state -> ArgResult_ prop state -> InternalReplacement_ state -> Result String (Replaced_ state, Tracker_ prop state, ArgResult_ prop state)
constructFromInternalReplacement_ parent tracker arguments replaceNode =
    let
        id = tracker.nextID
        (s, t) = addNode_ (Math.getState replaceNode |> Tuple.first |> Just) Nothing parent tracker -- properties should be captured in state
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
                        RList_ children -> SingleNodeReplaced_ (Math.BinaryNode {state = s, name = n.name, associative = n.associative, commutative = n.commutative, identity = n.identity, children = children})
                        FList_ _ pre post -> SingleNodeReplaced_ (Math.BinaryNode {state = s, name = n.name, associative = n.associative, commutative = n.commutative, identity = n.identity, children = pre ++ post})
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
                (oldS, Nothing) -> let newState = tracker.ops.copy oldS tracker.nextID in Ok
                    (    SingleNodeReplaced_ (Math.VariableNode {state = State_ tracker.nextID newState, constant = n.constant, name = n.name})
                    ,   {tracker | nextID = tracker.nextID + 1, parent = Dict.insert tracker.nextID parent tracker.parent}
                    ,   arguments
                    )
                (_, Just argNum) -> case Dict.get argNum arguments.args of
                    Nothing -> Err "Couldn't find value to replace"
                    Just value -> constructFromValue_ parent tracker {args = Dict.empty, exact = Dict.empty} value
                        |> Result.map (\(replaced, newT, newVal) -> replacedToTree_ parent newT replaced
                            |> \(tree, finalT) -> (SingleNodeReplaced_ tree, finalT, {arguments | args = Dict.insert argNum newVal arguments.args})
                        )
            Math.GenericNode n -> case n.state of
                (oldS, Nothing) -> constructChildren "" t n.children
                    |> Result.map (\(res, newT, newArg) ->
                        (   case res of
                                RList_ children -> SingleNodeReplaced_ (Math.GenericNode {state = s, name = n.name, arguments = n.arguments, children = children})
                                FList_ _ pre post -> SingleNodeReplaced_ (Math.GenericNode {state = s, name = n.name, arguments = n.arguments, children = pre ++ post})
                        , newT, newArg
                        )
                    )
                (_, Just argNum) -> case Dict.get argNum arguments.args of
                    Nothing -> Err "Couldn't find the function to replace"
                    Just value -> constructChildren "" tracker n.children
                        |> Result.andThen (\(res, newT, newArg) -> case res of
                            FList_ _ _ _ -> Err "Something went wrong: Grouping detected"
                            RList_ children -> List.indexedMap (\num child -> (num, AsIsValue_ False child)) children
                                |> \childArgs -> constructFromValue_ parent newT {args = Dict.fromList childArgs, exact = Dict.empty} value
                                |> Result.map (\(finalNode, finalT, newVal) -> (finalNode, finalT, {newArg | args = Dict.insert argNum newVal newArg.args}))
                        )

toReplacementList_: Int -> String -> Tracker_ prop state -> List (Replaced_ state) -> Result String (ReplacementList_ state, Tracker_ prop state)
toReplacementList_ parent name tracker = List.foldl (\child (list, oT) -> case (list, child) of
        (Nothing, SingleNodeReplaced_ tree) -> (Just (RList_ [tree]), oT)
        (Nothing, MultiNodeReplaced_ p pre post) -> if p.name == name then (Just (FList_ p pre post), oT)
            else addNode_ (Just p.state) Nothing parent oT
                |> \(s, t) -> (Just (RList_ [Math.BinaryNode {state = s, name = p.name, associative = p.associative, commutative = p.commutative, identity = p.identity, children = pre ++ post}]), t)
        (Just (RList_ n), SingleNodeReplaced_ tree) -> (Just (RList_ (tree :: n)), oT)
        (Just (RList_ n), MultiNodeReplaced_ p pre post) -> if p.name == name then (Just (FList_ p pre (post ++ n)), oT)
            else addNode_ (Just p.state) Nothing parent oT
                |> \(s, t) -> (Just (RList_ (Math.BinaryNode {state = s, name = p.name, associative = p.associative, commutative = p.commutative, identity = p.identity, children = pre ++ post}::n)), t)
        (Just (FList_ n preOp postOp), SingleNodeReplaced_ tree) -> (Just (FList_ n preOp (tree :: postOp)), oT)
        (Just (FList_ n preOp postOp), MultiNodeReplaced_ p pre post) -> if p.name == name then (Just (FList_ n preOp (pre ++ post ++ postOp)), oT)
            else addNode_ (Just p.state) Nothing parent oT
                |> \(s, t) -> (Just (FList_ n preOp (Math.BinaryNode {state = s, name = p.name, associative = p.associative, commutative = p.commutative, identity = p.identity, children = pre ++ post}:: postOp)), t)
    )
    (Nothing, tracker)
    >> (\(replacement, t) -> case replacement of
        Nothing -> Err "Missing children"
        Just r -> Ok (r, t)
    )

constructFromValue_: Int -> Tracker_ prop state -> ArgResult_ prop state -> Value_ prop state -> Result String (Replaced_ state, Tracker_ prop state, Value_ prop state)
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
                |> Result.map (\(newPost, finalT, _) -> (MultiNodeReplaced_ {name = p.op, associative = p.associative, commutative = p.commutative, identity = p.identity, state = p.state} (List.reverse newPre) (List.reverse newPost), finalT, value))
            )
    AsIsValue_ used tree -> if used then duplicateTree_ parent tracker tree |> \(newRoot, newTree) -> Ok (SingleNodeReplaced_ newRoot, newTree, value)
        else Ok (SingleNodeReplaced_ tree, {tracker | parent = Dict.insert (Math.getState tree |> getID) parent tracker.parent}, AsIsValue_ True tree)

duplicateTree_: Int -> Tracker_ prop state -> Math.Tree (State state) -> (Math.Tree (State state), Tracker_ prop state)
duplicateTree_ parent tracker root = Math.map (\p s t -> addNode_ (Math.getState s |> Just) Nothing (Maybe.map getID p |> Maybe.withDefault parent) t) tracker root
    |> \(finalRoot, finalT) -> (finalRoot, {finalT | parent = Dict.insert (Math.getState finalRoot |> getID) parent finalT.parent})

-- ## replaceAllOccurences: Using the Equation, switch between LHS<->RHS if it matches
replaceAllOccurrences: Set.Set Int -> Equation prop state -> Equation prop state -> Result String (Set.Set Int, Equation prop state)
replaceAllOccurrences roots with on = case with.root of
    Math.DeclarativeNode withN -> if withN.name /= "=" then Err "Chosen substitution equation is not an equation"
        else case withN.children of
            [left, right] -> createMatcherPair_ with.tracker.ops.extract left right
                |> Result.andThen (\pairs -> Set.foldl (replaceOnNode_ pairs) (Set.empty, on) roots
                    |> \(newRoots, newEq) -> if Set.isEmpty newRoots then Err "No selected nodes can be substituted"
                        else Ok (newRoots, newEq)
                )
            _ -> Err "Ambiguous equation. Ensure that it only equates 2 statements"
    _ -> Err "Chosen substitution equation is not an equation"

createMatcherPair_: (state -> Maybe prop) -> Math.Tree (State state) -> Math.Tree (State state) -> Result String (List (Matcher, Replacement prop))
createMatcherPair_ extract left right =
    let
        extractor = getState >> extract
        toMatcherPair (from, to) = case treeToMatcher_ (\_ _ -> Ok) Dict.empty () from of
            Ok (AnyMatcher n, _) -> let args = List.indexedMap (\index var -> (var, (0, index))) n.arguments |> Dict.fromList in
                toReplacement_ extractor False args to
                |> Result.map (\toR -> treeToMatcher_ (\_ _ -> Ok) args () to
                    |> Result.andThen (\(toM, _) -> toReplacement_ extractor False args from
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

replaceOnNode_: List (Matcher, Replacement prop) -> Int -> (Set.Set Int, Equation prop state) -> (Set.Set Int, Equation prop state)
replaceOnNode_ matches id (selected, eq) = processSubtree_ (searchPath_ eq.tracker.parent id) (\subEq ->
        List.foldl (\(matcher, replacement) result -> case result of
            Just _ -> result
            Nothing -> extractPattern_ Set.empty matcher subEq.root (Backtrack.init newResult)
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

encodeEquation: (state -> Encode.Value) -> Equation prop state -> Encode.Value
encodeEquation convert eq = Encode.object
    [   ("root", Math.encode (encodeState convert) eq.root)
    ,   ("tracker", Encode.object
        [   ("nextID", Encode.int eq.tracker.nextID)
        ,   ("parent", Encode.dict String.fromInt Encode.int eq.tracker.parent)
        ]
        )
    ]

encodeState: (state -> Encode.Value) -> State state -> Encode.Value
encodeState convert s = case s of
    State_ id innerState -> Encode.object [("id", Encode.int id), ("state", convert innerState)]

stateDecoder: Decode.Decoder state -> Decode.Decoder (State state)
stateDecoder innerDec = Decode.map2 State_ (Decode.field "id" Decode.int) (Decode.field "state" innerDec)

equationDecoder: StateOp prop state -> Decode.Decoder state -> Decode.Decoder (Equation prop state)
equationDecoder ops innerDec = Decode.map2 (\root tracker -> {root = root, tracker = tracker})
    (Decode.field "root" <| Math.decoder <| stateDecoder innerDec)
    (   Decode.field "tracker"
        <| Decode.map2 (\id p -> {nextID = id, parent = p, ops = ops})
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

encodeReplacement: (prop -> Encode.Value) -> Replacement prop -> Encode.Value
encodeReplacement propEnc = Math.encode (\(p, n) -> Encode.object
        [ ("num", case n of
                Nothing -> Encode.null
                Just num -> Encode.int num)
        , ("property", case p of
                Nothing -> Encode.null
                Just prop -> propEnc prop
                )
        ]
    )

replacementDecoder: Decode.Decoder prop -> Decode.Decoder (Replacement prop)
replacementDecoder propDec = Math.decoder
    (   Decode.map2 Tuple.pair
        (Decode.maybe <| Decode.field "property" propDec)
        (Decode.maybe <| Decode.field "num" Decode.int)
    )
