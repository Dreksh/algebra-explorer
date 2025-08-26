module Algo.History exposing (Model, Staged(..), Event(..),
    init, update, current, next, canUndo, canRedo,
    undo, redo, stage, flushAndCommit, commit,
    serialize, encode, decoder
    )

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import Helper

type alias Commit_ c =
    {   parent: Int
    ,   component: c
    ,   children: List Int
    }

type Staged c
    = Undo
    | Redo
    | Revert Int
    | Change c
    | Changes (List c)

type alias Model component =
    {   commits: Dict.Dict Int (Commit_ component)
    ,   root: Commit_ component -- index -1
    ,   staged: Maybe (Staged component)
    ,   visits: List Int -- history
    ,   undone: List Int -- future
    }

type Event c
    = Stage (Staged c)
    | StageAndCommit (Staged c)
    | Reset
    | Commit

init: component -> Model component
init c =
    {   commits = Dict.empty  -- intentionally do not include root here since it acts as a fallback
    ,   root = {parent = -1, component = c, children = []}
    ,   staged = Nothing
    ,   visits = []
    ,   undone = []
    }

update: Event component -> Model component -> Model component
update event model =
    case event of
        Stage staged -> { model | staged = Just staged }
        StageAndCommit staged -> { model | staged = Just staged } |> commit
        Reset -> { model | staged = Nothing }
        Commit -> commit model

canUndo: Model c -> Bool
canUndo model = List.length model.visits > 0

canRedo: Model c -> Bool
canRedo model = List.length model.undone > 0

-- always shows current commit
current: Model component -> component
current model = List.head model.visits
    |> Maybe.andThen (\n -> Dict.get n model.commits)
    |> Maybe.withDefault model.root
    |> .component

-- shows staged changes
next: Model component -> component
next model =
    let
        getFromIdx idx = model.commits
            |> Dict.get idx
            |> Maybe.withDefault model.root
            |> .component
    in
        case model.staged of
            Nothing -> current model
            Just s -> case s of
                Undo -> case model.visits of
                    [] -> model.root.component
                    [_] -> model.root.component
                    (_::idx::_) -> getFromIdx idx
                Redo -> case model.undone of
                    [] -> current model
                    (idx::_) -> getFromIdx idx
                Revert idx -> getFromIdx idx
                Change c -> c
                Changes cs -> List.head cs
                    |> Maybe.withDefault (current model)

currentNode_: Model component -> Int
currentNode_ model = List.head model.visits |> Maybe.withDefault -1

undo: Model component -> Model component
undo model = case model.visits of
    [] -> model
    (x::other) -> {model | visits = other, undone = x :: model.undone}

redo: Model component -> Model component
redo model = case model.undone of
    [] -> model
    (x::other) -> {model | visits = x :: model.visits, undone = other}

stage: component -> Model component -> Model component
stage c model = {model | staged = Just (Change c)}

flushAndCommit: component -> Model component -> Model component
flushAndCommit entry model = commit model |> commit_ entry

commit: Model component -> Model component
commit model = case model.staged of
    Nothing -> model
    Just s -> case s of
        Undo -> case model.visits of
            [] -> model
            (x::others) -> { model | visits = others, undone = x::model.undone, staged = Nothing }
        Redo -> case model.undone of
            [] -> model
            (x::others) -> { model | visits = x::model.visits, undone = others, staged = Nothing }
        Revert idx -> { model | visits = idx::model.visits, undone = [], staged = Nothing }
        Change c -> model |> commit_ c
        Changes cs -> model |> commitMany_ cs

commit_: component -> Model component -> Model component
commit_ c model =
    let
        id = currentNode_ model
        nextID = Dict.size model.commits
    in
        {model | commits = Dict.insert nextID {parent = id, component = c, children = []} model.commits}
        |> \newModel -> let m = { newModel | visits = nextID::model.visits, undone = [], staged = Nothing} in
            case Dict.get id m.commits of
                Maybe.Nothing -> let p = model.root in {m | root = {p | children = nextID :: p.children }}
                Just p -> {m | commits = Dict.insert id {p | children = nextID :: p.children } m.commits }

commitMany_: List component -> Model component -> Model component
commitMany_ list model =
    let
        id = currentNode_ model
        start = Dict.size model.commits
    in
        List.foldl (\c m ->
            {m | commits = Dict.insert (Dict.size m.commits) { parent = id, component = c, children = [] } m.commits}
        ) model list
        |> \newModel -> let m = { newModel | visits = (Dict.size newModel.commits) - 1 :: model.visits, undone = [], staged = Nothing } in
            let end = (Dict.size m.commits) - 1 in
            case Dict.get id model.commits of
                Maybe.Nothing -> let p = model.root in {m | root = { p | children = List.range start end ++ p.children }}
                Just p -> {m | commits = Dict.insert id {p | children = List.range start end ++ p.children } m.commits }


-- processNode is Current, ID, Value, Children as argument
serialize: (Bool -> Int -> component -> List a -> a) -> Model component -> a
serialize processNode model = serialize_ processNode (currentNode_ model) model.commits -1 model.root

serialize_: (Bool -> Int -> c -> List a -> a) -> Int -> Dict.Dict Int (Commit_ c) -> Int -> Commit_ c -> a
serialize_ processNode selectedID commits index n = List.filterMap
    (\i -> Dict.get i commits |> Maybe.map (serialize_ processNode selectedID commits i) )
    n.children
    |> processNode (selectedID == index) index n.component

{-
## State
-}

encode: (c -> Encode.Value) -> Model c -> Encode.Value
encode convert model = Encode.object
    [   ("root", encodeNode_ convert model.root)
    ,   ("commits", Encode.dict String.fromInt (encodeNode_ convert) model.commits)
    ,   ("visits", Encode.list Encode.int model.visits)
    ,   ("undone", Encode.list Encode.int model.undone)
    ]

encodeNode_: (c -> Encode.Value) -> Commit_ c -> Encode.Value
encodeNode_ convert node = Encode.object
    [   ("parent", Encode.int node.parent)
    ,   ("component", convert node.component)
    ,   ("children", Encode.list Encode.int node.children)
    ]

decoder: Decode.Decoder component -> Decode.Decoder (Model component)
decoder innerDec = Decode.map5 Model
    (Decode.field "commits" <| Helper.intDictDecoder <| nodeDecoder_ innerDec)
    (Decode.field "root" <| nodeDecoder_ innerDec)
    (Decode.succeed Nothing)
    (Decode.field "visits" <| Decode.list Decode.int)
    (Decode.field "undone" <| Decode.list Decode.int)

nodeDecoder_: Decode.Decoder component -> Decode.Decoder (Commit_ component)
nodeDecoder_ innerDec = Decode.map3 Commit_
    (Decode.field "parent" Decode.int)
    (Decode.field "component" innerDec)
    (Decode.field "children" <| Decode.list <| Decode.int)
