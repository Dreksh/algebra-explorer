module Algo.History exposing (Model, Staged(..), Event(..),
    init, update, current, next, canUndo, canRedo,
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
    | Change c
    | Changes (List c)

type alias Model component =
    {   commits: Dict.Dict Int (Commit_ component)
    ,   root: Commit_ component -- index 0
    ,   staged: Maybe (Staged component)
    ,   visits: List Int -- history
    ,   undone: List Int -- future
    }

type Event c
    = Stage (Staged c)
    | Reset
    | Commit
    | Revert Int

init: component -> Model component
init c =
    {   commits = Dict.empty
    ,   root = {parent = 0, component = c, children = []}
    ,   staged = Nothing
    ,   visits = [0]
    ,   undone = []
    }

update: Event component -> Model component -> Model component
update event model =
    case event of
        Stage staged -> case staged of
            Undo -> { model | staged = Just Undo }
            Redo -> { model | staged = Just Redo }
            Change c -> { model | staged = Just (Change c) }
            Changes cs -> { model | staged = Just (Changes cs) }
        Reset -> { model | staged = Nothing }
        Commit -> commit model
        Revert idx -> { model | visits = idx::model.visits, undone = [] }

canUndo: Model c -> Bool
canUndo model = case model.visits of
    [] -> False
    [_] -> False
    _ -> True

canRedo: Model c -> Bool
canRedo model = case model.undone of
    [] -> False
    _ -> True

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
        default = List.head model.visits
            |> Maybe.withDefault -1
            |> getFromIdx
    in
        case model.staged of
            Nothing -> default
            Just s -> case s of
                Undo -> case model.visits of
                    [] -> default
                    [_] -> default
                    (_::idx::_) -> getFromIdx idx
                Redo -> case model.undone of
                    [] -> default
                    (idx::_) -> getFromIdx idx
                Change c -> c
                Changes cs -> List.head cs
                    |> Maybe.withDefault default

currentNode_: Model component -> Int
currentNode_ model = List.head model.visits |> Maybe.withDefault 0

commit: Model component -> Model component
commit model = case model.staged of
    Nothing -> model
    Just s -> case s of
        Undo -> case model.visits of
            [] -> model
            [_] -> model
            (x::others) -> { model | visits = others, undone = x :: model.undone, staged = Nothing }
        Redo -> case model.undone of
            [] -> model
            (x::others) -> { model | visits = x::model.visits, undone = others, staged = Nothing }
        Change c -> model |> commit_ c
        Changes cs -> model |> commitMany_ cs

commit_: component -> Model component -> Model component
commit_ c model = let id = currentNode_ model in
    let nextID = (Dict.size model.commits) + 1 in
    {model | commits = Dict.insert nextID {parent = id, component = c, children = []} model.commits}
    |> \newModel -> let m = { newModel | visits = nextID::model.visits, undone = [], staged = Nothing} in
        if id == 0 then let p = model.root in {m | root = {p | children = nextID :: p.children }}
        else case Dict.get id m.commits of
            Maybe.Nothing -> m
            Just p -> {m | commits = Dict.insert id {p | children = nextID :: p.children } m.commits }

commitMany_: List component -> Model component -> Model component
commitMany_ list model = let id = currentNode_ model in
    let start = Dict.size model.commits + 1 in
    List.foldl (\c m ->
        {m | commits = Dict.insert ((Dict.size m.commits) + 1) { parent = id, component = c, children = [] } m.commits}
    ) model list
    |> \newModel -> let m = { newModel | visits = Dict.size newModel.commits :: model.visits, undone = [], staged = Nothing } in
        let end = Dict.size m.commits in
        if id == 0 then let p = model.root in { m | root = { p | children = List.range start end ++ p.children } }
        else case Dict.get id model.commits of
            Maybe.Nothing -> model
            Just p -> {m | commits = Dict.insert id {p | children = List.range start end ++ p.children } m.commits }


-- processNode is Current, ID, Value, Children as argument
serialize: (Bool -> Int -> component -> List a -> a) -> Model component -> a
serialize processNode model = serialize_ processNode (currentNode_ model) model.commits 0 model.root

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
