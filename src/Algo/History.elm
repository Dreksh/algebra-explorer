module Algo.History exposing (Model, Event(..), init, update,
    current, add, addAll, redo, undo, serialize,
    encode, decoder
    )

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import Helper

type alias Node_ c =
    {   parent: Int
    ,   component: c
    ,   children: List Int
    }

type alias Model component =
    {   nodes: Dict.Dict Int (Node_ component)
    ,   root: Node_ component -- index 0
    ,   visits: List Int -- history
    ,   undone: List Int -- future
    }

type Event =
    SelectPast Int

init: component -> Model component
init c =
    {   nodes = Dict.empty
    ,   root = {parent = 0, component = c, children = []}
    ,   visits = [0]
    ,   undone = []
    }

undo: Model component -> Model component
undo model = case model.visits of
    [] -> model
    [_] -> model
    (x::others) -> {model | visits = others, undone = x :: model.undone}

redo: Model component -> Model component
redo model = case model.undone of
    [] -> model
    (x::others) -> {model | visits = x::model.visits, undone = others}

current: Model component -> component
current model = List.head model.visits
    |> Maybe.andThen (\n -> Dict.get n model.nodes)
    |> Maybe.withDefault model.root
    |> .component

currentNode_: Model component -> Int
currentNode_ model = List.head model.visits |> Maybe.withDefault 0

add: component -> Model component -> Model component
add c model = let id = currentNode_ model in
    let nextID = (Dict.size model.nodes) + 1 in
    {model | nodes = Dict.insert nextID {parent = id, component = c, children = []} model.nodes}
    |> \newModel -> let m = { newModel | visits = nextID::model.visits, undone = []} in
        if id == 0 then let p = model.root in {m | root = {p | children = nextID :: p.children }}
        else case Dict.get id model.nodes of
            Nothing -> model
            Just p -> {m | nodes = Dict.insert id {p | children = nextID :: p.children } m.nodes }

addAll: List component -> Model component -> Model component
addAll list model = let id = currentNode_ model in
    let start = Dict.size model.nodes + 1 in
    List.foldl (\c m ->
        {m | nodes = Dict.insert ((Dict.size m.nodes) + 1) {parent = id, component = c, children = []} m.nodes}
    ) model list
    |> \newModel -> let m = { newModel | visits = Dict.size newModel.nodes ::model.visits, undone = []} in
        let end = Dict.size m.nodes in
        if id == 0 then let p = model.root in {m | root = {p | children = List.range start end ++ p.children }}
        else case Dict.get id model.nodes of
            Nothing -> model
            Just p -> {m | nodes = Dict.insert id {p | children = List.range start end ++ p.children } m.nodes }

update: Event -> Model component -> Model component
update e model = case e of
    SelectPast n -> {model | visits = n::model.visits, undone = []}

-- processNode is Current, ID, Value, Children as argument
serialize: (Bool -> Int -> component -> List a -> a) -> Model component -> a
serialize processNode model = serialize_ processNode (currentNode_ model) model.nodes 0 model.root

serialize_: (Bool -> Int -> c -> List a -> a) -> Int -> Dict.Dict Int (Node_ c) -> Int -> Node_ c -> a
serialize_ processNode selectedID nodes index n = List.filterMap
    (\i -> Dict.get i nodes |> Maybe.map (serialize_ processNode selectedID nodes i) )
    n.children
    |> processNode (selectedID == index) index n.component

{-
## State
-}

encode: (c -> Encode.Value) -> Model c -> Encode.Value
encode convert model = Encode.object
    [   ("root", encodeNode_ convert model.root)
    ,   ("nodes", Encode.dict String.fromInt (encodeNode_ convert) model.nodes)
    ,   ("visits", Encode.list Encode.int model.visits)
    ,   ("undone", Encode.list Encode.int model.undone)
    ]

encodeNode_: (c -> Encode.Value) -> Node_ c -> Encode.Value
encodeNode_ convert node = Encode.object
    [   ("parent", Encode.int node.parent)
    ,   ("component", convert node.component)
    ,   ("children", Encode.list Encode.int node.children)
    ]

decoder: Decode.Decoder component -> Decode.Decoder (Model component)
decoder innerDec = Decode.map4 Model
    (Decode.field "nodes"<| Helper.intDictDecoder <| nodeDecoder_ innerDec)
    (Decode.field "root" <| nodeDecoder_ innerDec)
    (Decode.field "visits" <| Decode.list Decode.int)
    (Decode.field "undone" <| Decode.list Decode.int)

nodeDecoder_: Decode.Decoder component -> Decode.Decoder (Node_ component)
nodeDecoder_ innerDec = Decode.map3 Node_
    (Decode.field "parent" Decode.int)
    (Decode.field "component" innerDec)
    (Decode.field "children" <| Decode.list <| Decode.int)