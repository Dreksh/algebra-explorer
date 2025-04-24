module Algo.History exposing (Model, Event(..), init, update,
    current, add, redo, undo, serialize,
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
    ,   children: List {index: Int, height: Int, width: Int}
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
    (x::others) -> {model | visits = x::others, undone = others}

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
    model.nodes
    |> Dict.insert nextID {parent = id, component = c, children = []}
    |> (\nodes -> updateNodeValues_ id {index = nextID, height=1, width = 1} {model | nodes = nodes})
    |> (\newModel -> { newModel | visits = nextID::model.visits, undone = []} )

-- Algorithm assumes that runtime will reuse cached result, and not recalculate each time
updateNodeValues_: Int -> {index: Int, height: Int, width: Int} -> Model component -> Model component
updateNodeValues_ index child model = if index == 0
    then let (_, _, newNode) = updateNode_ child model.root in {model | root = newNode}
    else case Dict.get index model.nodes of
        Nothing -> model
        Just n -> let (height, width, newNode) = updateNode_ child n in
            updateNodeValues_ n.parent {index = index, height = height, width = width} {model | nodes = Dict.insert index newNode model.nodes}

updateNode_: {index: Int, height: Int, width: Int} -> Node_ c -> (Int, Int, Node_ c)
updateNode_ child n = List.filter (\elem -> elem.index /= child.index) n.children
    |> (::) child
    |> List.sortWith (\left right ->
        if left.height /= right.height then
            if left.height < right.height then LT else GT
        else if left.width /= right.width then
            if left.width < right.width then LT else GT
        else EQ
    )
    |> (\newChildren ->
        List.foldl (\elem res -> {res | height = max res.height elem.height, width = res.width + elem.width})
        {height = 1, width = 0}
        newChildren
        |> (\r -> (r.height, r.width, {n | children = newChildren}))
    )

update: Event -> Model component -> Model component
update e model = case e of
    SelectPast n -> {model | visits = n::model.visits}

serialize: (Int -> component -> List a -> a) -> Model component -> a
serialize processNode model = serialize_ processNode model.nodes 0 model.root

serialize_: (Int -> c -> List a -> a) -> Dict.Dict Int (Node_ c) -> Int -> Node_ c -> a
serialize_ converter nodes index n = converter index n.component
    (   List.filterMap
        (\c -> Dict.get c.index nodes |> Maybe.map (serialize_ converter nodes c.index))
        n.children
    )

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
    ,   ("children", Encode.list (\c ->
            Encode.object
            [   ("index", Encode.int c.index)
            ,   ("height", Encode.int c.height)
            ,   ("width", Encode.int c.width)
            ]
            )
            node.children
        )
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
    (Decode.field "children" <| Decode.list <| Decode.map3 (\i h w -> {index = i, height = h, width = w})
        (Decode.field "index" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.field "width" Decode.int)
    )