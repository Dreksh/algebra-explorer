module Helper exposing (..)

import Dict
import Json.Decode as Decode
import Set

maybeAppend: Maybe a -> List a -> List a
maybeAppend thing list = case thing of
    Nothing -> list
    Just result -> list ++ [result]

listMapWithState: (b -> a -> (c, b)) -> b -> List a -> (List c, b)
listMapWithState process initial = List.foldl
    (\elem (list, s) -> let (res, newS) = process s elem in (res::list, newS) )
    ([], initial)
    >> (\(l, s) -> (List.reverse l, s))

listIndex: Int -> List a -> Maybe a
listIndex num list = case list of
    [] -> Nothing
    (x::other) -> if num == 0 then Just x
        else listIndex (num - 1) other

maybeList: (a -> b -> Maybe b) -> b -> List a -> Maybe b
maybeList process start = List.foldl (\elem res -> Maybe.andThen (process elem) res) (Just start)

maybeDict: (k -> v -> b -> Maybe b) -> b -> Dict.Dict k v -> Maybe b
maybeDict process start = Dict.foldl (\key value result -> Maybe.andThen (process key value) result) (Just start)

resultList: (a -> b -> Result String b) -> b -> List a -> Result String b
resultList process start = List.foldl (\elem res -> Result.andThen (process elem) res) (Ok start)

resultDict: (k -> v -> b -> Result String b) -> b -> Dict.Dict k v -> Result String b
resultDict process start = Dict.foldl (\key value result -> Result.andThen (process key value) result) (Ok start)

resultSet: (a -> b -> Result String b) -> b -> Set.Set a -> Result String b
resultSet process start = Set.foldl (\elem res -> Result.andThen (process elem) res) (Ok start)

resultToDecoder: Result String a -> Decode.Decoder a
resultToDecoder res = case res of
    Err str -> Decode.fail str
    Ok b -> Decode.succeed b