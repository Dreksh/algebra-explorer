module Custom exposing (..)

import Dict

maybeList: (a -> b -> Maybe b) -> b -> List a -> Maybe b
maybeList process start = List.foldl (\elem res -> Maybe.andThen (process elem) res) (Just start)

maybeDict: (k -> v -> b -> Maybe b) -> b -> Dict.Dict k v -> Maybe b
maybeDict process start = Dict.foldl (\key value result -> Maybe.andThen (process key value) result) (Just start)

resultList: (a -> b -> Result String b) -> b -> List a -> Result String b
resultList process start = List.foldl (\elem res -> Result.andThen (process elem) res) (Ok start)

resultDict: (k -> v -> b -> Result String b) -> b -> Dict.Dict k v -> Result String b
resultDict process start = Dict.foldl (\key value result -> Result.andThen (process key value) result) (Ok start)
