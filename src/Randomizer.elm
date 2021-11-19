module Randomizer exposing (generateInt, generateList)
import Random


generateInt: (Int, Int) -> Random.Seed -> (Int, Random.Seed)
generateInt range seed =
    let
        (min, max) = range
        (value, nextSeed) = Random.step (Random.int min max) seed
    in
        (value, nextSeed)

generateList: Int -> (Int, Int) -> Random.Seed -> (List Int, Random.Seed)
generateList length range seed =
    let
        (min, max) = range
        (list, nextSeed) = Random.step (Random.list length (Random.int min max)) seed
    in
        (list, nextSeed)
