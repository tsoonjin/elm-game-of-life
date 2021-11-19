module Grid exposing (initGrid, getCell, getNextCellValue, initRandomGrid, initWorld, Grid, flatten, tickWorld)
import Array
import Randomizer exposing (generateInt, generateList)
import Random

type alias Grid = Array.Array (Array.Array Int)
type alias RandomableList = (List (List Int), Random.Seed)

flatten plane = plane |> Array.foldr Array.append  (Array.fromList []) |> Array.toList

initGrid: Int -> Int -> Int -> Grid
initGrid col row default =
    Array.repeat row (Array.repeat col default)

initRandomGrid: Int -> Int -> Int -> RandomableList -> RandomableList
initRandomGrid col row range listSoFar =
    let
       (ls, currentSeed) = listSoFar
       (newRandomList, nextSeed) = generateList col (0, range) currentSeed
       newList = newRandomList :: ls
       newListSoFar = (newList, nextSeed)
    in
        if row == 0 then
            newListSoFar
        else
            initRandomGrid col (row - 1) range newListSoFar



initWorld: Random.Seed -> Int -> Int -> Int -> Grid
initWorld initialSeed col row range =
    let
        (initialList, nextSeed) = generateList col (0, range) initialSeed
        (generatedRandomList, _) = initRandomGrid col row range ([initialList], nextSeed)
    in
        List.map Array.fromList generatedRandomList
        |> Array.fromList

getCell grid col row =
    Maybe.withDefault -1 (Array.get col (Maybe.withDefault (Array.fromList []) (Array.get row grid)))

tickWorld: Grid -> Grid
tickWorld grid =
    let
        nextWorld = Array.indexedMap (\row_index -> \row -> (Array.indexedMap (\col_index -> \cell -> getNextCellValue grid col_index row_index) row)) grid
    in
        nextWorld

getNextCellValue: Grid -> Int -> Int -> Int
getNextCellValue grid col row =
    let
        curr_value = getCell grid col row
        grid_row = Array.length grid
        grid_col = Maybe.withDefault (Array.fromList []) (Array.get row grid)
            |> Array.length
        tl = getCell grid (col - 1) (row - 1)
        tc = getCell grid col (row - 1)
        tr = getCell grid (col + 1) (row - 1)
        cl = getCell grid (col - 1) row
        cr = getCell grid (col + 1) row
        bl = getCell grid (col - 1) (row + 1)
        bc = getCell grid col (row + 1)
        br = getCell grid (col + 1) (row + 1)
        total = tl + tc + tr + cl + cr + bl + bc + br
        next_value =
            if curr_value == 0 && total == 3 then
                1
            else if total < 2 || total > 3 then
                0
            else
                curr_value

    in
        next_value


