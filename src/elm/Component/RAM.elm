module Component.RAM exposing
    ( RAM
    , init
    , initZero
    , readWord8
    , toArray
    , writeWord8
    )

import Array exposing (Array)
import Random
import Types exposing (MemoryAddress)


type RAM
    = RAM (Array Int)


init : Int -> RAM
init size =
    Random.list size (Random.int 0x00 0xFF)
        |> Random.map Array.fromList
        |> (\generator -> Random.step generator (Random.initialSeed 0xCAFEBABE))
        |> Tuple.first
        |> RAM


initZero : Int -> RAM
initZero size =
    Array.initialize size (\_ -> 0x00) |> RAM


readWord8 : RAM -> MemoryAddress -> Int
readWord8 (RAM array) address =
    Array.get address array
        |> Maybe.withDefault 0xFF


writeWord8 : MemoryAddress -> Int -> RAM -> RAM
writeWord8 address value (RAM array) =
    Array.set address value array |> RAM


toArray : RAM -> Array Int
toArray (RAM array) =
    array
