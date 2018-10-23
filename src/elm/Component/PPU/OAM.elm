module Component.PPU.OAM exposing (foldRIndexes, searchVisibleObjects)

import Array exposing (Array)


{-| Returns a list of object indexes that are visible
-}
searchVisibleObjects : Int -> Int -> Array Int -> List Int
searchVisibleObjects line objectHeight oamRam =
    let
        minY =
            (17 - objectHeight) + line

        maxY =
            line + 16
    in
    foldRIndexes 40 [] <|
        \index acc ->
            case Array.get (index * 4) oamRam of
                Just objectY ->
                    if objectY >= minY && objectY <= maxY then
                        index :: acc

                    else
                        acc

                Nothing ->
                    acc


foldRIndexes : Int -> acc -> (Int -> acc -> acc) -> acc
foldRIndexes remaining acc f =
    if remaining == 0 then
        acc

    else
        foldRIndexes (remaining - 1) (f (remaining - 1) acc) f
