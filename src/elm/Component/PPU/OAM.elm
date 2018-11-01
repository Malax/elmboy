module Component.PPU.OAM exposing (searchVisibleObjects)

import Array exposing (Array)
import Util


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
    Util.foldRIndexes 40 [] <|
        \index acc ->
            case Array.get (index * 4) oamRam of
                Just objectY ->
                    if objectY >= minY && objectY <= maxY then
                        index :: acc

                    else
                        acc

                Nothing ->
                    acc
