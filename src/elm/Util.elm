module Util exposing (byteToSignedInt, chunkList, conditionalOrBitmask, maybePredicate, stringToBytes, word16ToString, word8ToString)

import Array exposing (Array)
import Bitwise
import Hex


word16ToString : Int -> String
word16ToString =
    Hex.toString >> String.toUpper >> String.padLeft 4 '0' >> (++) "0x"


word8ToString : Int -> String
word8ToString =
    Hex.toString >> String.toUpper >> String.padLeft 2 '0' >> (++) "0x"


byteToSignedInt : Int -> Int
byteToSignedInt value =
    let
        sanitizedByte =
            Bitwise.and 0xFF value
    in
    if Bitwise.and 0x80 sanitizedByte == 0x80 then
        negate (Bitwise.and 0xFF (Bitwise.complement sanitizedByte) + 1)

    else
        value


chunkList : Int -> List a -> List (List a)
chunkList size list =
    case list of
        [] ->
            []

        _ ->
            List.take size list :: chunkList size (List.drop size list)


maybePredicate : (a -> Bool) -> Maybe a -> Bool
maybePredicate predicate =
    Maybe.map predicate >> Maybe.withDefault False


stringToBytes : String -> Array Int
stringToBytes string =
    let
        length =
            String.length string

        byteAtPos : Int -> String -> Int
        byteAtPos pos =
            String.slice (pos * 2) (pos * 2 + 2)
                >> Hex.fromString
                >> Result.withDefault 0x00

        recurse : String -> Array Int -> Array Int
        recurse s acc =
            if Array.length acc < length // 2 then
                recurse s (Array.push (byteAtPos (Array.length acc) s) acc)

            else
                acc
    in
    recurse string Array.empty


conditionalOrBitmask : Bool -> Int -> Int
conditionalOrBitmask condition mask =
    if condition then
        mask

    else
        0x00
