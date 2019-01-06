module Util exposing (boolToBit, byteToSignedInt, chunkList, foldRIndexes, maybePredicate, stringToBytes, uint8ArrayDecoder, word16ToString, word8ToString)

import Array exposing (Array)
import Bitwise
import Bytes.Decode exposing (Step(..))
import Constants
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
    if Bitwise.and Constants.bit7Mask sanitizedByte == Constants.bit7Mask then
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


uint8ArrayDecoder : Int -> Bytes.Decode.Decoder (Array Int)
uint8ArrayDecoder width =
    Bytes.Decode.loop ( Array.empty, width ) <|
        \( acc, remainingBytes ) ->
            if remainingBytes > 0 then
                Bytes.Decode.unsignedInt8
                    |> Bytes.Decode.map (\byte -> Loop ( Array.push byte acc, remainingBytes - 1 ))

            else
                Bytes.Decode.succeed (Done acc)


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


foldRIndexes : Int -> acc -> (Int -> acc -> acc) -> acc
foldRIndexes remaining acc f =
    if remaining == 0 then
        acc

    else
        foldRIndexes (remaining - 1) (f (remaining - 1) acc) f


boolToBit : Bool -> Int
boolToBit value =
    if value then
        0x01

    else
        0x00
