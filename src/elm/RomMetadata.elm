module RomMetadata exposing (RomMetadata, fromBytes)

import Array exposing (Array)
import Bitwise
import Component.Cartridge.CartridgeType as CartridgeType exposing (CartridgeType)


type alias RomMetadata =
    { name : String
    , cartridgeType : CartridgeType
    , romSize : Int
    , ramSize : Int
    }


fromBytes : Array Int -> RomMetadata
fromBytes bytes =
    { name = Array.slice 0x0134 0x0144 bytes |> Array.map Char.fromCode |> Array.toList |> List.filter (\char -> char /= Char.fromCode 0) |> String.fromList
    , cartridgeType = Array.get 0x0147 bytes |> Maybe.map CartridgeType.fromId |> Maybe.withDefault CartridgeType.Unknown
    , romSize = Array.get 0x0148 bytes |> Maybe.map (\romSize -> Bitwise.shiftLeftBy romSize (32 * 1024)) |> Maybe.withDefault 0
    , ramSize = Array.get 0x0149 bytes |> Maybe.map mapRamSize |> Maybe.withDefault 0
    }


mapRamSize : Int -> Int
mapRamSize byte =
    case byte of
        0x00 ->
            0

        0x01 ->
            2 * 1024

        0x02 ->
            8 * 1024

        0x03 ->
            32 * 1024

        0x04 ->
            128 * 2014

        0x05 ->
            64 * 2014

        _ ->
            0
