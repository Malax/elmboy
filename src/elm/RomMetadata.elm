module RomMetadata exposing (CartridgeType(..), RomMetadata, fromBytes)

import Array exposing (Array)
import Bitwise


type CartridgeType
    = RomOnly
    | MBC1
    | MBC1Ram
    | MBC1RamBattery
    | MBC2
    | MBC2Battery
    | RomRam
    | RomRamBattery
    | MMM01
    | MMM01Ram
    | MMM01RamBattery
    | MBC3TimerBattery
    | MBC3TimerRamBattery
    | MBC3
    | MBC3Ram
    | MBC3RamBattery
    | MBC5
    | MBC5Ram
    | MBC5RamBattery
    | MBC5Rumble
    | MBC5RumbleRam
    | MBC5RumbleRamBattery
    | MBC6
    | MBC7SensorRumbleRamBattery
    | PocketCamera
    | BandaiTama5
    | HuC3
    | HuC1RamBattery
    | Unknown


type alias RomMetadata =
    { name : String
    , cartridgeType : CartridgeType
    , romSize : Int
    , ramSize : Int
    }


fromBytes : Array Int -> RomMetadata
fromBytes bytes =
    { name = Array.slice 0x0134 0x0144 bytes |> Array.map Char.fromCode |> Array.toList |> List.filter (\char -> char /= Char.fromCode 0) |> String.fromList
    , cartridgeType = Array.get 0x0147 bytes |> Maybe.map mapCartridgeType |> Maybe.withDefault Unknown
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


mapCartridgeType : Int -> CartridgeType
mapCartridgeType byte =
    case byte of
        0x00 ->
            RomOnly

        0x01 ->
            MBC1

        0x02 ->
            MBC1Ram

        0x03 ->
            MBC1RamBattery

        0x05 ->
            MBC2

        0x06 ->
            MBC2Battery

        0x08 ->
            RomRam

        0x09 ->
            RomRamBattery

        0x0B ->
            MMM01

        0x0C ->
            MMM01Ram

        0x0D ->
            MMM01RamBattery

        0x0F ->
            MBC3TimerBattery

        0x10 ->
            MBC3TimerRamBattery

        0x11 ->
            MBC3

        0x12 ->
            MBC3Ram

        0x13 ->
            MBC3RamBattery

        0x19 ->
            MBC5

        0x1A ->
            MBC5Ram

        0x1B ->
            MBC5RamBattery

        0x1C ->
            MBC5Rumble

        0x1D ->
            MBC5RumbleRam

        0x1E ->
            MBC5RumbleRamBattery

        0x20 ->
            MBC6

        0x22 ->
            MBC7SensorRumbleRamBattery

        0xFC ->
            PocketCamera

        0xFD ->
            BandaiTama5

        0xFE ->
            HuC3

        0xFF ->
            HuC1RamBattery

        _ ->
            Unknown
