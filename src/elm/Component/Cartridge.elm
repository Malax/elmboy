module Component.Cartridge exposing (Cartridge, fromBytes, readWord8, writeWord8)

import Array exposing (Array)
import Bitwise
import Component.Cartridge.CartridgeType as CartridgeType exposing (CartridgeType)
import Component.RAM as RAM exposing (RAM)
import Hex
import RomMetadata
import Types exposing (MemoryAddress)
import Util


type Cartridge
    = NoMBC (Array Int)
    | MBC1 { ramEnabled : Bool, bank1 : Int, bank2 : Int, mode : Int, ram : RAM } (Array Int)
    | MBC5 { ramEnabled : Bool, ramBank : Int, romBank : Int, ram : RAM } (Array Int)


fromBytes : Array Int -> Maybe Cartridge
fromBytes romBytes =
    case (RomMetadata.fromBytes romBytes).cartridgeType of
        CartridgeType.RomOnly ->
            Just (NoMBC romBytes)

        CartridgeType.MBC1 ->
            Just (initMbc1 romBytes)

        CartridgeType.MBC1Ram ->
            Just (initMbc1 romBytes)

        CartridgeType.MBC1RamBattery ->
            Just (initMbc1 romBytes)

        CartridgeType.MBC5RamBattery ->
            Just (initMbc5 romBytes)

        _ ->
            Nothing


readWord8 : Cartridge -> MemoryAddress -> Int
readWord8 cartridge address =
    case cartridge of
        NoMBC array ->
            Array.get address array
                |> Maybe.withDefault 0xFF

        MBC1 { ramEnabled, bank1, bank2, mode, ram } array ->
            if address <= 0x3FFF then
                Array.get address array |> Maybe.withDefault 0xFF

            else if address >= 0x4000 && address <= 0x7FFF then
                let
                    romBank =
                        Bitwise.or (Bitwise.shiftLeftBy 5 bank2) bank1
                in
                Array.get ((address - 0x4000) + (0x4000 * romBank)) array |> Maybe.withDefault 0xFF

            else if address >= 0xA000 && address <= 0xBFFF && ramEnabled then
                RAM.readWord8 ram (address - 0xA000)

            else
                0xFF

        MBC5 { ramEnabled, ramBank, romBank, ram } array ->
            if address <= 0x3FFF then
                Array.get address array |> Maybe.withDefault 0xFF

            else if address >= 0x4000 && address <= 0x7FFF then
                Array.get ((address - 0x4000) + (0x4000 * romBank)) array |> Maybe.withDefault 0xFF

            else if address >= 0xA000 && address <= 0xBFFF && ramEnabled then
                RAM.readWord8 ram ((address - 0xA000) + (0x2000 * ramBank))

            else
                0xFF


writeWord8 : MemoryAddress -> Int -> Cartridge -> Cartridge
writeWord8 address value cartridge =
    case cartridge of
        NoMBC _ ->
            cartridge

        MBC1 mbc1Data array ->
            if address <= 0x1FFF then
                MBC1 { mbc1Data | ramEnabled = Bitwise.and 0x0F value == 0x0A } array

            else if address >= 0x2000 && address <= 0x3FFF then
                let
                    maskedValue =
                        Bitwise.and 0x1F value

                    sanitizedMaskedValue =
                        if maskedValue == 0x00 then
                            0x01

                        else
                            maskedValue
                in
                MBC1 { mbc1Data | bank1 = sanitizedMaskedValue } array

            else if address >= 0x4000 && address <= 0x5FFF then
                MBC1 { mbc1Data | bank2 = Bitwise.and 0x03 value } array

            else if address >= 0x6000 && address <= 0x7FFF then
                MBC1 { mbc1Data | mode = Bitwise.and 0x01 value } array

            else if address >= 0xA000 && address <= 0xBFFF && mbc1Data.ramEnabled then
                MBC1 { mbc1Data | ram = RAM.writeWord8 (address - 0xA000) value mbc1Data.ram } array

            else
                cartridge

        MBC5 mbc5Data array ->
            if address <= 0x1FFF then
                MBC5 { mbc5Data | ramEnabled = Bitwise.and 0x0F value == 0x0A } array

            else if address >= 0x2000 && address <= 0x2FFF then
                MBC5 { mbc5Data | romBank = Bitwise.and 0x0100 mbc5Data.romBank + value } array

            else if address >= 0x3000 && address <= 0x3FFF then
                let
                    toAdd =
                        Bitwise.shiftLeftBy 8 (Bitwise.and 0x01 value)

                    cleanX =
                        Bitwise.and 0xFF mbc5Data.romBank
                in
                MBC5 { mbc5Data | romBank = cleanX + toAdd } array

            else if address >= 0x4000 && address <= 0x5FFF then
                MBC5 { mbc5Data | ramBank = Bitwise.and 0x0F value } array

            else if address >= 0xA000 && address <= 0xBFFF && mbc5Data.ramEnabled then
                MBC5 { mbc5Data | ram = RAM.writeWord8 (address - 0xA000) value mbc5Data.ram } array

            else
                cartridge


initMbc1 : Array Int -> Cartridge
initMbc1 romBytes =
    MBC1 { ramEnabled = False, bank1 = 0x01, bank2 = 0x00, mode = 0x00, ram = RAM.initZero 0x2000 } romBytes


initMbc5 : Array Int -> Cartridge
initMbc5 romBytes =
    MBC5 { ramEnabled = False, ramBank = 0x00, romBank = 0x01, ram = RAM.initZero (128 * 1024) } romBytes
