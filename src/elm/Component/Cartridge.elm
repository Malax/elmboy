module Component.Cartridge exposing (Cartridge, empty, fromBytes, readWord8, writeWord8)

import Array exposing (Array)
import Bitwise
import Component.Cartridge.CartridgeType as CartridgeType
import Component.RAM as RAM exposing (RAM)
import RomMetadata
import Types exposing (MemoryAddress)


type alias Cartridge =
    { bytes : Array Int
    , ram : RAM
    , selectedRomBank : Int
    , selectedRamBank : Int
    , ramEnabled : Bool
    , mbc1BankingMode : MBC1BankingMode
    , memoryBankController : MemoryBankController
    }


type MemoryBankController
    = ROM
    | MBC1
    | MBC3
    | MBC5


type MBC1BankingMode
    = ROMBanking
    | RAMBanking


fromBytes : Array Int -> Maybe Cartridge
fromBytes romBytes =
    let
        memoryBankController =
            case (RomMetadata.fromBytes romBytes).cartridgeType of
                CartridgeType.RomOnly ->
                    Just ROM

                CartridgeType.RomRam ->
                    Just ROM

                CartridgeType.RomRamBattery ->
                    Just ROM

                CartridgeType.MBC1 ->
                    Just MBC1

                CartridgeType.MBC1Ram ->
                    Just MBC1

                CartridgeType.MBC1RamBattery ->
                    Just MBC1

                CartridgeType.MBC3 ->
                    Just MBC3

                CartridgeType.MBC3Ram ->
                    Just MBC3

                CartridgeType.MBC3RamBattery ->
                    Just MBC3

                CartridgeType.MBC5 ->
                    Just MBC5

                CartridgeType.MBC5Ram ->
                    Just MBC5

                CartridgeType.MBC5RamBattery ->
                    Just MBC5

                _ ->
                    Nothing
    in
    memoryBankController
        |> Maybe.map
            (\memoryBankController2 ->
                { bytes = romBytes

                -- Even though most cartridges have less RAM, we initialize the maximum amout to
                -- simplify implementation.
                , ram = RAM.initZero (128 * 1024)
                , selectedRomBank = 0x01
                , selectedRamBank = 0x00
                , ramEnabled = False
                , mbc1BankingMode = RAMBanking
                , memoryBankController = memoryBankController2
                }
            )


readWord8 : Cartridge -> MemoryAddress -> Int
readWord8 cartridge address =
    if address <= 0x3FFF then
        Array.get address cartridge.bytes
            |> Maybe.withDefault 0xFF

    else if address >= 0x4000 && address <= 0x7FFF then
        let
            selectedRomBank =
                if cartridge.memoryBankController == MBC1 then
                    case cartridge.mbc1BankingMode of
                        ROMBanking ->
                            cartridge.selectedRomBank + Bitwise.shiftLeftBy 5 cartridge.selectedRamBank

                        RAMBanking ->
                            cartridge.selectedRomBank

                else
                    cartridge.selectedRomBank

            offset =
                (address - 0x4000) + (selectedRomBank * romBankSize)
        in
        Array.get offset cartridge.bytes
            |> Maybe.withDefault 0xFF

    else if address >= 0xA000 && address <= 0xBFFF then
        let
            selectedRamBank =
                if cartridge.memoryBankController == MBC1 then
                    case cartridge.mbc1BankingMode of
                        ROMBanking ->
                            0

                        RAMBanking ->
                            cartridge.selectedRamBank

                else
                    cartridge.selectedRamBank

            offset =
                (address - 0xA000) + (selectedRamBank * ramBankSize)
        in
        RAM.readWord8 cartridge.ram offset

    else
        0xFF


writeWord8 : MemoryAddress -> Int -> Cartridge -> Cartridge
writeWord8 address value cartridge =
    if address <= 0x1FFF then
        setRamEnabled (value == 0x0A) cartridge

    else if address >= 0x2000 && address <= 0x3FFF then
        let
            modifiedValue =
                case cartridge.memoryBankController of
                    ROM ->
                        value

                    MBC1 ->
                        Bitwise.and 0x1F value |> zeroToOne

                    MBC3 ->
                        Bitwise.and 0x7F value |> zeroToOne

                    MBC5 ->
                        if address >= 0x3000 then
                            let
                                highBit =
                                    Bitwise.shiftLeftBy 8 (Bitwise.and 0x01 value)

                                lowBits =
                                    Bitwise.and 0xFF cartridge.selectedRomBank
                            in
                            highBit + lowBits

                        else
                            value
        in
        setSelectedRomBank modifiedValue cartridge

    else if address >= 0x4000 && address <= 0x5FFF then
        setSelectedRamBank value cartridge

    else if address >= 0x6000 && address <= 0x7FFF && cartridge.memoryBankController == MBC1 then
        let
            mode =
                if Bitwise.and 0x01 value == 0x01 then
                    RAMBanking

                else
                    ROMBanking
        in
        setMbc1BankingMode mode cartridge

    else if address >= 0xA000 && address <= 0xBFFF then
        let
            offset =
                (address - 0xA000) + (cartridge.selectedRamBank * ramBankSize)
        in
        setRam (RAM.writeWord8 offset value cartridge.ram) cartridge

    else
        cartridge


empty : Cartridge
empty =
    { bytes = Array.empty
    , ram = RAM.init 0
    , selectedRomBank = 0
    , selectedRamBank = 0
    , ramEnabled = False
    , mbc1BankingMode = ROMBanking
    , memoryBankController = ROM
    }



-- Internal


romBankSize : Int
romBankSize =
    0x4000


ramBankSize : Int
ramBankSize =
    0x2000


zeroToOne : Int -> Int
zeroToOne value =
    if value == 0x00 then
        0x01

    else
        value


setSelectedRomBank : Int -> Cartridge -> Cartridge
setSelectedRomBank value cartridge =
    { bytes = cartridge.bytes
    , ram = cartridge.ram
    , selectedRomBank = value
    , selectedRamBank = cartridge.selectedRamBank
    , ramEnabled = cartridge.ramEnabled
    , mbc1BankingMode = cartridge.mbc1BankingMode
    , memoryBankController = cartridge.memoryBankController
    }


setSelectedRamBank : Int -> Cartridge -> Cartridge
setSelectedRamBank value cartridge =
    { bytes = cartridge.bytes
    , ram = cartridge.ram
    , selectedRomBank = cartridge.selectedRomBank
    , selectedRamBank = value
    , ramEnabled = cartridge.ramEnabled
    , mbc1BankingMode = cartridge.mbc1BankingMode
    , memoryBankController = cartridge.memoryBankController
    }


setRam : RAM -> Cartridge -> Cartridge
setRam value cartridge =
    { bytes = cartridge.bytes
    , ram = value
    , selectedRomBank = cartridge.selectedRomBank
    , selectedRamBank = cartridge.selectedRamBank
    , ramEnabled = cartridge.ramEnabled
    , mbc1BankingMode = cartridge.mbc1BankingMode
    , memoryBankController = cartridge.memoryBankController
    }


setRamEnabled : Bool -> Cartridge -> Cartridge
setRamEnabled value cartridge =
    { bytes = cartridge.bytes
    , ram = cartridge.ram
    , selectedRomBank = cartridge.selectedRomBank
    , selectedRamBank = cartridge.selectedRamBank
    , ramEnabled = value
    , mbc1BankingMode = cartridge.mbc1BankingMode
    , memoryBankController = cartridge.memoryBankController
    }


setMbc1BankingMode : MBC1BankingMode -> Cartridge -> Cartridge
setMbc1BankingMode value cartridge =
    { bytes = cartridge.bytes
    , ram = cartridge.ram
    , selectedRomBank = cartridge.selectedRomBank
    , selectedRamBank = cartridge.selectedRamBank
    , ramEnabled = cartridge.ramEnabled
    , mbc1BankingMode = value
    , memoryBankController = cartridge.memoryBankController
    }
