module Component.Cartridge.CartridgeType exposing (CartridgeType(..), fromId, toString)


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


toString : CartridgeType -> String
toString cartridgeType =
    case cartridgeType of
        RomOnly ->
            "ROM Only"

        MBC1 ->
            "MBC1"

        MBC1Ram ->
            "MBC1+RAM"

        MBC1RamBattery ->
            "MBC1+RAM+Battery"

        MBC2 ->
            "MBC2"

        MBC2Battery ->
            "MBC2+Battery"

        RomRam ->
            "ROM+RAM"

        RomRamBattery ->
            "ROM+RAM+Battery"

        MMM01 ->
            "MMM01"

        MMM01Ram ->
            "MMM01+RAM"

        MMM01RamBattery ->
            "MMM01+RAM+Battery"

        MBC3TimerBattery ->
            "MBC3+Timer+Battery"

        MBC3TimerRamBattery ->
            "MBC3+Timer+RAM+Battery"

        MBC3 ->
            "MBC3"

        MBC3Ram ->
            "MBC3+RAM"

        MBC3RamBattery ->
            "MBC3+RAM+Battery"

        MBC5 ->
            "MBC5"

        MBC5Ram ->
            "MBC5+RAM"

        MBC5RamBattery ->
            "MBC5+RAM+Battery"

        MBC5Rumble ->
            "MBC5+Rumble"

        MBC5RumbleRam ->
            "MBC5+Rumble+RAM"

        MBC5RumbleRamBattery ->
            "MBC5+Rumble+RAM+Battery"

        MBC6 ->
            "MBC6"

        MBC7SensorRumbleRamBattery ->
            "MBC7+Sensor+Rumble+RAM+Battery"

        PocketCamera ->
            "Pocket Camera"

        BandaiTama5 ->
            "BANDAI Tama 5"

        HuC3 ->
            "HuC3"

        HuC1RamBattery ->
            "HuC1+RAM+Battery"

        Unknown ->
            "Unknown"


fromId : Int -> CartridgeType
fromId byte =
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
