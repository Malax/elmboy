module Component.CPU exposing
    ( CPU
    , Register16(..)
    , Register8(..)
    , init
    , readRegister16
    , readRegister8
    , writeRegister16
    , writeRegister8
    )

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)


type alias CPU =
    { af : Int
    , bc : Int
    , de : Int
    , hl : Int
    , pc : Int
    , sp : Int
    , halted : Bool
    , interruptFlag : Int
    , interruptEnable : Int
    , interruptMasterEnable : Bool
    }


init : CPU
init =
    { af = 0x01B0
    , bc = 0x13
    , de = 0xD8
    , hl = 0x014D
    , pc = 0x0100
    , sp = 0xFFFE
    , halted = False
    , interruptFlag = 0xE1
    , interruptEnable = 0x00
    , interruptMasterEnable = True
    }


type Register8
    = A
    | B
    | C
    | D
    | E
    | F
    | H
    | L


type Register16
    = AF
    | BC
    | DE
    | HL
    | PC
    | SP


readRegister8 : Register8 -> CPU -> Int
readRegister8 r8 { af, bc, de, hl, pc, sp } =
    case r8 of
        A ->
            shiftRightZfBy 8 af

        B ->
            shiftRightZfBy 8 bc

        C ->
            and 0xFF bc

        D ->
            shiftRightZfBy 8 de

        E ->
            and 0xFF de

        F ->
            and 0xFF af

        H ->
            shiftRightZfBy 8 hl

        L ->
            and 0xFF hl


readRegister16 : Register16 -> CPU -> Int
readRegister16 r16 { af, bc, de, hl, pc, sp } =
    case r16 of
        AF ->
            af

        BC ->
            bc

        DE ->
            de

        HL ->
            hl

        PC ->
            pc

        SP ->
            sp


writeRegister8 : Register8 -> Int -> CPU -> CPU
writeRegister8 r8 value ({ af, bc, de, hl, pc, sp } as cpu) =
    case r8 of
        A ->
            { cpu | af = and 0xFF af |> or (shiftLeftBy 8 value) }

        F ->
            { cpu | af = and 0xFF00 af |> or value }

        B ->
            { cpu | bc = and 0xFF bc |> or (shiftLeftBy 8 value) }

        C ->
            { cpu | bc = and 0xFF00 bc |> or value }

        D ->
            { cpu | de = and 0xFF de |> or (shiftLeftBy 8 value) }

        E ->
            { cpu | de = and 0xFF00 de |> or value }

        H ->
            { cpu | hl = and 0xFF hl |> or (shiftLeftBy 8 value) }

        L ->
            { cpu | hl = and 0xFF00 hl |> or value }


writeRegister16 : Register16 -> Int -> CPU -> CPU
writeRegister16 r16 value cpu =
    case r16 of
        AF ->
            -- The lowest 4 bits are always discarded for the F register as per spec
            { cpu | af = and 0xFFF0 value }

        BC ->
            { cpu | bc = and 0xFFFF value }

        DE ->
            { cpu | de = and 0xFFFF value }

        HL ->
            { cpu | hl = and 0xFFFF value }

        PC ->
            { cpu | pc = and 0xFFFF value }

        SP ->
            { cpu | sp = and 0xFFFF value }
