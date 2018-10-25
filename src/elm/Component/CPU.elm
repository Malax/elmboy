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
readRegister8 r8 cpu =
    case r8 of
        A ->
            shiftRightZfBy 8 cpu.af

        B ->
            shiftRightZfBy 8 cpu.bc

        C ->
            and 0xFF cpu.bc

        D ->
            shiftRightZfBy 8 cpu.de

        E ->
            and 0xFF cpu.de

        F ->
            and 0xFF cpu.af

        H ->
            shiftRightZfBy 8 cpu.hl

        L ->
            and 0xFF cpu.hl


readRegister16 : Register16 -> CPU -> Int
readRegister16 r16 cpu =
    case r16 of
        AF ->
            cpu.af

        BC ->
            cpu.bc

        DE ->
            cpu.de

        HL ->
            cpu.hl

        PC ->
            cpu.pc

        SP ->
            cpu.sp



{- The following functions create new records instead of copying them. This increases performance up to 5000% in Firefox and 900% in Chrome.
   Writing registers is a very common operation, that's why I made the tradeoff here.
-}


writeRegister8 : Register8 -> Int -> CPU -> CPU
writeRegister8 r8 value { af, bc, de, hl, pc, sp, halted, interruptFlag, interruptEnable, interruptMasterEnable } =
    case r8 of
        A ->
            { af = and 0xFF af |> or (shiftLeftBy 8 value)

            -- 1:1 Copies
            , bc = bc
            , de = de
            , hl = hl
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        F ->
            { af = and 0xFF00 af |> or value

            -- 1:1 Copies
            , bc = bc
            , de = de
            , hl = hl
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        B ->
            { bc = and 0xFF bc |> or (shiftLeftBy 8 value)

            -- 1:1 Copies
            , af = af
            , de = de
            , hl = hl
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        C ->
            { bc = and 0xFF00 bc |> or value

            -- 1:1 Copies
            , af = af
            , de = de
            , hl = hl
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        D ->
            { de = and 0xFF de |> or (shiftLeftBy 8 value)

            -- 1:1 Copies
            , af = af
            , bc = bc
            , hl = hl
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        E ->
            { de = and 0xFF00 de |> or value

            -- 1:1 Copies
            , af = af
            , bc = bc
            , hl = hl
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        H ->
            { hl = and 0xFF hl |> or (shiftLeftBy 8 value)

            -- 1:1 Copies
            , af = af
            , de = de
            , bc = bc
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        L ->
            { hl = and 0xFF00 hl |> or value

            -- 1:1 Copies
            , af = af
            , de = de
            , bc = bc
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }


writeRegister16 : Register16 -> Int -> CPU -> CPU
writeRegister16 r16 value { af, bc, de, hl, pc, sp, halted, interruptFlag, interruptEnable, interruptMasterEnable } =
    case r16 of
        AF ->
            -- The lowest 4 bits are always discarded for the F register as per spec
            { af = and 0xFFF0 value

            -- 1:1 Copies
            , bc = bc
            , de = de
            , hl = hl
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        BC ->
            { bc = and 0xFFFF value

            -- 1:1 Copies
            , af = af
            , de = de
            , hl = hl
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        DE ->
            { de = and 0xFFFF value

            -- 1:1 Copies
            , af = af
            , bc = bc
            , hl = hl
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        HL ->
            { hl = and 0xFFFF value

            -- 1:1 Copies
            , af = af
            , bc = bc
            , de = de
            , pc = pc
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        PC ->
            { pc = and 0xFFFF value

            -- 1:1 Copies
            , af = af
            , bc = bc
            , de = de
            , hl = hl
            , sp = sp
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }

        SP ->
            { sp = and 0xFFFF value

            -- 1:1 Copies
            , af = af
            , bc = bc
            , de = de
            , hl = hl
            , pc = pc
            , halted = halted
            , interruptFlag = interruptFlag
            , interruptEnable = interruptEnable
            , interruptMasterEnable = interruptMasterEnable
            }
