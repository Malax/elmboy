module Component.CPU.OpcodeMapper exposing (get)

import Array exposing (Array)
import Component.CPU as CPU exposing (Register16(..), Register8(..))
import Component.CPU.Condition as Condition exposing (Condition(..))
import Component.CPU.Opcode as HighLevelOpcode
import Component.MMU as MMU
import CoreEffect exposing (readMemory16AdvancePC, readMemory8, readMemory8AdvancePC, readRegister16, readRegister8, writeMemory16, writeMemory8, writeRegister16, writeRegister8)
import Effect exposing (Effect, Reader, Writer, mapReader)
import GameBoy


get : Int -> Effect
get opcode =
    case Array.get opcode opcodes of
        Just effect ->
            effect

        Nothing ->
            identity



-- Internal


opcodes : Array Effect
opcodes =
    Array.fromList
        [ -- 0x00
          HighLevelOpcode.nop
        , -- 0x01
          HighLevelOpcode.ld (writeRegister16 BC) readMemory16AdvancePC
        , -- 0x02
          HighLevelOpcode.ld (readRegister16 BC |> writeMemory8) (readRegister8 A)
        , -- 0x03
          HighLevelOpcode.inc16 (readRegister16 BC) (writeRegister16 BC)
        , -- 0x04
          HighLevelOpcode.inc (readRegister8 B) (writeRegister8 B)
        , -- 0x05
          HighLevelOpcode.dec (readRegister8 B) (writeRegister8 B)
        , -- 0x06
          HighLevelOpcode.ld (writeRegister8 B) readMemory8AdvancePC
        , -- 0x07
          HighLevelOpcode.rlca
        , -- 0x08
          HighLevelOpcode.ld (writeMemory16 readMemory16AdvancePC) (readRegister16 SP)
        , -- 0x09
          HighLevelOpcode.add16 (readRegister16 BC)
        , -- 0x0A
          HighLevelOpcode.ld (writeRegister8 A) (readRegister16 BC |> readMemory8)
        , -- 0x0B
          HighLevelOpcode.dec16 (readRegister16 BC) (writeRegister16 BC)
        , -- 0x0C
          HighLevelOpcode.inc (readRegister8 C) (writeRegister8 C)
        , -- 0x0D
          HighLevelOpcode.dec (readRegister8 C) (writeRegister8 C)
        , -- 0x0E
          HighLevelOpcode.ld (writeRegister8 C) readMemory8AdvancePC
        , -- 0x0F
          HighLevelOpcode.rrca
        , -- 0x10
          HighLevelOpcode.nop -- Stop
        , -- 0x11
          HighLevelOpcode.ld (writeRegister16 DE) readMemory16AdvancePC
        , -- 0x12
          HighLevelOpcode.ld (readRegister16 DE |> writeMemory8) (readRegister8 A)
        , -- 0x13
          HighLevelOpcode.inc16 (readRegister16 DE) (writeRegister16 DE)
        , -- 0x14
          HighLevelOpcode.inc (readRegister8 D) (writeRegister8 D)
        , -- 0x15
          HighLevelOpcode.dec (readRegister8 D) (writeRegister8 D)
        , -- 0x16
          HighLevelOpcode.ld (writeRegister8 D) readMemory8AdvancePC
        , -- 0x17
          HighLevelOpcode.rla
        , -- 0x18
          HighLevelOpcode.jr Condition.Always
        , -- 0x19
          HighLevelOpcode.add16 (readRegister16 DE)
        , -- 0x1A
          HighLevelOpcode.ld (writeRegister8 A) (readRegister16 DE |> readMemory8)
        , -- 0x1B
          HighLevelOpcode.dec16 (readRegister16 DE) (writeRegister16 DE)
        , -- 0x1C
          HighLevelOpcode.inc (readRegister8 E) (writeRegister8 E)
        , -- 0x1D
          HighLevelOpcode.dec (readRegister8 E) (writeRegister8 E)
        , -- 0x1E
          HighLevelOpcode.ld (writeRegister8 E) readMemory8AdvancePC
        , -- 0x1F
          HighLevelOpcode.rra
        , -- 0x20
          HighLevelOpcode.jr Condition.NotZero
        , -- 0x21
          HighLevelOpcode.ld (writeRegister16 HL) readMemory16AdvancePC
        , -- 0x22
          HighLevelOpcode.ld writeIndirectHLPostIncrement (readRegister8 A)
        , -- 0x23
          HighLevelOpcode.inc16 (readRegister16 HL) (writeRegister16 HL)
        , -- 0x24
          HighLevelOpcode.inc (readRegister8 H) (writeRegister8 H)
        , -- 0x25
          HighLevelOpcode.dec (readRegister8 H) (writeRegister8 H)
        , -- 0x26
          HighLevelOpcode.ld (writeRegister8 H) readMemory8AdvancePC
        , -- 0x27
          HighLevelOpcode.daa
        , -- 0x28
          HighLevelOpcode.jr Condition.Zero
        , -- 0x29
          HighLevelOpcode.add16 (readRegister16 HL)
        , -- 0x2A
          HighLevelOpcode.ld (writeRegister8 A) readIndirectHLPostIncrement
        , -- 0x2B
          HighLevelOpcode.dec16 (readRegister16 HL) (writeRegister16 HL)
        , -- 0x2C
          HighLevelOpcode.inc (readRegister8 L) (writeRegister8 L)
        , -- 0x2D
          HighLevelOpcode.dec (readRegister8 L) (writeRegister8 L)
        , -- 0x2E
          HighLevelOpcode.ld (writeRegister8 L) readMemory8AdvancePC
        , -- 0x2F
          HighLevelOpcode.cpl
        , -- 0x30
          HighLevelOpcode.jr Condition.NotCarry
        , -- 0x31
          HighLevelOpcode.ld (writeRegister16 SP) readMemory16AdvancePC
        , -- 0x32
          HighLevelOpcode.ld writeIndirectHLPostDecrement (readRegister8 A)
        , -- 0x33
          HighLevelOpcode.inc16 (readRegister16 SP) (writeRegister16 SP)
        , -- 0x34
          HighLevelOpcode.inc readIndirectHL writeIndirectHL
        , -- 0x35
          HighLevelOpcode.dec readIndirectHL writeIndirectHL
        , -- 0x36
          HighLevelOpcode.ld writeIndirectHL readMemory8AdvancePC
        , -- 0x37
          HighLevelOpcode.scf
        , -- 0x38
          HighLevelOpcode.jr Condition.Carry
        , -- 0x39
          HighLevelOpcode.add16 (readRegister16 SP)
        , -- 0x3A
          HighLevelOpcode.ld (writeRegister8 A) readIndirectHLPostDecrement
        , -- 0x3B
          HighLevelOpcode.dec16 (readRegister16 SP) (writeRegister16 SP)
        , -- 0x3C
          HighLevelOpcode.inc (readRegister8 A) (writeRegister8 A)
        , -- 0x3D
          HighLevelOpcode.dec (readRegister8 A) (writeRegister8 A)
        , -- 0x3E
          HighLevelOpcode.ld (writeRegister8 A) readMemory8AdvancePC
        , -- 0x3F
          HighLevelOpcode.ccf
        , -- 0x40
          HighLevelOpcode.ld (writeRegister8 B) (readRegister8 B)
        , -- 0x41
          HighLevelOpcode.ld (writeRegister8 B) (readRegister8 C)
        , -- 0x42
          HighLevelOpcode.ld (writeRegister8 B) (readRegister8 D)
        , -- 0x43
          HighLevelOpcode.ld (writeRegister8 B) (readRegister8 E)
        , -- 0x44
          HighLevelOpcode.ld (writeRegister8 B) (readRegister8 H)
        , -- 0x45
          HighLevelOpcode.ld (writeRegister8 B) (readRegister8 L)
        , -- 0x46
          HighLevelOpcode.ld (writeRegister8 B) (readMemory8 (readRegister16 HL))
        , -- 0x47
          HighLevelOpcode.ld (writeRegister8 B) (readRegister8 A)
        , -- 0x48
          HighLevelOpcode.ld (writeRegister8 C) (readRegister8 B)
        , -- 0x49
          HighLevelOpcode.ld (writeRegister8 C) (readRegister8 C)
        , -- 0x4A
          HighLevelOpcode.ld (writeRegister8 C) (readRegister8 D)
        , -- 0x4B
          HighLevelOpcode.ld (writeRegister8 C) (readRegister8 E)
        , -- 0x4C
          HighLevelOpcode.ld (writeRegister8 C) (readRegister8 H)
        , -- 0x4D
          HighLevelOpcode.ld (writeRegister8 C) (readRegister8 L)
        , -- 0x4E
          HighLevelOpcode.ld (writeRegister8 C) (readMemory8 (readRegister16 HL))
        , -- 0x4F
          HighLevelOpcode.ld (writeRegister8 C) (readRegister8 A)
        , -- 0x50
          HighLevelOpcode.ld (writeRegister8 D) (readRegister8 B)
        , -- 0x51
          HighLevelOpcode.ld (writeRegister8 D) (readRegister8 C)
        , -- 0x52
          HighLevelOpcode.ld (writeRegister8 D) (readRegister8 D)
        , -- 0x53
          HighLevelOpcode.ld (writeRegister8 D) (readRegister8 E)
        , -- 0x54
          HighLevelOpcode.ld (writeRegister8 D) (readRegister8 H)
        , -- 0x55
          HighLevelOpcode.ld (writeRegister8 D) (readRegister8 L)
        , -- 0x56
          HighLevelOpcode.ld (writeRegister8 D) (readMemory8 (readRegister16 HL))
        , -- 0x57
          HighLevelOpcode.ld (writeRegister8 D) (readRegister8 A)
        , -- 0x58
          HighLevelOpcode.ld (writeRegister8 E) (readRegister8 B)
        , -- 0x59
          HighLevelOpcode.ld (writeRegister8 E) (readRegister8 C)
        , -- 0x5A
          HighLevelOpcode.ld (writeRegister8 E) (readRegister8 D)
        , -- 0x5B
          HighLevelOpcode.ld (writeRegister8 E) (readRegister8 E)
        , -- 0x5C
          HighLevelOpcode.ld (writeRegister8 E) (readRegister8 H)
        , -- 0x5D
          HighLevelOpcode.ld (writeRegister8 E) (readRegister8 L)
        , -- 0x5E
          HighLevelOpcode.ld (writeRegister8 E) (readMemory8 (readRegister16 HL))
        , -- 0x5F
          HighLevelOpcode.ld (writeRegister8 E) (readRegister8 A)
        , -- 0x60
          HighLevelOpcode.ld (writeRegister8 H) (readRegister8 B)
        , -- 0x61
          HighLevelOpcode.ld (writeRegister8 H) (readRegister8 C)
        , -- 0x62
          HighLevelOpcode.ld (writeRegister8 H) (readRegister8 D)
        , -- 0x63
          HighLevelOpcode.ld (writeRegister8 H) (readRegister8 E)
        , -- 0x64
          HighLevelOpcode.ld (writeRegister8 H) (readRegister8 H)
        , -- 0x65
          HighLevelOpcode.ld (writeRegister8 H) (readRegister8 L)
        , -- 0x66
          HighLevelOpcode.ld (writeRegister8 H) (readMemory8 (readRegister16 HL))
        , -- 0x67
          HighLevelOpcode.ld (writeRegister8 H) (readRegister8 A)
        , -- 0x68
          HighLevelOpcode.ld (writeRegister8 L) (readRegister8 B)
        , -- 0x69
          HighLevelOpcode.ld (writeRegister8 L) (readRegister8 C)
        , -- 0x6A
          HighLevelOpcode.ld (writeRegister8 L) (readRegister8 D)
        , -- 0x6B
          HighLevelOpcode.ld (writeRegister8 L) (readRegister8 E)
        , -- 0x6C
          HighLevelOpcode.ld (writeRegister8 L) (readRegister8 H)
        , -- 0x6D
          HighLevelOpcode.ld (writeRegister8 L) (readRegister8 L)
        , -- 0x6E
          HighLevelOpcode.ld (writeRegister8 L) (readMemory8 (readRegister16 HL))
        , -- 0x6F
          HighLevelOpcode.ld (writeRegister8 L) (readRegister8 A)
        , -- 0x70
          HighLevelOpcode.ld writeIndirectHL (readRegister8 B)
        , -- 0x71
          HighLevelOpcode.ld writeIndirectHL (readRegister8 C)
        , -- 0x72
          HighLevelOpcode.ld writeIndirectHL (readRegister8 D)
        , -- 0x73
          HighLevelOpcode.ld writeIndirectHL (readRegister8 E)
        , -- 0x74
          HighLevelOpcode.ld writeIndirectHL (readRegister8 H)
        , -- 0x75
          HighLevelOpcode.ld writeIndirectHL (readRegister8 L)
        , -- 0x76
          HighLevelOpcode.halt
        , -- 0x77
          HighLevelOpcode.ld writeIndirectHL (readRegister8 A)
        , -- 0x78
          HighLevelOpcode.ld (writeRegister8 A) (readRegister8 B)
        , -- 0x79
          HighLevelOpcode.ld (writeRegister8 A) (readRegister8 C)
        , -- 0x7A
          HighLevelOpcode.ld (writeRegister8 A) (readRegister8 D)
        , -- 0x7B
          HighLevelOpcode.ld (writeRegister8 A) (readRegister8 E)
        , -- 0x7C
          HighLevelOpcode.ld (writeRegister8 A) (readRegister8 H)
        , -- 0x7D
          HighLevelOpcode.ld (writeRegister8 A) (readRegister8 L)
        , -- 0x7E
          HighLevelOpcode.ld (writeRegister8 A) readIndirectHL
        , -- 0x7F
          HighLevelOpcode.ld (writeRegister8 A) (readRegister8 A)
        , -- 0x80
          HighLevelOpcode.add (readRegister8 B)
        , -- 0x81
          HighLevelOpcode.add (readRegister8 C)
        , -- 0x82
          HighLevelOpcode.add (readRegister8 D)
        , -- 0x83
          HighLevelOpcode.add (readRegister8 E)
        , -- 0x84
          HighLevelOpcode.add (readRegister8 H)
        , -- 0x85
          HighLevelOpcode.add (readRegister8 L)
        , -- 0x86
          HighLevelOpcode.add readIndirectHL
        , -- 0x87
          HighLevelOpcode.add (readRegister8 A)
        , -- 0x88
          HighLevelOpcode.adc (readRegister8 B)
        , -- 0x89
          HighLevelOpcode.adc (readRegister8 C)
        , -- 0x8A
          HighLevelOpcode.adc (readRegister8 D)
        , -- 0x8B
          HighLevelOpcode.adc (readRegister8 E)
        , -- 0x8C
          HighLevelOpcode.adc (readRegister8 H)
        , -- 0x8D
          HighLevelOpcode.adc (readRegister8 L)
        , -- 0x8E
          HighLevelOpcode.adc readIndirectHL
        , -- 0x8F
          HighLevelOpcode.adc (readRegister8 A)
        , -- 0x90
          HighLevelOpcode.sub (readRegister8 B)
        , -- 0x91
          HighLevelOpcode.sub (readRegister8 C)
        , -- 0x92
          HighLevelOpcode.sub (readRegister8 D)
        , -- 0x93
          HighLevelOpcode.sub (readRegister8 E)
        , -- 0x94
          HighLevelOpcode.sub (readRegister8 H)
        , -- 0x95
          HighLevelOpcode.sub (readRegister8 L)
        , -- 0x96
          HighLevelOpcode.sub readIndirectHL
        , -- 0x97
          HighLevelOpcode.sub (readRegister8 A)
        , -- 0x98
          HighLevelOpcode.sbc (readRegister8 B)
        , -- 0x99
          HighLevelOpcode.sbc (readRegister8 C)
        , -- 0x9A
          HighLevelOpcode.sbc (readRegister8 D)
        , -- 0x9B
          HighLevelOpcode.sbc (readRegister8 E)
        , -- 0x9C
          HighLevelOpcode.sbc (readRegister8 H)
        , -- 0x9D
          HighLevelOpcode.sbc (readRegister8 L)
        , -- 0x9E
          HighLevelOpcode.sbc readIndirectHL
        , -- 0x9F
          HighLevelOpcode.sbc (readRegister8 A)
        , -- 0xA0
          HighLevelOpcode.and (readRegister8 B)
        , -- 0xA1
          HighLevelOpcode.and (readRegister8 C)
        , -- 0xA2
          HighLevelOpcode.and (readRegister8 D)
        , -- 0xA3
          HighLevelOpcode.and (readRegister8 E)
        , -- 0xA4
          HighLevelOpcode.and (readRegister8 H)
        , -- 0xA5
          HighLevelOpcode.and (readRegister8 L)
        , -- 0xA6
          HighLevelOpcode.and readIndirectHL
        , -- 0xA7
          HighLevelOpcode.and (readRegister8 A)
        , -- 0xA8
          HighLevelOpcode.xor (readRegister8 B)
        , -- 0xA9
          HighLevelOpcode.xor (readRegister8 C)
        , -- 0xAA
          HighLevelOpcode.xor (readRegister8 D)
        , -- 0xAB
          HighLevelOpcode.xor (readRegister8 E)
        , -- 0xAC
          HighLevelOpcode.xor (readRegister8 H)
        , -- 0xAD
          HighLevelOpcode.xor (readRegister8 L)
        , -- 0xAE
          HighLevelOpcode.xor readIndirectHL
        , -- 0xAF
          HighLevelOpcode.xor (readRegister8 A)
        , -- 0xB0
          HighLevelOpcode.or (readRegister8 B)
        , -- 0xB1
          HighLevelOpcode.or (readRegister8 C)
        , -- 0xB2
          HighLevelOpcode.or (readRegister8 D)
        , -- 0xB3
          HighLevelOpcode.or (readRegister8 E)
        , -- 0xB4
          HighLevelOpcode.or (readRegister8 H)
        , -- 0xB5
          HighLevelOpcode.or (readRegister8 L)
        , -- 0xB6
          HighLevelOpcode.or readIndirectHL
        , -- 0xB7
          HighLevelOpcode.or (readRegister8 A)
        , -- 0xB8
          HighLevelOpcode.cp (readRegister8 B)
        , -- 0xB9
          HighLevelOpcode.cp (readRegister8 C)
        , -- 0xBA
          HighLevelOpcode.cp (readRegister8 D)
        , -- 0xBB
          HighLevelOpcode.cp (readRegister8 E)
        , -- 0xBC
          HighLevelOpcode.cp (readRegister8 H)
        , -- 0xBD
          HighLevelOpcode.cp (readRegister8 L)
        , -- 0xBE
          HighLevelOpcode.cp readIndirectHL
        , -- 0xBF
          HighLevelOpcode.cp (readRegister8 A)
        , -- 0xC0
          HighLevelOpcode.ret Condition.NotZero
        , -- 0xC1
          HighLevelOpcode.pop (writeRegister16 BC)
        , -- 0xC2
          HighLevelOpcode.jp Condition.NotZero readMemory16AdvancePC
        , -- 0xC3
          HighLevelOpcode.jp Condition.Always readMemory16AdvancePC
        , -- 0xC4
          HighLevelOpcode.call Condition.NotZero
        , -- 0xC5
          HighLevelOpcode.push (readRegister16 BC)
        , -- 0xC6
          HighLevelOpcode.add readMemory8AdvancePC
        , -- 0xC7
          HighLevelOpcode.rst 0x00
        , -- 0xC8
          HighLevelOpcode.ret Condition.Zero
        , -- 0xC9
          HighLevelOpcode.ret Condition.Always
        , -- 0xCA
          HighLevelOpcode.jp Condition.Zero readMemory16AdvancePC
        , -- 0xCB
          HighLevelOpcode.extensionOpcode
            (\opcode ->
                Array.get opcode prefixedOpcodes
                    |> Maybe.withDefault identity
            )
        , -- 0xCC
          HighLevelOpcode.call Condition.Zero
        , -- 0xCD
          HighLevelOpcode.call Condition.Always
        , -- 0xCE
          HighLevelOpcode.adc readMemory8AdvancePC
        , -- 0xCF
          HighLevelOpcode.rst 0x08
        , -- 0xD0
          HighLevelOpcode.ret Condition.NotCarry
        , -- 0xD1
          HighLevelOpcode.pop (writeRegister16 DE)
        , -- 0xD2
          HighLevelOpcode.jp Condition.NotCarry readMemory16AdvancePC
        , -- 0xD3
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xD4
          HighLevelOpcode.call Condition.NotCarry
        , -- 0xD5
          HighLevelOpcode.push (readRegister16 DE)
        , -- 0xD6
          HighLevelOpcode.sub readMemory8AdvancePC
        , -- 0xD7
          HighLevelOpcode.rst 0x10
        , -- 0xD8
          HighLevelOpcode.ret Condition.Carry
        , -- 0xD9
          HighLevelOpcode.reti
        , -- 0xDA
          HighLevelOpcode.jp Condition.Carry readMemory16AdvancePC
        , -- 0xDB
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xDC
          HighLevelOpcode.call Condition.Carry
        , -- 0xDD
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xDE
          HighLevelOpcode.sbc readMemory8AdvancePC
        , -- 0xDF
          HighLevelOpcode.rst 0x18
        , -- 0xE0
          HighLevelOpcode.ld (readMemory8AdvancePC |> asZeroPageAddress |> writeMemory8) (readRegister8 A)
        , -- 0xE1
          HighLevelOpcode.pop (writeRegister16 HL)
        , -- 0xE2
          HighLevelOpcode.ld (readRegister8 C |> asZeroPageAddress |> writeMemory8) (readRegister8 A)
        , -- 0xE3
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xE4
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xE5
          HighLevelOpcode.push (readRegister16 HL)
        , -- 0xE6
          HighLevelOpcode.and readMemory8AdvancePC
        , -- 0xE7
          HighLevelOpcode.rst 0x20
        , -- 0xE8
          HighLevelOpcode.addSPSignedImmediate (writeRegister16 SP)
        , -- 0xE9
          HighLevelOpcode.jp Always (readRegister16 HL)
        , -- 0xEA
          HighLevelOpcode.ld (writeMemory8 readMemory16AdvancePC) (readRegister8 A)
        , -- 0xEB
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xEC
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xED
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xEE
          HighLevelOpcode.xor readMemory8AdvancePC
        , -- 0xEF
          HighLevelOpcode.rst 0x28
        , -- 0xF0
          HighLevelOpcode.ld (writeRegister8 A) (readMemory8AdvancePC |> asZeroPageAddress |> readMemory8)
        , -- 0xF1
          HighLevelOpcode.pop (writeRegister16 AF)
        , -- 0xF2
          HighLevelOpcode.ld (writeRegister8 A) (readRegister8 C |> asZeroPageAddress |> readMemory8)
        , -- 0xF3
          HighLevelOpcode.di
        , -- 0xF4
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xF5
          HighLevelOpcode.push (readRegister16 AF)
        , -- 0xF6
          HighLevelOpcode.or readMemory8AdvancePC
        , -- 0xF7
          HighLevelOpcode.rst 0x30
        , -- 0xF8
          HighLevelOpcode.addSPSignedImmediate (writeRegister16 HL)
        , -- 0xF9
          HighLevelOpcode.ld (writeRegister16 SP) (readRegister16 HL)
        , -- 0xFA
          HighLevelOpcode.ld (writeRegister8 A) (readMemory8 readMemory16AdvancePC)
        , -- 0xFB
          HighLevelOpcode.ei
        , -- 0xFC
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xFD
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xFE
          HighLevelOpcode.cp readMemory8AdvancePC
        , -- 0xFF
          HighLevelOpcode.rst 0x38
        ]


prefixedOpcodes : Array Effect
prefixedOpcodes =
    Array.fromList
        [ -- 0x00
          HighLevelOpcode.rlc (readRegister8 B) (writeRegister8 B)
        , -- 0x01
          HighLevelOpcode.rlc (readRegister8 C) (writeRegister8 C)
        , -- 0x02
          HighLevelOpcode.rlc (readRegister8 D) (writeRegister8 D)
        , -- 0x03
          HighLevelOpcode.rlc (readRegister8 E) (writeRegister8 E)
        , -- 0x04
          HighLevelOpcode.rlc (readRegister8 H) (writeRegister8 H)
        , -- 0x05
          HighLevelOpcode.rlc (readRegister8 L) (writeRegister8 L)
        , -- 0x06
          HighLevelOpcode.rlc readIndirectHL writeIndirectHL
        , -- 0x07
          HighLevelOpcode.rlc (readRegister8 A) (writeRegister8 A)
        , -- 0x08
          HighLevelOpcode.rrc (readRegister8 B) (writeRegister8 B)
        , -- 0x09
          HighLevelOpcode.rrc (readRegister8 C) (writeRegister8 C)
        , -- 0x0A
          HighLevelOpcode.rrc (readRegister8 D) (writeRegister8 D)
        , -- 0x0B
          HighLevelOpcode.rrc (readRegister8 E) (writeRegister8 E)
        , -- 0x0C
          HighLevelOpcode.rrc (readRegister8 H) (writeRegister8 H)
        , -- 0x0D
          HighLevelOpcode.rrc (readRegister8 L) (writeRegister8 L)
        , -- 0x0E
          HighLevelOpcode.rrc readIndirectHL writeIndirectHL
        , -- 0x0F
          HighLevelOpcode.rrc (readRegister8 A) (writeRegister8 A)
        , -- 0x10
          HighLevelOpcode.rl (readRegister8 B) (writeRegister8 B)
        , -- 0x11
          HighLevelOpcode.rl (readRegister8 C) (writeRegister8 C)
        , -- 0x12
          HighLevelOpcode.rl (readRegister8 D) (writeRegister8 D)
        , -- 0x13
          HighLevelOpcode.rl (readRegister8 E) (writeRegister8 E)
        , -- 0x14
          HighLevelOpcode.rl (readRegister8 H) (writeRegister8 H)
        , -- 0x15
          HighLevelOpcode.rl (readRegister8 L) (writeRegister8 L)
        , -- 0x16
          HighLevelOpcode.rl readIndirectHL writeIndirectHL
        , -- 0x17
          HighLevelOpcode.rl (readRegister8 A) (writeRegister8 A)
        , -- 0x18
          HighLevelOpcode.rr (readRegister8 B) (writeRegister8 B)
        , -- 0x19
          HighLevelOpcode.rr (readRegister8 C) (writeRegister8 C)
        , -- 0x1A
          HighLevelOpcode.rr (readRegister8 D) (writeRegister8 D)
        , -- 0x1B
          HighLevelOpcode.rr (readRegister8 E) (writeRegister8 E)
        , -- 0x1C
          HighLevelOpcode.rr (readRegister8 H) (writeRegister8 H)
        , -- 0x1D
          HighLevelOpcode.rr (readRegister8 L) (writeRegister8 L)
        , -- 0x1E
          HighLevelOpcode.rr readIndirectHL writeIndirectHL
        , -- 0x1F
          HighLevelOpcode.rr (readRegister8 A) (writeRegister8 A)
        , -- 0x20
          HighLevelOpcode.sla (readRegister8 B) (writeRegister8 B)
        , -- 0x21
          HighLevelOpcode.sla (readRegister8 C) (writeRegister8 C)
        , -- 0x22
          HighLevelOpcode.sla (readRegister8 D) (writeRegister8 D)
        , -- 0x23
          HighLevelOpcode.sla (readRegister8 E) (writeRegister8 E)
        , -- 0x24
          HighLevelOpcode.sla (readRegister8 H) (writeRegister8 H)
        , -- 0x25
          HighLevelOpcode.sla (readRegister8 L) (writeRegister8 L)
        , -- 0x26
          HighLevelOpcode.sla readIndirectHL writeIndirectHL
        , -- 0x27
          HighLevelOpcode.sla (readRegister8 A) (writeRegister8 A)
        , -- 0x28
          HighLevelOpcode.sra (readRegister8 B) (writeRegister8 B)
        , -- 0x29
          HighLevelOpcode.sra (readRegister8 C) (writeRegister8 C)
        , -- 0x2A
          HighLevelOpcode.sra (readRegister8 D) (writeRegister8 D)
        , -- 0x2B
          HighLevelOpcode.sra (readRegister8 E) (writeRegister8 E)
        , -- 0x2C
          HighLevelOpcode.sra (readRegister8 H) (writeRegister8 H)
        , -- 0x2D
          HighLevelOpcode.sra (readRegister8 L) (writeRegister8 L)
        , -- 0x2E
          HighLevelOpcode.sra readIndirectHL writeIndirectHL
        , -- 0x2F
          HighLevelOpcode.sra (readRegister8 A) (writeRegister8 A)
        , -- 0x30
          HighLevelOpcode.swap (readRegister8 B) (writeRegister8 B)
        , -- 0x31
          HighLevelOpcode.swap (readRegister8 C) (writeRegister8 C)
        , -- 0x32
          HighLevelOpcode.swap (readRegister8 D) (writeRegister8 D)
        , -- 0x33
          HighLevelOpcode.swap (readRegister8 E) (writeRegister8 E)
        , -- 0x34
          HighLevelOpcode.swap (readRegister8 H) (writeRegister8 H)
        , -- 0x35
          HighLevelOpcode.swap (readRegister8 L) (writeRegister8 L)
        , -- 0x36
          HighLevelOpcode.swap readIndirectHL writeIndirectHL
        , -- 0x37
          HighLevelOpcode.swap (readRegister8 A) (writeRegister8 A)
        , -- 0x38
          HighLevelOpcode.srl (readRegister8 B) (writeRegister8 B)
        , -- 0x39
          HighLevelOpcode.srl (readRegister8 C) (writeRegister8 C)
        , -- 0x3A
          HighLevelOpcode.srl (readRegister8 D) (writeRegister8 D)
        , -- 0x3B
          HighLevelOpcode.srl (readRegister8 E) (writeRegister8 E)
        , -- 0x3C
          HighLevelOpcode.srl (readRegister8 H) (writeRegister8 H)
        , -- 0x3D
          HighLevelOpcode.srl (readRegister8 L) (writeRegister8 L)
        , -- 0x3E
          HighLevelOpcode.srl readIndirectHL writeIndirectHL
        , -- 0x3F
          HighLevelOpcode.srl (readRegister8 A) (writeRegister8 A)
        , -- 0x40
          HighLevelOpcode.bit 0 (readRegister8 B)
        , -- 0x41
          HighLevelOpcode.bit 0 (readRegister8 C)
        , -- 0x42
          HighLevelOpcode.bit 0 (readRegister8 D)
        , -- 0x43
          HighLevelOpcode.bit 0 (readRegister8 E)
        , -- 0x44
          HighLevelOpcode.bit 0 (readRegister8 H)
        , -- 0x45
          HighLevelOpcode.bit 0 (readRegister8 L)
        , -- 0x46
          HighLevelOpcode.bit 0 readIndirectHL
        , -- 0x47
          HighLevelOpcode.bit 0 (readRegister8 A)
        , -- 0x48
          HighLevelOpcode.bit 1 (readRegister8 B)
        , -- 0x49
          HighLevelOpcode.bit 1 (readRegister8 C)
        , -- 0x4A
          HighLevelOpcode.bit 1 (readRegister8 D)
        , -- 0x4B
          HighLevelOpcode.bit 1 (readRegister8 E)
        , -- 0x4C
          HighLevelOpcode.bit 1 (readRegister8 H)
        , -- 0x4D
          HighLevelOpcode.bit 1 (readRegister8 L)
        , -- 0x4E
          HighLevelOpcode.bit 1 readIndirectHL
        , -- 0x4F
          HighLevelOpcode.bit 1 (readRegister8 A)
        , -- 0x50
          HighLevelOpcode.bit 2 (readRegister8 B)
        , -- 0x51
          HighLevelOpcode.bit 2 (readRegister8 C)
        , -- 0x52
          HighLevelOpcode.bit 2 (readRegister8 D)
        , -- 0x53
          HighLevelOpcode.bit 2 (readRegister8 E)
        , -- 0x54
          HighLevelOpcode.bit 2 (readRegister8 H)
        , -- 0x55
          HighLevelOpcode.bit 2 (readRegister8 L)
        , -- 0x56
          HighLevelOpcode.bit 2 readIndirectHL
        , -- 0x57
          HighLevelOpcode.bit 2 (readRegister8 A)
        , -- 0x58
          HighLevelOpcode.bit 3 (readRegister8 B)
        , -- 0x59
          HighLevelOpcode.bit 3 (readRegister8 C)
        , -- 0x5A
          HighLevelOpcode.bit 3 (readRegister8 D)
        , -- 0x5B
          HighLevelOpcode.bit 3 (readRegister8 E)
        , -- 0x5C
          HighLevelOpcode.bit 3 (readRegister8 H)
        , -- 0x5D
          HighLevelOpcode.bit 3 (readRegister8 L)
        , -- 0x5E
          HighLevelOpcode.bit 3 readIndirectHL
        , -- 0x5F
          HighLevelOpcode.bit 3 (readRegister8 A)
        , -- 0x60
          HighLevelOpcode.bit 4 (readRegister8 B)
        , -- 0x61
          HighLevelOpcode.bit 4 (readRegister8 C)
        , -- 0x62
          HighLevelOpcode.bit 4 (readRegister8 D)
        , -- 0x63
          HighLevelOpcode.bit 4 (readRegister8 E)
        , -- 0x64
          HighLevelOpcode.bit 4 (readRegister8 H)
        , -- 0x65
          HighLevelOpcode.bit 4 (readRegister8 L)
        , -- 0x66
          HighLevelOpcode.bit 4 readIndirectHL
        , -- 0x67
          HighLevelOpcode.bit 4 (readRegister8 A)
        , -- 0x68
          HighLevelOpcode.bit 5 (readRegister8 B)
        , -- 0x69
          HighLevelOpcode.bit 5 (readRegister8 C)
        , -- 0x6A
          HighLevelOpcode.bit 5 (readRegister8 D)
        , -- 0x6B
          HighLevelOpcode.bit 5 (readRegister8 E)
        , -- 0x6C
          HighLevelOpcode.bit 5 (readRegister8 H)
        , -- 0x6D
          HighLevelOpcode.bit 5 (readRegister8 L)
        , -- 0x6E
          HighLevelOpcode.bit 5 readIndirectHL
        , -- 0x6F
          HighLevelOpcode.bit 5 (readRegister8 A)
        , -- 0x70
          HighLevelOpcode.bit 6 (readRegister8 B)
        , -- 0x71
          HighLevelOpcode.bit 6 (readRegister8 C)
        , -- 0x72
          HighLevelOpcode.bit 6 (readRegister8 D)
        , -- 0x73
          HighLevelOpcode.bit 6 (readRegister8 E)
        , -- 0x74
          HighLevelOpcode.bit 6 (readRegister8 H)
        , -- 0x75
          HighLevelOpcode.bit 6 (readRegister8 L)
        , -- 0x76
          HighLevelOpcode.bit 6 readIndirectHL
        , -- 0x77
          HighLevelOpcode.bit 6 (readRegister8 A)
        , -- 0x78
          HighLevelOpcode.bit 7 (readRegister8 B)
        , -- 0x79
          HighLevelOpcode.bit 7 (readRegister8 C)
        , -- 0x7A
          HighLevelOpcode.bit 7 (readRegister8 D)
        , -- 0x7B
          HighLevelOpcode.bit 7 (readRegister8 E)
        , -- 0x7C
          HighLevelOpcode.bit 7 (readRegister8 H)
        , -- 0x7D
          HighLevelOpcode.bit 7 (readRegister8 L)
        , -- 0x7E
          HighLevelOpcode.bit 7 readIndirectHL
        , -- 0x7F
          HighLevelOpcode.bit 7 (readRegister8 A)
        , -- 0x80
          HighLevelOpcode.res 0 (readRegister8 B) (writeRegister8 B)
        , -- 0x81
          HighLevelOpcode.res 0 (readRegister8 C) (writeRegister8 C)
        , -- 0x82
          HighLevelOpcode.res 0 (readRegister8 D) (writeRegister8 D)
        , -- 0x83
          HighLevelOpcode.res 0 (readRegister8 E) (writeRegister8 E)
        , -- 0x84
          HighLevelOpcode.res 0 (readRegister8 H) (writeRegister8 H)
        , -- 0x85
          HighLevelOpcode.res 0 (readRegister8 L) (writeRegister8 L)
        , -- 0x86
          HighLevelOpcode.res 0 readIndirectHL writeIndirectHL
        , -- 0x87
          HighLevelOpcode.res 0 (readRegister8 A) (writeRegister8 A)
        , -- 0x88
          HighLevelOpcode.res 1 (readRegister8 B) (writeRegister8 B)
        , -- 0x89
          HighLevelOpcode.res 1 (readRegister8 C) (writeRegister8 C)
        , -- 0x8A
          HighLevelOpcode.res 1 (readRegister8 D) (writeRegister8 D)
        , -- 0x8B
          HighLevelOpcode.res 1 (readRegister8 E) (writeRegister8 E)
        , -- 0x8C
          HighLevelOpcode.res 1 (readRegister8 H) (writeRegister8 H)
        , -- 0x8D
          HighLevelOpcode.res 1 (readRegister8 L) (writeRegister8 L)
        , -- 0x8E
          HighLevelOpcode.res 1 readIndirectHL writeIndirectHL
        , -- 0x8F
          HighLevelOpcode.res 1 (readRegister8 A) (writeRegister8 A)
        , -- 0x90
          HighLevelOpcode.res 2 (readRegister8 B) (writeRegister8 B)
        , -- 0x91
          HighLevelOpcode.res 2 (readRegister8 C) (writeRegister8 C)
        , -- 0x92
          HighLevelOpcode.res 2 (readRegister8 D) (writeRegister8 D)
        , -- 0x93
          HighLevelOpcode.res 2 (readRegister8 E) (writeRegister8 E)
        , -- 0x94
          HighLevelOpcode.res 2 (readRegister8 H) (writeRegister8 H)
        , -- 0x95
          HighLevelOpcode.res 2 (readRegister8 L) (writeRegister8 L)
        , -- 0x96
          HighLevelOpcode.res 2 readIndirectHL writeIndirectHL
        , -- 0x97
          HighLevelOpcode.res 2 (readRegister8 A) (writeRegister8 A)
        , -- 0x98
          HighLevelOpcode.res 3 (readRegister8 B) (writeRegister8 B)
        , -- 0x99
          HighLevelOpcode.res 3 (readRegister8 C) (writeRegister8 C)
        , -- 0x9A
          HighLevelOpcode.res 3 (readRegister8 D) (writeRegister8 D)
        , -- 0x9B
          HighLevelOpcode.res 3 (readRegister8 E) (writeRegister8 E)
        , -- 0x9C
          HighLevelOpcode.res 3 (readRegister8 H) (writeRegister8 H)
        , -- 0x9D
          HighLevelOpcode.res 3 (readRegister8 L) (writeRegister8 L)
        , -- 0x9E
          HighLevelOpcode.res 3 readIndirectHL writeIndirectHL
        , -- 0x9F
          HighLevelOpcode.res 3 (readRegister8 A) (writeRegister8 A)
        , -- 0xA0
          HighLevelOpcode.res 4 (readRegister8 B) (writeRegister8 B)
        , -- 0xA1
          HighLevelOpcode.res 4 (readRegister8 C) (writeRegister8 C)
        , -- 0xA2
          HighLevelOpcode.res 4 (readRegister8 D) (writeRegister8 D)
        , -- 0xA3
          HighLevelOpcode.res 4 (readRegister8 E) (writeRegister8 E)
        , -- 0xA4
          HighLevelOpcode.res 4 (readRegister8 H) (writeRegister8 H)
        , -- 0xA5
          HighLevelOpcode.res 4 (readRegister8 L) (writeRegister8 L)
        , -- 0xA6
          HighLevelOpcode.res 4 readIndirectHL writeIndirectHL
        , -- 0xA7
          HighLevelOpcode.res 4 (readRegister8 A) (writeRegister8 A)
        , -- 0xA8
          HighLevelOpcode.res 5 (readRegister8 B) (writeRegister8 B)
        , -- 0xA9
          HighLevelOpcode.res 5 (readRegister8 C) (writeRegister8 C)
        , -- 0xAA
          HighLevelOpcode.res 5 (readRegister8 D) (writeRegister8 D)
        , -- 0xAB
          HighLevelOpcode.res 5 (readRegister8 E) (writeRegister8 E)
        , -- 0xAC
          HighLevelOpcode.res 5 (readRegister8 H) (writeRegister8 H)
        , -- 0xAD
          HighLevelOpcode.res 5 (readRegister8 L) (writeRegister8 L)
        , -- 0xAE
          HighLevelOpcode.res 5 readIndirectHL writeIndirectHL
        , -- 0xAF
          HighLevelOpcode.res 5 (readRegister8 A) (writeRegister8 A)
        , -- 0xB0
          HighLevelOpcode.res 6 (readRegister8 B) (writeRegister8 B)
        , -- 0xB1
          HighLevelOpcode.res 6 (readRegister8 C) (writeRegister8 C)
        , -- 0xB2
          HighLevelOpcode.res 6 (readRegister8 D) (writeRegister8 D)
        , -- 0xB3
          HighLevelOpcode.res 6 (readRegister8 E) (writeRegister8 E)
        , -- 0xB4
          HighLevelOpcode.res 6 (readRegister8 H) (writeRegister8 H)
        , -- 0xB5
          HighLevelOpcode.res 6 (readRegister8 L) (writeRegister8 L)
        , -- 0xB6
          HighLevelOpcode.res 6 readIndirectHL writeIndirectHL
        , -- 0xB7
          HighLevelOpcode.res 6 (readRegister8 A) (writeRegister8 A)
        , -- 0xB8
          HighLevelOpcode.res 7 (readRegister8 B) (writeRegister8 B)
        , -- 0xB9
          HighLevelOpcode.res 7 (readRegister8 C) (writeRegister8 C)
        , -- 0xBA
          HighLevelOpcode.res 7 (readRegister8 D) (writeRegister8 D)
        , -- 0xBB
          HighLevelOpcode.res 7 (readRegister8 E) (writeRegister8 E)
        , -- 0xBC
          HighLevelOpcode.res 7 (readRegister8 H) (writeRegister8 H)
        , -- 0xBD
          HighLevelOpcode.res 7 (readRegister8 L) (writeRegister8 L)
        , -- 0xBE
          HighLevelOpcode.res 7 readIndirectHL writeIndirectHL
        , -- 0xBF
          HighLevelOpcode.res 7 (readRegister8 A) (writeRegister8 A)
        , -- 0xC0
          HighLevelOpcode.set 0 (readRegister8 B) (writeRegister8 B)
        , -- 0xC1
          HighLevelOpcode.set 0 (readRegister8 C) (writeRegister8 C)
        , -- 0xC2
          HighLevelOpcode.set 0 (readRegister8 D) (writeRegister8 D)
        , -- 0xC3
          HighLevelOpcode.set 0 (readRegister8 E) (writeRegister8 E)
        , -- 0xC4
          HighLevelOpcode.set 0 (readRegister8 H) (writeRegister8 H)
        , -- 0xC5
          HighLevelOpcode.set 0 (readRegister8 L) (writeRegister8 L)
        , -- 0xC6
          HighLevelOpcode.set 0 readIndirectHL writeIndirectHL
        , -- 0xC7
          HighLevelOpcode.set 0 (readRegister8 A) (writeRegister8 A)
        , -- 0xC8
          HighLevelOpcode.set 1 (readRegister8 B) (writeRegister8 B)
        , -- 0xC9
          HighLevelOpcode.set 1 (readRegister8 C) (writeRegister8 C)
        , -- 0xCA
          HighLevelOpcode.set 1 (readRegister8 D) (writeRegister8 D)
        , -- 0xCB
          HighLevelOpcode.set 1 (readRegister8 E) (writeRegister8 E)
        , -- 0xCC
          HighLevelOpcode.set 1 (readRegister8 H) (writeRegister8 H)
        , -- 0xCD
          HighLevelOpcode.set 1 (readRegister8 L) (writeRegister8 L)
        , -- 0xCE
          HighLevelOpcode.set 1 readIndirectHL writeIndirectHL
        , -- 0xCF
          HighLevelOpcode.set 1 (readRegister8 A) (writeRegister8 A)
        , -- 0xD0
          HighLevelOpcode.set 2 (readRegister8 B) (writeRegister8 B)
        , -- 0xD1
          HighLevelOpcode.set 2 (readRegister8 C) (writeRegister8 C)
        , -- 0xD2
          HighLevelOpcode.set 2 (readRegister8 D) (writeRegister8 D)
        , -- 0xD3
          HighLevelOpcode.set 2 (readRegister8 E) (writeRegister8 E)
        , -- 0xD4
          HighLevelOpcode.set 2 (readRegister8 H) (writeRegister8 H)
        , -- 0xD5
          HighLevelOpcode.set 2 (readRegister8 L) (writeRegister8 L)
        , -- 0xD6
          HighLevelOpcode.set 2 readIndirectHL writeIndirectHL
        , -- 0xD7
          HighLevelOpcode.set 2 (readRegister8 A) (writeRegister8 A)
        , -- 0xD8
          HighLevelOpcode.set 3 (readRegister8 B) (writeRegister8 B)
        , -- 0xD9
          HighLevelOpcode.set 3 (readRegister8 C) (writeRegister8 C)
        , -- 0xDA
          HighLevelOpcode.set 3 (readRegister8 D) (writeRegister8 D)
        , -- 0xDB
          HighLevelOpcode.set 3 (readRegister8 E) (writeRegister8 E)
        , -- 0xDC
          HighLevelOpcode.set 3 (readRegister8 H) (writeRegister8 H)
        , -- 0xDD
          HighLevelOpcode.set 3 (readRegister8 L) (writeRegister8 L)
        , -- 0xDE
          HighLevelOpcode.set 3 readIndirectHL writeIndirectHL
        , -- 0xDF
          HighLevelOpcode.set 3 (readRegister8 A) (writeRegister8 A)
        , -- 0xE0
          HighLevelOpcode.set 4 (readRegister8 B) (writeRegister8 B)
        , -- 0xE1
          HighLevelOpcode.set 4 (readRegister8 C) (writeRegister8 C)
        , -- 0xE2
          HighLevelOpcode.set 4 (readRegister8 D) (writeRegister8 D)
        , -- 0xE3
          HighLevelOpcode.set 4 (readRegister8 E) (writeRegister8 E)
        , -- 0xE4
          HighLevelOpcode.set 4 (readRegister8 H) (writeRegister8 H)
        , -- 0xE5
          HighLevelOpcode.set 4 (readRegister8 L) (writeRegister8 L)
        , -- 0xE6
          HighLevelOpcode.set 4 readIndirectHL writeIndirectHL
        , -- 0xE7
          HighLevelOpcode.set 4 (readRegister8 A) (writeRegister8 A)
        , -- 0xE8
          HighLevelOpcode.set 5 (readRegister8 B) (writeRegister8 B)
        , -- 0xE9
          HighLevelOpcode.set 5 (readRegister8 C) (writeRegister8 C)
        , -- 0xEA
          HighLevelOpcode.set 5 (readRegister8 D) (writeRegister8 D)
        , -- 0xEB
          HighLevelOpcode.set 5 (readRegister8 E) (writeRegister8 E)
        , -- 0xEC
          HighLevelOpcode.set 5 (readRegister8 H) (writeRegister8 H)
        , -- 0xED
          HighLevelOpcode.set 5 (readRegister8 L) (writeRegister8 L)
        , -- 0xEE
          HighLevelOpcode.set 5 readIndirectHL writeIndirectHL
        , -- 0xEF
          HighLevelOpcode.set 5 (readRegister8 A) (writeRegister8 A)
        , -- 0xF0
          HighLevelOpcode.set 6 (readRegister8 B) (writeRegister8 B)
        , -- 0xF1
          HighLevelOpcode.set 6 (readRegister8 C) (writeRegister8 C)
        , -- 0xF2
          HighLevelOpcode.set 6 (readRegister8 D) (writeRegister8 D)
        , -- 0xF3
          HighLevelOpcode.set 6 (readRegister8 E) (writeRegister8 E)
        , -- 0xF4
          HighLevelOpcode.set 6 (readRegister8 H) (writeRegister8 H)
        , -- 0xF5
          HighLevelOpcode.set 6 (readRegister8 L) (writeRegister8 L)
        , -- 0xF6
          HighLevelOpcode.set 6 readIndirectHL writeIndirectHL
        , -- 0xF7
          HighLevelOpcode.set 6 (readRegister8 A) (writeRegister8 A)
        , -- 0xF8
          HighLevelOpcode.set 7 (readRegister8 B) (writeRegister8 B)
        , -- 0xF9
          HighLevelOpcode.set 7 (readRegister8 C) (writeRegister8 C)
        , -- 0xFA
          HighLevelOpcode.set 7 (readRegister8 D) (writeRegister8 D)
        , -- 0xFB
          HighLevelOpcode.set 7 (readRegister8 E) (writeRegister8 E)
        , -- 0xFC
          HighLevelOpcode.set 7 (readRegister8 H) (writeRegister8 H)
        , -- 0xFD
          HighLevelOpcode.set 7 (readRegister8 L) (writeRegister8 L)
        , -- 0xFE
          HighLevelOpcode.set 7 readIndirectHL writeIndirectHL
        , -- 0xFF
          HighLevelOpcode.set 7 (readRegister8 A) (writeRegister8 A)
        ]



-- (HL) reader and writer


readIndirectHL : Reader Int
readIndirectHL =
    readRegister16 HL |> readMemory8


writeIndirectHL : Writer Int
writeIndirectHL =
    readRegister16 HL |> writeMemory8



-- (HL+) and (HL-) reader and writer


readIndirectHLPostIncrement : Reader Int
readIndirectHLPostIncrement gameBoy =
    let
        hlValue =
            CPU.readRegister16 HL gameBoy.cpu

        updatedCpu =
            CPU.writeRegister16 HL (hlValue + 1) gameBoy.cpu
    in
    ( MMU.readWord8 gameBoy hlValue, GameBoy.setCPU updatedCpu gameBoy )


readIndirectHLPostDecrement : Reader Int
readIndirectHLPostDecrement gameBoy =
    let
        hlValue =
            CPU.readRegister16 HL gameBoy.cpu

        updatedCpu =
            CPU.writeRegister16 HL (hlValue - 1) gameBoy.cpu
    in
    ( MMU.readWord8 gameBoy hlValue, GameBoy.setCPU updatedCpu gameBoy )


writeIndirectHLPostIncrement : Writer Int
writeIndirectHLPostIncrement value gameBoy =
    let
        hlValue =
            CPU.readRegister16 HL gameBoy.cpu

        updatedGameBoy =
            MMU.writeWord8 hlValue value gameBoy

        updatedCpu =
            CPU.writeRegister16 HL (hlValue + 1) updatedGameBoy.cpu
    in
    GameBoy.setCPU updatedCpu updatedGameBoy


writeIndirectHLPostDecrement : Writer Int
writeIndirectHLPostDecrement value gameBoy =
    let
        hlValue =
            CPU.readRegister16 HL gameBoy.cpu

        updatedGameBoy =
            MMU.writeWord8 hlValue value gameBoy

        updatedCpu =
            CPU.writeRegister16 HL (hlValue - 1) updatedGameBoy.cpu
    in
    GameBoy.setCPU updatedCpu updatedGameBoy



-- Helpers


asZeroPageAddress : Reader Int -> Reader Int
asZeroPageAddress =
    mapReader addZeroPageOffset


addZeroPageOffset : Int -> Int
addZeroPageOffset value =
    value + 0xFF00
