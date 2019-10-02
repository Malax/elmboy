module Component.CPU.OpcodeMapper exposing (get)

import Array exposing (Array)
import Component.CPU as CPU
import Component.CPU.Condition as Condition exposing (Condition(..))
import Component.CPU.Opcode as HighLevelOpcode
import Component.MMU as MMU
import CoreEffect exposing (readMemory16AdvancePC, readMemory8, readMemory8AdvancePC, writeMemory16, writeMemory8)
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
          HighLevelOpcode.ld CoreEffect.writeRegisterBC readMemory16AdvancePC
        , -- 0x02
          HighLevelOpcode.ld (CoreEffect.readRegisterBC |> writeMemory8) CoreEffect.readRegisterA
        , -- 0x03
          HighLevelOpcode.inc16 CoreEffect.readRegisterBC CoreEffect.writeRegisterBC
        , -- 0x04
          HighLevelOpcode.inc CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x05
          HighLevelOpcode.dec CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x06
          HighLevelOpcode.ld CoreEffect.writeRegisterB readMemory8AdvancePC
        , -- 0x07
          HighLevelOpcode.rlca
        , -- 0x08
          HighLevelOpcode.ld (writeMemory16 readMemory16AdvancePC) CoreEffect.readRegisterSP
        , -- 0x09
          HighLevelOpcode.add16 CoreEffect.readRegisterBC
        , -- 0x0A
          HighLevelOpcode.ld CoreEffect.writeRegisterA (CoreEffect.readRegisterBC |> readMemory8)
        , -- 0x0B
          HighLevelOpcode.dec16 CoreEffect.readRegisterBC CoreEffect.writeRegisterBC
        , -- 0x0C
          HighLevelOpcode.inc CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x0D
          HighLevelOpcode.dec CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x0E
          HighLevelOpcode.ld CoreEffect.writeRegisterC readMemory8AdvancePC
        , -- 0x0F
          HighLevelOpcode.rrca
        , -- 0x10
          HighLevelOpcode.nop -- Stop
        , -- 0x11
          HighLevelOpcode.ld CoreEffect.writeRegisterDE readMemory16AdvancePC
        , -- 0x12
          HighLevelOpcode.ld (CoreEffect.readRegisterDE |> writeMemory8) CoreEffect.readRegisterA
        , -- 0x13
          HighLevelOpcode.inc16 CoreEffect.readRegisterDE CoreEffect.writeRegisterDE
        , -- 0x14
          HighLevelOpcode.inc CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x15
          HighLevelOpcode.dec CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x16
          HighLevelOpcode.ld CoreEffect.writeRegisterD readMemory8AdvancePC
        , -- 0x17
          HighLevelOpcode.rla
        , -- 0x18
          HighLevelOpcode.jr Condition.Always
        , -- 0x19
          HighLevelOpcode.add16 CoreEffect.readRegisterDE
        , -- 0x1A
          HighLevelOpcode.ld CoreEffect.writeRegisterA (CoreEffect.readRegisterDE |> readMemory8)
        , -- 0x1B
          HighLevelOpcode.dec16 CoreEffect.readRegisterDE CoreEffect.writeRegisterDE
        , -- 0x1C
          HighLevelOpcode.inc CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x1D
          HighLevelOpcode.dec CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x1E
          HighLevelOpcode.ld CoreEffect.writeRegisterE readMemory8AdvancePC
        , -- 0x1F
          HighLevelOpcode.rra
        , -- 0x20
          HighLevelOpcode.jr Condition.NotZero
        , -- 0x21
          HighLevelOpcode.ld CoreEffect.writeRegisterHL readMemory16AdvancePC
        , -- 0x22
          HighLevelOpcode.ld writeIndirectHLPostIncrement CoreEffect.readRegisterA
        , -- 0x23
          HighLevelOpcode.inc16 CoreEffect.readRegisterHL CoreEffect.writeRegisterHL
        , -- 0x24
          HighLevelOpcode.inc CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x25
          HighLevelOpcode.dec CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x26
          HighLevelOpcode.ld CoreEffect.writeRegisterH readMemory8AdvancePC
        , -- 0x27
          HighLevelOpcode.daa
        , -- 0x28
          HighLevelOpcode.jr Condition.Zero
        , -- 0x29
          HighLevelOpcode.add16 CoreEffect.readRegisterHL
        , -- 0x2A
          HighLevelOpcode.ld CoreEffect.writeRegisterA readIndirectHLPostIncrement
        , -- 0x2B
          HighLevelOpcode.dec16 CoreEffect.readRegisterHL CoreEffect.writeRegisterHL
        , -- 0x2C
          HighLevelOpcode.inc CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x2D
          HighLevelOpcode.dec CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x2E
          HighLevelOpcode.ld CoreEffect.writeRegisterL readMemory8AdvancePC
        , -- 0x2F
          HighLevelOpcode.cpl
        , -- 0x30
          HighLevelOpcode.jr Condition.NotCarry
        , -- 0x31
          HighLevelOpcode.ld CoreEffect.writeRegisterSP readMemory16AdvancePC
        , -- 0x32
          HighLevelOpcode.ld writeIndirectHLPostDecrement CoreEffect.readRegisterA
        , -- 0x33
          HighLevelOpcode.inc16 CoreEffect.readRegisterSP CoreEffect.writeRegisterSP
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
          HighLevelOpcode.add16 CoreEffect.readRegisterSP
        , -- 0x3A
          HighLevelOpcode.ld CoreEffect.writeRegisterA readIndirectHLPostDecrement
        , -- 0x3B
          HighLevelOpcode.dec16 CoreEffect.readRegisterSP CoreEffect.writeRegisterSP
        , -- 0x3C
          HighLevelOpcode.inc CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x3D
          HighLevelOpcode.dec CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x3E
          HighLevelOpcode.ld CoreEffect.writeRegisterA readMemory8AdvancePC
        , -- 0x3F
          HighLevelOpcode.ccf
        , -- 0x40
          HighLevelOpcode.ld CoreEffect.writeRegisterB CoreEffect.readRegisterB
        , -- 0x41
          HighLevelOpcode.ld CoreEffect.writeRegisterB CoreEffect.readRegisterC
        , -- 0x42
          HighLevelOpcode.ld CoreEffect.writeRegisterB CoreEffect.readRegisterD
        , -- 0x43
          HighLevelOpcode.ld CoreEffect.writeRegisterB CoreEffect.readRegisterE
        , -- 0x44
          HighLevelOpcode.ld CoreEffect.writeRegisterB CoreEffect.readRegisterH
        , -- 0x45
          HighLevelOpcode.ld CoreEffect.writeRegisterB CoreEffect.readRegisterL
        , -- 0x46
          HighLevelOpcode.ld CoreEffect.writeRegisterB (readMemory8 CoreEffect.readRegisterHL)
        , -- 0x47
          HighLevelOpcode.ld CoreEffect.writeRegisterB CoreEffect.readRegisterA
        , -- 0x48
          HighLevelOpcode.ld CoreEffect.writeRegisterC CoreEffect.readRegisterB
        , -- 0x49
          HighLevelOpcode.ld CoreEffect.writeRegisterC CoreEffect.readRegisterC
        , -- 0x4A
          HighLevelOpcode.ld CoreEffect.writeRegisterC CoreEffect.readRegisterD
        , -- 0x4B
          HighLevelOpcode.ld CoreEffect.writeRegisterC CoreEffect.readRegisterE
        , -- 0x4C
          HighLevelOpcode.ld CoreEffect.writeRegisterC CoreEffect.readRegisterH
        , -- 0x4D
          HighLevelOpcode.ld CoreEffect.writeRegisterC CoreEffect.readRegisterL
        , -- 0x4E
          HighLevelOpcode.ld CoreEffect.writeRegisterC (readMemory8 CoreEffect.readRegisterHL)
        , -- 0x4F
          HighLevelOpcode.ld CoreEffect.writeRegisterC CoreEffect.readRegisterA
        , -- 0x50
          HighLevelOpcode.ld CoreEffect.writeRegisterD CoreEffect.readRegisterB
        , -- 0x51
          HighLevelOpcode.ld CoreEffect.writeRegisterD CoreEffect.readRegisterC
        , -- 0x52
          HighLevelOpcode.ld CoreEffect.writeRegisterD CoreEffect.readRegisterD
        , -- 0x53
          HighLevelOpcode.ld CoreEffect.writeRegisterD CoreEffect.readRegisterE
        , -- 0x54
          HighLevelOpcode.ld CoreEffect.writeRegisterD CoreEffect.readRegisterH
        , -- 0x55
          HighLevelOpcode.ld CoreEffect.writeRegisterD CoreEffect.readRegisterL
        , -- 0x56
          HighLevelOpcode.ld CoreEffect.writeRegisterD (readMemory8 CoreEffect.readRegisterHL)
        , -- 0x57
          HighLevelOpcode.ld CoreEffect.writeRegisterD CoreEffect.readRegisterA
        , -- 0x58
          HighLevelOpcode.ld CoreEffect.writeRegisterE CoreEffect.readRegisterB
        , -- 0x59
          HighLevelOpcode.ld CoreEffect.writeRegisterE CoreEffect.readRegisterC
        , -- 0x5A
          HighLevelOpcode.ld CoreEffect.writeRegisterE CoreEffect.readRegisterD
        , -- 0x5B
          HighLevelOpcode.ld CoreEffect.writeRegisterE CoreEffect.readRegisterE
        , -- 0x5C
          HighLevelOpcode.ld CoreEffect.writeRegisterE CoreEffect.readRegisterH
        , -- 0x5D
          HighLevelOpcode.ld CoreEffect.writeRegisterE CoreEffect.readRegisterL
        , -- 0x5E
          HighLevelOpcode.ld CoreEffect.writeRegisterE (readMemory8 CoreEffect.readRegisterHL)
        , -- 0x5F
          HighLevelOpcode.ld CoreEffect.writeRegisterE CoreEffect.readRegisterA
        , -- 0x60
          HighLevelOpcode.ld CoreEffect.writeRegisterH CoreEffect.readRegisterB
        , -- 0x61
          HighLevelOpcode.ld CoreEffect.writeRegisterH CoreEffect.readRegisterC
        , -- 0x62
          HighLevelOpcode.ld CoreEffect.writeRegisterH CoreEffect.readRegisterD
        , -- 0x63
          HighLevelOpcode.ld CoreEffect.writeRegisterH CoreEffect.readRegisterE
        , -- 0x64
          HighLevelOpcode.ld CoreEffect.writeRegisterH CoreEffect.readRegisterH
        , -- 0x65
          HighLevelOpcode.ld CoreEffect.writeRegisterH CoreEffect.readRegisterL
        , -- 0x66
          HighLevelOpcode.ld CoreEffect.writeRegisterH (readMemory8 CoreEffect.readRegisterHL)
        , -- 0x67
          HighLevelOpcode.ld CoreEffect.writeRegisterH CoreEffect.readRegisterA
        , -- 0x68
          HighLevelOpcode.ld CoreEffect.writeRegisterL CoreEffect.readRegisterB
        , -- 0x69
          HighLevelOpcode.ld CoreEffect.writeRegisterL CoreEffect.readRegisterC
        , -- 0x6A
          HighLevelOpcode.ld CoreEffect.writeRegisterL CoreEffect.readRegisterD
        , -- 0x6B
          HighLevelOpcode.ld CoreEffect.writeRegisterL CoreEffect.readRegisterE
        , -- 0x6C
          HighLevelOpcode.ld CoreEffect.writeRegisterL CoreEffect.readRegisterH
        , -- 0x6D
          HighLevelOpcode.ld CoreEffect.writeRegisterL CoreEffect.readRegisterL
        , -- 0x6E
          HighLevelOpcode.ld CoreEffect.writeRegisterL (readMemory8 CoreEffect.readRegisterHL)
        , -- 0x6F
          HighLevelOpcode.ld CoreEffect.writeRegisterL CoreEffect.readRegisterA
        , -- 0x70
          HighLevelOpcode.ld writeIndirectHL CoreEffect.readRegisterB
        , -- 0x71
          HighLevelOpcode.ld writeIndirectHL CoreEffect.readRegisterC
        , -- 0x72
          HighLevelOpcode.ld writeIndirectHL CoreEffect.readRegisterD
        , -- 0x73
          HighLevelOpcode.ld writeIndirectHL CoreEffect.readRegisterE
        , -- 0x74
          HighLevelOpcode.ld writeIndirectHL CoreEffect.readRegisterH
        , -- 0x75
          HighLevelOpcode.ld writeIndirectHL CoreEffect.readRegisterL
        , -- 0x76
          HighLevelOpcode.halt
        , -- 0x77
          HighLevelOpcode.ld writeIndirectHL CoreEffect.readRegisterA
        , -- 0x78
          HighLevelOpcode.ld CoreEffect.writeRegisterA CoreEffect.readRegisterB
        , -- 0x79
          HighLevelOpcode.ld CoreEffect.writeRegisterA CoreEffect.readRegisterC
        , -- 0x7A
          HighLevelOpcode.ld CoreEffect.writeRegisterA CoreEffect.readRegisterD
        , -- 0x7B
          HighLevelOpcode.ld CoreEffect.writeRegisterA CoreEffect.readRegisterE
        , -- 0x7C
          HighLevelOpcode.ld CoreEffect.writeRegisterA CoreEffect.readRegisterH
        , -- 0x7D
          HighLevelOpcode.ld CoreEffect.writeRegisterA CoreEffect.readRegisterL
        , -- 0x7E
          HighLevelOpcode.ld CoreEffect.writeRegisterA readIndirectHL
        , -- 0x7F
          HighLevelOpcode.ld CoreEffect.writeRegisterA CoreEffect.readRegisterA
        , -- 0x80
          HighLevelOpcode.add CoreEffect.readRegisterB
        , -- 0x81
          HighLevelOpcode.add CoreEffect.readRegisterC
        , -- 0x82
          HighLevelOpcode.add CoreEffect.readRegisterD
        , -- 0x83
          HighLevelOpcode.add CoreEffect.readRegisterE
        , -- 0x84
          HighLevelOpcode.add CoreEffect.readRegisterH
        , -- 0x85
          HighLevelOpcode.add CoreEffect.readRegisterL
        , -- 0x86
          HighLevelOpcode.add readIndirectHL
        , -- 0x87
          HighLevelOpcode.add CoreEffect.readRegisterA
        , -- 0x88
          HighLevelOpcode.adc CoreEffect.readRegisterB
        , -- 0x89
          HighLevelOpcode.adc CoreEffect.readRegisterC
        , -- 0x8A
          HighLevelOpcode.adc CoreEffect.readRegisterD
        , -- 0x8B
          HighLevelOpcode.adc CoreEffect.readRegisterE
        , -- 0x8C
          HighLevelOpcode.adc CoreEffect.readRegisterH
        , -- 0x8D
          HighLevelOpcode.adc CoreEffect.readRegisterL
        , -- 0x8E
          HighLevelOpcode.adc readIndirectHL
        , -- 0x8F
          HighLevelOpcode.adc CoreEffect.readRegisterA
        , -- 0x90
          HighLevelOpcode.sub CoreEffect.readRegisterB
        , -- 0x91
          HighLevelOpcode.sub CoreEffect.readRegisterC
        , -- 0x92
          HighLevelOpcode.sub CoreEffect.readRegisterD
        , -- 0x93
          HighLevelOpcode.sub CoreEffect.readRegisterE
        , -- 0x94
          HighLevelOpcode.sub CoreEffect.readRegisterH
        , -- 0x95
          HighLevelOpcode.sub CoreEffect.readRegisterL
        , -- 0x96
          HighLevelOpcode.sub readIndirectHL
        , -- 0x97
          HighLevelOpcode.sub CoreEffect.readRegisterA
        , -- 0x98
          HighLevelOpcode.sbc CoreEffect.readRegisterB
        , -- 0x99
          HighLevelOpcode.sbc CoreEffect.readRegisterC
        , -- 0x9A
          HighLevelOpcode.sbc CoreEffect.readRegisterD
        , -- 0x9B
          HighLevelOpcode.sbc CoreEffect.readRegisterE
        , -- 0x9C
          HighLevelOpcode.sbc CoreEffect.readRegisterH
        , -- 0x9D
          HighLevelOpcode.sbc CoreEffect.readRegisterL
        , -- 0x9E
          HighLevelOpcode.sbc readIndirectHL
        , -- 0x9F
          HighLevelOpcode.sbc CoreEffect.readRegisterA
        , -- 0xA0
          HighLevelOpcode.and CoreEffect.readRegisterB
        , -- 0xA1
          HighLevelOpcode.and CoreEffect.readRegisterC
        , -- 0xA2
          HighLevelOpcode.and CoreEffect.readRegisterD
        , -- 0xA3
          HighLevelOpcode.and CoreEffect.readRegisterE
        , -- 0xA4
          HighLevelOpcode.and CoreEffect.readRegisterH
        , -- 0xA5
          HighLevelOpcode.and CoreEffect.readRegisterL
        , -- 0xA6
          HighLevelOpcode.and readIndirectHL
        , -- 0xA7
          HighLevelOpcode.and CoreEffect.readRegisterA
        , -- 0xA8
          HighLevelOpcode.xor CoreEffect.readRegisterB
        , -- 0xA9
          HighLevelOpcode.xor CoreEffect.readRegisterC
        , -- 0xAA
          HighLevelOpcode.xor CoreEffect.readRegisterD
        , -- 0xAB
          HighLevelOpcode.xor CoreEffect.readRegisterE
        , -- 0xAC
          HighLevelOpcode.xor CoreEffect.readRegisterH
        , -- 0xAD
          HighLevelOpcode.xor CoreEffect.readRegisterL
        , -- 0xAE
          HighLevelOpcode.xor readIndirectHL
        , -- 0xAF
          HighLevelOpcode.xor CoreEffect.readRegisterA
        , -- 0xB0
          HighLevelOpcode.or CoreEffect.readRegisterB
        , -- 0xB1
          HighLevelOpcode.or CoreEffect.readRegisterC
        , -- 0xB2
          HighLevelOpcode.or CoreEffect.readRegisterD
        , -- 0xB3
          HighLevelOpcode.or CoreEffect.readRegisterE
        , -- 0xB4
          HighLevelOpcode.or CoreEffect.readRegisterH
        , -- 0xB5
          HighLevelOpcode.or CoreEffect.readRegisterL
        , -- 0xB6
          HighLevelOpcode.or readIndirectHL
        , -- 0xB7
          HighLevelOpcode.or CoreEffect.readRegisterA
        , -- 0xB8
          HighLevelOpcode.cp CoreEffect.readRegisterB
        , -- 0xB9
          HighLevelOpcode.cp CoreEffect.readRegisterC
        , -- 0xBA
          HighLevelOpcode.cp CoreEffect.readRegisterD
        , -- 0xBB
          HighLevelOpcode.cp CoreEffect.readRegisterE
        , -- 0xBC
          HighLevelOpcode.cp CoreEffect.readRegisterH
        , -- 0xBD
          HighLevelOpcode.cp CoreEffect.readRegisterL
        , -- 0xBE
          HighLevelOpcode.cp readIndirectHL
        , -- 0xBF
          HighLevelOpcode.cp CoreEffect.readRegisterA
        , -- 0xC0
          HighLevelOpcode.ret Condition.NotZero
        , -- 0xC1
          HighLevelOpcode.pop CoreEffect.writeRegisterBC
        , -- 0xC2
          HighLevelOpcode.jp Condition.NotZero readMemory16AdvancePC
        , -- 0xC3
          HighLevelOpcode.jp Condition.Always readMemory16AdvancePC
        , -- 0xC4
          HighLevelOpcode.call Condition.NotZero
        , -- 0xC5
          HighLevelOpcode.push CoreEffect.readRegisterBC
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
          HighLevelOpcode.pop CoreEffect.writeRegisterDE
        , -- 0xD2
          HighLevelOpcode.jp Condition.NotCarry readMemory16AdvancePC
        , -- 0xD3
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xD4
          HighLevelOpcode.call Condition.NotCarry
        , -- 0xD5
          HighLevelOpcode.push CoreEffect.readRegisterDE
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
          HighLevelOpcode.ld (readMemory8AdvancePC |> asZeroPageAddress |> writeMemory8) CoreEffect.readRegisterA
        , -- 0xE1
          HighLevelOpcode.pop CoreEffect.writeRegisterHL
        , -- 0xE2
          HighLevelOpcode.ld (CoreEffect.readRegisterC |> asZeroPageAddress |> writeMemory8) CoreEffect.readRegisterA
        , -- 0xE3
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xE4
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xE5
          HighLevelOpcode.push CoreEffect.readRegisterHL
        , -- 0xE6
          HighLevelOpcode.and readMemory8AdvancePC
        , -- 0xE7
          HighLevelOpcode.rst 0x20
        , -- 0xE8
          HighLevelOpcode.addSPSignedImmediate
        , -- 0xE9
          HighLevelOpcode.jpIndirectHL
        , -- 0xEA
          HighLevelOpcode.ld (writeMemory8 readMemory16AdvancePC) CoreEffect.readRegisterA
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
          HighLevelOpcode.ld CoreEffect.writeRegisterA (readMemory8AdvancePC |> asZeroPageAddress |> readMemory8)
        , -- 0xF1
          HighLevelOpcode.pop CoreEffect.writeRegisterAF
        , -- 0xF2
          HighLevelOpcode.ld CoreEffect.writeRegisterA (CoreEffect.readRegisterC |> asZeroPageAddress |> readMemory8)
        , -- 0xF3
          HighLevelOpcode.di
        , -- 0xF4
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xF5
          HighLevelOpcode.push CoreEffect.readRegisterAF
        , -- 0xF6
          HighLevelOpcode.or readMemory8AdvancePC
        , -- 0xF7
          HighLevelOpcode.rst 0x30
        , -- 0xF8
          HighLevelOpcode.ldHLSPPlusSignedImmediate
        , -- 0xF9
          -- Special case: Requires 4 extra cycles as this is a 16bit load.
          HighLevelOpcode.ld CoreEffect.writeRegisterSP (CoreEffect.extraCycles 4 >> CoreEffect.readRegisterHL)
        , -- 0xFA
          HighLevelOpcode.ld CoreEffect.writeRegisterA (readMemory8 readMemory16AdvancePC)
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
          HighLevelOpcode.rlc CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x01
          HighLevelOpcode.rlc CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x02
          HighLevelOpcode.rlc CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x03
          HighLevelOpcode.rlc CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x04
          HighLevelOpcode.rlc CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x05
          HighLevelOpcode.rlc CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x06
          HighLevelOpcode.rlc readIndirectHL writeIndirectHL
        , -- 0x07
          HighLevelOpcode.rlc CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x08
          HighLevelOpcode.rrc CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x09
          HighLevelOpcode.rrc CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x0A
          HighLevelOpcode.rrc CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x0B
          HighLevelOpcode.rrc CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x0C
          HighLevelOpcode.rrc CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x0D
          HighLevelOpcode.rrc CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x0E
          HighLevelOpcode.rrc readIndirectHL writeIndirectHL
        , -- 0x0F
          HighLevelOpcode.rrc CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x10
          HighLevelOpcode.rl CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x11
          HighLevelOpcode.rl CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x12
          HighLevelOpcode.rl CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x13
          HighLevelOpcode.rl CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x14
          HighLevelOpcode.rl CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x15
          HighLevelOpcode.rl CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x16
          HighLevelOpcode.rl readIndirectHL writeIndirectHL
        , -- 0x17
          HighLevelOpcode.rl CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x18
          HighLevelOpcode.rr CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x19
          HighLevelOpcode.rr CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x1A
          HighLevelOpcode.rr CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x1B
          HighLevelOpcode.rr CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x1C
          HighLevelOpcode.rr CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x1D
          HighLevelOpcode.rr CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x1E
          HighLevelOpcode.rr readIndirectHL writeIndirectHL
        , -- 0x1F
          HighLevelOpcode.rr CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x20
          HighLevelOpcode.sla CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x21
          HighLevelOpcode.sla CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x22
          HighLevelOpcode.sla CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x23
          HighLevelOpcode.sla CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x24
          HighLevelOpcode.sla CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x25
          HighLevelOpcode.sla CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x26
          HighLevelOpcode.sla readIndirectHL writeIndirectHL
        , -- 0x27
          HighLevelOpcode.sla CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x28
          HighLevelOpcode.sra CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x29
          HighLevelOpcode.sra CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x2A
          HighLevelOpcode.sra CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x2B
          HighLevelOpcode.sra CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x2C
          HighLevelOpcode.sra CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x2D
          HighLevelOpcode.sra CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x2E
          HighLevelOpcode.sra readIndirectHL writeIndirectHL
        , -- 0x2F
          HighLevelOpcode.sra CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x30
          HighLevelOpcode.swap CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x31
          HighLevelOpcode.swap CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x32
          HighLevelOpcode.swap CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x33
          HighLevelOpcode.swap CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x34
          HighLevelOpcode.swap CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x35
          HighLevelOpcode.swap CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x36
          HighLevelOpcode.swap readIndirectHL writeIndirectHL
        , -- 0x37
          HighLevelOpcode.swap CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x38
          HighLevelOpcode.srl CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x39
          HighLevelOpcode.srl CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x3A
          HighLevelOpcode.srl CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x3B
          HighLevelOpcode.srl CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x3C
          HighLevelOpcode.srl CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x3D
          HighLevelOpcode.srl CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x3E
          HighLevelOpcode.srl readIndirectHL writeIndirectHL
        , -- 0x3F
          HighLevelOpcode.srl CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x40
          HighLevelOpcode.bit 0 CoreEffect.readRegisterB
        , -- 0x41
          HighLevelOpcode.bit 0 CoreEffect.readRegisterC
        , -- 0x42
          HighLevelOpcode.bit 0 CoreEffect.readRegisterD
        , -- 0x43
          HighLevelOpcode.bit 0 CoreEffect.readRegisterE
        , -- 0x44
          HighLevelOpcode.bit 0 CoreEffect.readRegisterH
        , -- 0x45
          HighLevelOpcode.bit 0 CoreEffect.readRegisterL
        , -- 0x46
          HighLevelOpcode.bit 0 readIndirectHL
        , -- 0x47
          HighLevelOpcode.bit 0 CoreEffect.readRegisterA
        , -- 0x48
          HighLevelOpcode.bit 1 CoreEffect.readRegisterB
        , -- 0x49
          HighLevelOpcode.bit 1 CoreEffect.readRegisterC
        , -- 0x4A
          HighLevelOpcode.bit 1 CoreEffect.readRegisterD
        , -- 0x4B
          HighLevelOpcode.bit 1 CoreEffect.readRegisterE
        , -- 0x4C
          HighLevelOpcode.bit 1 CoreEffect.readRegisterH
        , -- 0x4D
          HighLevelOpcode.bit 1 CoreEffect.readRegisterL
        , -- 0x4E
          HighLevelOpcode.bit 1 readIndirectHL
        , -- 0x4F
          HighLevelOpcode.bit 1 CoreEffect.readRegisterA
        , -- 0x50
          HighLevelOpcode.bit 2 CoreEffect.readRegisterB
        , -- 0x51
          HighLevelOpcode.bit 2 CoreEffect.readRegisterC
        , -- 0x52
          HighLevelOpcode.bit 2 CoreEffect.readRegisterD
        , -- 0x53
          HighLevelOpcode.bit 2 CoreEffect.readRegisterE
        , -- 0x54
          HighLevelOpcode.bit 2 CoreEffect.readRegisterH
        , -- 0x55
          HighLevelOpcode.bit 2 CoreEffect.readRegisterL
        , -- 0x56
          HighLevelOpcode.bit 2 readIndirectHL
        , -- 0x57
          HighLevelOpcode.bit 2 CoreEffect.readRegisterA
        , -- 0x58
          HighLevelOpcode.bit 3 CoreEffect.readRegisterB
        , -- 0x59
          HighLevelOpcode.bit 3 CoreEffect.readRegisterC
        , -- 0x5A
          HighLevelOpcode.bit 3 CoreEffect.readRegisterD
        , -- 0x5B
          HighLevelOpcode.bit 3 CoreEffect.readRegisterE
        , -- 0x5C
          HighLevelOpcode.bit 3 CoreEffect.readRegisterH
        , -- 0x5D
          HighLevelOpcode.bit 3 CoreEffect.readRegisterL
        , -- 0x5E
          HighLevelOpcode.bit 3 readIndirectHL
        , -- 0x5F
          HighLevelOpcode.bit 3 CoreEffect.readRegisterA
        , -- 0x60
          HighLevelOpcode.bit 4 CoreEffect.readRegisterB
        , -- 0x61
          HighLevelOpcode.bit 4 CoreEffect.readRegisterC
        , -- 0x62
          HighLevelOpcode.bit 4 CoreEffect.readRegisterD
        , -- 0x63
          HighLevelOpcode.bit 4 CoreEffect.readRegisterE
        , -- 0x64
          HighLevelOpcode.bit 4 CoreEffect.readRegisterH
        , -- 0x65
          HighLevelOpcode.bit 4 CoreEffect.readRegisterL
        , -- 0x66
          HighLevelOpcode.bit 4 readIndirectHL
        , -- 0x67
          HighLevelOpcode.bit 4 CoreEffect.readRegisterA
        , -- 0x68
          HighLevelOpcode.bit 5 CoreEffect.readRegisterB
        , -- 0x69
          HighLevelOpcode.bit 5 CoreEffect.readRegisterC
        , -- 0x6A
          HighLevelOpcode.bit 5 CoreEffect.readRegisterD
        , -- 0x6B
          HighLevelOpcode.bit 5 CoreEffect.readRegisterE
        , -- 0x6C
          HighLevelOpcode.bit 5 CoreEffect.readRegisterH
        , -- 0x6D
          HighLevelOpcode.bit 5 CoreEffect.readRegisterL
        , -- 0x6E
          HighLevelOpcode.bit 5 readIndirectHL
        , -- 0x6F
          HighLevelOpcode.bit 5 CoreEffect.readRegisterA
        , -- 0x70
          HighLevelOpcode.bit 6 CoreEffect.readRegisterB
        , -- 0x71
          HighLevelOpcode.bit 6 CoreEffect.readRegisterC
        , -- 0x72
          HighLevelOpcode.bit 6 CoreEffect.readRegisterD
        , -- 0x73
          HighLevelOpcode.bit 6 CoreEffect.readRegisterE
        , -- 0x74
          HighLevelOpcode.bit 6 CoreEffect.readRegisterH
        , -- 0x75
          HighLevelOpcode.bit 6 CoreEffect.readRegisterL
        , -- 0x76
          HighLevelOpcode.bit 6 readIndirectHL
        , -- 0x77
          HighLevelOpcode.bit 6 CoreEffect.readRegisterA
        , -- 0x78
          HighLevelOpcode.bit 7 CoreEffect.readRegisterB
        , -- 0x79
          HighLevelOpcode.bit 7 CoreEffect.readRegisterC
        , -- 0x7A
          HighLevelOpcode.bit 7 CoreEffect.readRegisterD
        , -- 0x7B
          HighLevelOpcode.bit 7 CoreEffect.readRegisterE
        , -- 0x7C
          HighLevelOpcode.bit 7 CoreEffect.readRegisterH
        , -- 0x7D
          HighLevelOpcode.bit 7 CoreEffect.readRegisterL
        , -- 0x7E
          HighLevelOpcode.bit 7 readIndirectHL
        , -- 0x7F
          HighLevelOpcode.bit 7 CoreEffect.readRegisterA
        , -- 0x80
          HighLevelOpcode.res 0 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x81
          HighLevelOpcode.res 0 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x82
          HighLevelOpcode.res 0 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x83
          HighLevelOpcode.res 0 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x84
          HighLevelOpcode.res 0 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x85
          HighLevelOpcode.res 0 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x86
          HighLevelOpcode.res 0 readIndirectHL writeIndirectHL
        , -- 0x87
          HighLevelOpcode.res 0 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x88
          HighLevelOpcode.res 1 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x89
          HighLevelOpcode.res 1 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x8A
          HighLevelOpcode.res 1 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x8B
          HighLevelOpcode.res 1 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x8C
          HighLevelOpcode.res 1 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x8D
          HighLevelOpcode.res 1 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x8E
          HighLevelOpcode.res 1 readIndirectHL writeIndirectHL
        , -- 0x8F
          HighLevelOpcode.res 1 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x90
          HighLevelOpcode.res 2 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x91
          HighLevelOpcode.res 2 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x92
          HighLevelOpcode.res 2 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x93
          HighLevelOpcode.res 2 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x94
          HighLevelOpcode.res 2 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x95
          HighLevelOpcode.res 2 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x96
          HighLevelOpcode.res 2 readIndirectHL writeIndirectHL
        , -- 0x97
          HighLevelOpcode.res 2 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0x98
          HighLevelOpcode.res 3 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0x99
          HighLevelOpcode.res 3 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0x9A
          HighLevelOpcode.res 3 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0x9B
          HighLevelOpcode.res 3 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0x9C
          HighLevelOpcode.res 3 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0x9D
          HighLevelOpcode.res 3 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0x9E
          HighLevelOpcode.res 3 readIndirectHL writeIndirectHL
        , -- 0x9F
          HighLevelOpcode.res 3 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xA0
          HighLevelOpcode.res 4 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xA1
          HighLevelOpcode.res 4 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xA2
          HighLevelOpcode.res 4 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xA3
          HighLevelOpcode.res 4 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xA4
          HighLevelOpcode.res 4 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xA5
          HighLevelOpcode.res 4 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xA6
          HighLevelOpcode.res 4 readIndirectHL writeIndirectHL
        , -- 0xA7
          HighLevelOpcode.res 4 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xA8
          HighLevelOpcode.res 5 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xA9
          HighLevelOpcode.res 5 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xAA
          HighLevelOpcode.res 5 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xAB
          HighLevelOpcode.res 5 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xAC
          HighLevelOpcode.res 5 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xAD
          HighLevelOpcode.res 5 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xAE
          HighLevelOpcode.res 5 readIndirectHL writeIndirectHL
        , -- 0xAF
          HighLevelOpcode.res 5 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xB0
          HighLevelOpcode.res 6 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xB1
          HighLevelOpcode.res 6 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xB2
          HighLevelOpcode.res 6 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xB3
          HighLevelOpcode.res 6 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xB4
          HighLevelOpcode.res 6 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xB5
          HighLevelOpcode.res 6 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xB6
          HighLevelOpcode.res 6 readIndirectHL writeIndirectHL
        , -- 0xB7
          HighLevelOpcode.res 6 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xB8
          HighLevelOpcode.res 7 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xB9
          HighLevelOpcode.res 7 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xBA
          HighLevelOpcode.res 7 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xBB
          HighLevelOpcode.res 7 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xBC
          HighLevelOpcode.res 7 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xBD
          HighLevelOpcode.res 7 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xBE
          HighLevelOpcode.res 7 readIndirectHL writeIndirectHL
        , -- 0xBF
          HighLevelOpcode.res 7 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xC0
          HighLevelOpcode.set 0 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xC1
          HighLevelOpcode.set 0 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xC2
          HighLevelOpcode.set 0 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xC3
          HighLevelOpcode.set 0 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xC4
          HighLevelOpcode.set 0 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xC5
          HighLevelOpcode.set 0 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xC6
          HighLevelOpcode.set 0 readIndirectHL writeIndirectHL
        , -- 0xC7
          HighLevelOpcode.set 0 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xC8
          HighLevelOpcode.set 1 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xC9
          HighLevelOpcode.set 1 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xCA
          HighLevelOpcode.set 1 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xCB
          HighLevelOpcode.set 1 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xCC
          HighLevelOpcode.set 1 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xCD
          HighLevelOpcode.set 1 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xCE
          HighLevelOpcode.set 1 readIndirectHL writeIndirectHL
        , -- 0xCF
          HighLevelOpcode.set 1 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xD0
          HighLevelOpcode.set 2 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xD1
          HighLevelOpcode.set 2 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xD2
          HighLevelOpcode.set 2 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xD3
          HighLevelOpcode.set 2 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xD4
          HighLevelOpcode.set 2 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xD5
          HighLevelOpcode.set 2 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xD6
          HighLevelOpcode.set 2 readIndirectHL writeIndirectHL
        , -- 0xD7
          HighLevelOpcode.set 2 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xD8
          HighLevelOpcode.set 3 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xD9
          HighLevelOpcode.set 3 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xDA
          HighLevelOpcode.set 3 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xDB
          HighLevelOpcode.set 3 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xDC
          HighLevelOpcode.set 3 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xDD
          HighLevelOpcode.set 3 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xDE
          HighLevelOpcode.set 3 readIndirectHL writeIndirectHL
        , -- 0xDF
          HighLevelOpcode.set 3 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xE0
          HighLevelOpcode.set 4 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xE1
          HighLevelOpcode.set 4 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xE2
          HighLevelOpcode.set 4 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xE3
          HighLevelOpcode.set 4 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xE4
          HighLevelOpcode.set 4 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xE5
          HighLevelOpcode.set 4 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xE6
          HighLevelOpcode.set 4 readIndirectHL writeIndirectHL
        , -- 0xE7
          HighLevelOpcode.set 4 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xE8
          HighLevelOpcode.set 5 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xE9
          HighLevelOpcode.set 5 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xEA
          HighLevelOpcode.set 5 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xEB
          HighLevelOpcode.set 5 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xEC
          HighLevelOpcode.set 5 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xED
          HighLevelOpcode.set 5 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xEE
          HighLevelOpcode.set 5 readIndirectHL writeIndirectHL
        , -- 0xEF
          HighLevelOpcode.set 5 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xF0
          HighLevelOpcode.set 6 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xF1
          HighLevelOpcode.set 6 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xF2
          HighLevelOpcode.set 6 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xF3
          HighLevelOpcode.set 6 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xF4
          HighLevelOpcode.set 6 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xF5
          HighLevelOpcode.set 6 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xF6
          HighLevelOpcode.set 6 readIndirectHL writeIndirectHL
        , -- 0xF7
          HighLevelOpcode.set 6 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        , -- 0xF8
          HighLevelOpcode.set 7 CoreEffect.readRegisterB CoreEffect.writeRegisterB
        , -- 0xF9
          HighLevelOpcode.set 7 CoreEffect.readRegisterC CoreEffect.writeRegisterC
        , -- 0xFA
          HighLevelOpcode.set 7 CoreEffect.readRegisterD CoreEffect.writeRegisterD
        , -- 0xFB
          HighLevelOpcode.set 7 CoreEffect.readRegisterE CoreEffect.writeRegisterE
        , -- 0xFC
          HighLevelOpcode.set 7 CoreEffect.readRegisterH CoreEffect.writeRegisterH
        , -- 0xFD
          HighLevelOpcode.set 7 CoreEffect.readRegisterL CoreEffect.writeRegisterL
        , -- 0xFE
          HighLevelOpcode.set 7 readIndirectHL writeIndirectHL
        , -- 0xFF
          HighLevelOpcode.set 7 CoreEffect.readRegisterA CoreEffect.writeRegisterA
        ]



-- (HL) reader and writer


readIndirectHL : Reader Int
readIndirectHL =
    CoreEffect.readRegisterHL |> readMemory8


writeIndirectHL : Writer Int
writeIndirectHL =
    CoreEffect.readRegisterHL |> writeMemory8



-- (HL+) and (HL-) reader and writer


readIndirectHLPostIncrement : Reader Int
readIndirectHLPostIncrement gameBoy =
    let
        hlValue =
            CPU.readRegisterHL gameBoy.cpu

        updatedCpu =
            CPU.writeRegisterHL (hlValue + 1) gameBoy.cpu
    in
    ( MMU.readWord8 gameBoy hlValue, GameBoy.setCPUAndCycles updatedCpu (gameBoy.lastInstructionCycles + 4) gameBoy )


readIndirectHLPostDecrement : Reader Int
readIndirectHLPostDecrement gameBoy =
    let
        hlValue =
            CPU.readRegisterHL gameBoy.cpu

        updatedCpu =
            CPU.writeRegisterHL (hlValue - 1) gameBoy.cpu
    in
    ( MMU.readWord8 gameBoy hlValue, GameBoy.setCPUAndCycles updatedCpu (gameBoy.lastInstructionCycles + 4) gameBoy )


writeIndirectHLPostIncrement : Writer Int
writeIndirectHLPostIncrement value gameBoy =
    let
        hlValue =
            CPU.readRegisterHL gameBoy.cpu

        updatedGameBoy =
            MMU.writeWord8 hlValue value gameBoy

        updatedCpu =
            CPU.writeRegisterHL (hlValue + 1) updatedGameBoy.cpu
    in
    GameBoy.setCPUAndCycles updatedCpu (updatedGameBoy.lastInstructionCycles + 4) updatedGameBoy


writeIndirectHLPostDecrement : Writer Int
writeIndirectHLPostDecrement value gameBoy =
    let
        hlValue =
            CPU.readRegisterHL gameBoy.cpu

        updatedGameBoy =
            MMU.writeWord8 hlValue value gameBoy

        updatedCpu =
            CPU.writeRegisterHL (hlValue - 1) updatedGameBoy.cpu
    in
    GameBoy.setCPUAndCycles updatedCpu (updatedGameBoy.lastInstructionCycles + 4) updatedGameBoy



-- Helpers


asZeroPageAddress : Reader Int -> Reader Int
asZeroPageAddress =
    mapReader addZeroPageOffset


addZeroPageOffset : Int -> Int
addZeroPageOffset value =
    value + 0xFF00
