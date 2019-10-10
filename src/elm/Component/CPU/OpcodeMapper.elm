module Component.CPU.OpcodeMapper exposing (get)

import Array exposing (Array)
import Component.CPU.Condition as Condition exposing (Condition(..))
import Component.CPU.Opcode as HighLevelOpcode
import Effect exposing (Effect)
import Effect.Operand exposing (..)


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
          HighLevelOpcode.ld write16RegisterBC read16Immediate
        , -- 0x02
          HighLevelOpcode.ld write8IndirectBC read8RegisterA
        , -- 0x03
          HighLevelOpcode.inc16 read16RegisterBC write16RegisterBC
        , -- 0x04
          HighLevelOpcode.inc read8RegisterB write8RegisterB
        , -- 0x05
          HighLevelOpcode.dec read8RegisterB write8RegisterB
        , -- 0x06
          HighLevelOpcode.ld write8RegisterB read8Immediate
        , -- 0x07
          HighLevelOpcode.rlca
        , -- 0x08
          HighLevelOpcode.ld write16IndirectWord16Operand read16RegisterSP
        , -- 0x09
          HighLevelOpcode.add16 read16RegisterBC
        , -- 0x0A
          HighLevelOpcode.ld write8RegisterA read8IndirectBC
        , -- 0x0B
          HighLevelOpcode.dec16 read16RegisterBC write16RegisterBC
        , -- 0x0C
          HighLevelOpcode.inc read8RegisterC write8RegisterC
        , -- 0x0D
          HighLevelOpcode.dec read8RegisterC write8RegisterC
        , -- 0x0E
          HighLevelOpcode.ld write8RegisterC read8Immediate
        , -- 0x0F
          HighLevelOpcode.rrca
        , -- 0x10
          HighLevelOpcode.nop -- Stop
        , -- 0x11
          HighLevelOpcode.ld write16RegisterDE read16Immediate
        , -- 0x12
          HighLevelOpcode.ld write8IndirectDE read8RegisterA
        , -- 0x13
          HighLevelOpcode.inc16 read16RegisterDE write16RegisterDE
        , -- 0x14
          HighLevelOpcode.inc read8RegisterD write8RegisterD
        , -- 0x15
          HighLevelOpcode.dec read8RegisterD write8RegisterD
        , -- 0x16
          HighLevelOpcode.ld write8RegisterD read8Immediate
        , -- 0x17
          HighLevelOpcode.rla
        , -- 0x18
          HighLevelOpcode.jr Condition.Always
        , -- 0x19
          HighLevelOpcode.add16 read16RegisterDE
        , -- 0x1A
          HighLevelOpcode.ld write8RegisterA read8IndirectDE
        , -- 0x1B
          HighLevelOpcode.dec16 read16RegisterDE write16RegisterDE
        , -- 0x1C
          HighLevelOpcode.inc read8RegisterE write8RegisterE
        , -- 0x1D
          HighLevelOpcode.dec read8RegisterE write8RegisterE
        , -- 0x1E
          HighLevelOpcode.ld write8RegisterE read8Immediate
        , -- 0x1F
          HighLevelOpcode.rra
        , -- 0x20
          HighLevelOpcode.jr Condition.NotZero
        , -- 0x21
          HighLevelOpcode.ld write16RegisterHL read16Immediate
        , -- 0x22
          HighLevelOpcode.ld write8IndirectHLPostIncrement read8RegisterA
        , -- 0x23
          HighLevelOpcode.inc16 read16RegisterHL write16RegisterHL
        , -- 0x24
          HighLevelOpcode.inc read8RegisterH write8RegisterH
        , -- 0x25
          HighLevelOpcode.dec read8RegisterH write8RegisterH
        , -- 0x26
          HighLevelOpcode.ld write8RegisterH read8Immediate
        , -- 0x27
          HighLevelOpcode.daa
        , -- 0x28
          HighLevelOpcode.jr Condition.Zero
        , -- 0x29
          HighLevelOpcode.add16 read16RegisterHL
        , -- 0x2A
          HighLevelOpcode.ld write8RegisterA read8IndirectHLPostIncrement
        , -- 0x2B
          HighLevelOpcode.dec16 read16RegisterHL write16RegisterHL
        , -- 0x2C
          HighLevelOpcode.inc read8RegisterL write8RegisterL
        , -- 0x2D
          HighLevelOpcode.dec read8RegisterL write8RegisterL
        , -- 0x2E
          HighLevelOpcode.ld write8RegisterL read8Immediate
        , -- 0x2F
          HighLevelOpcode.cpl
        , -- 0x30
          HighLevelOpcode.jr Condition.NotCarry
        , -- 0x31
          HighLevelOpcode.ld write16RegisterSP read16Immediate
        , -- 0x32
          HighLevelOpcode.ld write8IndirectHLPostDecrement read8RegisterA
        , -- 0x33
          HighLevelOpcode.inc16 read16RegisterSP write16RegisterSP
        , -- 0x34
          HighLevelOpcode.inc read8IndirectHL write8IndirectHL
        , -- 0x35
          HighLevelOpcode.dec read8IndirectHL write8IndirectHL
        , -- 0x36
          HighLevelOpcode.ld write8IndirectHL read8Immediate
        , -- 0x37
          HighLevelOpcode.scf
        , -- 0x38
          HighLevelOpcode.jr Condition.Carry
        , -- 0x39
          HighLevelOpcode.add16 read16RegisterSP
        , -- 0x3A
          HighLevelOpcode.ld write8RegisterA read8IndirectHLPostDecrement
        , -- 0x3B
          HighLevelOpcode.dec16 read16RegisterSP write16RegisterSP
        , -- 0x3C
          HighLevelOpcode.inc read8RegisterA write8RegisterA
        , -- 0x3D
          HighLevelOpcode.dec read8RegisterA write8RegisterA
        , -- 0x3E
          HighLevelOpcode.ld write8RegisterA read8Immediate
        , -- 0x3F
          HighLevelOpcode.ccf
        , -- 0x40
          HighLevelOpcode.ld write8RegisterB read8RegisterB
        , -- 0x41
          HighLevelOpcode.ld write8RegisterB read8RegisterC
        , -- 0x42
          HighLevelOpcode.ld write8RegisterB read8RegisterD
        , -- 0x43
          HighLevelOpcode.ld write8RegisterB read8RegisterE
        , -- 0x44
          HighLevelOpcode.ld write8RegisterB read8RegisterH
        , -- 0x45
          HighLevelOpcode.ld write8RegisterB read8RegisterL
        , -- 0x46
          HighLevelOpcode.ld write8RegisterB read8IndirectHL
        , -- 0x47
          HighLevelOpcode.ld write8RegisterB read8RegisterA
        , -- 0x48
          HighLevelOpcode.ld write8RegisterC read8RegisterB
        , -- 0x49
          HighLevelOpcode.ld write8RegisterC read8RegisterC
        , -- 0x4A
          HighLevelOpcode.ld write8RegisterC read8RegisterD
        , -- 0x4B
          HighLevelOpcode.ld write8RegisterC read8RegisterE
        , -- 0x4C
          HighLevelOpcode.ld write8RegisterC read8RegisterH
        , -- 0x4D
          HighLevelOpcode.ld write8RegisterC read8RegisterL
        , -- 0x4E
          HighLevelOpcode.ld write8RegisterC read8IndirectHL
        , -- 0x4F
          HighLevelOpcode.ld write8RegisterC read8RegisterA
        , -- 0x50
          HighLevelOpcode.ld write8RegisterD read8RegisterB
        , -- 0x51
          HighLevelOpcode.ld write8RegisterD read8RegisterC
        , -- 0x52
          HighLevelOpcode.ld write8RegisterD read8RegisterD
        , -- 0x53
          HighLevelOpcode.ld write8RegisterD read8RegisterE
        , -- 0x54
          HighLevelOpcode.ld write8RegisterD read8RegisterH
        , -- 0x55
          HighLevelOpcode.ld write8RegisterD read8RegisterL
        , -- 0x56
          HighLevelOpcode.ld write8RegisterD read8IndirectHL
        , -- 0x57
          HighLevelOpcode.ld write8RegisterD read8RegisterA
        , -- 0x58
          HighLevelOpcode.ld write8RegisterE read8RegisterB
        , -- 0x59
          HighLevelOpcode.ld write8RegisterE read8RegisterC
        , -- 0x5A
          HighLevelOpcode.ld write8RegisterE read8RegisterD
        , -- 0x5B
          HighLevelOpcode.ld write8RegisterE read8RegisterE
        , -- 0x5C
          HighLevelOpcode.ld write8RegisterE read8RegisterH
        , -- 0x5D
          HighLevelOpcode.ld write8RegisterE read8RegisterL
        , -- 0x5E
          HighLevelOpcode.ld write8RegisterE read8IndirectHL
        , -- 0x5F
          HighLevelOpcode.ld write8RegisterE read8RegisterA
        , -- 0x60
          HighLevelOpcode.ld write8RegisterH read8RegisterB
        , -- 0x61
          HighLevelOpcode.ld write8RegisterH read8RegisterC
        , -- 0x62
          HighLevelOpcode.ld write8RegisterH read8RegisterD
        , -- 0x63
          HighLevelOpcode.ld write8RegisterH read8RegisterE
        , -- 0x64
          HighLevelOpcode.ld write8RegisterH read8RegisterH
        , -- 0x65
          HighLevelOpcode.ld write8RegisterH read8RegisterL
        , -- 0x66
          HighLevelOpcode.ld write8RegisterH read8IndirectHL
        , -- 0x67
          HighLevelOpcode.ld write8RegisterH read8RegisterA
        , -- 0x68
          HighLevelOpcode.ld write8RegisterL read8RegisterB
        , -- 0x69
          HighLevelOpcode.ld write8RegisterL read8RegisterC
        , -- 0x6A
          HighLevelOpcode.ld write8RegisterL read8RegisterD
        , -- 0x6B
          HighLevelOpcode.ld write8RegisterL read8RegisterE
        , -- 0x6C
          HighLevelOpcode.ld write8RegisterL read8RegisterH
        , -- 0x6D
          HighLevelOpcode.ld write8RegisterL read8RegisterL
        , -- 0x6E
          HighLevelOpcode.ld write8RegisterL read8IndirectHL
        , -- 0x6F
          HighLevelOpcode.ld write8RegisterL read8RegisterA
        , -- 0x70
          HighLevelOpcode.ld write8IndirectHL read8RegisterB
        , -- 0x71
          HighLevelOpcode.ld write8IndirectHL read8RegisterC
        , -- 0x72
          HighLevelOpcode.ld write8IndirectHL read8RegisterD
        , -- 0x73
          HighLevelOpcode.ld write8IndirectHL read8RegisterE
        , -- 0x74
          HighLevelOpcode.ld write8IndirectHL read8RegisterH
        , -- 0x75
          HighLevelOpcode.ld write8IndirectHL read8RegisterL
        , -- 0x76
          HighLevelOpcode.halt
        , -- 0x77
          HighLevelOpcode.ld write8IndirectHL read8RegisterA
        , -- 0x78
          HighLevelOpcode.ld write8RegisterA read8RegisterB
        , -- 0x79
          HighLevelOpcode.ld write8RegisterA read8RegisterC
        , -- 0x7A
          HighLevelOpcode.ld write8RegisterA read8RegisterD
        , -- 0x7B
          HighLevelOpcode.ld write8RegisterA read8RegisterE
        , -- 0x7C
          HighLevelOpcode.ld write8RegisterA read8RegisterH
        , -- 0x7D
          HighLevelOpcode.ld write8RegisterA read8RegisterL
        , -- 0x7E
          HighLevelOpcode.ld write8RegisterA read8IndirectHL
        , -- 0x7F
          HighLevelOpcode.ld write8RegisterA read8RegisterA
        , -- 0x80
          HighLevelOpcode.add read8RegisterB
        , -- 0x81
          HighLevelOpcode.add read8RegisterC
        , -- 0x82
          HighLevelOpcode.add read8RegisterD
        , -- 0x83
          HighLevelOpcode.add read8RegisterE
        , -- 0x84
          HighLevelOpcode.add read8RegisterH
        , -- 0x85
          HighLevelOpcode.add read8RegisterL
        , -- 0x86
          HighLevelOpcode.add read8IndirectHL
        , -- 0x87
          HighLevelOpcode.add read8RegisterA
        , -- 0x88
          HighLevelOpcode.adc read8RegisterB
        , -- 0x89
          HighLevelOpcode.adc read8RegisterC
        , -- 0x8A
          HighLevelOpcode.adc read8RegisterD
        , -- 0x8B
          HighLevelOpcode.adc read8RegisterE
        , -- 0x8C
          HighLevelOpcode.adc read8RegisterH
        , -- 0x8D
          HighLevelOpcode.adc read8RegisterL
        , -- 0x8E
          HighLevelOpcode.adc read8IndirectHL
        , -- 0x8F
          HighLevelOpcode.adc read8RegisterA
        , -- 0x90
          HighLevelOpcode.sub read8RegisterB
        , -- 0x91
          HighLevelOpcode.sub read8RegisterC
        , -- 0x92
          HighLevelOpcode.sub read8RegisterD
        , -- 0x93
          HighLevelOpcode.sub read8RegisterE
        , -- 0x94
          HighLevelOpcode.sub read8RegisterH
        , -- 0x95
          HighLevelOpcode.sub read8RegisterL
        , -- 0x96
          HighLevelOpcode.sub read8IndirectHL
        , -- 0x97
          HighLevelOpcode.sub read8RegisterA
        , -- 0x98
          HighLevelOpcode.sbc read8RegisterB
        , -- 0x99
          HighLevelOpcode.sbc read8RegisterC
        , -- 0x9A
          HighLevelOpcode.sbc read8RegisterD
        , -- 0x9B
          HighLevelOpcode.sbc read8RegisterE
        , -- 0x9C
          HighLevelOpcode.sbc read8RegisterH
        , -- 0x9D
          HighLevelOpcode.sbc read8RegisterL
        , -- 0x9E
          HighLevelOpcode.sbc read8IndirectHL
        , -- 0x9F
          HighLevelOpcode.sbc read8RegisterA
        , -- 0xA0
          HighLevelOpcode.and read8RegisterB
        , -- 0xA1
          HighLevelOpcode.and read8RegisterC
        , -- 0xA2
          HighLevelOpcode.and read8RegisterD
        , -- 0xA3
          HighLevelOpcode.and read8RegisterE
        , -- 0xA4
          HighLevelOpcode.and read8RegisterH
        , -- 0xA5
          HighLevelOpcode.and read8RegisterL
        , -- 0xA6
          HighLevelOpcode.and read8IndirectHL
        , -- 0xA7
          HighLevelOpcode.and read8RegisterA
        , -- 0xA8
          HighLevelOpcode.xor read8RegisterB
        , -- 0xA9
          HighLevelOpcode.xor read8RegisterC
        , -- 0xAA
          HighLevelOpcode.xor read8RegisterD
        , -- 0xAB
          HighLevelOpcode.xor read8RegisterE
        , -- 0xAC
          HighLevelOpcode.xor read8RegisterH
        , -- 0xAD
          HighLevelOpcode.xor read8RegisterL
        , -- 0xAE
          HighLevelOpcode.xor read8IndirectHL
        , -- 0xAF
          HighLevelOpcode.xor read8RegisterA
        , -- 0xB0
          HighLevelOpcode.or read8RegisterB
        , -- 0xB1
          HighLevelOpcode.or read8RegisterC
        , -- 0xB2
          HighLevelOpcode.or read8RegisterD
        , -- 0xB3
          HighLevelOpcode.or read8RegisterE
        , -- 0xB4
          HighLevelOpcode.or read8RegisterH
        , -- 0xB5
          HighLevelOpcode.or read8RegisterL
        , -- 0xB6
          HighLevelOpcode.or read8IndirectHL
        , -- 0xB7
          HighLevelOpcode.or read8RegisterA
        , -- 0xB8
          HighLevelOpcode.cp read8RegisterB
        , -- 0xB9
          HighLevelOpcode.cp read8RegisterC
        , -- 0xBA
          HighLevelOpcode.cp read8RegisterD
        , -- 0xBB
          HighLevelOpcode.cp read8RegisterE
        , -- 0xBC
          HighLevelOpcode.cp read8RegisterH
        , -- 0xBD
          HighLevelOpcode.cp read8RegisterL
        , -- 0xBE
          HighLevelOpcode.cp read8IndirectHL
        , -- 0xBF
          HighLevelOpcode.cp read8RegisterA
        , -- 0xC0
          HighLevelOpcode.ret Condition.NotZero
        , -- 0xC1
          HighLevelOpcode.pop write16RegisterBC
        , -- 0xC2
          HighLevelOpcode.jp Condition.NotZero read16Immediate
        , -- 0xC3
          HighLevelOpcode.jp Condition.Always read16Immediate
        , -- 0xC4
          HighLevelOpcode.call Condition.NotZero
        , -- 0xC5
          HighLevelOpcode.push read16RegisterBC
        , -- 0xC6
          HighLevelOpcode.add read8Immediate
        , -- 0xC7
          HighLevelOpcode.rst 0x00
        , -- 0xC8
          HighLevelOpcode.ret Condition.Zero
        , -- 0xC9
          HighLevelOpcode.ret Condition.Always
        , -- 0xCA
          HighLevelOpcode.jp Condition.Zero read16Immediate
        , -- 0xCB
          HighLevelOpcode.extensionOpcode prefixedOpcodes
        , -- 0xCC
          HighLevelOpcode.call Condition.Zero
        , -- 0xCD
          HighLevelOpcode.call Condition.Always
        , -- 0xCE
          HighLevelOpcode.adc read8Immediate
        , -- 0xCF
          HighLevelOpcode.rst 0x08
        , -- 0xD0
          HighLevelOpcode.ret Condition.NotCarry
        , -- 0xD1
          HighLevelOpcode.pop write16RegisterDE
        , -- 0xD2
          HighLevelOpcode.jp Condition.NotCarry read16Immediate
        , -- 0xD3
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xD4
          HighLevelOpcode.call Condition.NotCarry
        , -- 0xD5
          HighLevelOpcode.push read16RegisterDE
        , -- 0xD6
          HighLevelOpcode.sub read8Immediate
        , -- 0xD7
          HighLevelOpcode.rst 0x10
        , -- 0xD8
          HighLevelOpcode.ret Condition.Carry
        , -- 0xD9
          HighLevelOpcode.reti
        , -- 0xDA
          HighLevelOpcode.jp Condition.Carry read16Immediate
        , -- 0xDB
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xDC
          HighLevelOpcode.call Condition.Carry
        , -- 0xDD
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xDE
          HighLevelOpcode.sbc read8Immediate
        , -- 0xDF
          HighLevelOpcode.rst 0x18
        , -- 0xE0
          HighLevelOpcode.ld write8IndirectWord8Operand read8RegisterA
        , -- 0xE1
          HighLevelOpcode.pop write16RegisterHL
        , -- 0xE2
          HighLevelOpcode.ld write8IndirectC read8RegisterA
        , -- 0xE3
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xE4
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xE5
          HighLevelOpcode.push read16RegisterHL
        , -- 0xE6
          HighLevelOpcode.and read8Immediate
        , -- 0xE7
          HighLevelOpcode.rst 0x20
        , -- 0xE8
          HighLevelOpcode.addSPSignedImmediate
        , -- 0xE9
          HighLevelOpcode.jpIndirectHL
        , -- 0xEA
          HighLevelOpcode.ld write8IndirectWord16Operand read8RegisterA
        , -- 0xEB
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xEC
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xED
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xEE
          HighLevelOpcode.xor read8Immediate
        , -- 0xEF
          HighLevelOpcode.rst 0x28
        , -- 0xF0
          HighLevelOpcode.ld write8RegisterA read8IndirectWord8Operand
        , -- 0xF1
          HighLevelOpcode.pop write16RegisterAF
        , -- 0xF2
          HighLevelOpcode.ld write8RegisterA read8IndirectC
        , -- 0xF3
          HighLevelOpcode.di
        , -- 0xF4
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xF5
          HighLevelOpcode.push read8RegisterAF
        , -- 0xF6
          HighLevelOpcode.or read8Immediate
        , -- 0xF7
          HighLevelOpcode.rst 0x30
        , -- 0xF8
          HighLevelOpcode.ldHLSPPlusSignedImmediate
        , -- 0xF9
          HighLevelOpcode.ldSPHL
        , -- 0xFA
          HighLevelOpcode.ld write8RegisterA read8IndirectWord16Operand
        , -- 0xFB
          HighLevelOpcode.ei
        , -- 0xFC
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xFD
          HighLevelOpcode.nop --Illegal Opcode
        , -- 0xFE
          HighLevelOpcode.cp read8Immediate
        , -- 0xFF
          HighLevelOpcode.rst 0x38
        ]


prefixedOpcodes : Array Effect
prefixedOpcodes =
    Array.fromList
        [ -- 0x00
          HighLevelOpcode.rlc read8RegisterB write8RegisterB
        , -- 0x01
          HighLevelOpcode.rlc read8RegisterC write8RegisterC
        , -- 0x02
          HighLevelOpcode.rlc read8RegisterD write8RegisterD
        , -- 0x03
          HighLevelOpcode.rlc read8RegisterE write8RegisterE
        , -- 0x04
          HighLevelOpcode.rlc read8RegisterH write8RegisterH
        , -- 0x05
          HighLevelOpcode.rlc read8RegisterL write8RegisterL
        , -- 0x06
          HighLevelOpcode.rlc read8IndirectHL write8IndirectHL
        , -- 0x07
          HighLevelOpcode.rlc read8RegisterA write8RegisterA
        , -- 0x08
          HighLevelOpcode.rrc read8RegisterB write8RegisterB
        , -- 0x09
          HighLevelOpcode.rrc read8RegisterC write8RegisterC
        , -- 0x0A
          HighLevelOpcode.rrc read8RegisterD write8RegisterD
        , -- 0x0B
          HighLevelOpcode.rrc read8RegisterE write8RegisterE
        , -- 0x0C
          HighLevelOpcode.rrc read8RegisterH write8RegisterH
        , -- 0x0D
          HighLevelOpcode.rrc read8RegisterL write8RegisterL
        , -- 0x0E
          HighLevelOpcode.rrc read8IndirectHL write8IndirectHL
        , -- 0x0F
          HighLevelOpcode.rrc read8RegisterA write8RegisterA
        , -- 0x10
          HighLevelOpcode.rl read8RegisterB write8RegisterB
        , -- 0x11
          HighLevelOpcode.rl read8RegisterC write8RegisterC
        , -- 0x12
          HighLevelOpcode.rl read8RegisterD write8RegisterD
        , -- 0x13
          HighLevelOpcode.rl read8RegisterE write8RegisterE
        , -- 0x14
          HighLevelOpcode.rl read8RegisterH write8RegisterH
        , -- 0x15
          HighLevelOpcode.rl read8RegisterL write8RegisterL
        , -- 0x16
          HighLevelOpcode.rl read8IndirectHL write8IndirectHL
        , -- 0x17
          HighLevelOpcode.rl read8RegisterA write8RegisterA
        , -- 0x18
          HighLevelOpcode.rr read8RegisterB write8RegisterB
        , -- 0x19
          HighLevelOpcode.rr read8RegisterC write8RegisterC
        , -- 0x1A
          HighLevelOpcode.rr read8RegisterD write8RegisterD
        , -- 0x1B
          HighLevelOpcode.rr read8RegisterE write8RegisterE
        , -- 0x1C
          HighLevelOpcode.rr read8RegisterH write8RegisterH
        , -- 0x1D
          HighLevelOpcode.rr read8RegisterL write8RegisterL
        , -- 0x1E
          HighLevelOpcode.rr read8IndirectHL write8IndirectHL
        , -- 0x1F
          HighLevelOpcode.rr read8RegisterA write8RegisterA
        , -- 0x20
          HighLevelOpcode.sla read8RegisterB write8RegisterB
        , -- 0x21
          HighLevelOpcode.sla read8RegisterC write8RegisterC
        , -- 0x22
          HighLevelOpcode.sla read8RegisterD write8RegisterD
        , -- 0x23
          HighLevelOpcode.sla read8RegisterE write8RegisterE
        , -- 0x24
          HighLevelOpcode.sla read8RegisterH write8RegisterH
        , -- 0x25
          HighLevelOpcode.sla read8RegisterL write8RegisterL
        , -- 0x26
          HighLevelOpcode.sla read8IndirectHL write8IndirectHL
        , -- 0x27
          HighLevelOpcode.sla read8RegisterA write8RegisterA
        , -- 0x28
          HighLevelOpcode.sra read8RegisterB write8RegisterB
        , -- 0x29
          HighLevelOpcode.sra read8RegisterC write8RegisterC
        , -- 0x2A
          HighLevelOpcode.sra read8RegisterD write8RegisterD
        , -- 0x2B
          HighLevelOpcode.sra read8RegisterE write8RegisterE
        , -- 0x2C
          HighLevelOpcode.sra read8RegisterH write8RegisterH
        , -- 0x2D
          HighLevelOpcode.sra read8RegisterL write8RegisterL
        , -- 0x2E
          HighLevelOpcode.sra read8IndirectHL write8IndirectHL
        , -- 0x2F
          HighLevelOpcode.sra read8RegisterA write8RegisterA
        , -- 0x30
          HighLevelOpcode.swap read8RegisterB write8RegisterB
        , -- 0x31
          HighLevelOpcode.swap read8RegisterC write8RegisterC
        , -- 0x32
          HighLevelOpcode.swap read8RegisterD write8RegisterD
        , -- 0x33
          HighLevelOpcode.swap read8RegisterE write8RegisterE
        , -- 0x34
          HighLevelOpcode.swap read8RegisterH write8RegisterH
        , -- 0x35
          HighLevelOpcode.swap read8RegisterL write8RegisterL
        , -- 0x36
          HighLevelOpcode.swap read8IndirectHL write8IndirectHL
        , -- 0x37
          HighLevelOpcode.swap read8RegisterA write8RegisterA
        , -- 0x38
          HighLevelOpcode.srl read8RegisterB write8RegisterB
        , -- 0x39
          HighLevelOpcode.srl read8RegisterC write8RegisterC
        , -- 0x3A
          HighLevelOpcode.srl read8RegisterD write8RegisterD
        , -- 0x3B
          HighLevelOpcode.srl read8RegisterE write8RegisterE
        , -- 0x3C
          HighLevelOpcode.srl read8RegisterH write8RegisterH
        , -- 0x3D
          HighLevelOpcode.srl read8RegisterL write8RegisterL
        , -- 0x3E
          HighLevelOpcode.srl read8IndirectHL write8IndirectHL
        , -- 0x3F
          HighLevelOpcode.srl read8RegisterA write8RegisterA
        , -- 0x40
          HighLevelOpcode.bit 0 read8RegisterB
        , -- 0x41
          HighLevelOpcode.bit 0 read8RegisterC
        , -- 0x42
          HighLevelOpcode.bit 0 read8RegisterD
        , -- 0x43
          HighLevelOpcode.bit 0 read8RegisterE
        , -- 0x44
          HighLevelOpcode.bit 0 read8RegisterH
        , -- 0x45
          HighLevelOpcode.bit 0 read8RegisterL
        , -- 0x46
          HighLevelOpcode.bit 0 read8IndirectHL
        , -- 0x47
          HighLevelOpcode.bit 0 read8RegisterA
        , -- 0x48
          HighLevelOpcode.bit 1 read8RegisterB
        , -- 0x49
          HighLevelOpcode.bit 1 read8RegisterC
        , -- 0x4A
          HighLevelOpcode.bit 1 read8RegisterD
        , -- 0x4B
          HighLevelOpcode.bit 1 read8RegisterE
        , -- 0x4C
          HighLevelOpcode.bit 1 read8RegisterH
        , -- 0x4D
          HighLevelOpcode.bit 1 read8RegisterL
        , -- 0x4E
          HighLevelOpcode.bit 1 read8IndirectHL
        , -- 0x4F
          HighLevelOpcode.bit 1 read8RegisterA
        , -- 0x50
          HighLevelOpcode.bit 2 read8RegisterB
        , -- 0x51
          HighLevelOpcode.bit 2 read8RegisterC
        , -- 0x52
          HighLevelOpcode.bit 2 read8RegisterD
        , -- 0x53
          HighLevelOpcode.bit 2 read8RegisterE
        , -- 0x54
          HighLevelOpcode.bit 2 read8RegisterH
        , -- 0x55
          HighLevelOpcode.bit 2 read8RegisterL
        , -- 0x56
          HighLevelOpcode.bit 2 read8IndirectHL
        , -- 0x57
          HighLevelOpcode.bit 2 read8RegisterA
        , -- 0x58
          HighLevelOpcode.bit 3 read8RegisterB
        , -- 0x59
          HighLevelOpcode.bit 3 read8RegisterC
        , -- 0x5A
          HighLevelOpcode.bit 3 read8RegisterD
        , -- 0x5B
          HighLevelOpcode.bit 3 read8RegisterE
        , -- 0x5C
          HighLevelOpcode.bit 3 read8RegisterH
        , -- 0x5D
          HighLevelOpcode.bit 3 read8RegisterL
        , -- 0x5E
          HighLevelOpcode.bit 3 read8IndirectHL
        , -- 0x5F
          HighLevelOpcode.bit 3 read8RegisterA
        , -- 0x60
          HighLevelOpcode.bit 4 read8RegisterB
        , -- 0x61
          HighLevelOpcode.bit 4 read8RegisterC
        , -- 0x62
          HighLevelOpcode.bit 4 read8RegisterD
        , -- 0x63
          HighLevelOpcode.bit 4 read8RegisterE
        , -- 0x64
          HighLevelOpcode.bit 4 read8RegisterH
        , -- 0x65
          HighLevelOpcode.bit 4 read8RegisterL
        , -- 0x66
          HighLevelOpcode.bit 4 read8IndirectHL
        , -- 0x67
          HighLevelOpcode.bit 4 read8RegisterA
        , -- 0x68
          HighLevelOpcode.bit 5 read8RegisterB
        , -- 0x69
          HighLevelOpcode.bit 5 read8RegisterC
        , -- 0x6A
          HighLevelOpcode.bit 5 read8RegisterD
        , -- 0x6B
          HighLevelOpcode.bit 5 read8RegisterE
        , -- 0x6C
          HighLevelOpcode.bit 5 read8RegisterH
        , -- 0x6D
          HighLevelOpcode.bit 5 read8RegisterL
        , -- 0x6E
          HighLevelOpcode.bit 5 read8IndirectHL
        , -- 0x6F
          HighLevelOpcode.bit 5 read8RegisterA
        , -- 0x70
          HighLevelOpcode.bit 6 read8RegisterB
        , -- 0x71
          HighLevelOpcode.bit 6 read8RegisterC
        , -- 0x72
          HighLevelOpcode.bit 6 read8RegisterD
        , -- 0x73
          HighLevelOpcode.bit 6 read8RegisterE
        , -- 0x74
          HighLevelOpcode.bit 6 read8RegisterH
        , -- 0x75
          HighLevelOpcode.bit 6 read8RegisterL
        , -- 0x76
          HighLevelOpcode.bit 6 read8IndirectHL
        , -- 0x77
          HighLevelOpcode.bit 6 read8RegisterA
        , -- 0x78
          HighLevelOpcode.bit 7 read8RegisterB
        , -- 0x79
          HighLevelOpcode.bit 7 read8RegisterC
        , -- 0x7A
          HighLevelOpcode.bit 7 read8RegisterD
        , -- 0x7B
          HighLevelOpcode.bit 7 read8RegisterE
        , -- 0x7C
          HighLevelOpcode.bit 7 read8RegisterH
        , -- 0x7D
          HighLevelOpcode.bit 7 read8RegisterL
        , -- 0x7E
          HighLevelOpcode.bit 7 read8IndirectHL
        , -- 0x7F
          HighLevelOpcode.bit 7 read8RegisterA
        , -- 0x80
          HighLevelOpcode.res 0 read8RegisterB write8RegisterB
        , -- 0x81
          HighLevelOpcode.res 0 read8RegisterC write8RegisterC
        , -- 0x82
          HighLevelOpcode.res 0 read8RegisterD write8RegisterD
        , -- 0x83
          HighLevelOpcode.res 0 read8RegisterE write8RegisterE
        , -- 0x84
          HighLevelOpcode.res 0 read8RegisterH write8RegisterH
        , -- 0x85
          HighLevelOpcode.res 0 read8RegisterL write8RegisterL
        , -- 0x86
          HighLevelOpcode.res 0 read8IndirectHL write8IndirectHL
        , -- 0x87
          HighLevelOpcode.res 0 read8RegisterA write8RegisterA
        , -- 0x88
          HighLevelOpcode.res 1 read8RegisterB write8RegisterB
        , -- 0x89
          HighLevelOpcode.res 1 read8RegisterC write8RegisterC
        , -- 0x8A
          HighLevelOpcode.res 1 read8RegisterD write8RegisterD
        , -- 0x8B
          HighLevelOpcode.res 1 read8RegisterE write8RegisterE
        , -- 0x8C
          HighLevelOpcode.res 1 read8RegisterH write8RegisterH
        , -- 0x8D
          HighLevelOpcode.res 1 read8RegisterL write8RegisterL
        , -- 0x8E
          HighLevelOpcode.res 1 read8IndirectHL write8IndirectHL
        , -- 0x8F
          HighLevelOpcode.res 1 read8RegisterA write8RegisterA
        , -- 0x90
          HighLevelOpcode.res 2 read8RegisterB write8RegisterB
        , -- 0x91
          HighLevelOpcode.res 2 read8RegisterC write8RegisterC
        , -- 0x92
          HighLevelOpcode.res 2 read8RegisterD write8RegisterD
        , -- 0x93
          HighLevelOpcode.res 2 read8RegisterE write8RegisterE
        , -- 0x94
          HighLevelOpcode.res 2 read8RegisterH write8RegisterH
        , -- 0x95
          HighLevelOpcode.res 2 read8RegisterL write8RegisterL
        , -- 0x96
          HighLevelOpcode.res 2 read8IndirectHL write8IndirectHL
        , -- 0x97
          HighLevelOpcode.res 2 read8RegisterA write8RegisterA
        , -- 0x98
          HighLevelOpcode.res 3 read8RegisterB write8RegisterB
        , -- 0x99
          HighLevelOpcode.res 3 read8RegisterC write8RegisterC
        , -- 0x9A
          HighLevelOpcode.res 3 read8RegisterD write8RegisterD
        , -- 0x9B
          HighLevelOpcode.res 3 read8RegisterE write8RegisterE
        , -- 0x9C
          HighLevelOpcode.res 3 read8RegisterH write8RegisterH
        , -- 0x9D
          HighLevelOpcode.res 3 read8RegisterL write8RegisterL
        , -- 0x9E
          HighLevelOpcode.res 3 read8IndirectHL write8IndirectHL
        , -- 0x9F
          HighLevelOpcode.res 3 read8RegisterA write8RegisterA
        , -- 0xA0
          HighLevelOpcode.res 4 read8RegisterB write8RegisterB
        , -- 0xA1
          HighLevelOpcode.res 4 read8RegisterC write8RegisterC
        , -- 0xA2
          HighLevelOpcode.res 4 read8RegisterD write8RegisterD
        , -- 0xA3
          HighLevelOpcode.res 4 read8RegisterE write8RegisterE
        , -- 0xA4
          HighLevelOpcode.res 4 read8RegisterH write8RegisterH
        , -- 0xA5
          HighLevelOpcode.res 4 read8RegisterL write8RegisterL
        , -- 0xA6
          HighLevelOpcode.res 4 read8IndirectHL write8IndirectHL
        , -- 0xA7
          HighLevelOpcode.res 4 read8RegisterA write8RegisterA
        , -- 0xA8
          HighLevelOpcode.res 5 read8RegisterB write8RegisterB
        , -- 0xA9
          HighLevelOpcode.res 5 read8RegisterC write8RegisterC
        , -- 0xAA
          HighLevelOpcode.res 5 read8RegisterD write8RegisterD
        , -- 0xAB
          HighLevelOpcode.res 5 read8RegisterE write8RegisterE
        , -- 0xAC
          HighLevelOpcode.res 5 read8RegisterH write8RegisterH
        , -- 0xAD
          HighLevelOpcode.res 5 read8RegisterL write8RegisterL
        , -- 0xAE
          HighLevelOpcode.res 5 read8IndirectHL write8IndirectHL
        , -- 0xAF
          HighLevelOpcode.res 5 read8RegisterA write8RegisterA
        , -- 0xB0
          HighLevelOpcode.res 6 read8RegisterB write8RegisterB
        , -- 0xB1
          HighLevelOpcode.res 6 read8RegisterC write8RegisterC
        , -- 0xB2
          HighLevelOpcode.res 6 read8RegisterD write8RegisterD
        , -- 0xB3
          HighLevelOpcode.res 6 read8RegisterE write8RegisterE
        , -- 0xB4
          HighLevelOpcode.res 6 read8RegisterH write8RegisterH
        , -- 0xB5
          HighLevelOpcode.res 6 read8RegisterL write8RegisterL
        , -- 0xB6
          HighLevelOpcode.res 6 read8IndirectHL write8IndirectHL
        , -- 0xB7
          HighLevelOpcode.res 6 read8RegisterA write8RegisterA
        , -- 0xB8
          HighLevelOpcode.res 7 read8RegisterB write8RegisterB
        , -- 0xB9
          HighLevelOpcode.res 7 read8RegisterC write8RegisterC
        , -- 0xBA
          HighLevelOpcode.res 7 read8RegisterD write8RegisterD
        , -- 0xBB
          HighLevelOpcode.res 7 read8RegisterE write8RegisterE
        , -- 0xBC
          HighLevelOpcode.res 7 read8RegisterH write8RegisterH
        , -- 0xBD
          HighLevelOpcode.res 7 read8RegisterL write8RegisterL
        , -- 0xBE
          HighLevelOpcode.res 7 read8IndirectHL write8IndirectHL
        , -- 0xBF
          HighLevelOpcode.res 7 read8RegisterA write8RegisterA
        , -- 0xC0
          HighLevelOpcode.set 0 read8RegisterB write8RegisterB
        , -- 0xC1
          HighLevelOpcode.set 0 read8RegisterC write8RegisterC
        , -- 0xC2
          HighLevelOpcode.set 0 read8RegisterD write8RegisterD
        , -- 0xC3
          HighLevelOpcode.set 0 read8RegisterE write8RegisterE
        , -- 0xC4
          HighLevelOpcode.set 0 read8RegisterH write8RegisterH
        , -- 0xC5
          HighLevelOpcode.set 0 read8RegisterL write8RegisterL
        , -- 0xC6
          HighLevelOpcode.set 0 read8IndirectHL write8IndirectHL
        , -- 0xC7
          HighLevelOpcode.set 0 read8RegisterA write8RegisterA
        , -- 0xC8
          HighLevelOpcode.set 1 read8RegisterB write8RegisterB
        , -- 0xC9
          HighLevelOpcode.set 1 read8RegisterC write8RegisterC
        , -- 0xCA
          HighLevelOpcode.set 1 read8RegisterD write8RegisterD
        , -- 0xCB
          HighLevelOpcode.set 1 read8RegisterE write8RegisterE
        , -- 0xCC
          HighLevelOpcode.set 1 read8RegisterH write8RegisterH
        , -- 0xCD
          HighLevelOpcode.set 1 read8RegisterL write8RegisterL
        , -- 0xCE
          HighLevelOpcode.set 1 read8IndirectHL write8IndirectHL
        , -- 0xCF
          HighLevelOpcode.set 1 read8RegisterA write8RegisterA
        , -- 0xD0
          HighLevelOpcode.set 2 read8RegisterB write8RegisterB
        , -- 0xD1
          HighLevelOpcode.set 2 read8RegisterC write8RegisterC
        , -- 0xD2
          HighLevelOpcode.set 2 read8RegisterD write8RegisterD
        , -- 0xD3
          HighLevelOpcode.set 2 read8RegisterE write8RegisterE
        , -- 0xD4
          HighLevelOpcode.set 2 read8RegisterH write8RegisterH
        , -- 0xD5
          HighLevelOpcode.set 2 read8RegisterL write8RegisterL
        , -- 0xD6
          HighLevelOpcode.set 2 read8IndirectHL write8IndirectHL
        , -- 0xD7
          HighLevelOpcode.set 2 read8RegisterA write8RegisterA
        , -- 0xD8
          HighLevelOpcode.set 3 read8RegisterB write8RegisterB
        , -- 0xD9
          HighLevelOpcode.set 3 read8RegisterC write8RegisterC
        , -- 0xDA
          HighLevelOpcode.set 3 read8RegisterD write8RegisterD
        , -- 0xDB
          HighLevelOpcode.set 3 read8RegisterE write8RegisterE
        , -- 0xDC
          HighLevelOpcode.set 3 read8RegisterH write8RegisterH
        , -- 0xDD
          HighLevelOpcode.set 3 read8RegisterL write8RegisterL
        , -- 0xDE
          HighLevelOpcode.set 3 read8IndirectHL write8IndirectHL
        , -- 0xDF
          HighLevelOpcode.set 3 read8RegisterA write8RegisterA
        , -- 0xE0
          HighLevelOpcode.set 4 read8RegisterB write8RegisterB
        , -- 0xE1
          HighLevelOpcode.set 4 read8RegisterC write8RegisterC
        , -- 0xE2
          HighLevelOpcode.set 4 read8RegisterD write8RegisterD
        , -- 0xE3
          HighLevelOpcode.set 4 read8RegisterE write8RegisterE
        , -- 0xE4
          HighLevelOpcode.set 4 read8RegisterH write8RegisterH
        , -- 0xE5
          HighLevelOpcode.set 4 read8RegisterL write8RegisterL
        , -- 0xE6
          HighLevelOpcode.set 4 read8IndirectHL write8IndirectHL
        , -- 0xE7
          HighLevelOpcode.set 4 read8RegisterA write8RegisterA
        , -- 0xE8
          HighLevelOpcode.set 5 read8RegisterB write8RegisterB
        , -- 0xE9
          HighLevelOpcode.set 5 read8RegisterC write8RegisterC
        , -- 0xEA
          HighLevelOpcode.set 5 read8RegisterD write8RegisterD
        , -- 0xEB
          HighLevelOpcode.set 5 read8RegisterE write8RegisterE
        , -- 0xEC
          HighLevelOpcode.set 5 read8RegisterH write8RegisterH
        , -- 0xED
          HighLevelOpcode.set 5 read8RegisterL write8RegisterL
        , -- 0xEE
          HighLevelOpcode.set 5 read8IndirectHL write8IndirectHL
        , -- 0xEF
          HighLevelOpcode.set 5 read8RegisterA write8RegisterA
        , -- 0xF0
          HighLevelOpcode.set 6 read8RegisterB write8RegisterB
        , -- 0xF1
          HighLevelOpcode.set 6 read8RegisterC write8RegisterC
        , -- 0xF2
          HighLevelOpcode.set 6 read8RegisterD write8RegisterD
        , -- 0xF3
          HighLevelOpcode.set 6 read8RegisterE write8RegisterE
        , -- 0xF4
          HighLevelOpcode.set 6 read8RegisterH write8RegisterH
        , -- 0xF5
          HighLevelOpcode.set 6 read8RegisterL write8RegisterL
        , -- 0xF6
          HighLevelOpcode.set 6 read8IndirectHL write8IndirectHL
        , -- 0xF7
          HighLevelOpcode.set 6 read8RegisterA write8RegisterA
        , -- 0xF8
          HighLevelOpcode.set 7 read8RegisterB write8RegisterB
        , -- 0xF9
          HighLevelOpcode.set 7 read8RegisterC write8RegisterC
        , -- 0xFA
          HighLevelOpcode.set 7 read8RegisterD write8RegisterD
        , -- 0xFB
          HighLevelOpcode.set 7 read8RegisterE write8RegisterE
        , -- 0xFC
          HighLevelOpcode.set 7 read8RegisterH write8RegisterH
        , -- 0xFD
          HighLevelOpcode.set 7 read8RegisterL write8RegisterL
        , -- 0xFE
          HighLevelOpcode.set 7 read8IndirectHL write8IndirectHL
        , -- 0xFF
          HighLevelOpcode.set 7 read8RegisterA write8RegisterA
        ]
