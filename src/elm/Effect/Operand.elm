module Effect.Operand exposing
    ( read16Immediate
    , read16IndirectSP
    , read16RegisterBC
    , read16RegisterDE
    , read16RegisterHL
    , read16RegisterPC
    , read16RegisterSP
    , read8Immediate
    , read8IndirectBC
    , read8IndirectC
    , read8IndirectDE
    , read8IndirectHL
    , read8IndirectHLPostDecrement
    , read8IndirectHLPostIncrement
    , read8IndirectWord16Operand
    , read8IndirectWord8Operand
    , read8RegisterA
    , read8RegisterAF
    , read8RegisterB
    , read8RegisterC
    , read8RegisterD
    , read8RegisterE
    , read8RegisterF
    , read8RegisterH
    , read8RegisterL
    , write16IndirectWord16Operand
    , write16RegisterAF
    , write16RegisterBC
    , write16RegisterDE
    , write16RegisterHL
    , write16RegisterPC
    , write16RegisterSP
    , write8IndirectBC
    , write8IndirectC
    , write8IndirectDE
    , write8IndirectHL
    , write8IndirectHLPostDecrement
    , write8IndirectHLPostIncrement
    , write8IndirectWord16Operand
    , write8IndirectWord8Operand
    , write8RegisterA
    , write8RegisterB
    , write8RegisterC
    , write8RegisterD
    , write8RegisterE
    , write8RegisterF
    , write8RegisterH
    , write8RegisterL
    )

{-| Readers and Writers that implement opcode operands. Opcode functions use these either internally or as parameters
to build an Effect that handles a specific opcode. Since they are fundamental building blocks, they are implemented
with an eye on performance, avoiding deeply nested function calls and combinators.

This leads to some code duplication and sometimes inelegant implementations. This is a deliberate trade-off.

-}

import Component.CPU as CPU
import Component.MMU as MMU
import Effect.Reader exposing (Reader)
import Effect.Writer exposing (Writer)
import GameBoy


read8Immediate : Reader Int
read8Immediate gameBoy =
    let
        pc =
            CPU.readRegisterPC gameBoy.cpu

        value =
            MMU.readWord8 gameBoy pc

        updatedPc =
            pc + 1

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedCpu =
            CPU.writeRegisterPC updatedPc gameBoy.cpu

        updatedGameBoy =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read16Immediate : Reader Int
read16Immediate gameBoy =
    let
        pc =
            CPU.readRegisterPC gameBoy.cpu

        value =
            MMU.readWord16 gameBoy pc

        updatedPc =
            pc + 2

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 8

        updatedCpu =
            CPU.writeRegisterPC updatedPc gameBoy.cpu

        updatedGameBoy =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read8IndirectHL : Reader Int
read8IndirectHL gameBoy =
    let
        value =
            MMU.readWord8 gameBoy (CPU.readRegisterHL gameBoy.cpu)

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedGameBoy =
            GameBoy.setLastInstructionCycles updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read8IndirectBC : Reader Int
read8IndirectBC gameBoy =
    let
        value =
            MMU.readWord8 gameBoy (CPU.readRegisterBC gameBoy.cpu)

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedGameBoy =
            GameBoy.setLastInstructionCycles updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read8IndirectDE : Reader Int
read8IndirectDE gameBoy =
    let
        value =
            MMU.readWord8 gameBoy (CPU.readRegisterDE gameBoy.cpu)

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedGameBoy =
            GameBoy.setLastInstructionCycles updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read16IndirectSP : Reader Int
read16IndirectSP gameBoy =
    let
        value =
            MMU.readWord16 gameBoy (CPU.readRegisterSP gameBoy.cpu)

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 8

        updatedGameBoy =
            GameBoy.setLastInstructionCycles updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read8IndirectC : Reader Int
read8IndirectC gameBoy =
    let
        value =
            MMU.readWord8 gameBoy (CPU.readRegisterC gameBoy.cpu + 0xFF00)

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedGameBoy =
            GameBoy.setLastInstructionCycles updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read8IndirectWord16Operand : Reader Int
read8IndirectWord16Operand gameBoy =
    let
        pc =
            CPU.readRegisterPC gameBoy.cpu

        address =
            MMU.readWord16 gameBoy pc

        value =
            MMU.readWord8 gameBoy address

        updatedPc =
            pc + 2

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 12

        updatedCpu =
            CPU.writeRegisterPC updatedPc gameBoy.cpu

        updatedGameBoy =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read8IndirectWord8Operand : Reader Int
read8IndirectWord8Operand gameBoy =
    let
        pc =
            CPU.readRegisterPC gameBoy.cpu

        address =
            MMU.readWord8 gameBoy pc + 0xFF00

        value =
            MMU.readWord8 gameBoy address

        updatedPc =
            pc + 1

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 8

        updatedCpu =
            CPU.writeRegisterPC updatedPc gameBoy.cpu

        updatedGameBoy =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read8IndirectHLPostIncrement : Reader Int
read8IndirectHLPostIncrement gameBoy =
    let
        hl =
            CPU.readRegisterHL gameBoy.cpu

        value =
            MMU.readWord8 gameBoy hl

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedHl =
            hl + 1

        updatedCpu =
            CPU.writeRegisterHL updatedHl gameBoy.cpu

        updatedGameBoy =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


read8IndirectHLPostDecrement : Reader Int
read8IndirectHLPostDecrement gameBoy =
    let
        hl =
            CPU.readRegisterHL gameBoy.cpu

        value =
            MMU.readWord8 gameBoy hl

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedHl =
            hl - 1

        updatedCpu =
            CPU.writeRegisterHL updatedHl gameBoy.cpu

        updatedGameBoy =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles gameBoy
    in
    ( value, updatedGameBoy )


write8IndirectHL : Writer Int
write8IndirectHL value gameBoy =
    let
        address =
            CPU.readRegisterHL gameBoy.cpu

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedGameBoyA =
            MMU.writeWord8 address value gameBoy

        updatedGameBoyB =
            GameBoy.setLastInstructionCycles updatedLastInstructionCycles updatedGameBoyA
    in
    updatedGameBoyB


write8IndirectBC : Writer Int
write8IndirectBC value gameBoy =
    let
        address =
            CPU.readRegisterBC gameBoy.cpu

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedGameBoyA =
            MMU.writeWord8 address value gameBoy

        updatedGameBoyB =
            GameBoy.setLastInstructionCycles updatedLastInstructionCycles updatedGameBoyA
    in
    updatedGameBoyB


write8IndirectDE : Writer Int
write8IndirectDE value gameBoy =
    let
        address =
            CPU.readRegisterDE gameBoy.cpu

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedGameBoyA =
            MMU.writeWord8 address value gameBoy

        updatedGameBoyB =
            GameBoy.setLastInstructionCycles updatedLastInstructionCycles updatedGameBoyA
    in
    updatedGameBoyB


write8IndirectC : Writer Int
write8IndirectC value gameBoy =
    let
        address =
            CPU.readRegisterC gameBoy.cpu + 0xFF00

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedGameBoyA =
            MMU.writeWord8 address value gameBoy

        updatedGameBoyB =
            GameBoy.setLastInstructionCycles updatedLastInstructionCycles updatedGameBoyA
    in
    updatedGameBoyB


write16IndirectWord16Operand : Writer Int
write16IndirectWord16Operand value gameBoy =
    let
        pc =
            CPU.readRegisterPC gameBoy.cpu

        address =
            MMU.readWord16 gameBoy pc

        updatedPc =
            pc + 2

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 16

        updatedCpu =
            CPU.writeRegisterPC updatedPc gameBoy.cpu

        updatedGameBoyA =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles gameBoy

        updatedGameBoyB =
            MMU.writeWord16 address value updatedGameBoyA
    in
    updatedGameBoyB


write8IndirectWord8Operand : Writer Int
write8IndirectWord8Operand value gameBoy =
    let
        pc =
            CPU.readRegisterPC gameBoy.cpu

        address =
            MMU.readWord8 gameBoy pc + 0xFF00

        updatedPc =
            pc + 1

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 8

        updatedCpu =
            CPU.writeRegisterPC updatedPc gameBoy.cpu

        updatedGameBoyA =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles gameBoy

        updatedGameBoyB =
            MMU.writeWord8 address value updatedGameBoyA
    in
    updatedGameBoyB


write8IndirectWord16Operand : Writer Int
write8IndirectWord16Operand value gameBoy =
    let
        pc =
            CPU.readRegisterPC gameBoy.cpu

        address =
            MMU.readWord16 gameBoy pc

        updatedPc =
            pc + 2

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 12

        updatedCpu =
            CPU.writeRegisterPC updatedPc gameBoy.cpu

        updatedGameBoyA =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles gameBoy

        updatedGameBoyB =
            MMU.writeWord8 address value updatedGameBoyA
    in
    updatedGameBoyB


write8IndirectHLPostIncrement : Writer Int
write8IndirectHLPostIncrement value gameBoy =
    let
        hl =
            CPU.readRegisterHL gameBoy.cpu

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedHl =
            hl + 1

        updatedGameBoyA =
            MMU.writeWord8 hl value gameBoy

        updatedCpu =
            CPU.writeRegisterHL updatedHl updatedGameBoyA.cpu

        updatedGameBoyB =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles updatedGameBoyA
    in
    updatedGameBoyB


write8IndirectHLPostDecrement : Writer Int
write8IndirectHLPostDecrement value gameBoy =
    let
        hl =
            CPU.readRegisterHL gameBoy.cpu

        updatedLastInstructionCycles =
            gameBoy.lastInstructionCycles + 4

        updatedHl =
            hl - 1

        updatedGameBoyA =
            MMU.writeWord8 hl value gameBoy

        updatedCpu =
            CPU.writeRegisterHL updatedHl updatedGameBoyA.cpu

        updatedGameBoyB =
            GameBoy.setCPULastInstructionCycles updatedCpu updatedLastInstructionCycles updatedGameBoyA
    in
    updatedGameBoyB


read8RegisterA : Reader Int
read8RegisterA gameBoy =
    ( CPU.readRegisterA gameBoy.cpu, gameBoy )


read8RegisterAF : Reader Int
read8RegisterAF gameBoy =
    ( CPU.readRegisterAF gameBoy.cpu, gameBoy )


read8RegisterB : Reader Int
read8RegisterB gameBoy =
    ( CPU.readRegisterB gameBoy.cpu, gameBoy )


read16RegisterBC : Reader Int
read16RegisterBC gameBoy =
    ( CPU.readRegisterBC gameBoy.cpu, gameBoy )


read8RegisterC : Reader Int
read8RegisterC gameBoy =
    ( CPU.readRegisterC gameBoy.cpu, gameBoy )


read8RegisterD : Reader Int
read8RegisterD gameBoy =
    ( CPU.readRegisterD gameBoy.cpu, gameBoy )


read16RegisterDE : Reader Int
read16RegisterDE gameBoy =
    ( CPU.readRegisterDE gameBoy.cpu, gameBoy )


read8RegisterE : Reader Int
read8RegisterE gameBoy =
    ( CPU.readRegisterE gameBoy.cpu, gameBoy )


read8RegisterF : Reader Int
read8RegisterF gameBoy =
    ( CPU.readRegisterF gameBoy.cpu, gameBoy )


read8RegisterH : Reader Int
read8RegisterH gameBoy =
    ( CPU.readRegisterH gameBoy.cpu, gameBoy )


read16RegisterHL : Reader Int
read16RegisterHL gameBoy =
    ( CPU.readRegisterHL gameBoy.cpu, gameBoy )


read8RegisterL : Reader Int
read8RegisterL gameBoy =
    ( CPU.readRegisterL gameBoy.cpu, gameBoy )


read16RegisterPC : Reader Int
read16RegisterPC gameBoy =
    ( CPU.readRegisterPC gameBoy.cpu, gameBoy )


read16RegisterSP : Reader Int
read16RegisterSP gameBoy =
    ( CPU.readRegisterSP gameBoy.cpu, gameBoy )


write8RegisterA : Writer Int
write8RegisterA value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterA value gameBoy.cpu) gameBoy


write16RegisterAF : Writer Int
write16RegisterAF value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterAF value gameBoy.cpu) gameBoy


write8RegisterB : Writer Int
write8RegisterB value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterB value gameBoy.cpu) gameBoy


write16RegisterBC : Writer Int
write16RegisterBC value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterBC value gameBoy.cpu) gameBoy


write8RegisterC : Writer Int
write8RegisterC value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterC value gameBoy.cpu) gameBoy


write8RegisterD : Writer Int
write8RegisterD value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterD value gameBoy.cpu) gameBoy


write16RegisterDE : Writer Int
write16RegisterDE value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterDE value gameBoy.cpu) gameBoy


write8RegisterE : Writer Int
write8RegisterE value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterE value gameBoy.cpu) gameBoy


write8RegisterF : Writer Int
write8RegisterF value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterF value gameBoy.cpu) gameBoy


write8RegisterH : Writer Int
write8RegisterH value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterH value gameBoy.cpu) gameBoy


write16RegisterHL : Writer Int
write16RegisterHL value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterHL value gameBoy.cpu) gameBoy


write8RegisterL : Writer Int
write8RegisterL value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterL value gameBoy.cpu) gameBoy


write16RegisterPC : Writer Int
write16RegisterPC value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterPC value gameBoy.cpu) gameBoy


write16RegisterSP : Writer Int
write16RegisterSP value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterSP value gameBoy.cpu) gameBoy
