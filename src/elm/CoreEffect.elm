module CoreEffect exposing
    ( extraCycles
    , readMemory16
    , readMemory16AdvancePC
    , readMemory8
    , readMemory8AdvancePC
    , readRegisterA
    , readRegisterAF
    , readRegisterB
    , readRegisterBC
    , readRegisterC
    , readRegisterD
    , readRegisterDE
    , readRegisterE
    , readRegisterF
    , readRegisterH
    , readRegisterHL
    , readRegisterL
    , readRegisterPC
    , readRegisterSP
    , writeMemory16
    , writeMemory8
    , writeRegisterA
    , writeRegisterAF
    , writeRegisterB
    , writeRegisterBC
    , writeRegisterC
    , writeRegisterD
    , writeRegisterDE
    , writeRegisterE
    , writeRegisterF
    , writeRegisterH
    , writeRegisterHL
    , writeRegisterL
    , writeRegisterPC
    , writeRegisterSP
    )

import Component.CPU as CPU
import Component.MMU as MMU
import Effect exposing (Reader, Writer)
import GameBoy


readRegisterA : Reader Int
readRegisterA gameBoy =
    ( CPU.readRegisterA gameBoy.cpu, gameBoy )


readRegisterAF : Reader Int
readRegisterAF gameBoy =
    ( CPU.readRegisterAF gameBoy.cpu, gameBoy )


readRegisterB : Reader Int
readRegisterB gameBoy =
    ( CPU.readRegisterB gameBoy.cpu, gameBoy )


readRegisterBC : Reader Int
readRegisterBC gameBoy =
    ( CPU.readRegisterBC gameBoy.cpu, gameBoy )


readRegisterC : Reader Int
readRegisterC gameBoy =
    ( CPU.readRegisterC gameBoy.cpu, gameBoy )


readRegisterD : Reader Int
readRegisterD gameBoy =
    ( CPU.readRegisterD gameBoy.cpu, gameBoy )


readRegisterDE : Reader Int
readRegisterDE gameBoy =
    ( CPU.readRegisterDE gameBoy.cpu, gameBoy )


readRegisterE : Reader Int
readRegisterE gameBoy =
    ( CPU.readRegisterE gameBoy.cpu, gameBoy )


readRegisterF : Reader Int
readRegisterF gameBoy =
    ( CPU.readRegisterF gameBoy.cpu, gameBoy )


readRegisterH : Reader Int
readRegisterH gameBoy =
    ( CPU.readRegisterH gameBoy.cpu, gameBoy )


readRegisterHL : Reader Int
readRegisterHL gameBoy =
    ( CPU.readRegisterHL gameBoy.cpu, gameBoy )


readRegisterL : Reader Int
readRegisterL gameBoy =
    ( CPU.readRegisterL gameBoy.cpu, gameBoy )


readRegisterPC : Reader Int
readRegisterPC gameBoy =
    ( CPU.readRegisterPC gameBoy.cpu, gameBoy )


readRegisterSP : Reader Int
readRegisterSP gameBoy =
    ( CPU.readRegisterSP gameBoy.cpu, gameBoy )


writeRegisterA : Writer Int
writeRegisterA value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterA value gameBoy.cpu) gameBoy


writeRegisterAF : Writer Int
writeRegisterAF value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterAF value gameBoy.cpu) gameBoy


writeRegisterB : Writer Int
writeRegisterB value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterB value gameBoy.cpu) gameBoy


writeRegisterBC : Writer Int
writeRegisterBC value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterBC value gameBoy.cpu) gameBoy


writeRegisterC : Writer Int
writeRegisterC value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterC value gameBoy.cpu) gameBoy


writeRegisterD : Writer Int
writeRegisterD value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterD value gameBoy.cpu) gameBoy


writeRegisterDE : Writer Int
writeRegisterDE value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterDE value gameBoy.cpu) gameBoy


writeRegisterE : Writer Int
writeRegisterE value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterE value gameBoy.cpu) gameBoy


writeRegisterF : Writer Int
writeRegisterF value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterF value gameBoy.cpu) gameBoy


writeRegisterH : Writer Int
writeRegisterH value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterH value gameBoy.cpu) gameBoy


writeRegisterHL : Writer Int
writeRegisterHL value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterHL value gameBoy.cpu) gameBoy


writeRegisterL : Writer Int
writeRegisterL value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterL value gameBoy.cpu) gameBoy


writeRegisterPC : Writer Int
writeRegisterPC value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterPC value gameBoy.cpu) gameBoy


writeRegisterSP : Writer Int
writeRegisterSP value gameBoy =
    GameBoy.setCPU (CPU.writeRegisterSP value gameBoy.cpu) gameBoy


readMemory8 : Reader Int -> Reader Int
readMemory8 reader gameBoy =
    let
        ( memoryAddress, gameBoy2 ) =
            reader gameBoy
    in
    ( MMU.readWord8 gameBoy2 memoryAddress, GameBoy.setLastInstructionCycles (gameBoy2.lastInstructionCycles + 4) gameBoy2 )


readMemory16 : Reader Int -> Reader Int
readMemory16 reader gameBoy =
    let
        ( memoryAddress, gameBoy2 ) =
            reader gameBoy
    in
    ( MMU.readWord16 gameBoy2 memoryAddress, GameBoy.setLastInstructionCycles (gameBoy2.lastInstructionCycles + 8) gameBoy2 )


writeMemory8 : Reader Int -> Writer Int
writeMemory8 reader value gameBoy =
    let
        ( memoryAddress, gameBoy2 ) =
            reader gameBoy
    in
    MMU.writeWord8 memoryAddress value (GameBoy.setLastInstructionCycles (gameBoy2.lastInstructionCycles + 4) gameBoy2)


writeMemory16 : Reader Int -> Writer Int
writeMemory16 reader value gameBoy =
    let
        ( memoryAddress, gameBoy2 ) =
            reader gameBoy
    in
    MMU.writeWord16 memoryAddress value (GameBoy.setLastInstructionCycles (gameBoy2.lastInstructionCycles + 8) gameBoy2)


readMemory8AdvancePC : Reader Int
readMemory8AdvancePC ({ cpu } as gameBoy) =
    let
        pc =
            CPU.readRegisterPC cpu

        incrementedPc =
            pc + 1

        operand =
            MMU.readWord8 gameBoy pc
    in
    ( operand, GameBoy.setCPULastInstructionCycles (CPU.writeRegisterPC incrementedPc cpu) (gameBoy.lastInstructionCycles + 4) gameBoy )


readMemory16AdvancePC : Reader Int
readMemory16AdvancePC ({ cpu } as gameBoy) =
    let
        pc =
            CPU.readRegisterPC cpu

        incrementedPc =
            pc + 2

        operand =
            MMU.readWord16 gameBoy pc
    in
    ( operand, GameBoy.setCPULastInstructionCycles (CPU.writeRegisterPC incrementedPc cpu) (gameBoy.lastInstructionCycles + 8) gameBoy )


extraCycles : Writer Int
extraCycles value gameBoy =
    GameBoy.setLastInstructionCycles (gameBoy.lastInstructionCycles + value) gameBoy
