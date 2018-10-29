module CoreEffect exposing
    ( extraCycles
    , readMemory16
    , readMemory16AdvancePC
    , readMemory8
    , readMemory8AdvancePC
    , readRegister16
    , readRegister8
    , writeMemory16
    , writeMemory8
    , writeRegister16
    , writeRegister8
    )

import Component.CPU as CPU exposing (CPU, Register16(..), Register8(..))
import Component.MMU as MMU
import Effect exposing (..)
import GameBoy exposing (GameBoy)
import Util


readRegister8 : Register8 -> Reader Int
readRegister8 register gameBoy =
    ( CPU.readRegister8 register gameBoy.cpu, gameBoy )


readRegister16 : Register16 -> Reader Int
readRegister16 register gameBoy =
    ( CPU.readRegister16 register gameBoy.cpu, gameBoy )


writeRegister8 : Register8 -> Writer Int
writeRegister8 register value gameBoy =
    GameBoy.setCPU (CPU.writeRegister8 register value gameBoy.cpu) gameBoy


writeRegister16 : Register16 -> Writer Int
writeRegister16 register value gameBoy =
    GameBoy.setCPU (CPU.writeRegister16 register value gameBoy.cpu) gameBoy


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
            CPU.readRegister16 PC cpu

        incrementedPc =
            pc + 1

        operand =
            MMU.readWord8 gameBoy pc
    in
    ( operand, GameBoy.setCPULastInstructionCycles (CPU.writeRegister16 PC incrementedPc cpu) (gameBoy.lastInstructionCycles + 4) gameBoy )


readMemory16AdvancePC : Reader Int
readMemory16AdvancePC ({ cpu } as gameBoy) =
    let
        pc =
            CPU.readRegister16 PC cpu

        incrementedPc =
            pc + 2

        operand =
            MMU.readWord16 gameBoy pc
    in
    ( operand, GameBoy.setCPULastInstructionCycles (CPU.writeRegister16 PC incrementedPc cpu) (gameBoy.lastInstructionCycles + 8) gameBoy )


extraCycles : Writer Int
extraCycles value gameBoy =
    GameBoy.setLastInstructionCycles (gameBoy.lastInstructionCycles + value) gameBoy
