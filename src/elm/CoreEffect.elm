module CoreEffect exposing
    ( extraClocks
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

import Component.CPU as CPU exposing (Register16(..), Register8(..))
import Component.MMU as MMU
import Effect exposing (..)
import Util


readRegister8 : Register8 -> Reader Int
readRegister8 register gameBoy =
    ( CPU.readRegister8 register gameBoy.cpu, gameBoy )


readRegister16 : Register16 -> Reader Int
readRegister16 register gameBoy =
    ( CPU.readRegister16 register gameBoy.cpu, gameBoy )


writeRegister8 : Register8 -> Writer Int
writeRegister8 register value ({ cpu } as gameBoy) =
    { gameBoy | cpu = CPU.writeRegister8 register value cpu }


writeRegister16 : Register16 -> Writer Int
writeRegister16 register value ({ cpu } as gameBoy) =
    { gameBoy | cpu = CPU.writeRegister16 register value cpu }


readMemory8 : Reader Int -> Reader Int
readMemory8 reader gameBoy =
    let
        ( memoryAddress, gameBoy2 ) =
            reader gameBoy
    in
    ( MMU.readWord8 gameBoy2 memoryAddress, { gameBoy2 | lastCycleClocks = gameBoy2.lastCycleClocks + 4 } )


readMemory16 : Reader Int -> Reader Int
readMemory16 reader gameBoy =
    let
        ( memoryAddress, gameBoy2 ) =
            reader gameBoy
    in
    ( MMU.readWord16 gameBoy2 memoryAddress, { gameBoy2 | lastCycleClocks = gameBoy2.lastCycleClocks + 8 } )


writeMemory8 : Reader Int -> Writer Int
writeMemory8 reader value gameBoy =
    let
        ( memoryAddress, gameBoy2 ) =
            reader gameBoy
    in
    MMU.writeWord8 memoryAddress value { gameBoy2 | lastCycleClocks = gameBoy2.lastCycleClocks + 4 }


writeMemory16 : Reader Int -> Writer Int
writeMemory16 reader value gameBoy =
    let
        ( memoryAddress, gameBoy2 ) =
            reader gameBoy
    in
    MMU.writeWord16 memoryAddress value { gameBoy2 | lastCycleClocks = gameBoy2.lastCycleClocks + 8 }


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
    ( operand, { gameBoy | cpu = CPU.writeRegister16 PC incrementedPc cpu, lastCycleClocks = gameBoy.lastCycleClocks + 4 } )


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
    ( operand, { gameBoy | cpu = CPU.writeRegister16 PC incrementedPc cpu, lastCycleClocks = gameBoy.lastCycleClocks + 8 } )


extraClocks : Writer Int
extraClocks value gameBoy =
    { gameBoy | lastCycleClocks = gameBoy.lastCycleClocks + value }
