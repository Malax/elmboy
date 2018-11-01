module Emulator exposing
    ( emulateConditional
    , emulateCycles
    , emulateNextInstruction
    )

import Bitwise
import Component.CPU as CPU exposing (Register16(..), Register8(..))
import Component.CPU.Opcode as Opcode
import Component.CPU.OpcodeMapper as OpcodeMapper
import Component.MMU as MMU
import Component.PPU as PPU
import Component.PPU.Types exposing (PPUInterrupt(..))
import Component.Timer as Timer
import Constants
import CoreEffect exposing (..)
import Effect exposing (..)
import GameBoy exposing (GameBoy)
import Types exposing (MemoryAddress)
import Util


emulateNextInstruction : GameBoy -> GameBoy
emulateNextInstruction gameBoy =
    let
        emulatedGameBoy =
            emulateInstruction gameBoy

        ppu =
            PPU.emulate emulatedGameBoy.lastInstructionCycles emulatedGameBoy.ppu

        timer =
            Timer.emulate emulatedGameBoy.lastInstructionCycles emulatedGameBoy.timer

        updatedInterruptFlag =
            List.foldl Bitwise.or
                emulatedGameBoy.cpu.interruptFlag
                [ Util.conditionalOrBitmask (ppu.triggeredInterrupt == Just VBlankInterrupt) 0x01
                , Util.conditionalOrBitmask (ppu.triggeredInterrupt == Just HBlankInterrupt) 0x02
                , Util.conditionalOrBitmask (ppu.triggeredInterrupt == Just LineCompareInterrupt) 0x02
                , Util.conditionalOrBitmask (ppu.triggeredInterrupt == Just OamInterrupt) 0x02
                , Util.conditionalOrBitmask timer.triggeredInterrupt 0x04
                , Util.conditionalOrBitmask emulatedGameBoy.joypad.triggeredInterrupt 0x10
                ]

        cpu =
            CPU.setInterruptFlag updatedInterruptFlag emulatedGameBoy.cpu
    in
    GameBoy.setComponents cpu ppu timer emulatedGameBoy


emulateCycles : Int -> GameBoy -> GameBoy
emulateCycles cycles gameBoy =
    let
        emulatedGameBoy =
            emulateNextInstruction gameBoy

        remainingCycles =
            cycles - emulatedGameBoy.lastInstructionCycles
    in
    if remainingCycles <= 0 then
        gameBoy

    else
        emulateCycles remainingCycles emulatedGameBoy


emulateConditional : (GameBoy -> Bool) -> GameBoy -> GameBoy
emulateConditional predicate gameBoy =
    let
        emulatedGameBoy =
            emulateNextInstruction gameBoy
    in
    if predicate emulatedGameBoy then
        emulateConditional predicate emulatedGameBoy

    else
        emulatedGameBoy



-- Internal


emulateInstruction : Effect
emulateInstruction gameBoy =
    let
        gameBoyAfterInterruptHandling =
            handleNextInterrupt gameBoy
    in
    if not gameBoyAfterInterruptHandling.cpu.halted then
        let
            opcode =
                MMU.readWord8 gameBoyAfterInterruptHandling gameBoyAfterInterruptHandling.cpu.pc |> OpcodeMapper.get

            gameBoyAfterOpcodeFetching =
                GameBoy.setCPUAndCycles (CPU.writeRegister16 PC (gameBoyAfterInterruptHandling.cpu.pc + 1) gameBoyAfterInterruptHandling.cpu) 4 gameBoyAfterInterruptHandling
        in
        opcode gameBoyAfterOpcodeFetching

    else
        GameBoy.setLastInstructionCycles 4 gameBoyAfterInterruptHandling


handleNextInterrupt : Effect
handleNextInterrupt ({ cpu } as gameBoy) =
    let
        filteredInterruptFlags =
            Bitwise.and cpu.interruptFlag cpu.interruptEnable
    in
    if cpu.interruptMasterEnable && filteredInterruptFlags > 0x00 then
        if Bitwise.and Constants.bit0Mask filteredInterruptFlags == Constants.bit0Mask then
            -- VBlank
            performInterrupt 0x40 (Bitwise.xor Constants.bit0Mask cpu.interruptFlag) gameBoy

        else if Bitwise.and Constants.bit1Mask filteredInterruptFlags == Constants.bit1Mask then
            -- LCD Status
            performInterrupt 0x48 (Bitwise.xor Constants.bit1Mask cpu.interruptFlag) gameBoy

        else if Bitwise.and Constants.bit2Mask filteredInterruptFlags == Constants.bit2Mask then
            -- Timer Overflow
            performInterrupt 0x50 (Bitwise.xor Constants.bit2Mask cpu.interruptFlag) gameBoy

        else if Bitwise.and Constants.bit3Mask filteredInterruptFlags == Constants.bit3Mask then
            -- Serial
            performInterrupt 0x58 (Bitwise.xor Constants.bit3Mask cpu.interruptFlag) gameBoy

        else if Bitwise.and Constants.bit4Mask filteredInterruptFlags == Constants.bit4Mask then
            -- Joypad Press
            performInterrupt 0x60 (Bitwise.xor Constants.bit4Mask cpu.interruptFlag) gameBoy

        else
            gameBoy

    else
        gameBoy


performInterrupt : Int -> Int -> Effect
performInterrupt interruptServiceRoutineAddress modifiedInterruptFlag gameBoy =
    gameBoy
        |> GameBoy.setCPU (CPU.setInterruptData False modifiedInterruptFlag False gameBoy.cpu)
        |> Opcode.push (readRegister16 PC)
        |> writeRegister16 PC interruptServiceRoutineAddress
