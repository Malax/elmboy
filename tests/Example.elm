module Example exposing (suite)

import Component.CPU as CPU exposing (Register8(..))
import Component.CPU.FlagRegister as FlagRegister exposing (Flag(..))
import Component.CPU.OpcodeMapper as OpcodeMapper
import Component.Cartridge as Cartridge
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GameBoy exposing (GameBoy)
import Hex
import Test exposing (..)


suite : Test
suite =
    let
        gameBoy =
            GameBoy.init Cartridge.empty False
    in
    Test.concat
        [ checkInstructionTiming 0x00 (1 * 4) gameBoy
        , checkInstructionTiming 0x01 (3 * 4) gameBoy
        , checkInstructionTiming 0x02 (2 * 4) gameBoy
        , checkInstructionTiming 0x03 (2 * 4) gameBoy
        , checkInstructionTiming 0x04 (1 * 4) gameBoy
        , checkInstructionTiming 0x05 (1 * 4) gameBoy
        , checkInstructionTiming 0x06 (2 * 4) gameBoy
        , checkInstructionTiming 0x07 (1 * 4) gameBoy
        , checkInstructionTiming 0x08 (5 * 4) gameBoy
        , checkInstructionTiming 0x09 (2 * 4) gameBoy
        , checkInstructionTiming 0x0A (2 * 4) gameBoy
        , checkInstructionTiming 0x0B (2 * 4) gameBoy
        , checkInstructionTiming 0x0C (1 * 4) gameBoy
        , checkInstructionTiming 0x0D (1 * 4) gameBoy
        , checkInstructionTiming 0x0E (2 * 4) gameBoy
        , checkInstructionTiming 0x0F (1 * 4) gameBoy
        , checkInstructionTiming 0x11 (3 * 4) gameBoy
        , checkInstructionTiming 0x12 (2 * 4) gameBoy
        , checkInstructionTiming 0x13 (2 * 4) gameBoy
        , checkInstructionTiming 0x14 (1 * 4) gameBoy
        , checkInstructionTiming 0x15 (1 * 4) gameBoy
        , checkInstructionTiming 0x16 (2 * 4) gameBoy
        , checkInstructionTiming 0x17 (1 * 4) gameBoy
        , checkInstructionTiming 0x18 (3 * 4) gameBoy
        , checkInstructionTiming 0x19 (2 * 4) gameBoy
        , checkInstructionTiming 0x1A (2 * 4) gameBoy
        , checkInstructionTiming 0x1B (2 * 4) gameBoy
        , checkInstructionTiming 0x1C (1 * 4) gameBoy
        , checkInstructionTiming 0x1D (1 * 4) gameBoy
        , checkInstructionTiming 0x1E (2 * 4) gameBoy
        , checkInstructionTiming 0x1F (1 * 4) gameBoy
        , checkInstructionTiming 0x20 (2 * 4) gameBoy
        , checkInstructionTiming 0x21 (3 * 4) gameBoy
        , checkInstructionTiming 0x22 (2 * 4) gameBoy
        , checkInstructionTiming 0x23 (2 * 4) gameBoy
        , checkInstructionTiming 0x24 (1 * 4) gameBoy
        , checkInstructionTiming 0x25 (1 * 4) gameBoy
        , checkInstructionTiming 0x26 (2 * 4) gameBoy
        , checkInstructionTiming 0x27 (1 * 4) gameBoy
        , checkInstructionTimingFlagClear 0x28 Zero (2 * 4) gameBoy
        , checkInstructionTiming 0x29 (2 * 4) gameBoy
        , checkInstructionTiming 0x2A (2 * 4) gameBoy
        , checkInstructionTiming 0x2B (2 * 4) gameBoy
        , checkInstructionTiming 0x2C (1 * 4) gameBoy
        , checkInstructionTiming 0x2D (1 * 4) gameBoy
        , checkInstructionTiming 0x2E (2 * 4) gameBoy
        , checkInstructionTiming 0x2F (1 * 4) gameBoy
        , checkInstructionTiming 0x30 (2 * 4) gameBoy
        , checkInstructionTiming 0x31 (3 * 4) gameBoy
        , checkInstructionTiming 0x32 (2 * 4) gameBoy
        , checkInstructionTiming 0x33 (2 * 4) gameBoy
        , checkInstructionTiming 0x34 (3 * 4) gameBoy
        , checkInstructionTiming 0x35 (3 * 4) gameBoy
        , checkInstructionTiming 0x36 (3 * 4) gameBoy
        , checkInstructionTiming 0x37 (1 * 4) gameBoy
        , checkInstructionTimingFlagClear 0x38 Carry (2 * 4) gameBoy
        , checkInstructionTiming 0x39 (2 * 4) gameBoy
        , checkInstructionTiming 0x3A (2 * 4) gameBoy
        , checkInstructionTiming 0x3B (2 * 4) gameBoy
        , checkInstructionTiming 0x3C (1 * 4) gameBoy
        , checkInstructionTiming 0x3D (1 * 4) gameBoy
        , checkInstructionTiming 0x3E (2 * 4) gameBoy
        , checkInstructionTiming 0x3F (1 * 4) gameBoy
        , checkInstructionTiming 0x40 (1 * 4) gameBoy
        , checkInstructionTiming 0x41 (1 * 4) gameBoy
        , checkInstructionTiming 0x42 (1 * 4) gameBoy
        , checkInstructionTiming 0x43 (1 * 4) gameBoy
        , checkInstructionTiming 0x44 (1 * 4) gameBoy
        , checkInstructionTiming 0x45 (1 * 4) gameBoy
        , checkInstructionTiming 0x46 (2 * 4) gameBoy
        , checkInstructionTiming 0x47 (1 * 4) gameBoy
        , checkInstructionTiming 0x48 (1 * 4) gameBoy
        , checkInstructionTiming 0x49 (1 * 4) gameBoy
        , checkInstructionTiming 0x4A (1 * 4) gameBoy
        , checkInstructionTiming 0x4B (1 * 4) gameBoy
        , checkInstructionTiming 0x4C (1 * 4) gameBoy
        , checkInstructionTiming 0x4D (1 * 4) gameBoy
        , checkInstructionTiming 0x4E (2 * 4) gameBoy
        , checkInstructionTiming 0x4F (1 * 4) gameBoy
        , checkInstructionTiming 0x50 (1 * 4) gameBoy
        , checkInstructionTiming 0x51 (1 * 4) gameBoy
        , checkInstructionTiming 0x52 (1 * 4) gameBoy
        , checkInstructionTiming 0x53 (1 * 4) gameBoy
        , checkInstructionTiming 0x54 (1 * 4) gameBoy
        , checkInstructionTiming 0x55 (1 * 4) gameBoy
        , checkInstructionTiming 0x56 (2 * 4) gameBoy
        , checkInstructionTiming 0x57 (1 * 4) gameBoy
        , checkInstructionTiming 0x58 (1 * 4) gameBoy
        , checkInstructionTiming 0x59 (1 * 4) gameBoy
        , checkInstructionTiming 0x5A (1 * 4) gameBoy
        , checkInstructionTiming 0x5B (1 * 4) gameBoy
        , checkInstructionTiming 0x5C (1 * 4) gameBoy
        , checkInstructionTiming 0x5D (1 * 4) gameBoy
        , checkInstructionTiming 0x5E (2 * 4) gameBoy
        , checkInstructionTiming 0x5F (1 * 4) gameBoy
        , checkInstructionTiming 0x60 (1 * 4) gameBoy
        , checkInstructionTiming 0x61 (1 * 4) gameBoy
        , checkInstructionTiming 0x62 (1 * 4) gameBoy
        , checkInstructionTiming 0x63 (1 * 4) gameBoy
        , checkInstructionTiming 0x64 (1 * 4) gameBoy
        , checkInstructionTiming 0x65 (1 * 4) gameBoy
        , checkInstructionTiming 0x66 (2 * 4) gameBoy
        , checkInstructionTiming 0x67 (1 * 4) gameBoy
        , checkInstructionTiming 0x68 (1 * 4) gameBoy
        , checkInstructionTiming 0x69 (1 * 4) gameBoy
        , checkInstructionTiming 0x6A (1 * 4) gameBoy
        , checkInstructionTiming 0x6B (1 * 4) gameBoy
        , checkInstructionTiming 0x6C (1 * 4) gameBoy
        , checkInstructionTiming 0x6D (1 * 4) gameBoy
        , checkInstructionTiming 0x6E (2 * 4) gameBoy
        , checkInstructionTiming 0x6F (1 * 4) gameBoy
        , checkInstructionTiming 0x70 (2 * 4) gameBoy
        , checkInstructionTiming 0x71 (2 * 4) gameBoy
        , checkInstructionTiming 0x72 (2 * 4) gameBoy
        , checkInstructionTiming 0x73 (2 * 4) gameBoy
        , checkInstructionTiming 0x74 (2 * 4) gameBoy
        , checkInstructionTiming 0x75 (2 * 4) gameBoy
        , checkInstructionTiming 0x77 (2 * 4) gameBoy
        , checkInstructionTiming 0x78 (1 * 4) gameBoy
        , checkInstructionTiming 0x79 (1 * 4) gameBoy
        , checkInstructionTiming 0x7A (1 * 4) gameBoy
        , checkInstructionTiming 0x7B (1 * 4) gameBoy
        , checkInstructionTiming 0x7C (1 * 4) gameBoy
        , checkInstructionTiming 0x7D (1 * 4) gameBoy
        , checkInstructionTiming 0x7E (2 * 4) gameBoy
        , checkInstructionTiming 0x7F (1 * 4) gameBoy
        , checkInstructionTiming 0x80 (1 * 4) gameBoy
        , checkInstructionTiming 0x81 (1 * 4) gameBoy
        , checkInstructionTiming 0x82 (1 * 4) gameBoy
        , checkInstructionTiming 0x83 (1 * 4) gameBoy
        , checkInstructionTiming 0x84 (1 * 4) gameBoy
        , checkInstructionTiming 0x85 (1 * 4) gameBoy
        , checkInstructionTiming 0x86 (2 * 4) gameBoy
        , checkInstructionTiming 0x87 (1 * 4) gameBoy
        , checkInstructionTiming 0x88 (1 * 4) gameBoy
        , checkInstructionTiming 0x89 (1 * 4) gameBoy
        , checkInstructionTiming 0x8A (1 * 4) gameBoy
        , checkInstructionTiming 0x8B (1 * 4) gameBoy
        , checkInstructionTiming 0x8C (1 * 4) gameBoy
        , checkInstructionTiming 0x8D (1 * 4) gameBoy
        , checkInstructionTiming 0x8E (2 * 4) gameBoy
        , checkInstructionTiming 0x8F (1 * 4) gameBoy
        , checkInstructionTiming 0x90 (1 * 4) gameBoy
        , checkInstructionTiming 0x91 (1 * 4) gameBoy
        , checkInstructionTiming 0x92 (1 * 4) gameBoy
        , checkInstructionTiming 0x93 (1 * 4) gameBoy
        , checkInstructionTiming 0x94 (1 * 4) gameBoy
        , checkInstructionTiming 0x95 (1 * 4) gameBoy
        , checkInstructionTiming 0x96 (2 * 4) gameBoy
        , checkInstructionTiming 0x97 (1 * 4) gameBoy
        , checkInstructionTiming 0x98 (1 * 4) gameBoy
        , checkInstructionTiming 0x99 (1 * 4) gameBoy
        , checkInstructionTiming 0x9A (1 * 4) gameBoy
        , checkInstructionTiming 0x9B (1 * 4) gameBoy
        , checkInstructionTiming 0x9C (1 * 4) gameBoy
        , checkInstructionTiming 0x9D (1 * 4) gameBoy
        , checkInstructionTiming 0x9E (2 * 4) gameBoy
        , checkInstructionTiming 0x9F (1 * 4) gameBoy
        , checkInstructionTiming 0xA0 (1 * 4) gameBoy
        , checkInstructionTiming 0xA1 (1 * 4) gameBoy
        , checkInstructionTiming 0xA2 (1 * 4) gameBoy
        , checkInstructionTiming 0xA3 (1 * 4) gameBoy
        , checkInstructionTiming 0xA4 (1 * 4) gameBoy
        , checkInstructionTiming 0xA5 (1 * 4) gameBoy
        , checkInstructionTiming 0xA6 (2 * 4) gameBoy
        , checkInstructionTiming 0xA7 (1 * 4) gameBoy
        , checkInstructionTiming 0xA8 (1 * 4) gameBoy
        , checkInstructionTiming 0xA9 (1 * 4) gameBoy
        , checkInstructionTiming 0xAA (1 * 4) gameBoy
        , checkInstructionTiming 0xAB (1 * 4) gameBoy
        , checkInstructionTiming 0xAC (1 * 4) gameBoy
        , checkInstructionTiming 0xAD (1 * 4) gameBoy
        , checkInstructionTiming 0xAE (2 * 4) gameBoy
        , checkInstructionTiming 0xAF (1 * 4) gameBoy
        , checkInstructionTiming 0xB0 (1 * 4) gameBoy
        , checkInstructionTiming 0xB1 (1 * 4) gameBoy
        , checkInstructionTiming 0xB2 (1 * 4) gameBoy
        , checkInstructionTiming 0xB3 (1 * 4) gameBoy
        , checkInstructionTiming 0xB4 (1 * 4) gameBoy
        , checkInstructionTiming 0xB5 (1 * 4) gameBoy
        , checkInstructionTiming 0xB6 (2 * 4) gameBoy
        , checkInstructionTiming 0xB7 (1 * 4) gameBoy
        , checkInstructionTiming 0xB8 (1 * 4) gameBoy
        , checkInstructionTiming 0xB9 (1 * 4) gameBoy
        , checkInstructionTiming 0xBA (1 * 4) gameBoy
        , checkInstructionTiming 0xBB (1 * 4) gameBoy
        , checkInstructionTiming 0xBC (1 * 4) gameBoy
        , checkInstructionTiming 0xBD (1 * 4) gameBoy
        , checkInstructionTiming 0xBE (2 * 4) gameBoy
        , checkInstructionTiming 0xBF (1 * 4) gameBoy
        , checkInstructionTiming 0xC0 (2 * 4) gameBoy
        , checkInstructionTiming 0xC1 (3 * 4) gameBoy
        , checkInstructionTiming 0xC2 (3 * 4) gameBoy
        , checkInstructionTiming 0xC3 (4 * 4) gameBoy
        , checkInstructionTiming 0xC4 (3 * 4) gameBoy
        , checkInstructionTiming 0xC5 (4 * 4) gameBoy
        , checkInstructionTiming 0xC6 (2 * 4) gameBoy
        , checkInstructionTiming 0xC7 (4 * 4) gameBoy
        , checkInstructionTimingFlagClear 0xC8 Zero (2 * 4) gameBoy
        , checkInstructionTiming 0xC9 (4 * 4) gameBoy
        , checkInstructionTimingFlagClear 0xCA Zero (3 * 4) gameBoy
        , checkInstructionTimingFlagClear 0xCC Zero (3 * 4) gameBoy
        , checkInstructionTiming 0xCD (6 * 4) gameBoy
        , checkInstructionTiming 0xCE (2 * 4) gameBoy
        , checkInstructionTiming 0xCF (4 * 4) gameBoy
        , checkInstructionTiming 0xD0 (2 * 4) gameBoy
        , checkInstructionTiming 0xD1 (3 * 4) gameBoy
        , checkInstructionTiming 0xD2 (3 * 4) gameBoy
        , checkInstructionTiming 0xD4 (3 * 4) gameBoy
        , checkInstructionTiming 0xD5 (4 * 4) gameBoy
        , checkInstructionTiming 0xD6 (2 * 4) gameBoy
        , checkInstructionTiming 0xD7 (4 * 4) gameBoy
        , checkInstructionTimingFlagClear 0xD8 Carry (2 * 4) gameBoy
        , checkInstructionTiming 0xD9 (4 * 4) gameBoy
        , checkInstructionTimingFlagClear 0xDA Carry (3 * 4) gameBoy
        , checkInstructionTimingFlagClear 0xDC Carry (3 * 4) gameBoy
        , checkInstructionTiming 0xDE (2 * 4) gameBoy
        , checkInstructionTiming 0xDF (4 * 4) gameBoy
        , checkInstructionTiming 0xE0 (3 * 4) gameBoy
        , checkInstructionTiming 0xE1 (3 * 4) gameBoy
        , checkInstructionTiming 0xE2 (2 * 4) gameBoy
        , checkInstructionTiming 0xE5 (4 * 4) gameBoy
        , checkInstructionTiming 0xE6 (2 * 4) gameBoy
        , checkInstructionTiming 0xE7 (4 * 4) gameBoy
        , checkInstructionTiming 0xE8 (4 * 4) gameBoy
        , checkInstructionTiming 0xE9 (1 * 4) gameBoy
        , checkInstructionTiming 0xEA (4 * 4) gameBoy
        , checkInstructionTiming 0xEE (2 * 4) gameBoy
        , checkInstructionTiming 0xEF (4 * 4) gameBoy
        , checkInstructionTiming 0xF0 (3 * 4) gameBoy
        , checkInstructionTiming 0xF1 (3 * 4) gameBoy
        , checkInstructionTiming 0xF2 (2 * 4) gameBoy
        , checkInstructionTiming 0xF3 (1 * 4) gameBoy
        , checkInstructionTiming 0xF5 (4 * 4) gameBoy
        , checkInstructionTiming 0xF6 (2 * 4) gameBoy
        , checkInstructionTiming 0xF7 (4 * 4) gameBoy
        , checkInstructionTiming 0xF8 (3 * 4) gameBoy
        , checkInstructionTiming 0xF9 (2 * 4) gameBoy
        , checkInstructionTiming 0xFA (4 * 4) gameBoy
        , checkInstructionTiming 0xFB (1 * 4) gameBoy
        , checkInstructionTiming 0xFE (2 * 4) gameBoy
        , checkInstructionTiming 0xFF (4 * 4) gameBoy
        ]


checkInstructionTiming : Int -> Int -> GameBoy -> Test
checkInstructionTiming opcode cycles gameBoy =
    let
        gameBoyAfterExecuting =
            OpcodeMapper.get opcode gameBoy

        actualCycles =
            4 + (gameBoyAfterExecuting.lastInstructionCycles - gameBoy.lastInstructionCycles)

        message =
            "Expected " ++ String.fromInt cycles ++ " cycles, instruction ran for " ++ String.fromInt actualCycles
    in
    test ("0x" ++ Hex.toString opcode) (\_ -> Expect.true message (actualCycles == cycles))


checkInstructionTimingFlagSet : Int -> Flag -> Int -> GameBoy -> Test
checkInstructionTimingFlagSet opcode flag cycles gameBoy =
    let
        updatedCPU =
            CPU.writeRegister8 F (FlagRegister.setFlag flag True (CPU.readRegister8 F gameBoy.cpu)) gameBoy.cpu

        updatedGameBoy =
            GameBoy.setCPU updatedCPU gameBoy
    in
    checkInstructionTiming opcode cycles updatedGameBoy


checkInstructionTimingFlagClear : Int -> Flag -> Int -> GameBoy -> Test
checkInstructionTimingFlagClear opcode flag cycles gameBoy =
    let
        updatedCPU =
            CPU.writeRegister8 F (FlagRegister.setFlag flag False (CPU.readRegister8 F gameBoy.cpu)) gameBoy.cpu

        updatedGameBoy =
            GameBoy.setCPU updatedCPU gameBoy
    in
    checkInstructionTiming opcode cycles updatedGameBoy
