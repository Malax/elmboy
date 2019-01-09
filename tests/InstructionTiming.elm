module InstructionTiming exposing (suite)

import Bitwise
import Component.CPU as CPU exposing (Register8(..))
import Component.CPU.FlagRegister as FlagRegister exposing (Flag(..))
import Component.CPU.OpcodeMapper as OpcodeMapper
import Component.Cartridge as Cartridge
import Component.MMU as MMU
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
        [ checkInstructionTiming 0x00 4 gameBoy
        , checkInstructionTiming 0x01 12 gameBoy
        , checkInstructionTiming 0x02 8 gameBoy
        , checkInstructionTiming 0x03 8 gameBoy
        , checkInstructionTiming 0x04 4 gameBoy
        , checkInstructionTiming 0x05 4 gameBoy
        , checkInstructionTiming 0x06 8 gameBoy
        , checkInstructionTiming 0x07 4 gameBoy
        , checkInstructionTiming 0x08 20 gameBoy
        , checkInstructionTiming 0x09 8 gameBoy
        , checkInstructionTiming 0x0A 8 gameBoy
        , checkInstructionTiming 0x0B 8 gameBoy
        , checkInstructionTiming 0x0C 4 gameBoy
        , checkInstructionTiming 0x0D 4 gameBoy
        , checkInstructionTiming 0x0E 8 gameBoy
        , checkInstructionTiming 0x0F 4 gameBoy
        , checkInstructionTiming 0x11 12 gameBoy
        , checkInstructionTiming 0x12 8 gameBoy
        , checkInstructionTiming 0x13 8 gameBoy
        , checkInstructionTiming 0x14 4 gameBoy
        , checkInstructionTiming 0x15 4 gameBoy
        , checkInstructionTiming 0x16 8 gameBoy
        , checkInstructionTiming 0x17 4 gameBoy
        , checkInstructionTiming 0x18 12 gameBoy
        , checkInstructionTiming 0x19 8 gameBoy
        , checkInstructionTiming 0x1A 8 gameBoy
        , checkInstructionTiming 0x1B 8 gameBoy
        , checkInstructionTiming 0x1C 4 gameBoy
        , checkInstructionTiming 0x1D 4 gameBoy
        , checkInstructionTiming 0x1E 8 gameBoy
        , checkInstructionTiming 0x1F 4 gameBoy
        , checkInstructionTiming 0x20 8 gameBoy
        , checkInstructionTiming 0x21 12 gameBoy
        , checkInstructionTiming 0x22 8 gameBoy
        , checkInstructionTiming 0x23 8 gameBoy
        , checkInstructionTiming 0x24 4 gameBoy
        , checkInstructionTiming 0x25 4 gameBoy
        , checkInstructionTiming 0x26 8 gameBoy
        , checkInstructionTiming 0x27 4 gameBoy
        , checkInstructionTimingWithFlag 0x28 ( Zero, False ) 8 gameBoy
        , checkInstructionTimingWithFlag 0x28 ( Zero, True ) 12 gameBoy
        , checkInstructionTiming 0x29 8 gameBoy
        , checkInstructionTiming 0x2A 8 gameBoy
        , checkInstructionTiming 0x2B 8 gameBoy
        , checkInstructionTiming 0x2C 4 gameBoy
        , checkInstructionTiming 0x2D 4 gameBoy
        , checkInstructionTiming 0x2E 8 gameBoy
        , checkInstructionTiming 0x2F 4 gameBoy
        , checkInstructionTiming 0x30 8 gameBoy
        , checkInstructionTiming 0x31 12 gameBoy
        , checkInstructionTiming 0x32 8 gameBoy
        , checkInstructionTiming 0x33 8 gameBoy
        , checkInstructionTiming 0x34 12 gameBoy
        , checkInstructionTiming 0x35 12 gameBoy
        , checkInstructionTiming 0x36 12 gameBoy
        , checkInstructionTiming 0x37 4 gameBoy
        , checkInstructionTimingWithFlag 0x38 ( Carry, False ) 8 gameBoy
        , checkInstructionTimingWithFlag 0x38 ( Carry, True ) 12 gameBoy
        , checkInstructionTiming 0x39 8 gameBoy
        , checkInstructionTiming 0x3A 8 gameBoy
        , checkInstructionTiming 0x3B 8 gameBoy
        , checkInstructionTiming 0x3C 4 gameBoy
        , checkInstructionTiming 0x3D 4 gameBoy
        , checkInstructionTiming 0x3E 8 gameBoy
        , checkInstructionTiming 0x3F 4 gameBoy
        , checkInstructionTiming 0x40 4 gameBoy
        , checkInstructionTiming 0x41 4 gameBoy
        , checkInstructionTiming 0x42 4 gameBoy
        , checkInstructionTiming 0x43 4 gameBoy
        , checkInstructionTiming 0x44 4 gameBoy
        , checkInstructionTiming 0x45 4 gameBoy
        , checkInstructionTiming 0x46 8 gameBoy
        , checkInstructionTiming 0x47 4 gameBoy
        , checkInstructionTiming 0x48 4 gameBoy
        , checkInstructionTiming 0x49 4 gameBoy
        , checkInstructionTiming 0x4A 4 gameBoy
        , checkInstructionTiming 0x4B 4 gameBoy
        , checkInstructionTiming 0x4C 4 gameBoy
        , checkInstructionTiming 0x4D 4 gameBoy
        , checkInstructionTiming 0x4E 8 gameBoy
        , checkInstructionTiming 0x4F 4 gameBoy
        , checkInstructionTiming 0x50 4 gameBoy
        , checkInstructionTiming 0x51 4 gameBoy
        , checkInstructionTiming 0x52 4 gameBoy
        , checkInstructionTiming 0x53 4 gameBoy
        , checkInstructionTiming 0x54 4 gameBoy
        , checkInstructionTiming 0x55 4 gameBoy
        , checkInstructionTiming 0x56 8 gameBoy
        , checkInstructionTiming 0x57 4 gameBoy
        , checkInstructionTiming 0x58 4 gameBoy
        , checkInstructionTiming 0x59 4 gameBoy
        , checkInstructionTiming 0x5A 4 gameBoy
        , checkInstructionTiming 0x5B 4 gameBoy
        , checkInstructionTiming 0x5C 4 gameBoy
        , checkInstructionTiming 0x5D 4 gameBoy
        , checkInstructionTiming 0x5E 8 gameBoy
        , checkInstructionTiming 0x5F 4 gameBoy
        , checkInstructionTiming 0x60 4 gameBoy
        , checkInstructionTiming 0x61 4 gameBoy
        , checkInstructionTiming 0x62 4 gameBoy
        , checkInstructionTiming 0x63 4 gameBoy
        , checkInstructionTiming 0x64 4 gameBoy
        , checkInstructionTiming 0x65 4 gameBoy
        , checkInstructionTiming 0x66 8 gameBoy
        , checkInstructionTiming 0x67 4 gameBoy
        , checkInstructionTiming 0x68 4 gameBoy
        , checkInstructionTiming 0x69 4 gameBoy
        , checkInstructionTiming 0x6A 4 gameBoy
        , checkInstructionTiming 0x6B 4 gameBoy
        , checkInstructionTiming 0x6C 4 gameBoy
        , checkInstructionTiming 0x6D 4 gameBoy
        , checkInstructionTiming 0x6E 8 gameBoy
        , checkInstructionTiming 0x6F 4 gameBoy
        , checkInstructionTiming 0x70 8 gameBoy
        , checkInstructionTiming 0x71 8 gameBoy
        , checkInstructionTiming 0x72 8 gameBoy
        , checkInstructionTiming 0x73 8 gameBoy
        , checkInstructionTiming 0x74 8 gameBoy
        , checkInstructionTiming 0x75 8 gameBoy
        , checkInstructionTiming 0x77 8 gameBoy
        , checkInstructionTiming 0x78 4 gameBoy
        , checkInstructionTiming 0x79 4 gameBoy
        , checkInstructionTiming 0x7A 4 gameBoy
        , checkInstructionTiming 0x7B 4 gameBoy
        , checkInstructionTiming 0x7C 4 gameBoy
        , checkInstructionTiming 0x7D 4 gameBoy
        , checkInstructionTiming 0x7E 8 gameBoy
        , checkInstructionTiming 0x7F 4 gameBoy
        , checkInstructionTiming 0x80 4 gameBoy
        , checkInstructionTiming 0x81 4 gameBoy
        , checkInstructionTiming 0x82 4 gameBoy
        , checkInstructionTiming 0x83 4 gameBoy
        , checkInstructionTiming 0x84 4 gameBoy
        , checkInstructionTiming 0x85 4 gameBoy
        , checkInstructionTiming 0x86 8 gameBoy
        , checkInstructionTiming 0x87 4 gameBoy
        , checkInstructionTiming 0x88 4 gameBoy
        , checkInstructionTiming 0x89 4 gameBoy
        , checkInstructionTiming 0x8A 4 gameBoy
        , checkInstructionTiming 0x8B 4 gameBoy
        , checkInstructionTiming 0x8C 4 gameBoy
        , checkInstructionTiming 0x8D 4 gameBoy
        , checkInstructionTiming 0x8E 8 gameBoy
        , checkInstructionTiming 0x8F 4 gameBoy
        , checkInstructionTiming 0x90 4 gameBoy
        , checkInstructionTiming 0x91 4 gameBoy
        , checkInstructionTiming 0x92 4 gameBoy
        , checkInstructionTiming 0x93 4 gameBoy
        , checkInstructionTiming 0x94 4 gameBoy
        , checkInstructionTiming 0x95 4 gameBoy
        , checkInstructionTiming 0x96 8 gameBoy
        , checkInstructionTiming 0x97 4 gameBoy
        , checkInstructionTiming 0x98 4 gameBoy
        , checkInstructionTiming 0x99 4 gameBoy
        , checkInstructionTiming 0x9A 4 gameBoy
        , checkInstructionTiming 0x9B 4 gameBoy
        , checkInstructionTiming 0x9C 4 gameBoy
        , checkInstructionTiming 0x9D 4 gameBoy
        , checkInstructionTiming 0x9E 8 gameBoy
        , checkInstructionTiming 0x9F 4 gameBoy
        , checkInstructionTiming 0xA0 4 gameBoy
        , checkInstructionTiming 0xA1 4 gameBoy
        , checkInstructionTiming 0xA2 4 gameBoy
        , checkInstructionTiming 0xA3 4 gameBoy
        , checkInstructionTiming 0xA4 4 gameBoy
        , checkInstructionTiming 0xA5 4 gameBoy
        , checkInstructionTiming 0xA6 8 gameBoy
        , checkInstructionTiming 0xA7 4 gameBoy
        , checkInstructionTiming 0xA8 4 gameBoy
        , checkInstructionTiming 0xA9 4 gameBoy
        , checkInstructionTiming 0xAA 4 gameBoy
        , checkInstructionTiming 0xAB 4 gameBoy
        , checkInstructionTiming 0xAC 4 gameBoy
        , checkInstructionTiming 0xAD 4 gameBoy
        , checkInstructionTiming 0xAE 8 gameBoy
        , checkInstructionTiming 0xAF 4 gameBoy
        , checkInstructionTiming 0xB0 4 gameBoy
        , checkInstructionTiming 0xB1 4 gameBoy
        , checkInstructionTiming 0xB2 4 gameBoy
        , checkInstructionTiming 0xB3 4 gameBoy
        , checkInstructionTiming 0xB4 4 gameBoy
        , checkInstructionTiming 0xB5 4 gameBoy
        , checkInstructionTiming 0xB6 8 gameBoy
        , checkInstructionTiming 0xB7 4 gameBoy
        , checkInstructionTiming 0xB8 4 gameBoy
        , checkInstructionTiming 0xB9 4 gameBoy
        , checkInstructionTiming 0xBA 4 gameBoy
        , checkInstructionTiming 0xBB 4 gameBoy
        , checkInstructionTiming 0xBC 4 gameBoy
        , checkInstructionTiming 0xBD 4 gameBoy
        , checkInstructionTiming 0xBE 8 gameBoy
        , checkInstructionTiming 0xBF 4 gameBoy
        , checkInstructionTiming 0xC0 8 gameBoy
        , checkInstructionTiming 0xC1 12 gameBoy
        , checkInstructionTiming 0xC2 12 gameBoy
        , checkInstructionTiming 0xC3 16 gameBoy
        , checkInstructionTiming 0xC4 12 gameBoy
        , checkInstructionTiming 0xC5 16 gameBoy
        , checkInstructionTiming 0xC6 8 gameBoy
        , checkInstructionTiming 0xC7 16 gameBoy
        , checkInstructionTimingWithFlag 0xC8 ( Zero, False ) 8 gameBoy
        , checkInstructionTimingWithFlag 0xC8 ( Zero, True ) 20 gameBoy
        , checkInstructionTiming 0xC9 16 gameBoy
        , checkInstructionTimingWithFlag 0xCA ( Zero, False ) 12 gameBoy
        , checkInstructionTimingWithFlag 0xCA ( Zero, True ) 16 gameBoy
        , checkInstructionTimingWithFlag 0xCC ( Zero, False ) 12 gameBoy
        , checkInstructionTimingWithFlag 0xCC ( Zero, True ) 24 gameBoy
        , checkInstructionTiming 0xCD 24 gameBoy
        , checkInstructionTiming 0xCE 8 gameBoy
        , checkInstructionTiming 0xCF 16 gameBoy
        , checkInstructionTiming 0xD0 8 gameBoy
        , checkInstructionTiming 0xD1 12 gameBoy
        , checkInstructionTiming 0xD2 12 gameBoy
        , checkInstructionTiming 0xD4 12 gameBoy
        , checkInstructionTiming 0xD5 16 gameBoy
        , checkInstructionTiming 0xD6 8 gameBoy
        , checkInstructionTiming 0xD7 16 gameBoy
        , checkInstructionTimingWithFlag 0xD8 ( Carry, False ) 8 gameBoy
        , checkInstructionTimingWithFlag 0xD8 ( Carry, True ) 20 gameBoy
        , checkInstructionTiming 0xD9 16 gameBoy
        , checkInstructionTimingWithFlag 0xDA ( Carry, False ) 12 gameBoy
        , checkInstructionTimingWithFlag 0xDA ( Carry, True ) 16 gameBoy
        , checkInstructionTimingWithFlag 0xDC ( Carry, False ) 12 gameBoy
        , checkInstructionTimingWithFlag 0xDC ( Carry, True ) 24 gameBoy
        , checkInstructionTiming 0xDE 8 gameBoy
        , checkInstructionTiming 0xDF 16 gameBoy
        , checkInstructionTiming 0xE0 12 gameBoy
        , checkInstructionTiming 0xE1 12 gameBoy
        , checkInstructionTiming 0xE2 8 gameBoy
        , checkInstructionTiming 0xE5 16 gameBoy
        , checkInstructionTiming 0xE6 8 gameBoy
        , checkInstructionTiming 0xE7 16 gameBoy
        , checkInstructionTiming 0xE8 16 gameBoy
        , checkInstructionTiming 0xE9 4 gameBoy
        , checkInstructionTiming 0xEA 16 gameBoy
        , checkInstructionTiming 0xEE 8 gameBoy
        , checkInstructionTiming 0xEF 16 gameBoy
        , checkInstructionTiming 0xF0 12 gameBoy
        , checkInstructionTiming 0xF1 12 gameBoy
        , checkInstructionTiming 0xF2 8 gameBoy
        , checkInstructionTiming 0xF3 4 gameBoy
        , checkInstructionTiming 0xF5 16 gameBoy
        , checkInstructionTiming 0xF6 8 gameBoy
        , checkInstructionTiming 0xF7 16 gameBoy
        , checkInstructionTiming 0xF8 12 gameBoy
        , checkInstructionTiming 0xF9 8 gameBoy
        , checkInstructionTiming 0xFA 16 gameBoy
        , checkInstructionTiming 0xFB 4 gameBoy
        , checkInstructionTiming 0xFE 8 gameBoy
        , checkInstructionTiming 0xFF 16 gameBoy
        ]


checkInstructionTiming : Int -> Int -> GameBoy -> Test
checkInstructionTiming opcode =
    namedCheckInstructionTiming ("0x" ++ Hex.toString opcode) opcode


checkInstructionTimingWithFlag : Int -> ( Flag, Bool ) -> Int -> GameBoy -> Test
checkInstructionTimingWithFlag opcode ( flag, flagEnabled ) expectedCycles gameBoy =
    let
        updatedCPU =
            CPU.writeRegister8 F (FlagRegister.setFlag flag flagEnabled (CPU.readRegister8 F gameBoy.cpu)) gameBoy.cpu

        updatedGameBoy =
            GameBoy.setCPU updatedCPU gameBoy

        nameSuffix =
            if flagEnabled then
                "(Flag " ++ FlagRegister.toString flag ++ " explicitly set)"

            else
                "(Flag " ++ FlagRegister.toString flag ++ " explicitly clear)"
    in
    namedCheckInstructionTiming ("0x" ++ Hex.toString opcode ++ " " ++ nameSuffix) opcode expectedCycles updatedGameBoy


namedCheckInstructionTiming : String -> Int -> Int -> GameBoy -> Test
namedCheckInstructionTiming testName opcode expectedCycles gameBoy =
    let
        gameBoyAfterExecuting =
            OpcodeMapper.get opcode gameBoy

        actualCycles =
            4 + (gameBoyAfterExecuting.lastInstructionCycles - gameBoy.lastInstructionCycles)

        message =
            "Expected " ++ String.fromInt expectedCycles ++ " cycles, but instruction ran for " ++ String.fromInt actualCycles
    in
    test testName (\_ -> Expect.true message (actualCycles == expectedCycles))
