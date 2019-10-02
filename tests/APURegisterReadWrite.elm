module APURegisterReadWrite exposing (suite)

import Bitwise
import Component.CPU as CPU
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
        [ checkAPURegisterReadWrite 0xFF10 0x80 gameBoy
        , checkAPURegisterReadWrite 0xFF11 0x3F gameBoy
        , checkAPURegisterReadWrite 0xFF12 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF13 0xFF gameBoy
        , checkAPURegisterReadWrite 0xFF14 0xBF gameBoy
        , checkAPURegisterReadWrite 0xFF15 0xFF gameBoy
        , checkAPURegisterReadWrite 0xFF16 0x3F gameBoy
        , checkAPURegisterReadWrite 0xFF17 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF18 0xFF gameBoy
        , checkAPURegisterReadWrite 0xFF19 0xBF gameBoy
        , checkAPURegisterReadWrite 0xFF1A 0x7F gameBoy
        , checkAPURegisterReadWrite 0xFF1B 0xFF gameBoy
        , checkAPURegisterReadWrite 0xFF1C 0x9F gameBoy
        , checkAPURegisterReadWrite 0xFF1D 0xFF gameBoy
        , checkAPURegisterReadWrite 0xFF1E 0xBF gameBoy
        , checkAPURegisterReadWrite 0xFF1F 0xFF gameBoy
        , checkAPURegisterReadWrite 0xFF20 0xFF gameBoy
        , checkAPURegisterReadWrite 0xFF21 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF22 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF23 0xBF gameBoy
        , checkAPURegisterReadWrite 0xFF24 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF25 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF30 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF31 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF32 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF33 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF34 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF35 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF36 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF37 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF38 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF39 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF3A 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF3B 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF3C 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF3D 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF3E 0x00 gameBoy
        , checkAPURegisterReadWrite 0xFF3F 0x00 gameBoy
        ]


checkAPURegisterReadWrite : Int -> Int -> GameBoy -> Test
checkAPURegisterReadWrite address readOrMask gameBoy =
    fuzz word8Fuzzer ("APU Register R/W 0x" ++ Hex.toString address) (\value -> checkMemoryWriteRead address value (Bitwise.or readOrMask) gameBoy)


checkMemoryWriteRead : Int -> Int -> (Int -> Int) -> GameBoy -> Expectation
checkMemoryWriteRead address value expectedValueF gameBoy =
    let
        gameBoyAfterWrite =
            MMU.writeWord8 address value gameBoy
    in
    Expect.equal (MMU.readWord8 gameBoyAfterWrite address) (expectedValueF value)


word8Fuzzer : Fuzzer Int
word8Fuzzer =
    Fuzz.intRange 0x00 0xFF
