module Tests exposing (all, register16Test, register8Test, word16Fuzzer, word8Fuzzer)

import Data.Registers as Registers exposing (Register16(..), Register8(..), Registers)
import Data.Word16 as Word16 exposing (Word16)
import Data.Word8 as Word8 exposing (Word8)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ register8Test A
        , register8Test B
        , register8Test C
        , register8Test D
        , register8Test E
        , register8Test F
        , register8Test H
        , register8Test L
        , register16Test AF
        , register16Test BC
        , register16Test DE
        , register16Test HL
        , register16Test PC
        , register16Test SP
        ]


word8Fuzzer : Fuzzer Word8
word8Fuzzer =
    Fuzz.intRange 0x00 0xFF |> Fuzz.map Word8.fromInt


word16Fuzzer : Fuzzer Word16
word16Fuzzer =
    Fuzz.intRange 0x00 0xFFFF |> Fuzz.map Word16.fromInt


register8Test : Register8 -> Test
register8Test r8 =
    fuzz word8Fuzzer ("Register " ++ toString r8 ++ " should work") <|
        \value ->
            Registers.init
                |> Registers.writeRegister8 r8 value
                |> Registers.readRegister8 r8
                |> Expect.equal value


register16Test : Register16 -> Test
register16Test r16 =
    fuzz word16Fuzzer ("Register " ++ toString r16 ++ " should work") <|
        \value ->
            Registers.init
                |> Registers.writeRegister16 r16 value
                |> Registers.readRegister16 r16
                |> Expect.equal value
