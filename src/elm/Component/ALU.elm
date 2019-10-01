module Component.ALU exposing
    ( adc
    , add
    , add16
    , add16Signed8
    , and
    , bit
    , cpl
    , daa
    , dec
    , dec16
    , inc
    , inc16
    , or
    , res
    , rl
    , rla
    , rlc
    , rlca
    , rr
    , rra
    , rrc
    , rrca
    , sbc
    , set
    , sla
    , sra
    , srl
    , sub
    , swap
    , xor
    )

import Bitwise
import Component.CPU.FlagRegister as Flag exposing (Flag(..), FlagDelta(..), FlagsRegisterDelta)
import Util


type alias ALUResultWithFlagModification =
    ( Int, FlagsRegisterDelta )


type alias ALUResultWithFlags =
    ( Int, Flags )


type alias ALUResult =
    Int


type alias Flags =
    Int


swap : Int -> ALUResultWithFlags
swap value =
    let
        result =
            Bitwise.shiftLeftBy 4 (Bitwise.and 0x0F value) + Bitwise.shiftRightZfBy 4 (Bitwise.and 0xF0 value)

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0x00
    in
    ( result, Flag.setAllFlags zeroFlag False False False )


adc : Int -> Int -> Flags -> ALUResultWithFlags
adc operand1 operand2 flags =
    let
        carry =
            if Flag.getFlag Carry flags then
                0x01

            else
                0x00

        result =
            operand1 + operand2 + carry

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        halfCarryFlag =
            Bitwise.and 0x0F operand1 + Bitwise.and 0x0F operand2 + carry > 0x0F

        carryFlag =
            result > 0xFF
    in
    ( maskedResult, Flag.setAllFlags zeroFlag False halfCarryFlag carryFlag )


sbc : Int -> Int -> Flags -> ALUResultWithFlags
sbc operand1 operand2 flags =
    let
        carry =
            if Flag.getFlag Carry flags then
                0x01

            else
                0x00

        result =
            operand1 - operand2 - carry

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        halfCarryFlag =
            Bitwise.and
                (Bitwise.xor (Bitwise.xor operand1 operand2) maskedResult)
                (Bitwise.shiftLeftBy 4 1)
                /= 0x00

        carryFlag =
            result < 0x00
    in
    ( maskedResult, Flag.setAllFlags zeroFlag True halfCarryFlag carryFlag )


add : Int -> Int -> ALUResultWithFlags
add operand1 operand2 =
    let
        result =
            operand1 + operand2

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        carryFlag =
            result > 0xFF

        halfCarryFlag =
            Bitwise.and 0x0F operand1 + Bitwise.and 0x0F operand2 > 0x0F
    in
    ( maskedResult, Flag.setAllFlags zeroFlag False halfCarryFlag carryFlag )


sub : Int -> Int -> ALUResultWithFlags
sub operand1 operand2 =
    let
        result =
            operand1 - operand2

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        carryFlag =
            operand1 < operand2

        halfCarryFlag =
            Bitwise.and 0x0F operand1 < Bitwise.and 0x0F operand2
    in
    ( maskedResult, Flag.setAllFlags zeroFlag True halfCarryFlag carryFlag )


inc : Int -> ALUResultWithFlagModification
inc operand =
    let
        result =
            operand + 1

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        halfCarryFlag =
            Bitwise.and 0x0F operand + 1 > 0x0F
    in
    ( maskedResult, Flag.modifyFlags (StaticValue zeroFlag) (StaticValue False) (StaticValue halfCarryFlag) Unchanged )


dec : Int -> ALUResultWithFlagModification
dec operand =
    let
        result =
            operand - 1

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        halfCarryFlag =
            Bitwise.and 0x0F operand == 0x00
    in
    ( maskedResult, Flag.modifyFlags (StaticValue zeroFlag) (StaticValue True) (StaticValue halfCarryFlag) Unchanged )


and : Int -> Int -> ALUResultWithFlags
and operand1 operand2 =
    let
        result =
            Bitwise.and operand1 operand2

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0
    in
    ( maskedResult, Flag.setAllFlags zeroFlag False True False )


or : Int -> Int -> ALUResultWithFlags
or operand1 operand2 =
    let
        result =
            Bitwise.or operand1 operand2

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0
    in
    ( maskedResult, Flag.setAllFlags zeroFlag False False False )


xor : Int -> Int -> ALUResultWithFlags
xor operand1 operand2 =
    let
        result =
            Bitwise.xor operand1 operand2

        maskedResult =
            Bitwise.and 0xFF result

        zeroFlag =
            maskedResult == 0
    in
    ( maskedResult, Flag.setAllFlags zeroFlag False False False )


daa : Int -> Flags -> ALUResultWithFlagModification
daa operand1 flags =
    let
        subtractHalfCarry value =
            if Flag.getFlag HalfCarry flags then
                Bitwise.and (value - 0x06) 0xFF

            else
                value

        subtractCarry value =
            if Flag.getFlag Carry flags then
                Bitwise.and (value - 0x60) 0xFF

            else
                value

        additionHalfCarry value =
            if Flag.getFlag HalfCarry flags || Bitwise.and value 0x0F > 9 then
                value + 0x06

            else
                value

        additionCarry value =
            if Flag.getFlag Carry flags || value > 0x9F then
                value + 0x60

            else
                value

        result =
            if Flag.getFlag Subtract flags then
                operand1 |> subtractHalfCarry |> subtractCarry

            else
                operand1 |> additionHalfCarry |> additionCarry

        maskedResult =
            Bitwise.and 0xFF result

        {- Contrary to other opcodes, this flag is not set with the result of
           the carry, but only if the result is acutally a carry. That means if
           the carry flag was set prior to this opcode Bitwise.and the carry of this opcode
           is "no carry", the flag stays enabled.
        -}
        carryFlagDelta =
            if result > 0xFF then
                StaticValue True

            else
                Unchanged

        zeroFlag =
            maskedResult == 0x00
    in
    ( maskedResult, Flag.modifyFlags (StaticValue zeroFlag) Unchanged (StaticValue False) carryFlagDelta )


add16 : Int -> Int -> ALUResultWithFlagModification
add16 operand1 operand2 =
    let
        result =
            operand1 + operand2

        maskedResult =
            Bitwise.and 0xFFFF result

        carryFlag =
            result > 0xFFFF

        halfCarryFlag =
            Bitwise.and 0x0FFF operand1 + Bitwise.and 0x0FFF operand2 > 0x0FFF
    in
    ( maskedResult, Flag.modifyFlags Unchanged (StaticValue False) (StaticValue halfCarryFlag) (StaticValue carryFlag) )


add16Signed8 : Int -> Int -> ALUResultWithFlags
add16Signed8 operand1 operand2 =
    let
        operandSigned =
            Util.byteToSignedInt operand2

        result =
            operand1 + operandSigned

        maskedResult =
            Bitwise.and 0xFFFF result

        carryFlag =
            Bitwise.and 0xFF operand1 + Bitwise.and 0xFF operand2 > 0xFF

        halfCarryFlag =
            Bitwise.and 0x0F operand1 + Bitwise.and 0x0F operand2 > 0x0F
    in
    ( maskedResult, Flag.setAllFlags False False halfCarryFlag carryFlag )


inc16 : Int -> ALUResult
inc16 operand =
    Bitwise.and (operand + 1) 0xFFFF


dec16 : Int -> ALUResult
dec16 operand =
    Bitwise.and (operand - 1) 0xFFFF


bit : Int -> Int -> FlagsRegisterDelta
bit index operand =
    let
        bitValue =
            Bitwise.and operand (Bitwise.shiftLeftBy index 0x01) > 0
    in
    Flag.modifyFlags (StaticValue (not bitValue)) (StaticValue False) (StaticValue True) Unchanged


set : Int -> Int -> ALUResult
set index operand =
    operand
        |> Bitwise.or (Bitwise.shiftLeftBy index 0x01)
        |> Bitwise.and 0xFF


res : Int -> Int -> ALUResult
res index operand =
    operand
        |> Bitwise.and (Bitwise.shiftLeftBy index 0x01 |> Bitwise.complement)
        |> Bitwise.and 0xFF


cpl : Int -> ALUResultWithFlagModification
cpl operand =
    let
        result =
            Bitwise.complement operand

        maskedResult =
            Bitwise.and 0xFF result
    in
    ( maskedResult, Flag.modifyFlags Unchanged (StaticValue True) (StaticValue True) Unchanged )


rlc : Int -> ALUResultWithFlags
rlc operand =
    let
        rotatedValue =
            Bitwise.shiftRightZfBy 7 (Bitwise.and operand 0x80)

        result =
            Bitwise.shiftLeftBy 1 operand
                |> Bitwise.or rotatedValue
                |> Bitwise.and 0xFF

        carryFlag =
            rotatedValue > 0

        zeroFlag =
            result == 0x00
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


rrc : Int -> ALUResultWithFlags
rrc operand =
    let
        rotatedValue =
            Bitwise.shiftLeftBy 7 (Bitwise.and operand 0x01)

        result =
            Bitwise.shiftRightZfBy 1 operand
                |> Bitwise.or rotatedValue
                |> Bitwise.and 0xFF

        carryFlag =
            rotatedValue > 0

        zeroFlag =
            result == 0x00
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


rl : Int -> Flags -> ALUResultWithFlags
rl operand flags =
    let
        carry =
            if Flag.getFlag Carry flags then
                0x01

            else
                0x00

        result =
            Bitwise.shiftLeftBy 1 operand
                |> Bitwise.or carry
                |> Bitwise.and 0xFF

        carryFlag =
            Bitwise.and 0x80 operand > 0

        zeroFlag =
            result == 0x00
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


rr : Int -> Flags -> ALUResultWithFlags
rr operand flags =
    let
        carry =
            if Flag.getFlag Carry flags then
                0x80

            else
                0x00

        result =
            Bitwise.shiftRightZfBy 1 operand
                |> Bitwise.or carry
                |> Bitwise.and 0xFF

        carryFlag =
            Bitwise.and 0x01 operand > 0

        zeroFlag =
            result == 0x00
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


sla : Int -> ALUResultWithFlags
sla operand =
    let
        result =
            Bitwise.shiftLeftBy 1 operand
                |> Bitwise.and 0xFF

        zeroFlag =
            result == 0x00

        carryFlag =
            Bitwise.and 0x80 operand > 0
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


sra : Int -> ALUResultWithFlags
sra operand =
    let
        signMask =
            Bitwise.and 0x80 operand

        result =
            Bitwise.shiftRightBy 1 operand
                |> Bitwise.or signMask
                |> Bitwise.and 0xFF

        zeroFlag =
            result == 0x00

        carryFlag =
            Bitwise.and 0x01 operand > 0
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


srl : Int -> ALUResultWithFlags
srl operand =
    let
        result =
            Bitwise.shiftRightZfBy 1 operand
                |> Bitwise.and 0xFF

        zeroFlag =
            result == 0x00

        carryFlag =
            Bitwise.and 0x01 operand > 0
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


rla : Int -> Flags -> ALUResultWithFlags
rla operand flags =
    rl operand flags
        |> Tuple.mapSecond (Flag.setFlag Zero False)


rra : Int -> Flags -> ALUResultWithFlags
rra operand flags =
    rr operand flags
        |> Tuple.mapSecond (Flag.setFlag Zero False)


rlca : Int -> ALUResultWithFlags
rlca operand =
    rlc operand
        |> Tuple.mapSecond (Flag.setFlag Zero False)


rrca : Int -> ALUResultWithFlags
rrca operand =
    rrc operand
        |> Tuple.mapSecond (Flag.setFlag Zero False)
