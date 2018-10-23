module Component.ALU exposing
    ( adc
    , add
    , add16
    , add16Signed8
    , and_
    , bit
    , cpl
    , daa
    , dec
    , dec16
    , inc
    , inc16
    , or_
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
    , xor_
    )

import Bitwise exposing (and, complement, or, shiftLeftBy, shiftRightBy, shiftRightZfBy, xor)
import Component.CPU.FlagRegister as Flag exposing (Flag(..), FlagDelta(..), FlagsRegisterDelta)
import Util


type alias ALUResultWithFlagModification =
    ( Int, FlagsRegisterDelta )


type alias ALUResultWithFlags =
    ( Int, Flags )


type alias ALUResult =
    Int


type alias ALUResultOnlyFlags =
    Flags


type alias Flags =
    Int


swap : Int -> ALUResultWithFlags
swap value =
    let
        result =
            shiftLeftBy 4 (and 0x0F value) + shiftRightZfBy 4 (and 0xF0 value)

        maskedResult =
            and 0xFF result

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
            and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        halfCarryFlag =
            and 0x0F operand1 + and 0x0F operand2 + carry > 0x0F

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
            and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        halfCarryFlag =
            and
                (xor (xor operand1 operand2) maskedResult)
                (shiftLeftBy 4 1)
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
            and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        carryFlag =
            result > 0xFF

        halfCarryFlag =
            and 0x0F operand1 + and 0x0F operand2 > 0x0F
    in
    ( maskedResult, Flag.setAllFlags zeroFlag False halfCarryFlag carryFlag )


sub : Int -> Int -> ALUResultWithFlags
sub operand1 operand2 =
    let
        result =
            operand1 - operand2

        maskedResult =
            and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        carryFlag =
            operand1 < operand2

        halfCarryFlag =
            and 0x0F operand1 < and 0x0F operand2
    in
    ( maskedResult, Flag.setAllFlags zeroFlag True halfCarryFlag carryFlag )


inc : Int -> ALUResultWithFlagModification
inc operand =
    let
        result =
            operand + 1

        maskedResult =
            and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        halfCarryFlag =
            and 0x0F operand + 1 > 0x0F
    in
    ( maskedResult, Flag.modifyFlags (StaticValue zeroFlag) (StaticValue False) (StaticValue halfCarryFlag) Unchanged )


dec : Int -> ALUResultWithFlagModification
dec operand =
    let
        result =
            operand - 1

        maskedResult =
            and 0xFF result

        zeroFlag =
            maskedResult == 0x00

        halfCarryFlag =
            and 0x0F operand == 0x00
    in
    ( maskedResult, Flag.modifyFlags (StaticValue zeroFlag) (StaticValue True) (StaticValue halfCarryFlag) Unchanged )


and_ : Int -> Int -> ALUResultWithFlags
and_ operand1 operand2 =
    let
        result =
            and operand1 operand2

        maskedResult =
            and 0xFF result

        zeroFlag =
            maskedResult == 0
    in
    ( maskedResult, Flag.setAllFlags zeroFlag False True False )


or_ : Int -> Int -> ALUResultWithFlags
or_ operand1 operand2 =
    let
        result =
            or operand1 operand2

        maskedResult =
            and 0xFF result

        zeroFlag =
            maskedResult == 0
    in
    ( maskedResult, Flag.setAllFlags zeroFlag False False False )


xor_ : Int -> Int -> ALUResultWithFlags
xor_ operand1 operand2 =
    let
        result =
            xor operand1 operand2

        maskedResult =
            and 0xFF result

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
            and 0xFF result

        {- Contrary to other opcodes, this flag is not set with the result of
           the carry, but only if the result is acutally a carry. That means if
           the carry flag was set prior to this opcode and the carry of this opcode
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
            and 0xFFFF result

        zeroFlag =
            maskedResult == 0x00

        carryFlag =
            result > 0xFFFF

        halfCarryFlag =
            and 0x0FFF operand1 + and 0x0FFF operand2 > 0x0FFF
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
            and 0xFFFF result

        carryFlag =
            and 0xFF operand1 + and 0xFF operand2 > 0xFF

        halfCarryFlag =
            and 0x0F operand1 + and 0x0F operand2 > 0x0F
    in
    ( maskedResult, Flag.setAllFlags False False halfCarryFlag carryFlag )


inc16 : Int -> ALUResult
inc16 operand =
    and (operand + 1) 0xFFFF


dec16 : Int -> ALUResult
dec16 operand =
    and (operand - 1) 0xFFFF


bit : Int -> Int -> FlagsRegisterDelta
bit index operand =
    let
        bitValue =
            and operand (shiftLeftBy index 0x01) > 0
    in
    Flag.modifyFlags (StaticValue (not bitValue)) (StaticValue False) (StaticValue True) Unchanged


set : Int -> Int -> ALUResult
set index operand =
    operand
        |> or (shiftLeftBy index 0x01)
        |> and 0xFF


res : Int -> Int -> ALUResult
res index operand =
    operand
        |> and (shiftLeftBy index 0x01 |> complement)
        |> and 0xFF


cpl : Int -> ALUResultWithFlagModification
cpl operand =
    let
        result =
            complement operand

        maskedResult =
            and 0xFF result
    in
    ( maskedResult, Flag.modifyFlags Unchanged (StaticValue True) (StaticValue True) Unchanged )


rlc : Int -> ALUResultWithFlags
rlc operand =
    let
        rotatedValue =
            shiftRightZfBy 7 (and operand 0x80)

        result =
            shiftLeftBy 1 operand
                |> or rotatedValue
                |> and 0xFF

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
            shiftLeftBy 7 (and operand 0x01)

        result =
            shiftRightZfBy 1 operand
                |> or rotatedValue
                |> and 0xFF

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
            shiftLeftBy 1 operand
                |> or carry
                |> and 0xFF

        carryFlag =
            and 0x80 operand > 0

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
            shiftRightZfBy 1 operand
                |> or carry
                |> and 0xFF

        carryFlag =
            and 0x01 operand > 0

        zeroFlag =
            result == 0x00
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


sla : Int -> ALUResultWithFlags
sla operand =
    let
        result =
            shiftLeftBy 1 operand
                |> and 0xFF

        zeroFlag =
            result == 0x00

        carryFlag =
            and 0x80 operand > 0
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


sra : Int -> ALUResultWithFlags
sra operand =
    let
        signMask =
            and 0x80 operand

        result =
            shiftRightBy 1 operand
                |> or signMask
                |> and 0xFF

        zeroFlag =
            result == 0x00

        carryFlag =
            and 0x01 operand > 0
    in
    ( result, Flag.setAllFlags zeroFlag False False carryFlag )


srl : Int -> ALUResultWithFlags
srl operand =
    let
        result =
            shiftRightZfBy 1 operand
                |> and 0xFF

        zeroFlag =
            result == 0x00

        carryFlag =
            and 0x01 operand > 0
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
