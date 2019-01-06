module Component.CPU.Opcode exposing
    ( adc
    , add
    , add16
    , addSPSignedImmediate
    , and
    , bit
    , call
    , ccf
    , cp
    , cpl
    , daa
    , dec
    , dec16
    , di
    , ei
    , extensionOpcode
    , halt
    , inc
    , inc16
    , jp
    , jr
    , ld
    , nop
    , or
    , pop
    , push
    , res
    , ret
    , reti
    , rl
    , rla
    , rlc
    , rlca
    , rr
    , rra
    , rrc
    , rrca
    , rst
    , sbc
    , scf
    , set
    , sla
    , sra
    , srl
    , sub
    , swap
    , xor
    )

import Component.ALU as ALU
import Component.CPU as CPU exposing (Register16(..), Register8(..))
import Component.CPU.Condition as Condition exposing (Condition(..))
import Component.CPU.FlagRegister as FlagRegister exposing (FlagDelta(..), FlagsRegisterDelta)
import CoreEffect exposing (extraCycles, readMemory16, readMemory16AdvancePC, readMemory8AdvancePC, readRegister16, readRegister8, writeMemory16, writeRegister16, writeRegister8)
import Effect exposing (Effect, Reader, Writer, join, join2, join3, mapReader, mapReader2, mapReader3)
import GameBoy
import Util



-- 8-bit Arithmetic and Logic Instructions


adc : Reader Int -> Effect
adc reader =
    mapReader3 ALU.adc (readRegister8 A) reader (readRegister8 F)
        |> flagSettingOpcode (writeRegister8 A)


sbc : Reader Int -> Effect
sbc reader =
    mapReader3 ALU.sbc (readRegister8 A) reader (readRegister8 F)
        |> flagSettingOpcode (writeRegister8 A)


add : Reader Int -> Effect
add reader =
    mapReader2 ALU.add (readRegister8 A) reader
        |> flagSettingOpcode (writeRegister8 A)


sub : Reader Int -> Effect
sub reader =
    mapReader2 ALU.sub (readRegister8 A) reader
        |> flagSettingOpcode (writeRegister8 A)


and : Reader Int -> Effect
and reader =
    mapReader2 ALU.and_ (readRegister8 A) reader
        |> flagSettingOpcode (writeRegister8 A)


cp : Reader Int -> Effect
cp reader =
    mapReader2 ALU.sub (readRegister8 A) reader
        |> flagSettingOnlyOpcode


dec : Reader Int -> Writer Int -> Effect
dec reader writer =
    mapReader ALU.dec reader
        |> flagModifyingOpcode writer


inc : Reader Int -> Writer Int -> Effect
inc reader writer =
    mapReader ALU.inc reader
        |> flagModifyingOpcode writer


or : Reader Int -> Effect
or reader =
    mapReader2 ALU.or_ (readRegister8 A) reader
        |> flagSettingOpcode (writeRegister8 A)


xor : Reader Int -> Effect
xor reader =
    mapReader2 ALU.xor_ (readRegister8 A) reader
        |> flagSettingOpcode (writeRegister8 A)



-- 16-bit Arithmetic Instructions


add16 : Reader Int -> Effect
add16 reader =
    mapReader2 ALU.add16 (readRegister16 HL) reader
        |> flagModifyingOpcode (writeRegister16 HL)


dec16 : Reader Int -> Writer Int -> Effect
dec16 reader writer =
    extraCycles 4 >> join (mapReader ALU.dec16 reader) writer


inc16 : Reader Int -> Writer Int -> Effect
inc16 reader writer =
    extraCycles 4 >> join (mapReader ALU.inc16 reader) writer



-- Bit Operations Instructions


bit : Int -> Reader Int -> Effect
bit index reader =
    mapReader (ALU.bit index) reader
        |> flagModifyingOnlyOpcode


set : Int -> Reader Int -> Writer Int -> Effect
set index reader writer =
    join (mapReader (ALU.set index) reader) writer


res : Int -> Reader Int -> Writer Int -> Effect
res index reader writer =
    join (mapReader (ALU.res index) reader) writer


swap : Reader Int -> Writer Int -> Effect
swap reader writer =
    mapReader ALU.swap reader
        |> flagSettingOpcode writer



-- Bit Shift Instructions


rlc : Reader Int -> Writer Int -> Effect
rlc reader writer =
    mapReader ALU.rlc reader
        |> flagSettingOpcode writer


rrc : Reader Int -> Writer Int -> Effect
rrc reader writer =
    mapReader ALU.rrc reader
        |> flagSettingOpcode writer


rl : Reader Int -> Writer Int -> Effect
rl reader writer =
    mapReader2 ALU.rl reader (readRegister8 F)
        |> flagSettingOpcode writer


rr : Reader Int -> Writer Int -> Effect
rr reader writer =
    mapReader2 ALU.rr reader (readRegister8 F)
        |> flagSettingOpcode writer


sla : Reader Int -> Writer Int -> Effect
sla reader writer =
    mapReader ALU.sla reader
        |> flagSettingOpcode writer


sra : Reader Int -> Writer Int -> Effect
sra reader writer =
    mapReader ALU.sra reader
        |> flagSettingOpcode writer


srl : Reader Int -> Writer Int -> Effect
srl reader writer =
    mapReader ALU.srl reader
        |> flagSettingOpcode writer


rla : Effect
rla =
    mapReader2 ALU.rla (readRegister8 A) (readRegister8 F)
        |> flagSettingOpcode (writeRegister8 A)


rra : Effect
rra =
    mapReader2 ALU.rra (readRegister8 A) (readRegister8 F)
        |> flagSettingOpcode (writeRegister8 A)


rlca : Effect
rlca =
    mapReader ALU.rlca (readRegister8 A)
        |> flagSettingOpcode (writeRegister8 A)


rrca : Effect
rrca =
    mapReader ALU.rrca (readRegister8 A)
        |> flagSettingOpcode (writeRegister8 A)



-- Load Instructions


ld : Writer Int -> Reader Int -> Effect
ld writer reader =
    join reader writer



-- Jumps and Subroutines


jp : Condition -> Reader Int -> Effect
jp condition reader =
    join2
        reader
        (readRegister8 F)
        (\address flags ->
            if Condition.check condition flags then
                -- TODO: "JP (HL)" only uses 4 cycles in total, we might need a specialized opcode implementation for that.
                -- This is because unconditional jumps do not need 4 extra cycles!
                extraCycles 4 >> writeRegister16 PC address

            else
                identity
        )


jr : Condition -> Effect
jr condition =
    join3
        (mapReader Util.byteToSignedInt readMemory8AdvancePC)
        (readRegister16 PC)
        (readRegister8 F)
        (\offset currentAddress flags ->
            if Condition.check condition flags then
                extraCycles 4 >> writeRegister16 PC (currentAddress + offset)

            else
                identity
        )


call : Condition -> Effect
call condition =
    join2 (readRegister8 F)
        readMemory16AdvancePC
        (\flags address ->
            if Condition.check condition flags then
                push (readRegister16 PC) >> writeRegister16 PC address

            else
                identity
        )


ret : Condition -> Effect
ret condition =
    join (readRegister8 F)
        (\flags ->
            if Condition.check condition flags then
                let
                    extraCyclesAmount =
                        if condition == Always then
                            4

                        else
                            8
                in
                extraCycles extraCyclesAmount >> pop (writeRegister16 PC)

            else
                extraCycles 4
        )


reti : Effect
reti =
    extraCycles 4 >> pop (writeRegister16 PC) >> ei


rst : Int -> Effect
rst memoryAddress =
    push (readRegister16 PC) >> writeRegister16 PC memoryAddress



-- Stack Operations Instructions


push : Reader Int -> Effect
push reader =
    join2 reader (readRegister16 SP) <|
        \value sp ->
            extraCycles 4 >> writeRegister16 SP (sp - 2) >> writeMemory16 (readRegister16 SP) value


pop : Writer Int -> Effect
pop writer =
    join2 (readRegister16 SP) (readMemory16 (readRegister16 SP)) <|
        \sp value ->
            writeRegister16 SP (sp + 2) >> writer value


addSPSignedImmediate : Writer Int -> Effect
addSPSignedImmediate writer =
    mapReader2 ALU.add16Signed8 (readRegister16 SP) readMemory8AdvancePC
        |> flagSettingOpcode writer



-- Miscellaneous Instructions


nop : Effect
nop =
    identity


cpl : Effect
cpl =
    mapReader ALU.cpl (readRegister8 A)
        |> flagModifyingOpcode (writeRegister8 A)


scf : Effect
scf =
    join (mapReader (FlagRegister.modifyFlags Unchanged (StaticValue False) (StaticValue False) (StaticValue True)) (readRegister8 F)) (writeRegister8 F)


ccf : Effect
ccf =
    join (mapReader (FlagRegister.modifyFlags Unchanged (StaticValue False) (StaticValue False) Complemented) (readRegister8 F)) (writeRegister8 F)


ei : Effect
ei gameBoy =
    GameBoy.setCPU (CPU.setInterruptMasterEnable True gameBoy.cpu) gameBoy


di : Effect
di gameBoy =
    GameBoy.setCPU (CPU.setInterruptMasterEnable False gameBoy.cpu) gameBoy


daa : Effect
daa =
    mapReader2 ALU.daa (readRegister8 A) (readRegister8 F)
        |> flagModifyingOpcode (writeRegister8 A)


halt : Effect
halt gameBoy =
    GameBoy.setCPU (CPU.setHalted True gameBoy.cpu) gameBoy


extensionOpcode : (Int -> Effect) -> Effect
extensionOpcode mapper gameBoy =
    let
        ( opcode, gameBoyB ) =
            readMemory8AdvancePC gameBoy
    in
    mapper opcode gameBoyB



-- Helpers


flagModifyingOpcode : Writer a -> Reader ( a, FlagsRegisterDelta ) -> Effect
flagModifyingOpcode writer reader =
    join2 reader (readRegister8 F) <|
        \( value, modifier ) flags ->
            writer value >> writeRegister8 F (modifier flags)


flagSettingOpcode : Writer a -> Reader ( a, Int ) -> Effect
flagSettingOpcode writer reader =
    join reader <|
        \( value, flags ) ->
            writer value >> writeRegister8 F flags


flagSettingOnlyOpcode : Reader ( a, Int ) -> Effect
flagSettingOnlyOpcode reader =
    join reader <|
        \( _, flags ) ->
            writeRegister8 F flags


flagModifyingOnlyOpcode : Reader FlagsRegisterDelta -> Effect
flagModifyingOnlyOpcode reader =
    join2 reader (readRegister8 F) <|
        \modifier flags ->
            writeRegister8 F (modifier flags)
