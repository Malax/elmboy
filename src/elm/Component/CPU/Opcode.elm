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
    , jpIndirectHL
    , jr
    , ld
    , ldHLSPPlusSignedImmediate
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
import Component.CPU as CPU
import Component.CPU.Condition as Condition exposing (Condition(..))
import Component.CPU.FlagRegister as FlagRegister exposing (FlagDelta(..), FlagsRegisterDelta)
import CoreEffect exposing (extraCycles, readMemory16, readMemory16AdvancePC, readMemory8AdvancePC, writeMemory16)
import Effect exposing (Effect, Reader, Writer, join, join2, join3, mapReader, mapReader2, mapReader3)
import GameBoy
import Util



-- 8-bit Arithmetic and Logic Instructions


adc : Reader Int -> Effect
adc reader =
    mapReader3 ALU.adc CoreEffect.readRegisterA reader CoreEffect.readRegisterF
        |> flagSettingOpcode CoreEffect.writeRegisterA


sbc : Reader Int -> Effect
sbc reader =
    mapReader3 ALU.sbc CoreEffect.readRegisterA reader CoreEffect.readRegisterF
        |> flagSettingOpcode CoreEffect.writeRegisterA


add : Reader Int -> Effect
add reader =
    mapReader2 ALU.add CoreEffect.readRegisterA reader
        |> flagSettingOpcode CoreEffect.writeRegisterA


sub : Reader Int -> Effect
sub reader =
    mapReader2 ALU.sub CoreEffect.readRegisterA reader
        |> flagSettingOpcode CoreEffect.writeRegisterA


and : Reader Int -> Effect
and reader =
    mapReader2 ALU.and CoreEffect.readRegisterA reader
        |> flagSettingOpcode CoreEffect.writeRegisterA


cp : Reader Int -> Effect
cp reader =
    mapReader2 ALU.sub CoreEffect.readRegisterA reader
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
    mapReader2 ALU.or CoreEffect.readRegisterA reader
        |> flagSettingOpcode CoreEffect.writeRegisterA


xor : Reader Int -> Effect
xor reader =
    mapReader2 ALU.xor CoreEffect.readRegisterA reader
        |> flagSettingOpcode CoreEffect.writeRegisterA



-- 16-bit Arithmetic Instructions


add16 : Reader Int -> Effect
add16 reader =
    mapReader2 ALU.add16 (extraCycles 4 >> CoreEffect.readRegisterHL) reader
        |> flagModifyingOpcode CoreEffect.writeRegisterHL


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
    mapReader2 ALU.rl reader CoreEffect.readRegisterF
        |> flagSettingOpcode writer


rr : Reader Int -> Writer Int -> Effect
rr reader writer =
    mapReader2 ALU.rr reader CoreEffect.readRegisterF
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
    mapReader2 ALU.rla CoreEffect.readRegisterA CoreEffect.readRegisterF
        |> flagSettingOpcode CoreEffect.writeRegisterA


rra : Effect
rra =
    mapReader2 ALU.rra CoreEffect.readRegisterA CoreEffect.readRegisterF
        |> flagSettingOpcode CoreEffect.writeRegisterA


rlca : Effect
rlca =
    mapReader ALU.rlca CoreEffect.readRegisterA
        |> flagSettingOpcode CoreEffect.writeRegisterA


rrca : Effect
rrca =
    mapReader ALU.rrca CoreEffect.readRegisterA
        |> flagSettingOpcode CoreEffect.writeRegisterA



-- Load Instructions


ld : Writer Int -> Reader Int -> Effect
ld writer reader =
    join reader writer



-- Jumps and Subroutines


jpIndirectHL : Effect
jpIndirectHL =
    -- JP (HL) is a special case as it only uses 4 cycles in total
    join CoreEffect.readRegisterHL CoreEffect.writeRegisterPC


jp : Condition -> Reader Int -> Effect
jp condition reader =
    join2
        reader
        CoreEffect.readRegisterF
        (\address flags ->
            if Condition.check condition flags then
                extraCycles 4 >> CoreEffect.writeRegisterPC address

            else
                identity
        )


jr : Condition -> Effect
jr condition =
    join3
        (mapReader Util.byteToSignedInt readMemory8AdvancePC)
        CoreEffect.readRegisterPC
        CoreEffect.readRegisterF
        (\offset currentAddress flags ->
            if Condition.check condition flags then
                extraCycles 4 >> CoreEffect.writeRegisterPC (currentAddress + offset)

            else
                identity
        )


call : Condition -> Effect
call condition =
    join2 CoreEffect.readRegisterF
        readMemory16AdvancePC
        (\flags address ->
            if Condition.check condition flags then
                push CoreEffect.readRegisterPC >> CoreEffect.writeRegisterPC address

            else
                identity
        )


ret : Condition -> Effect
ret condition =
    join CoreEffect.readRegisterF
        (\flags ->
            if Condition.check condition flags then
                let
                    extraCyclesAmount =
                        if condition == Always then
                            4

                        else
                            8
                in
                extraCycles extraCyclesAmount >> pop CoreEffect.writeRegisterPC

            else
                extraCycles 4
        )


reti : Effect
reti =
    extraCycles 4 >> pop CoreEffect.writeRegisterPC >> ei


rst : Int -> Effect
rst memoryAddress =
    push CoreEffect.readRegisterPC >> CoreEffect.writeRegisterPC memoryAddress



-- Stack Operations Instructions


push : Reader Int -> Effect
push reader =
    join2 reader CoreEffect.readRegisterSP <|
        \value sp ->
            extraCycles 4 >> CoreEffect.writeRegisterSP (sp - 2) >> writeMemory16 CoreEffect.readRegisterSP value


pop : Writer Int -> Effect
pop writer =
    join2 CoreEffect.readRegisterSP (readMemory16 CoreEffect.readRegisterSP) <|
        \sp value ->
            CoreEffect.writeRegisterSP (sp + 2) >> writer value


addSPSignedImmediate : Effect
addSPSignedImmediate =
    mapReader2 ALU.add16Signed8 (extraCycles 8 >> CoreEffect.readRegisterSP) readMemory8AdvancePC
        |> flagSettingOpcode CoreEffect.writeRegisterSP


ldHLSPPlusSignedImmediate : Effect
ldHLSPPlusSignedImmediate =
    mapReader2 ALU.add16Signed8 (extraCycles 4 >> CoreEffect.readRegisterSP) readMemory8AdvancePC
        |> flagSettingOpcode CoreEffect.writeRegisterHL



-- Miscellaneous Instructions


nop : Effect
nop =
    identity


cpl : Effect
cpl =
    mapReader ALU.cpl CoreEffect.readRegisterA
        |> flagModifyingOpcode CoreEffect.writeRegisterA


scf : Effect
scf =
    join (mapReader (FlagRegister.modifyFlags Unchanged (StaticValue False) (StaticValue False) (StaticValue True)) CoreEffect.readRegisterF) CoreEffect.writeRegisterF


ccf : Effect
ccf =
    join (mapReader (FlagRegister.modifyFlags Unchanged (StaticValue False) (StaticValue False) Complemented) CoreEffect.readRegisterF) CoreEffect.writeRegisterF


ei : Effect
ei gameBoy =
    GameBoy.setCPU (CPU.setInterruptMasterEnable True gameBoy.cpu) gameBoy


di : Effect
di gameBoy =
    GameBoy.setCPU (CPU.setInterruptMasterEnable False gameBoy.cpu) gameBoy


daa : Effect
daa =
    mapReader2 ALU.daa CoreEffect.readRegisterA CoreEffect.readRegisterF
        |> flagModifyingOpcode CoreEffect.writeRegisterA


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
    join2 reader CoreEffect.readRegisterF <|
        \( value, modifier ) flags ->
            writer value >> CoreEffect.writeRegisterF (modifier flags)


flagSettingOpcode : Writer a -> Reader ( a, Int ) -> Effect
flagSettingOpcode writer reader =
    join reader <|
        \( value, flags ) ->
            writer value >> CoreEffect.writeRegisterF flags


flagSettingOnlyOpcode : Reader ( a, Int ) -> Effect
flagSettingOnlyOpcode reader =
    join reader <|
        \( _, flags ) ->
            CoreEffect.writeRegisterF flags


flagModifyingOnlyOpcode : Reader FlagsRegisterDelta -> Effect
flagModifyingOnlyOpcode reader =
    join2 reader CoreEffect.readRegisterF <|
        \modifier flags ->
            CoreEffect.writeRegisterF (modifier flags)
