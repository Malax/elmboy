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
    , ldSPHL
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

import Array exposing (Array)
import Component.ALU as ALU
import Component.CPU as CPU
import Component.CPU.Condition as Condition exposing (Condition(..))
import Component.CPU.FlagRegister as FlagRegister exposing (FlagDelta(..), FlagsRegisterDelta)
import Component.MMU as MMU
import Effect exposing (Effect, join, join2, join3)
import Effect.Operand
import Effect.Reader exposing (Reader)
import Effect.Writer exposing (Writer)
import GameBoy
import Util



-- 8-bit Arithmetic and Logic Instructions


adc : Reader Int -> Effect
adc reader =
    Effect.Reader.map3 ALU.adc Effect.Operand.read8RegisterA reader Effect.Operand.read8RegisterF
        |> flagSettingOpcode Effect.Operand.write8RegisterA


sbc : Reader Int -> Effect
sbc reader =
    Effect.Reader.map3 ALU.sbc Effect.Operand.read8RegisterA reader Effect.Operand.read8RegisterF
        |> flagSettingOpcode Effect.Operand.write8RegisterA


add : Reader Int -> Effect
add reader =
    Effect.Reader.map2 ALU.add Effect.Operand.read8RegisterA reader
        |> flagSettingOpcode Effect.Operand.write8RegisterA


sub : Reader Int -> Effect
sub reader =
    Effect.Reader.map2 ALU.sub Effect.Operand.read8RegisterA reader
        |> flagSettingOpcode Effect.Operand.write8RegisterA


and : Reader Int -> Effect
and reader =
    Effect.Reader.map2 ALU.and Effect.Operand.read8RegisterA reader
        |> flagSettingOpcode Effect.Operand.write8RegisterA


cp : Reader Int -> Effect
cp reader =
    Effect.Reader.map2 ALU.sub Effect.Operand.read8RegisterA reader
        |> flagSettingOnlyOpcode


dec : Reader Int -> Writer Int -> Effect
dec reader writer =
    Effect.Reader.map ALU.dec reader
        |> flagModifyingOpcode writer


inc : Reader Int -> Writer Int -> Effect
inc reader writer =
    Effect.Reader.map ALU.inc reader
        |> flagModifyingOpcode writer


or : Reader Int -> Effect
or reader =
    Effect.Reader.map2 ALU.or Effect.Operand.read8RegisterA reader
        |> flagSettingOpcode Effect.Operand.write8RegisterA


xor : Reader Int -> Effect
xor reader =
    Effect.Reader.map2 ALU.xor Effect.Operand.read8RegisterA reader
        |> flagSettingOpcode Effect.Operand.write8RegisterA



-- 16-bit Arithmetic Instructions


add16 : Reader Int -> Effect
add16 reader =
    Effect.Reader.map2 ALU.add16 (extraCycles 4 >> Effect.Operand.read16RegisterHL) reader
        |> flagModifyingOpcode Effect.Operand.write16RegisterHL


dec16 : Reader Int -> Writer Int -> Effect
dec16 reader writer =
    extraCycles 4 >> join (Effect.Reader.map ALU.dec16 reader) writer


inc16 : Reader Int -> Writer Int -> Effect
inc16 reader writer =
    extraCycles 4 >> join (Effect.Reader.map ALU.inc16 reader) writer



-- Bit Operations Instructions


bit : Int -> Reader Int -> Effect
bit index reader =
    Effect.Reader.map (ALU.bit index) reader
        |> flagModifyingOnlyOpcode


set : Int -> Reader Int -> Writer Int -> Effect
set index reader writer =
    join (Effect.Reader.map (ALU.set index) reader) writer


res : Int -> Reader Int -> Writer Int -> Effect
res index reader writer =
    join (Effect.Reader.map (ALU.res index) reader) writer


swap : Reader Int -> Writer Int -> Effect
swap reader writer =
    Effect.Reader.map ALU.swap reader
        |> flagSettingOpcode writer



-- Bit Shift Instructions


rlc : Reader Int -> Writer Int -> Effect
rlc reader writer =
    Effect.Reader.map ALU.rlc reader
        |> flagSettingOpcode writer


rrc : Reader Int -> Writer Int -> Effect
rrc reader writer =
    Effect.Reader.map ALU.rrc reader
        |> flagSettingOpcode writer


rl : Reader Int -> Writer Int -> Effect
rl reader writer =
    Effect.Reader.map2 ALU.rl reader Effect.Operand.read8RegisterF
        |> flagSettingOpcode writer


rr : Reader Int -> Writer Int -> Effect
rr reader writer =
    Effect.Reader.map2 ALU.rr reader Effect.Operand.read8RegisterF
        |> flagSettingOpcode writer


sla : Reader Int -> Writer Int -> Effect
sla reader writer =
    Effect.Reader.map ALU.sla reader
        |> flagSettingOpcode writer


sra : Reader Int -> Writer Int -> Effect
sra reader writer =
    Effect.Reader.map ALU.sra reader
        |> flagSettingOpcode writer


srl : Reader Int -> Writer Int -> Effect
srl reader writer =
    Effect.Reader.map ALU.srl reader
        |> flagSettingOpcode writer


rla : Effect
rla =
    Effect.Reader.map2 ALU.rla Effect.Operand.read8RegisterA Effect.Operand.read8RegisterF
        |> flagSettingOpcode Effect.Operand.write8RegisterA


rra : Effect
rra =
    Effect.Reader.map2 ALU.rra Effect.Operand.read8RegisterA Effect.Operand.read8RegisterF
        |> flagSettingOpcode Effect.Operand.write8RegisterA


rlca : Effect
rlca =
    Effect.Reader.map ALU.rlca Effect.Operand.read8RegisterA
        |> flagSettingOpcode Effect.Operand.write8RegisterA


rrca : Effect
rrca =
    Effect.Reader.map ALU.rrca Effect.Operand.read8RegisterA
        |> flagSettingOpcode Effect.Operand.write8RegisterA



-- Load Instructions


ld : Writer Int -> Reader Int -> Effect
ld writer reader =
    join reader writer


ldSPHL : Effect
ldSPHL =
    extraCycles 4 >> join Effect.Operand.read16RegisterHL Effect.Operand.write16RegisterSP



-- Jumps and Subroutines


jpIndirectHL : Effect
jpIndirectHL =
    -- JP (HL) is a special case as it only uses 4 cycles in total
    join Effect.Operand.read16RegisterHL Effect.Operand.write16RegisterPC


jp : Condition -> Reader Int -> Effect
jp condition reader =
    join2
        reader
        Effect.Operand.read8RegisterF
        (\address flags ->
            if Condition.check condition flags then
                extraCycles 4 >> Effect.Operand.write16RegisterPC address

            else
                identity
        )


jr : Condition -> Effect
jr condition =
    join3
        (Effect.Reader.map Util.byteToSignedInt Effect.Operand.read8Immediate)
        Effect.Operand.read16RegisterPC
        Effect.Operand.read8RegisterF
        (\offset currentAddress flags ->
            if Condition.check condition flags then
                extraCycles 4 >> Effect.Operand.write16RegisterPC (currentAddress + offset)

            else
                identity
        )


call : Condition -> Effect
call condition =
    join2 Effect.Operand.read8RegisterF
        Effect.Operand.read16Immediate
        (\flags address ->
            if Condition.check condition flags then
                push Effect.Operand.read16RegisterPC >> Effect.Operand.write16RegisterPC address

            else
                identity
        )


ret : Condition -> Effect
ret condition =
    join Effect.Operand.read8RegisterF
        (\flags ->
            if Condition.check condition flags then
                let
                    extraCyclesAmount =
                        if condition == Always then
                            4

                        else
                            8
                in
                extraCycles extraCyclesAmount >> pop Effect.Operand.write16RegisterPC

            else
                extraCycles 4
        )


reti : Effect
reti =
    extraCycles 4 >> pop Effect.Operand.write16RegisterPC >> ei


rst : Int -> Effect
rst memoryAddress =
    push Effect.Operand.read16RegisterPC >> Effect.Operand.write16RegisterPC memoryAddress



-- Stack Operations Instructions


push : Reader Int -> Effect
push reader =
    join2 reader Effect.Operand.read16RegisterSP <|
        \value sp ->
            extraCycles 4 >> Effect.Operand.write16RegisterSP (sp - 2) >> writeMemory16 Effect.Operand.read16RegisterSP value


pop : Writer Int -> Effect
pop writer =
    join2 Effect.Operand.read16RegisterSP Effect.Operand.read16IndirectSP <|
        \sp value ->
            Effect.Operand.write16RegisterSP (sp + 2) >> writer value


addSPSignedImmediate : Effect
addSPSignedImmediate =
    Effect.Reader.map2 ALU.add16Signed8 (extraCycles 8 >> Effect.Operand.read16RegisterSP) Effect.Operand.read8Immediate
        |> flagSettingOpcode Effect.Operand.write16RegisterSP


ldHLSPPlusSignedImmediate : Effect
ldHLSPPlusSignedImmediate =
    Effect.Reader.map2 ALU.add16Signed8 (extraCycles 4 >> Effect.Operand.read16RegisterSP) Effect.Operand.read8Immediate
        |> flagSettingOpcode Effect.Operand.write16RegisterHL



-- Miscellaneous Instructions


nop : Effect
nop =
    identity


cpl : Effect
cpl =
    Effect.Reader.map ALU.cpl Effect.Operand.read8RegisterA
        |> flagModifyingOpcode Effect.Operand.write8RegisterA


scf : Effect
scf =
    join (Effect.Reader.map (FlagRegister.modifyFlags Unchanged (StaticValue False) (StaticValue False) (StaticValue True)) Effect.Operand.read8RegisterF) Effect.Operand.write8RegisterF


ccf : Effect
ccf =
    join (Effect.Reader.map (FlagRegister.modifyFlags Unchanged (StaticValue False) (StaticValue False) Complemented) Effect.Operand.read8RegisterF) Effect.Operand.write8RegisterF


ei : Effect
ei gameBoy =
    GameBoy.setCPU (CPU.setInterruptMasterEnable True gameBoy.cpu) gameBoy


di : Effect
di gameBoy =
    GameBoy.setCPU (CPU.setInterruptMasterEnable False gameBoy.cpu) gameBoy


daa : Effect
daa =
    Effect.Reader.map2 ALU.daa Effect.Operand.read8RegisterA Effect.Operand.read8RegisterF
        |> flagModifyingOpcode Effect.Operand.write8RegisterA


halt : Effect
halt gameBoy =
    GameBoy.setCPU (CPU.setHalted True gameBoy.cpu) gameBoy


extensionOpcode : Array Effect -> Effect
extensionOpcode mapper gameBoy =
    let
        ( opcode, gameBoyB ) =
            Effect.Operand.read8Immediate gameBoy
    in
    case Array.get opcode mapper of
        Just effect ->
            effect gameBoyB

        Nothing ->
            gameBoyB



-- Helpers


flagModifyingOpcode : Writer a -> Reader ( a, FlagsRegisterDelta ) -> Effect
flagModifyingOpcode writer reader =
    join2 reader Effect.Operand.read8RegisterF <|
        \( value, modifier ) flags ->
            writer value >> Effect.Operand.write8RegisterF (modifier flags)


flagSettingOpcode : Writer a -> Reader ( a, Int ) -> Effect
flagSettingOpcode writer reader =
    join reader <|
        \( value, flags ) ->
            writer value >> Effect.Operand.write8RegisterF flags


flagSettingOnlyOpcode : Reader ( a, Int ) -> Effect
flagSettingOnlyOpcode reader =
    join reader <|
        \( _, flags ) ->
            Effect.Operand.write8RegisterF flags


flagModifyingOnlyOpcode : Reader FlagsRegisterDelta -> Effect
flagModifyingOnlyOpcode reader =
    join2 reader Effect.Operand.read8RegisterF <|
        \modifier flags ->
            Effect.Operand.write8RegisterF (modifier flags)


extraCycles : Writer Int
extraCycles value gameBoy =
    GameBoy.setLastInstructionCycles (gameBoy.lastInstructionCycles + value) gameBoy


writeMemory16 : Reader Int -> Writer Int
writeMemory16 reader value gameBoy =
    let
        ( memoryAddress, gameBoy2 ) =
            reader gameBoy
    in
    MMU.writeWord16 memoryAddress value (GameBoy.setLastInstructionCycles (gameBoy2.lastInstructionCycles + 8) gameBoy2)
