module Component.APU.Channel2 exposing (Channel2, emulate, init, writeNR21, writeNR22, writeNR23, writeNR24)

import Array exposing (Array)
import Bitwise
import Component.APU.Constants as APUConstants
import Component.APU.SquareWave as SquareWave exposing (DutyCycle(..))
import Component.APU.VolumeEnvelope as VolumeEnvelope exposing (VolumeEnvelope)
import Constants


type alias Channel2 =
    { dutyCycle : DutyCycle
    , volumeEnvelope : VolumeEnvelope

    -- Frequency
    , frequency : Int

    -- Length stuff
    , lengthEnabled : Bool
    , lengthLoad : Int

    -- State
    , timerValue : Int
    , dutyCycleIndex : Int
    , cycleAccumulator : Int
    , lengthCounter : Int
    , currentSample : Float
    }


init : Channel2
init =
    { dutyCycle = Two
    , frequency = 2048 - (131072 // 440) -- 2048 - (131072 / feq)
    , timerValue = 0
    , dutyCycleIndex = 0
    , cycleAccumulator = 0
    , lengthCounter = 0
    , lengthEnabled = False
    , currentSample = 0
    , lengthLoad = 0
    , volumeEnvelope = VolumeEnvelope.init
    }


emulate : Int -> Channel2 -> Channel2
emulate cycles channel =
    let
        ( updatedCycleAccumulator, generateSample ) =
            ( remainderBy APUConstants.cyclesPerSample (channel.cycleAccumulator + cycles), channel.cycleAccumulator + cycles >= APUConstants.cyclesPerSample )

        ( updatedTimerValue, timerUnderflowed ) =
            let
                x =
                    channel.timerValue - cycles

                underflowed =
                    x <= 0

                v =
                    if underflowed then
                        (2048 - channel.frequency) * 4 + x

                    else
                        x
            in
            ( v, underflowed )

        updatedCycleIndex =
            if timerUnderflowed then
                remainderBy 8 (channel.dutyCycleIndex + 1)

            else
                channel.dutyCycleIndex

        updatedVolumeEnvelope =
            VolumeEnvelope.emulate cycles channel.volumeEnvelope

        sampleX =
            if generateSample then
                SquareWave.sample channel.dutyCycle updatedCycleIndex
                    |> VolumeEnvelope.modifySample updatedVolumeEnvelope

            else
                0
    in
    { dutyCycle = channel.dutyCycle
    , frequency = channel.frequency
    , timerValue = updatedTimerValue
    , dutyCycleIndex = updatedCycleIndex
    , cycleAccumulator = updatedCycleAccumulator
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    , lengthLoad = channel.lengthLoad
    , currentSample = sampleX
    , volumeEnvelope = updatedVolumeEnvelope
    }


writeNR21 : Int -> Channel2 -> Channel2
writeNR21 value channel =
    let
        dutyBits =
            value
                |> Bitwise.and 0xC0
                |> Bitwise.shiftRightZfBy 6

        updatedDuty =
            case dutyBits of
                0x00 ->
                    Zero

                0x01 ->
                    One

                0x02 ->
                    Two

                0x03 ->
                    Three

                _ ->
                    Zero

        updatedLengthLoad =
            Bitwise.and 0x3F value
    in
    { dutyCycle = updatedDuty
    , frequency = channel.frequency
    , timerValue = channel.timerValue
    , dutyCycleIndex = channel.dutyCycleIndex
    , cycleAccumulator = channel.cycleAccumulator
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    , lengthLoad = updatedLengthLoad
    , currentSample = channel.currentSample
    , volumeEnvelope = channel.volumeEnvelope
    }


writeNR22 : Int -> Channel2 -> Channel2
writeNR22 value channel =
    { dutyCycle = channel.dutyCycle
    , frequency = channel.frequency
    , timerValue = channel.timerValue
    , dutyCycleIndex = channel.dutyCycleIndex
    , cycleAccumulator = channel.cycleAccumulator
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    , lengthLoad = channel.lengthLoad
    , currentSample = channel.currentSample
    , volumeEnvelope = VolumeEnvelope.writeRegister value channel.volumeEnvelope
    }


writeNR23 : Int -> Channel2 -> Channel2
writeNR23 value channel =
    { dutyCycle = channel.dutyCycle
    , frequency = Bitwise.or (Bitwise.and 0x0700 channel.frequency) value
    , timerValue = channel.timerValue
    , dutyCycleIndex = channel.dutyCycleIndex
    , cycleAccumulator = channel.cycleAccumulator
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    , lengthLoad = channel.lengthLoad
    , currentSample = channel.currentSample
    , volumeEnvelope = channel.volumeEnvelope
    }


writeNR24 : Int -> Channel2 -> Channel2
writeNR24 value channel =
    let
        isTrigger =
            Bitwise.and Constants.bit7Mask value == Constants.bit7Mask

        updatedFrequency =
            value
                |> Bitwise.and 0x07
                |> Bitwise.shiftLeftBy 8
                |> Bitwise.or (Bitwise.and 0xFF channel.frequency)
    in
    { dutyCycle = channel.dutyCycle
    , frequency = updatedFrequency
    , timerValue = channel.timerValue
    , dutyCycleIndex = channel.dutyCycleIndex
    , cycleAccumulator = channel.cycleAccumulator
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    , lengthLoad = channel.lengthLoad
    , currentSample = channel.currentSample
    , volumeEnvelope =
        if isTrigger then
            VolumeEnvelope.handleTrigger channel.volumeEnvelope

        else
            channel.volumeEnvelope
    }
