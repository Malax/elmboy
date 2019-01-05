module Component.APU.PulseChannel exposing
    ( PulseChannel
    , clockLengthCounter
    , clockSweepUnit
    , clockTimer
    , clockVolumeEnvelope
    , init
    , sample
    , writeNRx0
    , writeNRx1
    , writeNRx2
    , writeNRx3
    , writeNRx4
    )

import Bitwise
import Component.APU.Constants as APUConstants
import Component.APU.DutyCycle as DutyCycle exposing (DutyCycle(..))
import Constants


type alias PulseChannel =
    { dutyCycle : DutyCycle
    , wavePosition : Int
    , volume : Int
    , enabled : Bool
    , frequency : Int
    , timerValue : Int

    -- Frequency Sweep
    , sweepPeriod : Int
    , sweepNegate : Bool
    , sweepShift : Int
    , sweepShadowFrequency : Int
    , sweepCounter : Int
    , sweepEnabled : Bool

    -- Volume Envelope
    , envelopeStartingVolume : Int
    , envelopeAdd : Bool
    , envelopePeriod : Int
    , envelopeCounter : Int

    -- Length
    , lengthCounter : Int
    , lengthEnabled : Bool
    }


init : PulseChannel
init =
    { dutyCycle = One
    , wavePosition = 0
    , volume = 0
    , enabled = False
    , frequency = 0
    , sweepPeriod = 0
    , sweepNegate = False
    , sweepShift = 0
    , envelopeStartingVolume = 0
    , envelopeAdd = False
    , envelopePeriod = 0
    , envelopeCounter = 0
    , timerValue = 0
    , sweepShadowFrequency = 0
    , sweepCounter = 0
    , sweepEnabled = False
    , lengthCounter = 0
    , lengthEnabled = False
    }


clockTimer : Int -> PulseChannel -> PulseChannel
clockTimer cycles channel =
    let
        triggered =
            channel.timerValue - cycles <= 0

        timerValue =
            if not triggered then
                channel.timerValue - cycles

            else
                ((2048 - channel.frequency) * 4) + (channel.timerValue - cycles)

        wavePosition =
            if triggered then
                remainderBy 8 (channel.wavePosition + 1)

            else
                channel.wavePosition
    in
    setWavePositionTimerValue wavePosition timerValue channel


clockLengthCounter : PulseChannel -> PulseChannel
clockLengthCounter channel =
    if channel.lengthEnabled && channel.lengthCounter > 0 then
        setEnabledLengthCounter (channel.lengthCounter - 1 > 0) (channel.lengthCounter - 1) channel

    else
        channel


clockSweepUnit : PulseChannel -> PulseChannel
clockSweepUnit channel =
    let
        triggered =
            channel.sweepCounter - 1 <= 0

        sweepCounter =
            if triggered then
                channel.sweepPeriod

            else
                channel.sweepCounter - 1
    in
    if triggered && channel.sweepEnabled && channel.sweepPeriod /= 0 then
        let
            calculatedFrequency =
                frequencyCalculation channel.sweepShadowFrequency channel.sweepShift channel.sweepNegate
        in
        if calculatedFrequency < 2047 && channel.sweepShift /= 0 then
            { dutyCycle = channel.dutyCycle
            , wavePosition = channel.wavePosition
            , volume = channel.volume
            , enabled = channel.enabled && frequencyCalculation calculatedFrequency channel.sweepShift channel.sweepNegate < 2047
            , frequency = calculatedFrequency
            , sweepPeriod = channel.sweepPeriod
            , sweepNegate = channel.sweepNegate
            , sweepShift = channel.sweepShift
            , envelopeStartingVolume = channel.envelopeStartingVolume
            , envelopeAdd = channel.envelopeAdd
            , envelopePeriod = channel.envelopePeriod
            , envelopeCounter = channel.envelopeCounter
            , timerValue = channel.timerValue
            , sweepShadowFrequency = calculatedFrequency
            , sweepCounter = sweepCounter
            , sweepEnabled = channel.sweepEnabled
            , lengthCounter = channel.lengthCounter
            , lengthEnabled = channel.lengthEnabled
            }

        else
            setEnabledSweepCounter (channel.enabled && calculatedFrequency < 2047) sweepCounter channel

    else
        setSweepCounter sweepCounter channel


clockVolumeEnvelope : PulseChannel -> PulseChannel
clockVolumeEnvelope channel =
    let
        triggered =
            channel.envelopeCounter - 1 <= 0

        envelopeCounter =
            if not triggered then
                channel.envelopeCounter - 1

            else
                channel.envelopePeriod

        volume =
            if triggered && envelopeCounter /= 0 then
                if channel.envelopeAdd && channel.volume < maxVolume then
                    channel.volume + 1

                else if not channel.envelopeAdd && channel.volume > minVolume then
                    channel.volume - 1

                else
                    channel.volume

            else
                channel.volume
    in
    setVolumeEnvelopeCounter volume envelopeCounter channel


sample : PulseChannel -> Float
sample { enabled, dutyCycle, wavePosition, volume } =
    if enabled then
        (toFloat volume * volumeConversionFactor) * DutyCycle.sample dutyCycle wavePosition

    else
        APUConstants.silence


writeNRx0 : Int -> PulseChannel -> PulseChannel
writeNRx0 value channel =
    let
        sweepPeriod =
            Bitwise.shiftRightZfBy 4 value

        sweepNegate =
            Bitwise.and Constants.bit3Mask value == Constants.bit3Mask

        sweepShift =
            Bitwise.and 0x07 value
    in
    setSweepPeriodNegateShift sweepPeriod sweepNegate sweepShift channel


writeNRx1 : Int -> PulseChannel -> PulseChannel
writeNRx1 value channel =
    let
        dutyCycle =
            case Bitwise.shiftRightZfBy 6 value of
                0x00 ->
                    Zero

                0x01 ->
                    One

                0x02 ->
                    Two

                _ ->
                    Three

        length =
            64 - Bitwise.and 0x3F value
    in
    setDutyCycleLengthCounter dutyCycle length channel


writeNRx2 : Int -> PulseChannel -> PulseChannel
writeNRx2 value channel =
    let
        envelopeStartingVolume =
            Bitwise.shiftRightZfBy 4 value

        envelopeAdd =
            Bitwise.and Constants.bit3Mask value == Constants.bit3Mask

        envelopePeriod =
            Bitwise.and 0x07 value
    in
    setEnvelopeStartingVolumeAddPeriod envelopeStartingVolume envelopeAdd envelopePeriod channel


writeNRx3 : Int -> PulseChannel -> PulseChannel
writeNRx3 value channel =
    let
        frequency =
            Bitwise.or (Bitwise.and 0x0700 channel.frequency) value
    in
    setFrequency frequency channel


writeNRx4 : Int -> PulseChannel -> PulseChannel
writeNRx4 value channel =
    let
        trigger =
            Bitwise.and Constants.bit7Mask value == Constants.bit7Mask

        lengthEnabled =
            Bitwise.and Constants.bit6Mask value == Constants.bit6Mask

        frequency =
            value
                |> Bitwise.and 0x07
                |> Bitwise.shiftLeftBy 8
                |> Bitwise.or (Bitwise.and 0xFF channel.frequency)
    in
    if trigger then
        { dutyCycle = channel.dutyCycle
        , wavePosition = 0
        , volume = channel.envelopeStartingVolume
        , enabled =
            if channel.sweepShift /= 0 then
                frequencyCalculation frequency channel.sweepShift channel.sweepNegate < 2047

            else
                dacEnabled channel.envelopeStartingVolume channel.envelopeAdd
        , frequency = frequency
        , sweepPeriod = channel.sweepPeriod
        , sweepNegate = channel.sweepNegate
        , sweepShift = channel.sweepShift
        , envelopeStartingVolume = channel.envelopeStartingVolume
        , envelopeAdd = channel.envelopeAdd
        , envelopePeriod = channel.envelopePeriod
        , envelopeCounter = channel.envelopePeriod
        , timerValue = (2048 - frequency) * 4
        , sweepShadowFrequency = frequency
        , sweepCounter = channel.sweepPeriod
        , sweepEnabled = channel.sweepPeriod /= 0 || channel.sweepShift /= 0
        , lengthCounter =
            if channel.lengthCounter == 0 then
                64

            else
                channel.lengthCounter
        , lengthEnabled = lengthEnabled
        }

    else
        setFrequencyLengthEnabled frequency lengthEnabled channel



-- Internal


frequencyCalculation : Int -> Int -> Bool -> Int
frequencyCalculation shadowFrequency shift negate =
    let
        shiftedShadowFrequency =
            Bitwise.shiftRightZfBy shift shadowFrequency
    in
    if negate then
        shadowFrequency - shiftedShadowFrequency

    else
        shadowFrequency + shiftedShadowFrequency


dacEnabled : Int -> Bool -> Bool
dacEnabled envelopeStartingVolume envelopeAdd =
    envelopeStartingVolume /= 0x00 || envelopeAdd


minVolume : Int
minVolume =
    0


maxVolume : Int
maxVolume =
    15


volumeConversionFactor : Float
volumeConversionFactor =
    1 / toFloat maxVolume



-- Internal performance setters


setFrequencyLengthEnabled : Int -> Bool -> PulseChannel -> PulseChannel
setFrequencyLengthEnabled frequency lengthEnabled channel =
    { dutyCycle = channel.dutyCycle
    , wavePosition = channel.wavePosition
    , volume = channel.volume
    , enabled = channel.enabled
    , frequency = frequency
    , sweepPeriod = channel.sweepPeriod
    , sweepNegate = channel.sweepNegate
    , sweepShift = channel.sweepShift
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , timerValue = channel.timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = channel.sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = lengthEnabled
    }


setFrequency : Int -> PulseChannel -> PulseChannel
setFrequency frequency channel =
    { dutyCycle = channel.dutyCycle
    , wavePosition = channel.wavePosition
    , volume = channel.volume
    , enabled = channel.enabled
    , frequency = frequency
    , sweepPeriod = channel.sweepPeriod
    , sweepNegate = channel.sweepNegate
    , sweepShift = channel.sweepShift
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , timerValue = channel.timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = channel.sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setEnvelopeStartingVolumeAddPeriod : Int -> Bool -> Int -> PulseChannel -> PulseChannel
setEnvelopeStartingVolumeAddPeriod envelopeStartingVolume envelopeAdd envelopePeriod channel =
    { dutyCycle = channel.dutyCycle
    , wavePosition = channel.wavePosition
    , volume = channel.volume
    , enabled = channel.enabled
    , frequency = channel.frequency
    , sweepPeriod = channel.sweepPeriod
    , sweepNegate = channel.sweepNegate
    , sweepShift = channel.sweepShift
    , envelopeStartingVolume = envelopeStartingVolume
    , envelopeAdd = envelopeAdd
    , envelopePeriod = envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , timerValue = channel.timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = channel.sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setDutyCycleLengthCounter : DutyCycle -> Int -> PulseChannel -> PulseChannel
setDutyCycleLengthCounter dutyCycle length channel =
    { dutyCycle = dutyCycle
    , wavePosition = channel.wavePosition
    , volume = channel.volume
    , enabled = channel.enabled
    , frequency = channel.frequency
    , sweepPeriod = channel.sweepPeriod
    , sweepNegate = channel.sweepNegate
    , sweepShift = channel.sweepShift
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , timerValue = channel.timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = channel.sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = length
    , lengthEnabled = channel.lengthEnabled
    }


setSweepPeriodNegateShift : Int -> Bool -> Int -> PulseChannel -> PulseChannel
setSweepPeriodNegateShift sweepPeriod sweepNegate sweepShift channel =
    { dutyCycle = channel.dutyCycle
    , wavePosition = channel.wavePosition
    , volume = channel.volume
    , enabled = channel.enabled
    , frequency = channel.frequency
    , sweepPeriod = sweepPeriod
    , sweepNegate = sweepNegate
    , sweepShift = sweepShift
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , timerValue = channel.timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = channel.sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setWavePositionTimerValue : Int -> Int -> PulseChannel -> PulseChannel
setWavePositionTimerValue wavePosition timerValue channel =
    { dutyCycle = channel.dutyCycle
    , wavePosition = wavePosition
    , volume = channel.volume
    , enabled = channel.enabled
    , frequency = channel.frequency
    , sweepPeriod = channel.sweepPeriod
    , sweepNegate = channel.sweepNegate
    , sweepShift = channel.sweepShift
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , timerValue = timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = channel.sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setEnabledLengthCounter : Bool -> Int -> PulseChannel -> PulseChannel
setEnabledLengthCounter enabled lengthCounter channel =
    { dutyCycle = channel.dutyCycle
    , wavePosition = channel.wavePosition
    , volume = channel.volume
    , enabled = enabled
    , frequency = channel.frequency
    , sweepPeriod = channel.sweepPeriod
    , sweepNegate = channel.sweepNegate
    , sweepShift = channel.sweepShift
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , timerValue = channel.timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = channel.sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setEnabledSweepCounter : Bool -> Int -> PulseChannel -> PulseChannel
setEnabledSweepCounter enabled sweepCounter channel =
    { dutyCycle = channel.dutyCycle
    , wavePosition = channel.wavePosition
    , volume = channel.volume
    , enabled = enabled
    , frequency = channel.frequency
    , sweepPeriod = channel.sweepPeriod
    , sweepNegate = channel.sweepNegate
    , sweepShift = channel.sweepShift
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , timerValue = channel.timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setSweepCounter : Int -> PulseChannel -> PulseChannel
setSweepCounter sweepCounter channel =
    { dutyCycle = channel.dutyCycle
    , wavePosition = channel.wavePosition
    , volume = channel.volume
    , enabled = channel.enabled
    , frequency = channel.frequency
    , sweepPeriod = channel.sweepPeriod
    , sweepNegate = channel.sweepNegate
    , sweepShift = channel.sweepShift
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , timerValue = channel.timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setVolumeEnvelopeCounter : Int -> Int -> PulseChannel -> PulseChannel
setVolumeEnvelopeCounter volume envelopeCounter channel =
    { dutyCycle = channel.dutyCycle
    , wavePosition = channel.wavePosition
    , volume = volume
    , enabled = channel.enabled
    , frequency = channel.frequency
    , sweepPeriod = channel.sweepPeriod
    , sweepNegate = channel.sweepNegate
    , sweepShift = channel.sweepShift
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = envelopeCounter
    , timerValue = channel.timerValue
    , sweepShadowFrequency = channel.sweepShadowFrequency
    , sweepCounter = channel.sweepCounter
    , sweepEnabled = channel.sweepEnabled
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }
