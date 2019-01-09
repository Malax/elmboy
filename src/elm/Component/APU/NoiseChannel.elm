module Component.APU.NoiseChannel exposing
    ( NoiseChannel
    , clockLengthCounter
    , clockTimer
    , clockVolumeEnvelope
    , init
    , readNRx1
    , readNRx2
    , readNRx3
    , readNRx4
    , reset
    , sample
    , setEnabledLengthCounter
    , writeNRx1
    , writeNRx2
    , writeNRx3
    , writeNRx4
    )

import Bitwise
import Component.APU.Constants as APUConstants
import Constants
import Util


type WidthMode
    = FifteenBit
    | SevenBit


type alias NoiseChannel =
    { bits : Int
    , width : WidthMode
    , clockShift : Int
    , divisor : Int
    , timerCounter : Int
    , enabled : Bool
    , volume : Int
    , lastShiftRegisterOutput : Float

    -- Volume Envelope
    , envelopeStartingVolume : Int
    , envelopeAdd : Bool
    , envelopePeriod : Int
    , envelopeCounter : Int

    -- Length
    , lengthCounter : Int
    , lengthEnabled : Bool
    }


init : NoiseChannel
init =
    { bits = 0xFFFF
    , width = FifteenBit
    , clockShift = 0
    , divisor = 8
    , timerCounter = 0
    , enabled = False
    , volume = 0
    , lastShiftRegisterOutput = 0
    , envelopeStartingVolume = 0
    , envelopeAdd = False
    , envelopePeriod = 0
    , envelopeCounter = 0
    , lengthCounter = 0
    , lengthEnabled = False
    }


clockTimer : Int -> NoiseChannel -> NoiseChannel
clockTimer cycles channel =
    let
        triggered =
            channel.timerCounter - cycles <= 0

        timerCounter =
            if not triggered then
                channel.timerCounter - cycles

            else
                Bitwise.shiftLeftBy channel.clockShift channel.divisor + (channel.timerCounter - cycles)

        ( bits, lastShiftRegisterOutput ) =
            if triggered then
                shiftRegister channel.bits channel.width

            else
                ( channel.bits, channel.lastShiftRegisterOutput )
    in
    setLastShiftRegisterOutputBitsTimerCounter lastShiftRegisterOutput bits timerCounter channel


clockLengthCounter : NoiseChannel -> NoiseChannel
clockLengthCounter channel =
    if channel.lengthEnabled && channel.lengthCounter > 0 then
        setEnabledLengthCounter (channel.lengthCounter - 1 > 0) (channel.lengthCounter - 1) channel

    else
        channel


clockVolumeEnvelope : NoiseChannel -> NoiseChannel
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
                    channel.volume - 1

                else if not channel.envelopeAdd && channel.volume > minVolume then
                    channel.volume - 1

                else
                    channel.volume

            else
                channel.volume
    in
    setVolumeEnvelopeCounter volume envelopeCounter channel


sample : NoiseChannel -> Float
sample channel =
    if channel.enabled then
        (toFloat channel.volume * volumeConversionFactor) * channel.lastShiftRegisterOutput

    else
        APUConstants.silence


reset : NoiseChannel -> NoiseChannel
reset _ =
    init


writeNRx1 : Int -> NoiseChannel -> NoiseChannel
writeNRx1 value channel =
    let
        length =
            64 - Bitwise.and 0x3F value
    in
    setLengthCounter length channel


writeNRx2 : Int -> NoiseChannel -> NoiseChannel
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


writeNRx3 : Int -> NoiseChannel -> NoiseChannel
writeNRx3 value channel =
    let
        clockShift =
            Bitwise.shiftRightZfBy 4 value

        divisor =
            case Bitwise.and 0x07 value of
                0 ->
                    8

                1 ->
                    16

                2 ->
                    32

                3 ->
                    48

                4 ->
                    64

                5 ->
                    80

                6 ->
                    96

                _ ->
                    123

        widthMode =
            if Bitwise.and Constants.bit3Mask value == Constants.bit3Mask then
                SevenBit

            else
                FifteenBit
    in
    setClockShiftDivisorWidth clockShift divisor widthMode channel


writeNRx4 : Int -> NoiseChannel -> NoiseChannel
writeNRx4 value channel =
    let
        trigger =
            Bitwise.and Constants.bit7Mask value == Constants.bit7Mask

        lengthEnabled =
            Bitwise.and Constants.bit6Mask value == Constants.bit6Mask
    in
    if trigger then
        { bits = 0x7FFF
        , width = channel.width
        , clockShift = channel.clockShift
        , divisor = channel.divisor
        , timerCounter = Bitwise.shiftLeftBy channel.clockShift channel.divisor
        , volume = channel.envelopeStartingVolume
        , lastShiftRegisterOutput = channel.lastShiftRegisterOutput
        , enabled = dacEnabled channel.envelopeStartingVolume channel.envelopeAdd
        , envelopeStartingVolume = channel.envelopeStartingVolume
        , envelopeAdd = channel.envelopeAdd
        , envelopePeriod = channel.envelopePeriod
        , envelopeCounter = channel.envelopePeriod
        , lengthCounter =
            if channel.lengthCounter == 0 then
                64

            else
                channel.lengthCounter
        , lengthEnabled = lengthEnabled
        }

    else
        setLengthEnabled lengthEnabled channel


readNRx1 : NoiseChannel -> Int
readNRx1 channel =
    -- This is intentional, register always reads as 0xFF!
    0xFF


readNRx2 : NoiseChannel -> Int
readNRx2 channel =
    channel.envelopeStartingVolume
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit channel.envelopeAdd)
        |> Bitwise.shiftLeftBy 3
        |> Bitwise.or channel.envelopePeriod


readNRx3 : NoiseChannel -> Int
readNRx3 channel =
    let
        widthModeBit =
            case channel.width of
                FifteenBit ->
                    0

                SevenBit ->
                    1

        divisorCode =
            case channel.divisor of
                8 ->
                    0

                16 ->
                    1

                32 ->
                    2

                48 ->
                    3

                64 ->
                    4

                80 ->
                    5

                96 ->
                    6

                _ ->
                    7
    in
    channel.clockShift
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or widthModeBit
        |> Bitwise.shiftLeftBy 3
        |> Bitwise.or divisorCode


readNRx4 : NoiseChannel -> Int
readNRx4 channel =
    Util.boolToBit channel.lengthEnabled
        |> Bitwise.shiftLeftBy 6
        |> Bitwise.or 0xBF



-- Internal


dacEnabled : Int -> Bool -> Bool
dacEnabled envelopeStartingVolume envelopeAdd =
    envelopeStartingVolume /= 0x00 || envelopeAdd


shiftRegister : Int -> WidthMode -> ( Int, Float )
shiftRegister bits widthMode =
    let
        bit0 =
            Bitwise.and 0x01 bits

        bit1 =
            bits |> Bitwise.shiftRightZfBy 1 |> Bitwise.and 0x01

        newHighBit =
            Bitwise.xor bit0 bit1

        updatedBits =
            case widthMode of
                FifteenBit ->
                    bits
                        |> Bitwise.shiftRightZfBy 1
                        |> Bitwise.or (Bitwise.shiftLeftBy 14 newHighBit)

                SevenBit ->
                    bits
                        |> Bitwise.shiftRightZfBy 1
                        |> Bitwise.and 0xBF
                        |> Bitwise.or (Bitwise.shiftLeftBy 5 newHighBit)

        generatedSample =
            if bit0 == 0x01 then
                -1

            else
                1
    in
    ( updatedBits, generatedSample )


minVolume : Int
minVolume =
    0


maxVolume : Int
maxVolume =
    15


volumeConversionFactor : Float
volumeConversionFactor =
    1 / toFloat maxVolume



-- Perf


setLengthEnabled : Bool -> NoiseChannel -> NoiseChannel
setLengthEnabled lengthEnabled channel =
    { bits = channel.bits
    , width = channel.width
    , clockShift = channel.clockShift
    , divisor = channel.divisor
    , timerCounter = channel.timerCounter
    , volume = channel.volume
    , lastShiftRegisterOutput = channel.lastShiftRegisterOutput
    , enabled = channel.enabled
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = lengthEnabled
    }


setClockShiftDivisorWidth : Int -> Int -> WidthMode -> NoiseChannel -> NoiseChannel
setClockShiftDivisorWidth clockShift divisor width channel =
    { bits = channel.bits
    , width = width
    , clockShift = clockShift
    , divisor = divisor
    , timerCounter = channel.timerCounter
    , volume = channel.volume
    , lastShiftRegisterOutput = channel.lastShiftRegisterOutput
    , enabled = channel.enabled
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setEnvelopeStartingVolumeAddPeriod : Int -> Bool -> Int -> NoiseChannel -> NoiseChannel
setEnvelopeStartingVolumeAddPeriod envelopeStartingVolume envelopeAdd envelopePeriod channel =
    { bits = channel.bits
    , width = channel.width
    , clockShift = channel.clockShift
    , divisor = channel.divisor
    , timerCounter = channel.timerCounter
    , volume = channel.volume
    , lastShiftRegisterOutput = channel.lastShiftRegisterOutput
    , enabled = channel.enabled
    , envelopeStartingVolume = envelopeStartingVolume
    , envelopeAdd = envelopeAdd
    , envelopePeriod = envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setLengthCounter : Int -> NoiseChannel -> NoiseChannel
setLengthCounter lengthCounter channel =
    { bits = channel.bits
    , width = channel.width
    , clockShift = channel.clockShift
    , divisor = channel.divisor
    , timerCounter = channel.timerCounter
    , volume = channel.volume
    , lastShiftRegisterOutput = channel.lastShiftRegisterOutput
    , enabled = channel.enabled
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , lengthCounter = lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setEnabledLengthCounter : Bool -> Int -> NoiseChannel -> NoiseChannel
setEnabledLengthCounter enabled lengthCounter channel =
    { bits = channel.bits
    , width = channel.width
    , clockShift = channel.clockShift
    , divisor = channel.divisor
    , timerCounter = channel.timerCounter
    , volume = channel.volume
    , lastShiftRegisterOutput = channel.lastShiftRegisterOutput
    , enabled = enabled
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , lengthCounter = lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setLastShiftRegisterOutputBitsTimerCounter : Float -> Int -> Int -> NoiseChannel -> NoiseChannel
setLastShiftRegisterOutputBitsTimerCounter lastShiftRegisterOutput bits timerCounter channel =
    { bits = bits
    , width = channel.width
    , clockShift = channel.clockShift
    , divisor = channel.divisor
    , timerCounter = timerCounter
    , volume = channel.volume
    , lastShiftRegisterOutput = lastShiftRegisterOutput
    , enabled = channel.enabled
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = channel.envelopeCounter
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setVolumeEnvelopeCounter : Int -> Int -> NoiseChannel -> NoiseChannel
setVolumeEnvelopeCounter volume envelopeCounter channel =
    { bits = channel.bits
    , width = channel.width
    , clockShift = channel.clockShift
    , divisor = channel.divisor
    , timerCounter = channel.timerCounter
    , volume = volume
    , lastShiftRegisterOutput = channel.lastShiftRegisterOutput
    , enabled = channel.enabled
    , envelopeStartingVolume = channel.envelopeStartingVolume
    , envelopeAdd = channel.envelopeAdd
    , envelopePeriod = channel.envelopePeriod
    , envelopeCounter = envelopeCounter
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }
