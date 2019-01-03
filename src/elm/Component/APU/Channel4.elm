module Component.APU.Channel4 exposing (Channel4, emulate, init, writeNR41, writeNR42, writeNR43, writeNR44)

import Bitwise
import Component.APU.Constants as APUConstants
import Component.APU.Timer as Timer exposing (Timer)
import Component.APU.VolumeEnvelope as VolumeEnvelope exposing (VolumeEnvelope)
import Constants


type WidthMode
    = FifteenBit
    | SevenBit


type alias Channel4 =
    { currentSample : Float
    , bits : Int
    , width : WidthMode
    , clockShift : Int
    , divisorCode : Int
    , volumeEnvelope : VolumeEnvelope
    , timer : Timer
    }


init : Channel4
init =
    { currentSample = 0
    , bits = 0xAA
    , clockShift = 0x00
    , width = FifteenBit
    , divisorCode = 0x00
    , volumeEnvelope = VolumeEnvelope.init
    , timer = Timer.init 0
    }


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

        sample =
            if bit0 == 0x01 then
                -1

            else
                1
    in
    ( updatedBits, sample )


divisor : Int -> Int
divisor code =
    case code of
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

        7 ->
            123

        _ ->
            0


emulate : Int -> Channel4 -> Channel4
emulate cycles channel =
    let
        ( updatedTimer, timerUnderflowed ) =
            Timer.update cycles channel.timer

        updatedVolumeEnvelope =
            VolumeEnvelope.emulate cycles channel.volumeEnvelope

        ( updatedBits, sampleX ) =
            if timerUnderflowed then
                shiftRegister channel.bits channel.width
                    |> Tuple.mapSecond (VolumeEnvelope.modifySample updatedVolumeEnvelope)

            else
                ( channel.bits, channel.currentSample )
    in
    { currentSample = sampleX
    , bits = updatedBits
    , clockShift = channel.clockShift
    , width = channel.width
    , divisorCode = channel.divisorCode
    , volumeEnvelope = updatedVolumeEnvelope
    , timer = updatedTimer
    }


writeNR41 : Int -> Channel4 -> Channel4
writeNR41 value channel =
    channel


writeNR42 : Int -> Channel4 -> Channel4
writeNR42 value channel =
    { currentSample = channel.currentSample
    , bits = channel.bits
    , clockShift = channel.clockShift
    , width = channel.width
    , divisorCode = channel.divisorCode
    , volumeEnvelope = VolumeEnvelope.writeRegister value channel.volumeEnvelope
    , timer = channel.timer
    }


writeNR43 : Int -> Channel4 -> Channel4
writeNR43 value channel =
    let
        clockShift =
            Bitwise.shiftRightZfBy 4 value

        divisorCode =
            Bitwise.and 0x07 value

        widthMode =
            if Bitwise.and Constants.bit3Mask value == Constants.bit3Mask then
                SevenBit

            else
                FifteenBit
    in
    { currentSample = channel.currentSample
    , bits = channel.bits
    , clockShift = clockShift
    , width = widthMode
    , divisorCode = divisorCode
    , volumeEnvelope = channel.volumeEnvelope
    , timer = channel.timer
    }


writeNR44 : Int -> Channel4 -> Channel4
writeNR44 value channel =
    let
        isTrigger =
            Bitwise.and Constants.bit7Mask value == Constants.bit7Mask
    in
    { currentSample = channel.currentSample
    , bits =
        if isTrigger then
            0x7FFF

        else
            channel.bits
    , clockShift = channel.clockShift
    , width = channel.width
    , divisorCode = channel.divisorCode
    , volumeEnvelope = VolumeEnvelope.handleTrigger channel.volumeEnvelope
    , timer = Timer.init (Bitwise.shiftLeftBy channel.clockShift (divisor channel.divisorCode))
    }
