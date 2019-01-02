module Component.APU.VolumeEnvelope exposing (EnvelopeMode(..), VolumeEnvelope, emulate, handleTrigger, init, modifySample, writeRegister)

import Bitwise
import Constants


type EnvelopeMode
    = Add
    | Subtract


type alias VolumeEnvelope =
    { startingVolume : Int
    , currentVolume : Int
    , envelopeMode : EnvelopeMode
    , envelopePeriod : Int
    , envelopeCounter : Int
    , timer : Int
    }


init : VolumeEnvelope
init =
    { startingVolume = 0x0F
    , currentVolume = 0x0F
    , envelopeMode = Add
    , envelopePeriod = 0x00
    , envelopeCounter = 0x00
    , timer = Constants.cyclesPerSecond // 64
    }


writeRegister : Int -> VolumeEnvelope -> VolumeEnvelope
writeRegister value volumeEnvelope =
    let
        updatedStartingVolume =
            value
                |> Bitwise.and 0xF0
                |> Bitwise.shiftRightZfBy 4

        updatedEnvelopeMode =
            if Bitwise.and Constants.bit4Mask value == Constants.bit4Mask then
                Add

            else
                Subtract

        updatedPeriod =
            Bitwise.and 0x07 value
    in
    { startingVolume = updatedStartingVolume
    , currentVolume = volumeEnvelope.currentVolume
    , envelopeMode = updatedEnvelopeMode
    , envelopePeriod = updatedPeriod
    , envelopeCounter = volumeEnvelope.envelopeCounter
    , timer = volumeEnvelope.timer
    }


emulate : Int -> VolumeEnvelope -> VolumeEnvelope
emulate cycles volumeEnvelope =
    let
        ( updatedTimer, timerUnderflowed ) =
            if volumeEnvelope.timer - cycles <= 0 then
                ( (Constants.cyclesPerSecond // 64) - abs (volumeEnvelope.timer - cycles), True )

            else
                ( volumeEnvelope.timer - cycles, False )

        ( updatedEnvelopeCounter, counterUnderflowed ) =
            if timerUnderflowed then
                if volumeEnvelope.envelopeCounter - 1 > 0 then
                    ( volumeEnvelope.envelopeCounter - 1, False )

                else
                    ( volumeEnvelope.envelopePeriod, True )

            else
                ( volumeEnvelope.envelopeCounter, False )

        updatedVolume =
            if counterUnderflowed && volumeEnvelope.envelopeMode == Add && volumeEnvelope.currentVolume < 15 then
                volumeEnvelope.currentVolume + 1

            else if counterUnderflowed && volumeEnvelope.envelopeMode == Subtract && volumeEnvelope.currentVolume > 0 then
                volumeEnvelope.currentVolume - 1

            else
                volumeEnvelope.currentVolume
    in
    { currentVolume = updatedVolume
    , envelopeCounter = updatedEnvelopeCounter
    , timer = updatedTimer
    , startingVolume = volumeEnvelope.startingVolume
    , envelopeMode = volumeEnvelope.envelopeMode
    , envelopePeriod = volumeEnvelope.envelopePeriod
    }


handleTrigger : VolumeEnvelope -> VolumeEnvelope
handleTrigger volumeEnvelope =
    { startingVolume = volumeEnvelope.startingVolume
    , currentVolume = volumeEnvelope.startingVolume
    , envelopeMode = volumeEnvelope.envelopeMode
    , envelopePeriod = volumeEnvelope.envelopePeriod
    , envelopeCounter = volumeEnvelope.envelopePeriod
    , timer = volumeEnvelope.timer
    }


modifySample : VolumeEnvelope -> Float -> Float
modifySample volumeEnvelope sample =
    sample * convert volumeEnvelope.currentVolume


convert : Int -> Float
convert value =
    toFloat value * (1 / 15)
