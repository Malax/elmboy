module Component.APU.Channel2 exposing (Channel2, emulate, init, writeNR21, writeNR22, writeNR23, writeNR24)

import Array exposing (Array)
import Bitwise
import Component.APU.Constants as APUConstants
import Component.APU.SquareWave as SquareWave exposing (DutyCycle(..))
import Constants


type alias Channel2 =
    { dutyCycle : DutyCycle
    , frequency : Int
    , timerValue : Int
    , dutyCycleIndex : Int
    , cycleAccumulator : Int
    , lengthCounter : Int
    , lengthEnabled : Bool
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
    }


emulate : Int -> Channel2 -> Channel2
emulate cycles channel2 =
    let
        ( updatedCycleAccumulator, generateSample ) =
            ( remainderBy APUConstants.cyclesPerSample (channel2.cycleAccumulator + cycles), channel2.cycleAccumulator + cycles >= APUConstants.cyclesPerSample )

        ( updatedTimerValue, timerUnderflowed ) =
            let
                x =
                    channel2.timerValue - cycles

                underflowed =
                    x <= 0

                v =
                    if underflowed then
                        (2048 - channel2.frequency) * 4 + x

                    else
                        x
            in
            ( v, underflowed )

        updatedCycleIndex =
            if timerUnderflowed then
                remainderBy 8 (channel2.dutyCycleIndex + 1)

            else
                channel2.dutyCycleIndex

        sampleX =
            if generateSample then
                SquareWave.sample channel2.dutyCycle updatedCycleIndex

            else
                0
    in
    { dutyCycle = channel2.dutyCycle
    , frequency = channel2.frequency
    , timerValue = updatedTimerValue
    , dutyCycleIndex = updatedCycleIndex
    , cycleAccumulator = updatedCycleAccumulator
    , lengthCounter = channel2.lengthCounter
    , lengthEnabled = channel2.lengthEnabled
    , currentSample = sampleX
    }


writeNR21 : Int -> Channel2 -> Channel2
writeNR21 value channel =
    channel


writeNR22 : Int -> Channel2 -> Channel2
writeNR22 value channel =
    channel


writeNR23 : Int -> Channel2 -> Channel2
writeNR23 value channel2 =
    { dutyCycle = channel2.dutyCycle
    , frequency = Bitwise.or (Bitwise.and 0x0700 channel2.frequency) value
    , timerValue = channel2.timerValue
    , dutyCycleIndex = channel2.dutyCycleIndex
    , cycleAccumulator = channel2.cycleAccumulator
    , lengthCounter = channel2.lengthCounter
    , lengthEnabled = channel2.lengthEnabled
    , currentSample = channel2.currentSample
    }


writeNR24 : Int -> Channel2 -> Channel2
writeNR24 value channel2 =
    let
        updatedFrequency =
            value
                |> Bitwise.and 0x07
                |> Bitwise.shiftLeftBy 8
                |> Bitwise.or (Bitwise.and 0xFF channel2.frequency)
    in
    { dutyCycle = channel2.dutyCycle
    , frequency = updatedFrequency
    , timerValue = channel2.timerValue
    , dutyCycleIndex = channel2.dutyCycleIndex
    , cycleAccumulator = channel2.cycleAccumulator
    , lengthCounter = channel2.lengthCounter
    , lengthEnabled = channel2.lengthEnabled
    , currentSample = channel2.currentSample
    }
