module Component.APU.WaveChannel exposing (WaveChannel, clockLengthCounter, clockTimer, init, sample, writeNRx0, writeNRx1, writeNRx2, writeNRx3, writeNRx4, writeWaveRam)

import Bitwise
import Component.APU.Constants as APUConstants
import Component.RAM as RAM exposing (RAM)
import Constants


type alias WaveChannel =
    { waveRam : RAM
    , frequency : Int
    , wavePosition : Int
    , timerValue : Int
    , dacPower : Bool
    , enabled : Bool
    , volume : Int

    -- Length
    , lengthCounter : Int
    , lengthEnabled : Bool
    }


init : WaveChannel
init =
    { waveRam = RAM.init sampleCount
    , frequency = 0
    , wavePosition = 0
    , timerValue = 0
    , dacPower = False
    , enabled = False
    , volume = 0
    , lengthCounter = 0
    , lengthEnabled = False
    }


clockTimer : Int -> WaveChannel -> WaveChannel
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
                remainderBy sampleCount (channel.wavePosition + 1)

            else
                channel.wavePosition
    in
    setWavePositionTimerValue wavePosition timerValue channel


clockLengthCounter : WaveChannel -> WaveChannel
clockLengthCounter channel =
    if channel.lengthEnabled && channel.lengthCounter > 0 then
        setEnabledLengthCounter (channel.lengthCounter - 1 > 0) (channel.lengthCounter - 1) channel

    else
        channel


sample : WaveChannel -> Float
sample channel =
    if channel.enabled && channel.dacPower then
        (toFloat channel.volume * (1 / 15)) * sampleFromWaveRam channel.wavePosition channel.waveRam

    else
        APUConstants.silence


writeNRx0 : Int -> WaveChannel -> WaveChannel
writeNRx0 value channel =
    let
        dacPower =
            Bitwise.and Constants.bit7Mask value == Constants.bit7Mask
    in
    setDacPower dacPower channel


writeNRx1 : Int -> WaveChannel -> WaveChannel
writeNRx1 value channel =
    { waveRam = channel.waveRam
    , frequency = channel.frequency
    , wavePosition = channel.wavePosition
    , timerValue = channel.timerValue
    , dacPower = channel.dacPower
    , enabled = channel.enabled
    , volume = channel.volume
    , lengthCounter = 256 - value
    , lengthEnabled = channel.lengthEnabled
    }


writeNRx2 : Int -> WaveChannel -> WaveChannel
writeNRx2 value channel =
    let
        volume =
            case Bitwise.shiftRightZfBy 5 value of
                0x00 ->
                    0

                0x01 ->
                    maxVolume

                0x02 ->
                    maxVolume // 2

                _ ->
                    maxVolume // 4
    in
    setVolume volume channel


writeNRx3 : Int -> WaveChannel -> WaveChannel
writeNRx3 value channel =
    let
        frequency =
            Bitwise.or (Bitwise.and 0x0700 channel.frequency) value
    in
    setFrequency frequency channel


writeNRx4 : Int -> WaveChannel -> WaveChannel
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
        { waveRam = channel.waveRam
        , frequency = frequency
        , wavePosition = 0
        , timerValue = (2048 - frequency) * 4
        , dacPower = channel.dacPower
        , enabled = channel.dacPower
        , volume = channel.volume
        , lengthCounter =
            if channel.lengthCounter == 0 then
                256

            else
                channel.lengthCounter
        , lengthEnabled = lengthEnabled
        }

    else
        setFrequencyLengthEnabled frequency lengthEnabled channel


writeWaveRam : Int -> Int -> WaveChannel -> WaveChannel
writeWaveRam address value channel =
    { waveRam = RAM.writeWord8 address value channel.waveRam
    , frequency = channel.frequency
    , wavePosition = channel.wavePosition
    , timerValue = channel.timerValue
    , dacPower = channel.dacPower
    , enabled = channel.enabled
    , volume = channel.volume
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }



-- Internal


sampleFromWaveRam : Int -> RAM -> Float
sampleFromWaveRam index ram =
    let
        sanitizedIndex =
            remainderBy sampleCount index

        byte =
            RAM.readWord8 ram (sanitizedIndex // 2)

        rawSample =
            if remainderBy 2 sanitizedIndex == 0 then
                Bitwise.and 0x0F byte

            else
                Bitwise.shiftRightZfBy 4 byte
    in
    toFloat rawSample * (1 / 15)


sampleCount : Int
sampleCount =
    32


maxVolume : Int
maxVolume =
    15



-- Perf


setEnabledLengthCounter : Bool -> Int -> WaveChannel -> WaveChannel
setEnabledLengthCounter enabled lengthCounter channel =
    { waveRam = channel.waveRam
    , frequency = channel.frequency
    , wavePosition = channel.wavePosition
    , timerValue = channel.timerValue
    , dacPower = channel.dacPower
    , enabled = enabled
    , volume = channel.volume
    , lengthCounter = lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setFrequencyLengthEnabled : Int -> Bool -> WaveChannel -> WaveChannel
setFrequencyLengthEnabled frequency lengthEnabled channel =
    { waveRam = channel.waveRam
    , frequency = frequency
    , wavePosition = channel.wavePosition
    , timerValue = channel.timerValue
    , dacPower = channel.dacPower
    , enabled = channel.enabled
    , volume = channel.volume
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = lengthEnabled
    }


setDacPower : Bool -> WaveChannel -> WaveChannel
setDacPower dacPower channel =
    { waveRam = channel.waveRam
    , frequency = channel.frequency
    , wavePosition = channel.wavePosition
    , timerValue = channel.timerValue
    , dacPower = dacPower
    , enabled = channel.enabled
    , volume = channel.volume
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setFrequency : Int -> WaveChannel -> WaveChannel
setFrequency frequency channel =
    { waveRam = channel.waveRam
    , frequency = frequency
    , wavePosition = channel.wavePosition
    , timerValue = channel.timerValue
    , dacPower = channel.dacPower
    , enabled = channel.enabled
    , volume = channel.volume
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setVolume : Int -> WaveChannel -> WaveChannel
setVolume volume channel =
    { waveRam = channel.waveRam
    , frequency = channel.frequency
    , wavePosition = channel.wavePosition
    , timerValue = channel.timerValue
    , dacPower = channel.dacPower
    , enabled = channel.enabled
    , volume = volume
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }


setWavePositionTimerValue : Int -> Int -> WaveChannel -> WaveChannel
setWavePositionTimerValue wavePosition timerValue channel =
    { waveRam = channel.waveRam
    , frequency = channel.frequency
    , wavePosition = wavePosition
    , timerValue = timerValue
    , dacPower = channel.dacPower
    , enabled = channel.enabled
    , volume = channel.volume
    , lengthCounter = channel.lengthCounter
    , lengthEnabled = channel.lengthEnabled
    }
