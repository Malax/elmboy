module Component.APU.Channel3 exposing (Channel3, emulate, init, writeNR30, writeNR31, writeNR32, writeNR33, writeNR34, writeWaveRam)

import Bitwise
import Component.APU.Timer as Timer exposing (Timer)
import Component.RAM as RAM exposing (RAM)
import Constants


type alias Channel3 =
    { currentSample : Float
    , waveRam : RAM
    , frequency : Int
    , wavePosition : Int
    , timer : Timer
    , dacPower : Bool
    }


init : Channel3
init =
    { currentSample = 0
    , waveRam = RAM.init 0x10
    , frequency = 0
    , wavePosition = 0
    , timer = Timer.init 0
    , dacPower = False
    }


emulate : Int -> Channel3 -> Channel3
emulate cycles channel =
    let
        ( updatedTimer, changeSample ) =
            Timer.update cycles channel.timer

        updatedWavePosition =
            if changeSample then
                remainderBy 32 (channel.wavePosition + 1)

            else
                channel.wavePosition

        updatedCurrentSample =
            if not channel.dacPower then
                0

            else if changeSample then
                sampleFromWaveRam channel.wavePosition channel.waveRam

            else
                channel.currentSample
    in
    { currentSample = updatedCurrentSample
    , waveRam = channel.waveRam
    , frequency = channel.frequency
    , wavePosition = updatedWavePosition
    , timer = updatedTimer
    , dacPower = channel.dacPower
    }


sampleFromWaveRam : Int -> RAM -> Float
sampleFromWaveRam index ram =
    let
        sanitizedIndex =
            remainderBy 32 index

        byte =
            RAM.readWord8 ram (sanitizedIndex // 2)

        rawSample =
            if remainderBy 2 sanitizedIndex == 0 then
                Bitwise.and 0x0F byte

            else
                Bitwise.shiftRightZfBy 4 byte
    in
    toFloat rawSample * (1 / 15)


writeWaveRam : Int -> Int -> Channel3 -> Channel3
writeWaveRam address value channel =
    { currentSample = channel.currentSample
    , waveRam = RAM.writeWord8 address value channel.waveRam
    , frequency = channel.frequency
    , wavePosition = channel.wavePosition
    , timer = channel.timer
    , dacPower = channel.dacPower
    }


writeNR30 : Int -> Channel3 -> Channel3
writeNR30 value channel =
    { currentSample = channel.currentSample
    , waveRam = channel.waveRam
    , frequency = channel.frequency
    , wavePosition = channel.wavePosition
    , timer = channel.timer
    , dacPower = Bitwise.and Constants.bit7Mask value == Constants.bit7Mask
    }


writeNR31 : Int -> Channel3 -> Channel3
writeNR31 value channel =
    channel


writeNR32 : Int -> Channel3 -> Channel3
writeNR32 value channel =
    channel


writeNR33 : Int -> Channel3 -> Channel3
writeNR33 value channel =
    { currentSample = channel.currentSample
    , waveRam = channel.waveRam
    , wavePosition = channel.wavePosition
    , timer = channel.timer
    , frequency = Bitwise.or (Bitwise.and 0x0700 channel.frequency) value
    , dacPower = channel.dacPower
    }


writeNR34 : Int -> Channel3 -> Channel3
writeNR34 value channel =
    let
        isTrigger =
            Bitwise.and Constants.bit7Mask value == Constants.bit7Mask

        updatedFrequency =
            value
                |> Bitwise.and 0x07
                |> Bitwise.shiftLeftBy 8
                |> Bitwise.or (Bitwise.and 0xFF channel.frequency)
    in
    { currentSample = channel.currentSample
    , waveRam = channel.waveRam
    , wavePosition = 0
    , timer = Timer.init ((2048 - updatedFrequency) * 2)
    , frequency = updatedFrequency
    , dacPower = channel.dacPower
    }
