module Component.APU exposing
    ( APU
    , drainAudioBuffer
    , emulate
    , init
    , writeNR10
    , writeNR11
    , writeNR12
    , writeNR13
    , writeNR14
    , writeNR21
    , writeNR22
    , writeNR23
    , writeNR24
    , writeNR30
    , writeNR31
    , writeNR32
    , writeNR33
    , writeNR34
    , writeNR41
    , writeNR42
    , writeNR43
    , writeNR44
    , writeNR50
    , writeNR51
    , writeNR52
    )

import Array exposing (Array)
import Component.APU.Channel1 as Channel1 exposing (Channel1)
import Component.APU.Channel2 as Channel2 exposing (Channel2)
import Component.APU.Channel3 as Channel3 exposing (Channel3)
import Component.APU.Channel4 as Channel4 exposing (Channel4)
import Component.APU.Constants as APUConstants
import Component.RAM as RAM exposing (RAM)
import Constants


type alias APU =
    { channel1 : Channel1
    , channel2 : Channel2
    , channel3 : Channel3
    , channel4 : Channel4
    , sampleBuffer : Array Float
    , cycleAccumulator : Int
    }


init : APU
init =
    { channel1 = Channel1.init
    , channel2 = Channel2.init
    , channel3 = Channel3.init
    , channel4 = Channel4.init
    , sampleBuffer = Array.empty
    , cycleAccumulator = 0
    }


emulate : Int -> APU -> APU
emulate cycles apu =
    let
        ( updatedCycleAccumulator, generateSample ) =
            ( remainderBy APUConstants.cyclesPerSample (apu.cycleAccumulator + cycles), apu.cycleAccumulator + cycles >= APUConstants.cyclesPerSample )

        updatedChannel1 =
            Channel1.emulate cycles apu.channel1

        updatedChannel2 =
            Channel2.emulate cycles apu.channel2

        updatedChannel3 =
            Channel3.emulate cycles apu.channel3

        updatedChannel4 =
            Channel4.emulate cycles apu.channel4

        updatedSampleBuffer =
            if generateSample then
                Array.push ((updatedChannel1.currentSample + updatedChannel2.currentSample + updatedChannel3.currentSample + updatedChannel4.currentSample) / 4) apu.sampleBuffer

            else
                apu.sampleBuffer
    in
    { sampleBuffer = updatedSampleBuffer
    , cycleAccumulator = updatedCycleAccumulator
    , channel1 = updatedChannel1
    , channel2 = updatedChannel2
    , channel3 = updatedChannel3
    , channel4 = updatedChannel4
    }


drainAudioBuffer : APU -> ( APU, Array Float )
drainAudioBuffer apu =
    ( { sampleBuffer = Array.empty
      , cycleAccumulator = apu.cycleAccumulator
      , channel1 = apu.channel1
      , channel2 = apu.channel2
      , channel3 = apu.channel3
      , channel4 = apu.channel4
      }
    , apu.sampleBuffer
    )


writeNR10 : Int -> APU -> APU
writeNR10 value apu =
    setChannel1 (Channel1.writeNR10 value apu.channel1) apu


writeNR11 : Int -> APU -> APU
writeNR11 value apu =
    setChannel1 (Channel1.writeNR11 value apu.channel1) apu


writeNR12 : Int -> APU -> APU
writeNR12 value apu =
    setChannel1 (Channel1.writeNR12 value apu.channel1) apu


writeNR13 : Int -> APU -> APU
writeNR13 value apu =
    setChannel1 (Channel1.writeNR13 value apu.channel1) apu


writeNR14 : Int -> APU -> APU
writeNR14 value apu =
    setChannel1 (Channel1.writeNR14 value apu.channel1) apu


writeNR21 : Int -> APU -> APU
writeNR21 value apu =
    setChannel2 (Channel2.writeNR21 value apu.channel2) apu


writeNR22 : Int -> APU -> APU
writeNR22 value apu =
    setChannel2 (Channel2.writeNR22 value apu.channel2) apu


writeNR23 : Int -> APU -> APU
writeNR23 value apu =
    setChannel2 (Channel2.writeNR23 value apu.channel2) apu


writeNR24 : Int -> APU -> APU
writeNR24 value apu =
    setChannel2 (Channel2.writeNR24 value apu.channel2) apu


writeNR30 : Int -> APU -> APU
writeNR30 value apu =
    setChannel3 (Channel3.writeNR30 value apu.channel3) apu


writeNR31 : Int -> APU -> APU
writeNR31 value apu =
    setChannel3 (Channel3.writeNR31 value apu.channel3) apu


writeNR32 : Int -> APU -> APU
writeNR32 value apu =
    setChannel3 (Channel3.writeNR32 value apu.channel3) apu


writeNR33 : Int -> APU -> APU
writeNR33 value apu =
    setChannel3 (Channel3.writeNR33 value apu.channel3) apu


writeNR34 : Int -> APU -> APU
writeNR34 value apu =
    setChannel3 (Channel3.writeNR34 value apu.channel3) apu


writeNR41 : Int -> APU -> APU
writeNR41 value apu =
    setChannel4 (Channel4.writeNR41 value apu.channel4) apu


writeNR42 : Int -> APU -> APU
writeNR42 value apu =
    setChannel4 (Channel4.writeNR42 value apu.channel4) apu


writeNR43 : Int -> APU -> APU
writeNR43 value apu =
    setChannel4 (Channel4.writeNR43 value apu.channel4) apu


writeNR44 : Int -> APU -> APU
writeNR44 value apu =
    setChannel4 (Channel4.writeNR44 value apu.channel4) apu


writeNR50 : Int -> APU -> APU
writeNR50 value apu =
    -- TODO: Implement
    apu


writeNR51 : Int -> APU -> APU
writeNR51 value apu =
    -- TODO: Implement
    apu


writeNR52 : Int -> APU -> APU
writeNR52 value apu =
    -- TODO: Implement
    apu



-- Internal


setChannel1 : Channel1 -> APU -> APU
setChannel1 channel apu =
    { channel1 = channel
    , channel2 = apu.channel2
    , channel3 = apu.channel3
    , channel4 = apu.channel4
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    }


setChannel2 : Channel2 -> APU -> APU
setChannel2 channel apu =
    { channel1 = apu.channel1
    , channel2 = channel
    , channel3 = apu.channel3
    , channel4 = apu.channel4
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    }


setChannel3 : Channel3 -> APU -> APU
setChannel3 channel apu =
    { channel1 = apu.channel1
    , channel2 = apu.channel2
    , channel3 = channel
    , channel4 = apu.channel4
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    }


setChannel4 : Channel4 -> APU -> APU
setChannel4 channel apu =
    { channel1 = apu.channel1
    , channel2 = apu.channel2
    , channel3 = apu.channel3
    , channel4 = channel
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    }
