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
    , writeWaveRam
    )

import Array exposing (Array)
import Component.APU.Constants as APUConstants
import Component.APU.NoiseChannel as NoiseChannel exposing (NoiseChannel)
import Component.APU.PulseChannel as PulseChannel exposing (PulseChannel)
import Component.APU.WaveChannel as WaveChannel exposing (WaveChannel)
import Constants


type alias APU =
    { channel1 : PulseChannel
    , channel2 : PulseChannel
    , channel3 : WaveChannel
    , channel4 : NoiseChannel
    , sampleBuffer : Array Float
    , cycleAccumulator : Int
    , frameSequencerCounter : Int
    , frameSequence : Int
    , enabled : Bool
    }


init : Bool -> APU
init enabled =
    { channel1 = PulseChannel.init
    , channel2 = PulseChannel.init
    , channel3 = WaveChannel.init
    , channel4 = NoiseChannel.init
    , sampleBuffer = Array.empty
    , cycleAccumulator = 0
    , frameSequencerCounter = 0
    , frameSequence = 0
    , enabled = enabled
    }


emulate : Int -> APU -> APU
emulate cycles apu =
    if apu.enabled then
        let
            ( updatedCycleAccumulator, generateSample ) =
                ( remainderBy APUConstants.cyclesPerSample (apu.cycleAccumulator + cycles), apu.cycleAccumulator + cycles >= APUConstants.cyclesPerSample )

            frameSequencerTriggered =
                apu.frameSequencerCounter - cycles <= 0

            frameSequencerCounter =
                if frameSequencerTriggered then
                    Constants.cyclesPerSecond // 512 + (apu.frameSequencerCounter - cycles)

                else
                    apu.frameSequencerCounter - cycles

            frameSequence =
                if frameSequencerTriggered then
                    remainderBy 8 (apu.frameSequence + 1)

                else
                    apu.frameSequence

            updatedChannel1 =
                apu.channel1
                    |> updateChannelAfterFrameSequencer frameSequencerTriggered frameSequence PulseChannel.clockLengthCounter PulseChannel.clockVolumeEnvelope PulseChannel.clockSweepUnit
                    |> PulseChannel.clockTimer cycles

            updatedChannel2 =
                apu.channel2
                    |> updateChannelAfterFrameSequencer frameSequencerTriggered frameSequence PulseChannel.clockLengthCounter PulseChannel.clockVolumeEnvelope PulseChannel.clockSweepUnit
                    |> PulseChannel.clockTimer cycles

            updatedChannel3 =
                apu.channel3
                    |> updateChannelAfterFrameSequencer frameSequencerTriggered frameSequence WaveChannel.clockLengthCounter identity identity
                    |> WaveChannel.clockTimer cycles

            updatedChannel4 =
                apu.channel4
                    |> updateChannelAfterFrameSequencer frameSequencerTriggered frameSequence NoiseChannel.clockLengthCounter NoiseChannel.clockVolumeEnvelope identity
                    |> NoiseChannel.clockTimer cycles

            updatedSampleBuffer =
                if generateSample then
                    Array.push ((PulseChannel.sample updatedChannel1 + PulseChannel.sample updatedChannel2 + WaveChannel.sample updatedChannel3 + NoiseChannel.sample updatedChannel4) / 4) apu.sampleBuffer

                else
                    apu.sampleBuffer
        in
        { sampleBuffer = updatedSampleBuffer
        , cycleAccumulator = updatedCycleAccumulator
        , channel1 = updatedChannel1
        , channel2 = updatedChannel2
        , channel3 = updatedChannel3
        , channel4 = updatedChannel4
        , frameSequencerCounter = frameSequencerCounter
        , frameSequence = frameSequence
        , enabled = apu.enabled
        }

    else
        apu


updateChannelAfterFrameSequencer : Bool -> Int -> (a -> a) -> (a -> a) -> (a -> a) -> a -> a
updateChannelAfterFrameSequencer triggered seq clockLength clockVolEnv clockSweep channel =
    if triggered then
        case seq of
            0 ->
                clockLength channel

            2 ->
                channel |> clockLength |> clockSweep

            4 ->
                clockLength channel

            6 ->
                channel |> clockLength |> clockSweep

            7 ->
                clockVolEnv channel

            _ ->
                channel

    else
        channel


drainAudioBuffer : APU -> ( APU, Array Float )
drainAudioBuffer apu =
    if apu.enabled then
        ( { sampleBuffer = Array.empty
          , cycleAccumulator = apu.cycleAccumulator
          , channel1 = apu.channel1
          , channel2 = apu.channel2
          , channel3 = apu.channel3
          , channel4 = apu.channel4
          , frameSequencerCounter = apu.frameSequencerCounter
          , frameSequence = apu.frameSequence
          , enabled = apu.enabled
          }
        , apu.sampleBuffer
        )

    else
        ( apu, Array.empty )


writeNR10 : Int -> APU -> APU
writeNR10 value apu =
    setChannel1 (PulseChannel.writeNRx0 value apu.channel1) apu


writeNR11 : Int -> APU -> APU
writeNR11 value apu =
    setChannel1 (PulseChannel.writeNRx1 value apu.channel1) apu


writeNR12 : Int -> APU -> APU
writeNR12 value apu =
    setChannel1 (PulseChannel.writeNRx2 value apu.channel1) apu


writeNR13 : Int -> APU -> APU
writeNR13 value apu =
    setChannel1 (PulseChannel.writeNRx3 value apu.channel1) apu


writeNR14 : Int -> APU -> APU
writeNR14 value apu =
    setChannel1 (PulseChannel.writeNRx4 value apu.channel1) apu


writeNR21 : Int -> APU -> APU
writeNR21 value apu =
    setChannel2 (PulseChannel.writeNRx1 value apu.channel2) apu


writeNR22 : Int -> APU -> APU
writeNR22 value apu =
    setChannel2 (PulseChannel.writeNRx2 value apu.channel2) apu


writeNR23 : Int -> APU -> APU
writeNR23 value apu =
    setChannel2 (PulseChannel.writeNRx3 value apu.channel2) apu


writeNR24 : Int -> APU -> APU
writeNR24 value apu =
    setChannel2 (PulseChannel.writeNRx4 value apu.channel2) apu


writeNR30 : Int -> APU -> APU
writeNR30 value apu =
    setChannel3 (WaveChannel.writeNRx0 value apu.channel3) apu


writeNR31 : Int -> APU -> APU
writeNR31 value apu =
    setChannel3 (WaveChannel.writeNRx1 value apu.channel3) apu


writeNR32 : Int -> APU -> APU
writeNR32 value apu =
    setChannel3 (WaveChannel.writeNRx2 value apu.channel3) apu


writeNR33 : Int -> APU -> APU
writeNR33 value apu =
    setChannel3 (WaveChannel.writeNRx3 value apu.channel3) apu


writeNR34 : Int -> APU -> APU
writeNR34 value apu =
    setChannel3 (WaveChannel.writeNRx4 value apu.channel3) apu


writeNR41 : Int -> APU -> APU
writeNR41 value apu =
    setChannel4 (NoiseChannel.writeNRx1 value apu.channel4) apu


writeNR42 : Int -> APU -> APU
writeNR42 value apu =
    setChannel4 (NoiseChannel.writeNRx2 value apu.channel4) apu


writeNR43 : Int -> APU -> APU
writeNR43 value apu =
    setChannel4 (NoiseChannel.writeNRx3 value apu.channel4) apu


writeNR44 : Int -> APU -> APU
writeNR44 value apu =
    setChannel4 (NoiseChannel.writeNRx4 value apu.channel4) apu


writeNR50 : Int -> APU -> APU
writeNR50 _ apu =
    -- TODO: Implement
    apu


writeNR51 : Int -> APU -> APU
writeNR51 _ apu =
    -- TODO: Implement
    apu


writeNR52 : Int -> APU -> APU
writeNR52 _ apu =
    -- TODO: Implement
    apu


writeWaveRam : Int -> Int -> APU -> APU
writeWaveRam address value apu =
    setChannel3 (WaveChannel.writeWaveRam address value apu.channel3) apu



-- Internal


setChannel1 : PulseChannel -> APU -> APU
setChannel1 channel apu =
    { channel1 = channel
    , channel2 = apu.channel2
    , channel3 = apu.channel3
    , channel4 = apu.channel4
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , frameSequencerCounter = apu.frameSequencerCounter
    , frameSequence = apu.frameSequence
    , enabled = apu.enabled
    }


setChannel2 : PulseChannel -> APU -> APU
setChannel2 channel apu =
    { channel1 = apu.channel1
    , channel2 = channel
    , channel3 = apu.channel3
    , channel4 = apu.channel4
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , frameSequencerCounter = apu.frameSequencerCounter
    , frameSequence = apu.frameSequence
    , enabled = apu.enabled
    }


setChannel3 : WaveChannel -> APU -> APU
setChannel3 channel apu =
    { channel1 = apu.channel1
    , channel2 = apu.channel2
    , channel3 = channel
    , channel4 = apu.channel4
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , frameSequencerCounter = apu.frameSequencerCounter
    , frameSequence = apu.frameSequence
    , enabled = apu.enabled
    }


setChannel4 : NoiseChannel -> APU -> APU
setChannel4 channel apu =
    { channel1 = apu.channel1
    , channel2 = apu.channel2
    , channel3 = apu.channel3
    , channel4 = channel
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , frameSequencerCounter = apu.frameSequencerCounter
    , frameSequence = apu.frameSequence
    , enabled = apu.enabled
    }
