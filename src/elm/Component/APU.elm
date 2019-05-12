module Component.APU exposing
    ( APU
    , drainAudioBuffer
    , emulate
    , init
    , readNR10
    , readNR11
    , readNR12
    , readNR13
    , readNR14
    , readNR21
    , readNR22
    , readNR23
    , readNR24
    , readNR30
    , readNR31
    , readNR32
    , readNR33
    , readNR34
    , readNR41
    , readNR42
    , readNR43
    , readNR44
    , readNR50
    , readNR51
    , readNR52
    , readWaveRam
    , setEnabled
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

import Bitwise
import Component.APU.Constants as APUConstants
import Component.APU.NoiseChannel as NoiseChannel exposing (NoiseChannel)
import Component.APU.PulseChannel as PulseChannel exposing (PulseChannel)
import Component.APU.WaveChannel as WaveChannel exposing (WaveChannel)
import Constants
import Util


type alias APU =
    { channel1 : PulseChannel
    , channel2 : PulseChannel
    , channel3 : WaveChannel
    , channel4 : NoiseChannel
    , sampleBuffer : List Float
    , cycleAccumulator : Int
    , frameSequencerCounter : Int
    , frameSequence : Int
    , enabled : Bool
    , powerOn : Bool
    , leftVolume : Int
    , rightVolume : Int
    , vinLeftEnable : Bool
    , vinRightEnable : Bool
    , enabledChannels :
        { channel1Left : Bool
        , channel2Left : Bool
        , channel3Left : Bool
        , channel4Left : Bool
        , channel1Right : Bool
        , channel2Right : Bool
        , channel3Right : Bool
        , channel4Right : Bool
        }
    }


init : Bool -> APU
init enabled =
    { channel1 = PulseChannel.init
    , channel2 = PulseChannel.init
    , channel3 = WaveChannel.init
    , channel4 = NoiseChannel.init
    , sampleBuffer = []
    , cycleAccumulator = 0
    , frameSequencerCounter = 0
    , frameSequence = 0
    , enabled = enabled
    , powerOn = True
    , leftVolume = 0
    , rightVolume = 0
    , vinLeftEnable = False
    , vinRightEnable = False
    , enabledChannels =
        { channel1Left = False
        , channel2Left = False
        , channel3Left = False
        , channel4Left = False
        , channel1Right = False
        , channel2Right = False
        , channel3Right = False
        , channel4Right = False
        }
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
                    mixSamples updatedChannel1 updatedChannel2 updatedChannel3 updatedChannel4 apu apu.sampleBuffer

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
        , powerOn = apu.powerOn
        , leftVolume = apu.leftVolume
        , rightVolume = apu.rightVolume
        , vinLeftEnable = apu.vinLeftEnable
        , vinRightEnable = apu.vinRightEnable
        , enabledChannels = apu.enabledChannels
        }

    else
        apu


drainAudioBuffer : APU -> ( APU, List Float )
drainAudioBuffer apu =
    if apu.enabled then
        ( { sampleBuffer = []
          , cycleAccumulator = apu.cycleAccumulator
          , channel1 = apu.channel1
          , channel2 = apu.channel2
          , channel3 = apu.channel3
          , channel4 = apu.channel4
          , frameSequencerCounter = apu.frameSequencerCounter
          , frameSequence = apu.frameSequence
          , enabled = apu.enabled
          , powerOn = apu.powerOn
          , leftVolume = apu.leftVolume
          , rightVolume = apu.rightVolume
          , vinLeftEnable = apu.vinLeftEnable
          , vinRightEnable = apu.vinRightEnable
          , enabledChannels = apu.enabledChannels
          }
        , apu.sampleBuffer
        )

    else
        ( apu, [] )


setEnabled : Bool -> APU -> APU
setEnabled enabled apu =
    { sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , channel1 = apu.channel1
    , channel2 = apu.channel2
    , channel3 = apu.channel3
    , channel4 = apu.channel4
    , frameSequencerCounter = apu.frameSequencerCounter
    , frameSequence = apu.frameSequence
    , enabled = enabled
    , powerOn = apu.powerOn
    , leftVolume = apu.leftVolume
    , rightVolume = apu.rightVolume
    , vinLeftEnable = apu.vinLeftEnable
    , vinRightEnable = apu.vinRightEnable
    , enabledChannels = apu.enabledChannels
    }


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
writeNR50 value apu =
    let
        vinLeftEnable =
            Bitwise.and Constants.bit7Mask value == Constants.bit7Mask

        vinRightEnable =
            Bitwise.and Constants.bit3Mask value == Constants.bit3Mask

        leftVolume =
            value
                |> Bitwise.and 0x70
                |> Bitwise.shiftRightZfBy 4

        rightVolume =
            Bitwise.and 0x07 value
    in
    { channel1 = apu.channel1
    , channel2 = apu.channel2
    , channel3 = apu.channel3
    , channel4 = apu.channel4
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , frameSequencerCounter = apu.frameSequencerCounter
    , frameSequence = apu.frameSequence
    , enabled = apu.enabled
    , powerOn = apu.powerOn
    , leftVolume = leftVolume
    , rightVolume = rightVolume
    , vinLeftEnable = vinLeftEnable
    , vinRightEnable = vinRightEnable
    , enabledChannels = apu.enabledChannels
    }


writeNR51 : Int -> APU -> APU
writeNR51 value apu =
    { channel1 = apu.channel1
    , channel2 = apu.channel2
    , channel3 = apu.channel3
    , channel4 = apu.channel4
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , frameSequencerCounter = apu.frameSequencerCounter
    , frameSequence = apu.frameSequence
    , enabled = apu.enabled
    , powerOn = apu.powerOn
    , leftVolume = apu.leftVolume
    , rightVolume = apu.rightVolume
    , vinLeftEnable = apu.vinLeftEnable
    , vinRightEnable = apu.vinRightEnable
    , enabledChannels =
        { channel4Left =
            Bitwise.and Constants.bit7Mask value == Constants.bit7Mask
        , channel3Left =
            Bitwise.and Constants.bit6Mask value == Constants.bit6Mask
        , channel2Left =
            Bitwise.and Constants.bit5Mask value == Constants.bit5Mask
        , channel1Left =
            Bitwise.and Constants.bit4Mask value == Constants.bit4Mask
        , channel4Right =
            Bitwise.and Constants.bit3Mask value == Constants.bit3Mask
        , channel3Right =
            Bitwise.and Constants.bit2Mask value == Constants.bit2Mask
        , channel2Right =
            Bitwise.and Constants.bit1Mask value == Constants.bit1Mask
        , channel1Right =
            Bitwise.and Constants.bit0Mask value == Constants.bit0Mask
        }
    }


writeNR52 : Int -> APU -> APU
writeNR52 value apu =
    let
        powerOn =
            Bitwise.and Constants.bit7Mask value == Constants.bit7Mask
    in
    { channel1 = PulseChannel.reset apu.channel1
    , channel2 = PulseChannel.reset apu.channel2
    , channel3 = WaveChannel.reset apu.channel3
    , channel4 = NoiseChannel.reset apu.channel4
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , frameSequencerCounter = apu.frameSequencerCounter
    , frameSequence = apu.frameSequence
    , enabled = apu.enabled
    , powerOn = powerOn
    , leftVolume = 0
    , rightVolume = 0
    , vinLeftEnable = False
    , vinRightEnable = False
    , enabledChannels =
        { channel1Left = False
        , channel2Left = False
        , channel3Left = False
        , channel4Left = False
        , channel1Right = False
        , channel2Right = False
        , channel3Right = False
        , channel4Right = False
        }
    }


writeWaveRam : Int -> Int -> APU -> APU
writeWaveRam address value apu =
    setChannel3 (WaveChannel.writeWaveRam address value apu.channel3) apu


readNR10 : APU -> Int
readNR10 apu =
    PulseChannel.readNRx0 apu.channel1


readNR11 : APU -> Int
readNR11 apu =
    PulseChannel.readNRx1 apu.channel1


readNR12 : APU -> Int
readNR12 apu =
    PulseChannel.readNRx2 apu.channel1


readNR13 : APU -> Int
readNR13 apu =
    PulseChannel.readNRx3 apu.channel1


readNR14 : APU -> Int
readNR14 apu =
    PulseChannel.readNRx4 apu.channel1


readNR21 : APU -> Int
readNR21 apu =
    PulseChannel.readNRx1 apu.channel2


readNR22 : APU -> Int
readNR22 apu =
    PulseChannel.readNRx2 apu.channel2


readNR23 : APU -> Int
readNR23 apu =
    PulseChannel.readNRx3 apu.channel2


readNR24 : APU -> Int
readNR24 apu =
    PulseChannel.readNRx4 apu.channel2


readNR30 : APU -> Int
readNR30 apu =
    WaveChannel.readNRx0 apu.channel3


readNR31 : APU -> Int
readNR31 apu =
    WaveChannel.readNRx1 apu.channel3


readNR32 : APU -> Int
readNR32 apu =
    WaveChannel.readNRx2 apu.channel3


readNR33 : APU -> Int
readNR33 apu =
    WaveChannel.readNRx3 apu.channel3


readNR34 : APU -> Int
readNR34 apu =
    WaveChannel.readNRx4 apu.channel3


readNR41 : APU -> Int
readNR41 apu =
    NoiseChannel.readNRx1 apu.channel4


readNR42 : APU -> Int
readNR42 apu =
    NoiseChannel.readNRx2 apu.channel4


readNR43 : APU -> Int
readNR43 apu =
    NoiseChannel.readNRx3 apu.channel4


readNR44 : APU -> Int
readNR44 apu =
    NoiseChannel.readNRx4 apu.channel4


readNR50 : APU -> Int
readNR50 apu =
    Util.boolToBit apu.vinLeftEnable
        |> Bitwise.shiftLeftBy 3
        |> Bitwise.or apu.leftVolume
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.vinRightEnable)
        |> Bitwise.shiftLeftBy 3
        |> Bitwise.or apu.rightVolume


readNR51 : APU -> Int
readNR51 apu =
    0x00
        |> Bitwise.or (Util.boolToBit apu.enabledChannels.channel4Left)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.enabledChannels.channel3Left)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.enabledChannels.channel2Left)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.enabledChannels.channel1Left)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.enabledChannels.channel4Right)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.enabledChannels.channel3Right)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.enabledChannels.channel2Right)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.enabledChannels.channel1Right)


readNR52 : APU -> Int
readNR52 apu =
    0x00
        |> Bitwise.or (Util.boolToBit apu.powerOn)
        |> Bitwise.shiftLeftBy 4
        |> Bitwise.or (Util.boolToBit apu.channel4.enabled)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.channel3.enabled)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.channel2.enabled)
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (Util.boolToBit apu.channel1.enabled)
        |> Bitwise.or 0x70


readWaveRam : Int -> APU -> Int
readWaveRam address apu =
    WaveChannel.readWaveRam address apu.channel3



-- Internal


mixSamples : PulseChannel -> PulseChannel -> WaveChannel -> NoiseChannel -> APU -> List Float -> List Float
mixSamples channel1 channel2 channel3 channel4 { leftVolume, enabledChannels } oldBuffer =
    let
        channel1Sample =
            PulseChannel.sample channel1

        channel2Sample =
            PulseChannel.sample channel2

        channel3Sample =
            WaveChannel.sample channel3

        channel4Sample =
            NoiseChannel.sample channel4

        leftChannel1 =
            if enabledChannels.channel1Left then
                channel1Sample

            else
                APUConstants.silence

        leftChannel2 =
            if enabledChannels.channel2Left then
                channel2Sample

            else
                APUConstants.silence

        leftChannel3 =
            if enabledChannels.channel3Left then
                channel3Sample

            else
                APUConstants.silence

        leftChannel4 =
            if enabledChannels.channel4Left then
                channel4Sample

            else
                APUConstants.silence

        mixed =
            ((leftChannel1 + leftChannel2 + leftChannel3 + leftChannel4) / 4) * (toFloat leftVolume * (1 / 7))
    in
    (leftChannel1 * (toFloat leftVolume * (1 / 7)))
        :: (leftChannel2 * (toFloat leftVolume * (1 / 7)))
        :: (leftChannel3 * (toFloat leftVolume * (1 / 7)))
        :: (leftChannel4 * (toFloat leftVolume * (1 / 7)))
        :: mixed
        :: oldBuffer


updateChannelAfterFrameSequencer : Bool -> Int -> (a -> a) -> (a -> a) -> (a -> a) -> a -> a
updateChannelAfterFrameSequencer triggered sequence clockLength clockVolEnv clockSweep channel =
    if triggered then
        case sequence of
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
    , powerOn = apu.powerOn
    , leftVolume = apu.leftVolume
    , rightVolume = apu.rightVolume
    , vinLeftEnable = apu.vinLeftEnable
    , vinRightEnable = apu.vinRightEnable
    , enabledChannels = apu.enabledChannels
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
    , powerOn = apu.powerOn
    , leftVolume = apu.leftVolume
    , rightVolume = apu.rightVolume
    , vinLeftEnable = apu.vinLeftEnable
    , vinRightEnable = apu.vinRightEnable
    , enabledChannels = apu.enabledChannels
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
    , powerOn = apu.powerOn
    , leftVolume = apu.leftVolume
    , rightVolume = apu.rightVolume
    , vinLeftEnable = apu.vinLeftEnable
    , vinRightEnable = apu.vinRightEnable
    , enabledChannels = apu.enabledChannels
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
    , powerOn = apu.powerOn
    , leftVolume = apu.leftVolume
    , rightVolume = apu.rightVolume
    , vinLeftEnable = apu.vinLeftEnable
    , vinRightEnable = apu.vinRightEnable
    , enabledChannels = apu.enabledChannels
    }
