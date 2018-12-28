module Component.APU exposing (APU, drainAudioBuffer, emulate, init)

import Array exposing (Array)
import Component.APU.Channel2 as Channel2 exposing (Channel2)
import Component.RAM as RAM exposing (RAM)
import Constants


type alias APU =
    { -- Square 1
      nr10 : Int -- -PPP NSSS Sweep period, negate, shift
    , nr11 : Int -- DDLL LLLL Duty, Length load (64-L)
    , nr12 : Int -- VVVV APPP Starting volume, Envelope add mode, period
    , nr13 : Int -- FFFF FFFF Frequency LSB
    , nr14 : Int -- TL-- -FFF Trigger, Length enable, Frequency MSB

    -- Square 2
    , nr21 : Int -- DDLL LLLL Duty, Length load (64-L)
    , nr22 : Int -- VVVV APPP Starting volume, Envelope add mode, period
    , nr23 : Int -- FFFF FFFF Frequency LSB
    , nr24 : Int -- TL-- -FFF Trigger, Length enable, Frequency MSB

    -- Wave
    , nr30 : Int -- E--- ---- DAC power
    , nr31 : Int -- LLLL LLLL Length load (256-L)
    , nr32 : Int -- -VV- ---- Volume code (00=0%, 01=100%, 10=50%, 11=25%)
    , nr33 : Int -- FFFF FFFF Frequency LSB
    , nr34 : Int -- TL-- -FFF Trigger, Length enable, Frequency MSB
    , wavePatternRam : RAM

    -- Noise
    , nr41 : Int -- --LL LLLL Length load (64-L)
    , nr42 : Int -- VVVV APPP Starting volume, Envelope add mode, period
    , nr43 : Int -- SSSS WDDD Clock shift, Width mode of LFSR, Divisor code
    , nr44 : Int -- TL-- ---- Trigger, Length enable

    -- Control
    , nr50 : Int -- ALLL BRRR Vin L enable, Left vol, Vin R enable, Right vol
    , nr51 : Int -- NW21 NW21 Left enables, Right enables
    , nr52 : Int -- P--- NW21 Power control/status, Channel length statuses

    -- Elmboy
    , sampleBuffer : Array Float
    , cycleAccumulator : Int
    , channel2 : Channel2
    }


init : APU
init =
    { nr10 = 0xFF
    , nr11 = 0xFF
    , nr12 = 0xFF
    , nr13 = 0xFF
    , nr14 = 0xFF
    , nr21 = 0xFF
    , nr22 = 0xFF
    , nr23 = 0xFF
    , nr24 = 0xFF
    , nr30 = 0xFF
    , nr31 = 0xFF
    , nr32 = 0xFF
    , nr33 = 0xFF
    , nr34 = 0xFF
    , wavePatternRam = RAM.init 0x0F
    , nr41 = 0xFF
    , nr42 = 0xFF
    , nr43 = 0xFF
    , nr44 = 0xFF
    , nr50 = 0xFF
    , nr51 = 0xFF
    , nr52 = 0xFF
    , sampleBuffer = a4 0.5
    , cycleAccumulator = 0
    , channel2 = Channel2.init
    }


emulate : Int -> APU -> APU
emulate cycles apu =
    let
        ( channel2, sample ) =
            Channel2.emulate cycles apu.channel2

        updatedApu =
            setChannels channel2 apu
    in
    case sample of
        Just s ->
            appendSample s updatedApu

        Nothing ->
            updatedApu


drainAudioBuffer : APU -> ( APU, Array Float )
drainAudioBuffer apu =
    ( { nr10 = apu.nr10
      , nr11 = apu.nr11
      , nr12 = apu.nr12
      , nr13 = apu.nr13
      , nr14 = apu.nr14
      , nr21 = apu.nr21
      , nr22 = apu.nr22
      , nr23 = apu.nr23
      , nr24 = apu.nr24
      , nr30 = apu.nr30
      , nr31 = apu.nr31
      , nr32 = apu.nr32
      , nr33 = apu.nr33
      , nr34 = apu.nr34
      , wavePatternRam = apu.wavePatternRam
      , nr41 = apu.nr41
      , nr42 = apu.nr42
      , nr43 = apu.nr43
      , nr44 = apu.nr44
      , nr50 = apu.nr50
      , nr51 = apu.nr51
      , nr52 = apu.nr52
      , sampleBuffer = Array.empty
      , cycleAccumulator = apu.cycleAccumulator
      , channel2 = apu.channel2
      }
    , apu.sampleBuffer
    )



-- Internal


appendSample : Float -> APU -> APU
appendSample sample apu =
    { nr10 = apu.nr10
    , nr11 = apu.nr11
    , nr12 = apu.nr12
    , nr13 = apu.nr13
    , nr14 = apu.nr14
    , nr21 = apu.nr21
    , nr22 = apu.nr22
    , nr23 = apu.nr23
    , nr24 = apu.nr24
    , nr30 = apu.nr30
    , nr31 = apu.nr31
    , nr32 = apu.nr32
    , nr33 = apu.nr33
    , nr34 = apu.nr34
    , wavePatternRam = apu.wavePatternRam
    , nr41 = apu.nr41
    , nr42 = apu.nr42
    , nr43 = apu.nr43
    , nr44 = apu.nr44
    , nr50 = apu.nr50
    , nr51 = apu.nr51
    , nr52 = apu.nr52
    , sampleBuffer = Array.push sample apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , channel2 = apu.channel2
    }


appendSamples : Array Float -> APU -> APU
appendSamples samples apu =
    { nr10 = apu.nr10
    , nr11 = apu.nr11
    , nr12 = apu.nr12
    , nr13 = apu.nr13
    , nr14 = apu.nr14
    , nr21 = apu.nr21
    , nr22 = apu.nr22
    , nr23 = apu.nr23
    , nr24 = apu.nr24
    , nr30 = apu.nr30
    , nr31 = apu.nr31
    , nr32 = apu.nr32
    , nr33 = apu.nr33
    , nr34 = apu.nr34
    , wavePatternRam = apu.wavePatternRam
    , nr41 = apu.nr41
    , nr42 = apu.nr42
    , nr43 = apu.nr43
    , nr44 = apu.nr44
    , nr50 = apu.nr50
    , nr51 = apu.nr51
    , nr52 = apu.nr52
    , sampleBuffer = Array.append apu.sampleBuffer samples
    , cycleAccumulator = apu.cycleAccumulator
    , channel2 = apu.channel2
    }


setCycleAccumulator : Int -> APU -> APU
setCycleAccumulator cycles apu =
    { nr10 = apu.nr10
    , nr11 = apu.nr11
    , nr12 = apu.nr12
    , nr13 = apu.nr13
    , nr14 = apu.nr14
    , nr21 = apu.nr21
    , nr22 = apu.nr22
    , nr23 = apu.nr23
    , nr24 = apu.nr24
    , nr30 = apu.nr30
    , nr31 = apu.nr31
    , nr32 = apu.nr32
    , nr33 = apu.nr33
    , nr34 = apu.nr34
    , wavePatternRam = apu.wavePatternRam
    , nr41 = apu.nr41
    , nr42 = apu.nr42
    , nr43 = apu.nr43
    , nr44 = apu.nr44
    , nr50 = apu.nr50
    , nr51 = apu.nr51
    , nr52 = apu.nr52
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = cycles
    , channel2 = apu.channel2
    }


setChannels : Channel2 -> APU -> APU
setChannels channel2 apu =
    { nr10 = apu.nr10
    , nr11 = apu.nr11
    , nr12 = apu.nr12
    , nr13 = apu.nr13
    , nr14 = apu.nr14
    , nr21 = apu.nr21
    , nr22 = apu.nr22
    , nr23 = apu.nr23
    , nr24 = apu.nr24
    , nr30 = apu.nr30
    , nr31 = apu.nr31
    , nr32 = apu.nr32
    , nr33 = apu.nr33
    , nr34 = apu.nr34
    , wavePatternRam = apu.wavePatternRam
    , nr41 = apu.nr41
    , nr42 = apu.nr42
    , nr43 = apu.nr43
    , nr44 = apu.nr44
    , nr50 = apu.nr50
    , nr51 = apu.nr51
    , nr52 = apu.nr52
    , sampleBuffer = apu.sampleBuffer
    , cycleAccumulator = apu.cycleAccumulator
    , channel2 = channel2
    }


sinewave : Float -> Float -> Float -> Float
sinewave amplitude frequency time =
    amplitude * sin (2 * pi * frequency * time)


a4 : Float -> Array Float
a4 duration =
    Array.initialize (ceiling (duration * sampleRate)) (\t -> sinewave 1 440 (toFloat t / sampleRate))


cyclesPerSample : Int
cyclesPerSample =
    Constants.cyclesPerSecond // sampleRate


sampleRate =
    44100
