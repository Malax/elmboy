module Component.APU exposing (APU, emulate, init)

import Array exposing (Array)
import Component.RAM as RAM exposing (RAM)


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

    -- Control registers
    , nr50 = 0xFF
    , nr51 = 0xFF
    , nr52 = 0xFF
    }


emulate : Int -> APU -> ( APU, Array Float )
emulate cyles apu =
    ( apu, Array.empty )
