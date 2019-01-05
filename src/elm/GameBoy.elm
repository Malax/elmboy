module GameBoy exposing
    ( GameBoy
    , drainAudioBuffer
    , init
    , setAPU
    , setButtonStatus
    , setCPU
    , setCPUAndCycles
    , setCPULastInstructionCycles
    , setCartridge
    , setComponents
    , setHRAM
    , setJoypad
    , setLastInstructionCycles
    , setPPU
    , setTimer
    , setWorkRamBank0
    , setWorkRamBank1
    )

import Array exposing (Array)
import Component.APU as APU exposing (APU)
import Component.CPU as CPU exposing (CPU)
import Component.Cartridge exposing (Cartridge)
import Component.Joypad as Joypad exposing (GameBoyButton(..), Joypad)
import Component.PPU as PPU
import Component.PPU.Types exposing (PPU)
import Component.RAM as RAM exposing (RAM)
import Component.Timer as Timer exposing (Timer)


type alias GameBoy =
    { cpu : CPU
    , ppu : PPU
    , timer : Timer
    , apu : APU
    , workRamBank0 : RAM
    , workRamBank1 : RAM
    , hram : RAM
    , bootRomDisabled : Bool
    , cartridge : Cartridge
    , joypad : Joypad
    , lastInstructionCycles : Int
    }


init : Cartridge -> Bool -> GameBoy
init cartridge apuEnabled =
    { cpu = CPU.init
    , ppu = PPU.init
    , timer = Timer.init
    , apu = APU.init apuEnabled
    , workRamBank0 = RAM.initZero 0x1000
    , workRamBank1 = RAM.initZero 0x1000
    , hram = RAM.init 0x7F
    , bootRomDisabled = True
    , cartridge = cartridge
    , joypad = Joypad.init
    , lastInstructionCycles = 0
    }


setButtonStatus : GameBoyButton -> Bool -> GameBoy -> GameBoy
setButtonStatus button status gameBoy =
    let
        joypad =
            gameBoy.joypad

        updatedJoypad =
            case button of
                Up ->
                    Joypad.setUpPressed status joypad

                Down ->
                    Joypad.setDownPressed status joypad

                Left ->
                    Joypad.setLeftPressed status joypad

                Right ->
                    Joypad.setRightPressed status joypad

                A ->
                    Joypad.setAPressed status joypad

                B ->
                    Joypad.setBPressed status joypad

                Start ->
                    Joypad.setStartPressed status joypad

                Select ->
                    Joypad.setSelectPressed status joypad
    in
    setJoypad updatedJoypad gameBoy


drainAudioBuffer : Int -> GameBoy -> ( GameBoy, Array ( Float, Float ) )
drainAudioBuffer minSamples gameBoy =
    if Array.length gameBoy.apu.sampleBuffer >= minSamples then
        APU.drainAudioBuffer gameBoy.apu
            |> Tuple.mapFirst (\apu -> setAPU apu gameBoy)

    else
        ( gameBoy, Array.empty )



-- Performance Optimized Setters


setPPU : PPU -> GameBoy -> GameBoy
setPPU ppu gameBoy =
    { cpu = gameBoy.cpu
    , ppu = ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setAPU : APU -> GameBoy -> GameBoy
setAPU apu gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setJoypad : Joypad -> GameBoy -> GameBoy
setJoypad joypad gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setWorkRamBank0 : RAM -> GameBoy -> GameBoy
setWorkRamBank0 ram gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = ram
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setWorkRamBank1 : RAM -> GameBoy -> GameBoy
setWorkRamBank1 ram gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = ram
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setHRAM : RAM -> GameBoy -> GameBoy
setHRAM ram gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = ram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setComponents : CPU -> PPU -> Timer -> APU -> GameBoy -> GameBoy
setComponents cpu ppu timer apu gameBoy =
    { cpu = cpu
    , ppu = ppu
    , timer = timer
    , apu = apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setCPUAndCycles : CPU -> Int -> GameBoy -> GameBoy
setCPUAndCycles cpu cycles gameBoy =
    { cpu = cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = cycles
    }


setTimer : Timer -> GameBoy -> GameBoy
setTimer timer gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setCartridge : Cartridge -> GameBoy -> GameBoy
setCartridge cartridge gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setCPU : CPU -> GameBoy -> GameBoy
setCPU cpu gameBoy =
    { cpu = cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = gameBoy.lastInstructionCycles
    }


setLastInstructionCycles : Int -> GameBoy -> GameBoy
setLastInstructionCycles lastInstructionCycles gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = lastInstructionCycles
    }


setCPULastInstructionCycles : CPU -> Int -> GameBoy -> GameBoy
setCPULastInstructionCycles cpu lastInstructionCycles gameBoy =
    { cpu = cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , apu = gameBoy.apu
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastInstructionCycles = lastInstructionCycles
    }
