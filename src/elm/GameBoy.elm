module GameBoy exposing
    ( GameBoy
    , init
    , setButtonStatus
    , setCPU
    , setCPUAndCycles
    , setCPULastCycleClocks
    , setCartridge
    , setComponents
    , setHRAM
    , setJoypad
    , setLastCycleClocks
    , setPPU
    , setTimer
    , setWorkRamBank0
    , setWorkRamBank1
    )

import Component.CPU as CPU exposing (CPU)
import Component.Cartridge as Cartridge exposing (Cartridge)
import Component.Joypad as Joypad exposing (GameBoyButton(..), Joypad)
import Component.PPU as PPU
import Component.PPU.Types exposing (PPU)
import Component.RAM as RAM exposing (RAM)
import Component.Timer as Timer exposing (Timer)


type alias GameBoy =
    { cpu : CPU
    , ppu : PPU
    , timer : Timer
    , workRamBank0 : RAM
    , workRamBank1 : RAM
    , hram : RAM
    , bootRomDisabled : Bool
    , cartridge : Cartridge
    , joypad : Joypad
    , lastCycleClocks : Int
    }


init : Cartridge -> GameBoy
init cartridge =
    { cpu = CPU.init
    , ppu = PPU.init
    , timer = Timer.init
    , workRamBank0 = RAM.initZero 0x1000
    , workRamBank1 = RAM.initZero 0x1000
    , hram = RAM.init 0x7F
    , bootRomDisabled = True
    , cartridge = cartridge
    , joypad = Joypad.init
    , lastCycleClocks = 0
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



-- Performance Optimized Setters


setPPU : PPU -> GameBoy -> GameBoy
setPPU ppu gameBoy =
    { cpu = gameBoy.cpu
    , ppu = ppu
    , timer = gameBoy.timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = gameBoy.lastCycleClocks
    }


setJoypad : Joypad -> GameBoy -> GameBoy
setJoypad joypad gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = joypad
    , lastCycleClocks = gameBoy.lastCycleClocks
    }


setWorkRamBank0 : RAM -> GameBoy -> GameBoy
setWorkRamBank0 ram gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , workRamBank0 = ram
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = gameBoy.lastCycleClocks
    }


setWorkRamBank1 : RAM -> GameBoy -> GameBoy
setWorkRamBank1 ram gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = ram
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = gameBoy.lastCycleClocks
    }


setHRAM : RAM -> GameBoy -> GameBoy
setHRAM ram gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = ram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = gameBoy.lastCycleClocks
    }


setComponents : CPU -> PPU -> Timer -> GameBoy -> GameBoy
setComponents cpu ppu timer gameBoy =
    { cpu = cpu
    , ppu = ppu
    , timer = timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = gameBoy.lastCycleClocks
    }


setCPUAndCycles : CPU -> Int -> GameBoy -> GameBoy
setCPUAndCycles cpu cycles gameBoy =
    { cpu = cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = cycles
    }


setTimer : Timer -> GameBoy -> GameBoy
setTimer timer gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = gameBoy.lastCycleClocks
    }


setCartridge : Cartridge -> GameBoy -> GameBoy
setCartridge cartridge gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = gameBoy.lastCycleClocks
    }


setCPU : CPU -> GameBoy -> GameBoy
setCPU cpu gameBoy =
    { cpu = cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = gameBoy.lastCycleClocks
    }


setLastCycleClocks : Int -> GameBoy -> GameBoy
setLastCycleClocks lastCycleClocks gameBoy =
    { cpu = gameBoy.cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = lastCycleClocks
    }


setCPULastCycleClocks : CPU -> Int -> GameBoy -> GameBoy
setCPULastCycleClocks cpu lastCycleClocks gameBoy =
    { cpu = cpu
    , ppu = gameBoy.ppu
    , timer = gameBoy.timer
    , workRamBank0 = gameBoy.workRamBank0
    , workRamBank1 = gameBoy.workRamBank1
    , hram = gameBoy.hram
    , bootRomDisabled = gameBoy.bootRomDisabled
    , cartridge = gameBoy.cartridge
    , joypad = gameBoy.joypad
    , lastCycleClocks = lastCycleClocks
    }
