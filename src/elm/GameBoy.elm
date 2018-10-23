module GameBoy exposing
    ( GameBoy
    , init
    , modifyCpu
    , setButtonStatus
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


modifyCpu : (CPU -> CPU) -> GameBoy -> GameBoy
modifyCpu f gameBoy =
    { gameBoy | cpu = f gameBoy.cpu }


setButtonStatus : GameBoyButton -> Bool -> GameBoy -> GameBoy
setButtonStatus button status gameBoy =
    let
        joypad =
            gameBoy.joypad

        updatedJoypad =
            case button of
                Up ->
                    { joypad | upPressed = status }

                Down ->
                    { joypad | downPressed = status }

                Left ->
                    { joypad | leftPressed = status }

                Right ->
                    { joypad | rightPressed = status }

                A ->
                    { joypad | aPressed = status }

                B ->
                    { joypad | bPressed = status }

                Start ->
                    { joypad | startPressed = status }

                Select ->
                    { joypad | selectPressed = status }
    in
    { gameBoy | joypad = updatedJoypad }
