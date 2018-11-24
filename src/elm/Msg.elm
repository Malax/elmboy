module Msg exposing (Msg(..))

import Array exposing (Array)
import Component.Cartridge exposing (Cartridge)
import Component.Joypad exposing (GameBoyButton)
import File exposing (File)


type Msg
    = OpenFileSelect
    | FileSelected File
    | CartridgeSelected (Maybe Cartridge)
    | AnimationFrameDelta Float
    | ButtonDown (Maybe GameBoyButton)
    | ButtonUp (Maybe GameBoyButton)
    | Reset
    | Pause
    | Resume
    | CloseErrorModal
