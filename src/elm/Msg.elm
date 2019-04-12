module Msg exposing (Msg(..))

import Component.Cartridge exposing (Cartridge)
import Component.Joypad exposing (GameBoyButton)
import File exposing (File)


type Msg
    = OpenFileSelect
    | FileSelected File
    | CartridgeSelected (Maybe Cartridge)
    | AnimationFrameDelta Float
    | ButtonDown GameBoyButton
    | ButtonUp GameBoyButton
    | Reset
    | Pause
    | Resume
    | CloseErrorModal
    | EnableAPU
    | DisableAPU
    | VirtualDPadInputDown String
    | VirtualDPadInputUp String
