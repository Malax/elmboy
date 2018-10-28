module Msg exposing (Msg(..))

import Array exposing (Array)
import Component.Joypad exposing (GameBoyButton)
import Gamepad.Simple


type Msg
    = FileSelected
    | FileDataReceived (Array Int)
    | AnimationFrame Gamepad.Simple.FrameStuff
    | ButtonDown (Maybe GameBoyButton)
    | ButtonUp (Maybe GameBoyButton)
    | Reset
    | Pause
    | Resume
    | CloseErrorModal
