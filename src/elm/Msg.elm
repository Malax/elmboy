module Msg exposing (DebuggerMsg(..), Msg(..))

import Array exposing (Array)
import Bootstrap.Dropdown exposing (State)
import Component.Cartridge exposing (Cartridge)
import Component.Joypad exposing (GameBoyButton)
import File exposing (File)
import Model exposing (MemoryArea(..))


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
    | Debugger DebuggerMsg


type DebuggerMsg
    = RunToProgramCounterValueChange String
    | RunToProgramCounter
    | RunNextInstruction
    | SelectMemoryArea MemoryArea
    | MemoryAreaDropdownStateChange State
    | ToggleEmulateOnAnimationFrame
