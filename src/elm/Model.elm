module Model exposing (ErrorModal, MemoryArea(..), Model)

import Bootstrap.Dropdown exposing (State)
import Bootstrap.Modal as Modal
import GameBoy exposing (GameBoy)


type alias Model =
    { gameBoy : Maybe GameBoy
    , frameTimes : List Float
    , errorModal : Maybe ErrorModal
    , debuggerEnabled : Bool
    , debugger :
        { runToProgramCounter : Int
        , memoryArea : MemoryArea
        , memoryAreaDropdownState : State
        }
    , emulateOnAnimationFrame : Bool
    , apuEnabled : Bool
    }


type alias ErrorModal =
    { visibility : Modal.Visibility
    , title : String
    , body : String
    }


type MemoryArea
    = CartridgeROMBank0
    | CartridgeROMBankN
    | VRAM
    | CartridgeRAM
    | WorkRAMBank0
    | WorkRAMBank1
    | OAM
    | IORegisters
    | HRAM
