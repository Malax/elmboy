module Model exposing (ErrorModal, Model)

import Bootstrap.Modal as Modal
import GameBoy exposing (GameBoy)


type alias Model =
    { gameBoy : Maybe GameBoy
    , frameTimes : List Float
    , errorModal : Maybe ErrorModal
    , debuggerEnabled : Bool
    , emulateOnAnimationFrame : Bool
    }


type alias ErrorModal =
    { visibility : Modal.Visibility
    , title : String
    , body : String
    }
