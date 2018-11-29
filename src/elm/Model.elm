module Model exposing (EmulationMode(..), ErrorModal, Model)

import Bootstrap.Modal as Modal
import GameBoy exposing (GameBoy)


type alias Model =
    { emulationMode : EmulationMode
    , frameTimes : List Float
    , gameBoy : Maybe GameBoy
    , errorModal : Maybe ErrorModal
    }


type EmulationMode
    = OnAnimationFrame
    | Manual


type alias ErrorModal =
    { visibility : Modal.Visibility
    , title : String
    , body : String
    }
