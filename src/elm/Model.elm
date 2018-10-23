module Model exposing (EmulationModel, ErrorModal, Model(..))

import Bootstrap.Modal as Modal
import GameBoy exposing (GameBoy)


type Model
    = Idle IdleModel
    | Emulation EmulationModel


type alias ErrorModal =
    { visibility : Modal.Visibility
    , title : String
    , body : String
    }


type alias IdleModel =
    { errorModal : Maybe ErrorModal }


type alias EmulationModel =
    { paused : Bool, lastFrameTime : Float, gameBoy : GameBoy }
