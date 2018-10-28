module Gamepad.Private exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)


-- Gamepad


type Gamepad
    = Gamepad Mapping GamepadFrame GamepadFrame



-- Types populated by gamepadPort.js


type alias GamepadFrame =
    { axes : Array Float
    , buttons : Array ( Bool, Float )
    , id : String
    , index : Int
    , mapping : String
    }


type alias Environment =
    { userMappings : String
    , languages : List String
    }


emptyEnvironment : Environment
emptyEnvironment =
    { userMappings = "{}"
    , languages = []
    }


type alias BlobFrame =
    { gamepads : List GamepadFrame
    , timestamp : Float
    }


emptyBlobFrame : BlobFrame
emptyBlobFrame =
    { timestamp = 0
    , gamepads = []
    }


type alias Blob =
    ( BlobFrame, BlobFrame, Environment )


emptyBlob : Blob
emptyBlob =
    ( emptyBlobFrame, emptyBlobFrame, emptyEnvironment )



-- Mapping


type OriginType
    = Axis
    | Button


type Origin
    = Origin Bool OriginType Int


type alias Mapping =
    Dict String Origin



-- Misc


boolToNumber : Bool -> number
boolToNumber bool =
    if bool then
        1
    else
        0
