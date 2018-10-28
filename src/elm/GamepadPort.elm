port module GamepadPort exposing (..)

import Gamepad.Advanced exposing (Blob)


port onBlob : (Blob -> msg) -> Sub msg


port saveToLocalStorage : String -> Cmd a
