port module Ports exposing (fileData, requestFileData, setPixelsFromBatches)

import Array exposing (Array)


port setPixelsFromBatches : { canvasId : String, pixelBatches : List Int } -> Cmd msg


port requestFileData : String -> Cmd msg


port fileData : (Array Int -> msg) -> Sub msg
