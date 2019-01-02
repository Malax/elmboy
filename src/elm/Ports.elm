port module Ports exposing (setPixelsFromBatches)


port setPixelsFromBatches : { canvasId : String, pixelBatches : List Int } -> Cmd msg
