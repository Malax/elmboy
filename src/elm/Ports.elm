port module Ports exposing (setPixelsFromBatches)

import Array exposing (Array)


port setPixelsFromBatches : { canvasId : String, pixelBatches : List Int } -> Cmd msg
