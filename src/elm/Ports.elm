port module Ports exposing (queueAudioSamples, setPixelsFromBatches)


port setPixelsFromBatches : { canvasId : String, pixelBatches : List Int } -> Cmd msg


port queueAudioSamples : Array Float -> Cmd msg
