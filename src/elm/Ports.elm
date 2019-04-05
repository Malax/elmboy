port module Ports exposing (queueAudioSamples, setPixelsFromBatches)


port setPixelsFromBatches : { canvasId : String, pixelBatches : List Int } -> Cmd msg


port queueAudioSamples : List ( Float, Float ) -> Cmd msg
