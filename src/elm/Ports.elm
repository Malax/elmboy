port module Ports exposing (queueAudioSamples, setPixelsFromBatches, virtualDPadInput, virtualDPadInputUp)


port setPixelsFromBatches : { canvasId : String, pixelBatches : List Int } -> Cmd msg


port queueAudioSamples : List ( Float, Float ) -> Cmd msg


port virtualDPadInput : (String -> msg) -> Sub msg


port virtualDPadInputUp : (String -> msg) -> Sub msg
