module Component.APU.Constants exposing (cyclesPerSample, silence)

import Constants


cyclesPerSample : Int
cyclesPerSample =
    Constants.cyclesPerSecond // Constants.outputSampleRate


silence : Float
silence =
    0
