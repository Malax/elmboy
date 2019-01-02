module Component.APU.Constants exposing (cyclesPerSample)

import Constants


cyclesPerSample : Int
cyclesPerSample =
    Constants.cyclesPerSecond // Constants.outputSampleRate
