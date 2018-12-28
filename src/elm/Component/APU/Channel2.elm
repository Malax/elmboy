module Component.APU.Channel2 exposing (Channel2, emulate, init)

import Array exposing (Array)
import Constants


type DutyCycle
    = Zero
    | One
    | Two
    | Three


type alias Channel2 =
    { dutyCycle : DutyCycle
    , frequency : Int
    , timerValue : Int
    , dutyCycleIndex : Int
    , cycleAccumulator : Int
    }


init : Channel2
init =
    { dutyCycle = Two
    , frequency = 440
    , timerValue = 0
    , dutyCycleIndex = 0
    , cycleAccumulator = 0
    }


emulate : Int -> Channel2 -> ( Channel2, Maybe Float )
emulate cycles channel2 =
    let
        ( updatedCycleAccumulator, generateSample ) =
            ( remainderBy cyclesPerSample (channel2.cycleAccumulator + cycles), channel2.cycleAccumulator + cycles >= cyclesPerSample )

        ( updatedTimerValue, timerUnderflowed ) =
            let
                x =
                    channel2.timerValue - cycles

                underflowed =
                    x <= 0

                v =
                    if underflowed then
                        (2048 - channel2.frequency) * 4 + x

                    else
                        x
            in
            ( v, underflowed )

        updatedCycleIndex =
            if timerUnderflowed then
                remainderBy 8 (channel2.dutyCycleIndex + 1)

            else
                channel2.dutyCycleIndex

        samples =
            if generateSample then
                sample channel2.dutyCycle updatedCycleIndex

            else
                Nothing
    in
    ( updateShit updatedTimerValue updatedCycleIndex updatedCycleAccumulator channel2, samples )


updateShit : Int -> Int -> Int -> Channel2 -> Channel2
updateShit timerValue dutyCycleIndex cycleAccumulator channel =
    { dutyCycle = channel.dutyCycle
    , frequency = channel.frequency
    , timerValue = timerValue
    , dutyCycleIndex = dutyCycleIndex
    , cycleAccumulator = cycleAccumulator
    }


sample : DutyCycle -> Int -> Maybe Float
sample duty index =
    let
        array =
            case duty of
                Zero ->
                    dutyZeroCycles

                One ->
                    dutyOneCycles

                Two ->
                    dutyTwoCycles

                Three ->
                    dutyThreeCycles
    in
    Array.get index array |> Maybe.map ((*) 0.3)


dutyZeroCycles : Array Float
dutyZeroCycles =
    Array.fromList
        [ -1, -1, -1, -1, -1, -1, -1, 1 ]


dutyOneCycles : Array Float
dutyOneCycles =
    Array.fromList
        [ 1, -1, -1, -1, -1, -1, -1, 1 ]


dutyTwoCycles : Array Float
dutyTwoCycles =
    Array.fromList
        [ 1, -1, -1, -1, -1, 1, 1, 1 ]


dutyThreeCycles : Array Float
dutyThreeCycles =
    Array.fromList
        [ -1, 1, 1, 1, 1, 1, 1, -1 ]



-- Internal


cyclesPerSample : Int
cyclesPerSample =
    Constants.cyclesPerSecond // 44100
