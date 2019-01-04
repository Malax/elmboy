module Component.APU.DutyCycle exposing (DutyCycle(..), sample)

import Array exposing (Array)
import Component.APU.Constants as APUConstants


type DutyCycle
    = Zero
    | One
    | Two
    | Three


sample : DutyCycle -> Int -> Float
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
    array
        |> Array.get (remainderBy 8 index)
        |> Maybe.withDefault APUConstants.silence


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
