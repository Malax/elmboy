module Component.Timer exposing
    ( Timer
    , emulateClocks
    , init
    , readDiv
    , readTac
    , readTima
    , readTma
    , writeDiv
    , writeTac
    , writeTima
    , writeTma
    )

import Bitwise


type alias Timer =
    { div : Int
    , divAcc : Int
    , tima : Int
    , timaAcc : Int
    , tma : Int
    , tac : Int
    , triggeredInterrupt : Bool
    }


init : Timer
init =
    { div = 0x00
    , divAcc = 0
    , tima = 0x00
    , timaAcc = 0
    , tma = 0x00
    , tac = 0x00
    , triggeredInterrupt = False
    }


emulateClocks : Int -> Timer -> Timer
emulateClocks cycles ({ divAcc, tac, timaAcc, tima, tma, div } as timer) =
    let
        updatedDivAcc =
            divAcc + cycles

        updatedTimaAcc =
            timaAcc + cycles

        timaCyclesPerIncrement =
            case Bitwise.and 0x03 tac of
                0 ->
                    cyclesPerSecond // 4096

                1 ->
                    cyclesPerSecond // 262144

                2 ->
                    cyclesPerSecond // 65536

                _ ->
                    cyclesPerSecond // 16384

        timaEnabled =
            Bitwise.and 0x04 tac == 0x04

        updatedTima =
            if timaEnabled && updatedTimaAcc >= timaCyclesPerIncrement then
                tima + 1

            else
                tima

        updatedDiv =
            if updatedDivAcc >= (cyclesPerSecond // divSpeed) then
                Bitwise.and 0xFF (div + 1)

            else
                div

        updatedFinalTima =
            if updatedTima > 0xFF then
                tma

            else
                updatedTima
    in
    { timer
        | div = updatedDiv
        , tima = updatedFinalTima
        , divAcc = remainderBy (cyclesPerSecond // divSpeed) updatedDivAcc
        , timaAcc = remainderBy timaCyclesPerIncrement updatedFinalTima
        , triggeredInterrupt = updatedTima > 0xFF
    }



-- Register Reads


readDiv : Timer -> Int
readDiv { div } =
    div


readTima : Timer -> Int
readTima { tima } =
    tima


readTma : Timer -> Int
readTma { tma } =
    tma


readTac : Timer -> Int
readTac { tac } =
    tac



-- Register Writes


writeDiv : Int -> Timer -> Timer
writeDiv _ timer =
    -- Writing will always reset to 0x00, regadless of actual written value
    { timer | div = 0x00 }


writeTima : Int -> Timer -> Timer
writeTima value timer =
    { timer | tima = value }


writeTma : Int -> Timer -> Timer
writeTma value timer =
    { timer | tma = value }


writeTac : Int -> Timer -> Timer
writeTac value timer =
    -- TODO: Do we have to reset the accumulators on changing freqs?
    { timer | tac = value, timaAcc = 0x00 }



-- Internal


divSpeed : Int
divSpeed =
    16384


cyclesPerSecond =
    4194304
