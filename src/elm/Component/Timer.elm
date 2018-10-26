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
    { div = updatedDiv
    , divAcc = remainderBy (cyclesPerSecond // divSpeed) updatedDivAcc
    , tima = updatedFinalTima
    , timaAcc = remainderBy timaCyclesPerIncrement updatedFinalTima
    , tma = timer.tma
    , tac = timer.tac
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
    { div = 0x00
    , divAcc = timer.divAcc
    , tima = timer.tima
    , timaAcc = timer.timaAcc
    , tma = timer.tma
    , tac = timer.tac
    , triggeredInterrupt = timer.triggeredInterrupt
    }


writeTima : Int -> Timer -> Timer
writeTima value timer =
    { div = timer.div
    , divAcc = timer.divAcc
    , tima = value
    , timaAcc = timer.timaAcc
    , tma = timer.tma
    , tac = timer.tac
    , triggeredInterrupt = timer.triggeredInterrupt
    }


writeTma : Int -> Timer -> Timer
writeTma value timer =
    { div = timer.div
    , divAcc = timer.divAcc
    , tima = timer.tima
    , timaAcc = timer.timaAcc
    , tma = value
    , tac = timer.tac
    , triggeredInterrupt = timer.triggeredInterrupt
    }


writeTac : Int -> Timer -> Timer
writeTac value timer =
    -- TODO: Do we have to reset the accumulators on changing freqs?
    { div = timer.div
    , divAcc = timer.divAcc
    , tima = timer.tima
    , timaAcc = 0x00
    , tma = value
    , tac = timer.tac
    , triggeredInterrupt = timer.triggeredInterrupt
    }



-- Internal


divSpeed : Int
divSpeed =
    16384


cyclesPerSecond =
    4194304
