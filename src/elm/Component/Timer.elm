module Component.Timer exposing
    ( Timer
    , emulate
    , init
    , readDivider
    , readTac
    , readTima
    , readTma
    , writeDivider
    , writeTac
    , writeTima
    , writeTma
    )

import Bitwise


type alias Timer =
    { divider : Int
    , dividerAcc : Int
    , tima : Int
    , timaAcc : Int
    , tma : Int
    , tac : Int
    , triggeredInterrupt : Bool
    }


init : Timer
init =
    { divider = 0x00
    , dividerAcc = 0
    , tima = 0x00
    , timaAcc = 0
    , tma = 0x00
    , tac = 0x00
    , triggeredInterrupt = False
    }


emulate : Int -> Timer -> Timer
emulate cycles ({ dividerAcc, tac, timaAcc, tima, tma, divider } as timer) =
    let
        updatedDivAcc =
            dividerAcc + cycles

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
            if updatedDivAcc >= (cyclesPerSecond // dividerSpeed) then
                Bitwise.and 0xFF (divider + 1)

            else
                divider

        updatedFinalTima =
            if updatedTima > 0xFF then
                tma

            else
                updatedTima
    in
    { divider = updatedDiv
    , dividerAcc = remainderBy (cyclesPerSecond // dividerSpeed) updatedDivAcc
    , tima = updatedFinalTima
    , timaAcc = remainderBy timaCyclesPerIncrement updatedTimaAcc
    , tma = timer.tma
    , tac = timer.tac
    , triggeredInterrupt = updatedTima > 0xFF
    }



-- Register Reads


readDivider : Timer -> Int
readDivider { divider } =
    divider


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


writeDivider : Int -> Timer -> Timer
writeDivider _ timer =
    -- Writing will always reset to 0x00, regadless of actual written value
    { divider = 0x00
    , dividerAcc = 0x00
    , tima = timer.tima
    , timaAcc = 0x00
    , tma = timer.tma
    , tac = timer.tac
    , triggeredInterrupt = timer.triggeredInterrupt
    }


writeTima : Int -> Timer -> Timer
writeTima value timer =
    { divider = timer.divider
    , dividerAcc = timer.dividerAcc
    , tima = value
    , timaAcc = timer.timaAcc
    , tma = timer.tma
    , tac = timer.tac
    , triggeredInterrupt = timer.triggeredInterrupt
    }


writeTma : Int -> Timer -> Timer
writeTma value timer =
    { divider = timer.divider
    , dividerAcc = timer.dividerAcc
    , tima = timer.tima
    , timaAcc = timer.timaAcc
    , tma = value
    , tac = timer.tac
    , triggeredInterrupt = timer.triggeredInterrupt
    }


writeTac : Int -> Timer -> Timer
writeTac value timer =
    -- TODO: Do we have to reset the accumulators on changing freqs?
    { divider = timer.divider
    , dividerAcc = timer.dividerAcc
    , tima = timer.tima
    , timaAcc = timer.timaAcc
    , tma = timer.tma
    , tac = value
    , triggeredInterrupt = timer.triggeredInterrupt
    }



-- Internal


dividerSpeed : Int
dividerSpeed =
    16384


cyclesPerSecond =
    4194304
