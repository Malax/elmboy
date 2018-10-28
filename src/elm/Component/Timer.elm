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
    , tac : Int
    , tima : Int
    , tma : Int
    , triggeredInterrupt : Bool
    }


init : Timer
init =
    { divider = 0
    , tac = 0
    , tima = 0
    , tma = 0
    , triggeredInterrupt = False
    }


emulate : Int -> Timer -> Timer
emulate cycles timer =
    let
        updatedTima =
            if timerEnabled timer then
                timer.tima + countOverflows timer.divider cycles (timaOverflowBitPosition timer)

            else
                timer.tima

        timaOverflowed =
            updatedTima > 0xFF
    in
    { divider = Bitwise.and 0xFFFF (timer.divider + cycles)
    , tac = timer.tac
    , tima =
        if timaOverflowed then
            timer.tma + Bitwise.and 0xFF updatedTima

        else
            updatedTima
    , tma = timer.tma
    , triggeredInterrupt = timaOverflowed
    }


readDivider : Timer -> Int
readDivider timer =
    -- Internally, div is 16-bit wide. But only the least significant byte is exposed.
    Bitwise.and 0xFF timer.divider


readTac : Timer -> Int
readTac timer =
    timer.tac


readTima : Timer -> Int
readTima timer =
    timer.tima


readTma : Timer -> Int
readTma timer =
    timer.tma


writeDivider : Int -> Timer -> Timer
writeDivider _ timer =
    let
        updatedTima =
            if timerEnabled timer then
                timer.divider
                    |> Bitwise.shiftRightZfBy (timaOverflowBitPosition timer)
                    |> Bitwise.and 0x01
                    |> (+) timer.tima

            else
                timer.tima

        timaOverflowed =
            updatedTima > 0xFF
    in
    -- Writing will always reset to 0x00, regadless of actual written value
    { divider = 0x00
    , tac = timer.tac
    , tima =
        if timaOverflowed then
            timer.tma

        else
            updatedTima
    , tma = timer.tma
    , triggeredInterrupt = timaOverflowed || timer.triggeredInterrupt
    }


writeTac : Int -> Timer -> Timer
writeTac value timer =
    { divider = timer.divider
    , tac = value
    , tima = timer.tima
    , tma = timer.tma
    , triggeredInterrupt = timer.triggeredInterrupt
    }


writeTima : Int -> Timer -> Timer
writeTima value timer =
    { divider = timer.divider
    , tac = timer.tac
    , tima = value
    , tma = timer.tma
    , triggeredInterrupt = timer.triggeredInterrupt
    }


writeTma : Int -> Timer -> Timer
writeTma value timer =
    { divider = timer.divider
    , tac = timer.tac
    , tima = value
    , tma = timer.tma
    , triggeredInterrupt = timer.triggeredInterrupt
    }



-- Internals


countOverflows : Int -> Int -> Int -> Int
countOverflows a b bitPosition =
    let
        previousOverflows =
            Bitwise.shiftRightZfBy (bitPosition + 1) a

        overflows =
            Bitwise.shiftRightZfBy (bitPosition + 1) (a + b)
    in
    overflows - previousOverflows


timerEnabled : Timer -> Bool
timerEnabled timer =
    Bitwise.and 0x04 timer.tac == 0x04


timaOverflowBitPosition : Timer -> Int
timaOverflowBitPosition timer =
    case Bitwise.and 0x03 timer.tac of
        0x00 ->
            9

        0x01 ->
            3

        0x02 ->
            5

        -- Only 0x03 can happen, but the compiler cannot infer that. So we just match on everything.
        _ ->
            7
