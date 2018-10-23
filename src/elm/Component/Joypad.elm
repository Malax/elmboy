module Component.Joypad exposing
    ( GameBoyButton(..)
    , Joypad
    , init
    , readRegister
    , writeRegister
    )

import Bitwise


type alias Joypad =
    { selectDirectionKeys : Bool
    , selectButtonKeys : Bool
    , upPressed : Bool
    , downPressed : Bool
    , leftPressed : Bool
    , rightPressed : Bool
    , aPressed : Bool
    , bPressed : Bool
    , startPressed : Bool
    , selectPressed : Bool
    }


type GameBoyButton
    = Up
    | Down
    | Left
    | Right
    | A
    | B
    | Start
    | Select


init : Joypad
init =
    { selectDirectionKeys = False
    , selectButtonKeys = False
    , upPressed = False
    , downPressed = False
    , leftPressed = False
    , rightPressed = False
    , aPressed = False
    , bPressed = False
    , startPressed = False
    , selectPressed = False
    }


readRegister : Joypad -> Int
readRegister joypad =
    let
        bitmasks =
            if joypad.selectDirectionKeys then
                [ 0xD0 -- 1101 0000
                , conditionalBitmask joypad.downPressed joypadDownBitmask
                , conditionalBitmask joypad.upPressed joypadUpBitmask
                , conditionalBitmask joypad.leftPressed joypadLeftBitmask
                , conditionalBitmask joypad.rightPressed joypadRightBitmask
                ]

            else if joypad.selectButtonKeys then
                [ 0xE0 -- 1110 0000
                , conditionalBitmask joypad.startPressed joypadStartBitmask
                , conditionalBitmask joypad.selectPressed joypadSelectBitmask
                , conditionalBitmask joypad.aPressed joypadABitmask
                , conditionalBitmask joypad.bPressed joypadBBitmask
                ]

            else
                -- 1100 0000
                [ 0xC0 ]
    in
    bitmasks
        |> List.foldr Bitwise.or 0x00
        |> Bitwise.complement


writeRegister : Int -> Joypad -> Joypad
writeRegister value joypad =
    { joypad
        | selectButtonKeys = Bitwise.and 0x20 value /= 0x20
        , selectDirectionKeys = Bitwise.and 0x10 value /= 0x10
    }



-- Utilities


conditionalBitmask : Bool -> Int -> Int
conditionalBitmask condition bitmask =
    if condition then
        bitmask

    else
        0x00



-- Direction Key Bitmasks


joypadRightBitmask : Int
joypadRightBitmask =
    0x01


joypadLeftBitmask : Int
joypadLeftBitmask =
    Bitwise.shiftLeftBy 1 0x01


joypadUpBitmask : Int
joypadUpBitmask =
    Bitwise.shiftLeftBy 2 0x01


joypadDownBitmask : Int
joypadDownBitmask =
    Bitwise.shiftLeftBy 3 0x01



-- Button Key Bitmasks


joypadABitmask : Int
joypadABitmask =
    0x01


joypadBBitmask : Int
joypadBBitmask =
    Bitwise.shiftLeftBy 1 0x01


joypadSelectBitmask : Int
joypadSelectBitmask =
    Bitwise.shiftLeftBy 2 0x01


joypadStartBitmask : Int
joypadStartBitmask =
    Bitwise.shiftLeftBy 3 0x01
