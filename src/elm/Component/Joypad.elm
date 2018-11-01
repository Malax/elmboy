module Component.Joypad exposing
    ( GameBoyButton(..)
    , Joypad
    , init
    , readRegister
    , setAPressed
    , setBPressed
    , setDownPressed
    , setLeftPressed
    , setRightPressed
    , setSelectButtonKeys
    , setSelectDirectionKeys
    , setSelectPressed
    , setStartPressed
    , setUpPressed
    , writeRegister
    )

import Bitwise
import Constants


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
    , triggeredInterrupt : Bool
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
    , triggeredInterrupt = False
    }


readRegister : Joypad -> Int
readRegister joypad =
    let
        bitmasks =
            if joypad.selectDirectionKeys then
                [ 0xD0 -- 1101 0000
                , conditionalBitmask joypad.downPressed Constants.bit3Mask
                , conditionalBitmask joypad.upPressed Constants.bit2Mask
                , conditionalBitmask joypad.leftPressed Constants.bit1Mask
                , conditionalBitmask joypad.rightPressed Constants.bit0Mask
                ]

            else if joypad.selectButtonKeys then
                [ 0xE0 -- 1110 0000
                , conditionalBitmask joypad.startPressed Constants.bit3Mask
                , conditionalBitmask joypad.selectPressed Constants.bit2Mask
                , conditionalBitmask joypad.bPressed Constants.bit1Mask
                , conditionalBitmask joypad.aPressed Constants.bit0Mask
                ]

            else
                -- 1100 0000
                [ 0xC0 ]
    in
    bitmasks
        |> List.foldr Bitwise.or 0x00
        |> Bitwise.complement
        |> Bitwise.and 0xFF


writeRegister : Int -> Joypad -> Joypad
writeRegister value joypad =
    { joypad
        | selectButtonKeys = Bitwise.and Constants.bit5Mask value /= Constants.bit5Mask
        , selectDirectionKeys = Bitwise.and Constants.bit4Mask value /= Constants.bit4Mask
    }



-- Performance Optimized Setters


setSelectDirectionKeys : Bool -> Joypad -> Joypad
setSelectDirectionKeys value joypad =
    { selectDirectionKeys = value
    , selectButtonKeys = joypad.selectButtonKeys
    , upPressed = joypad.upPressed
    , downPressed = joypad.downPressed
    , leftPressed = joypad.leftPressed
    , rightPressed = joypad.rightPressed
    , aPressed = joypad.aPressed
    , bPressed = joypad.bPressed
    , startPressed = joypad.startPressed
    , selectPressed = joypad.selectPressed
    , triggeredInterrupt = joypad.triggeredInterrupt
    }


setSelectButtonKeys : Bool -> Joypad -> Joypad
setSelectButtonKeys value joypad =
    { selectDirectionKeys = joypad.selectDirectionKeys
    , selectButtonKeys = value
    , upPressed = joypad.upPressed
    , downPressed = joypad.downPressed
    , leftPressed = joypad.leftPressed
    , rightPressed = joypad.rightPressed
    , aPressed = joypad.aPressed
    , bPressed = joypad.bPressed
    , startPressed = joypad.startPressed
    , selectPressed = joypad.selectPressed
    , triggeredInterrupt = joypad.triggeredInterrupt
    }


setUpPressed : Bool -> Joypad -> Joypad
setUpPressed value joypad =
    { selectDirectionKeys = joypad.selectDirectionKeys
    , selectButtonKeys = joypad.selectButtonKeys
    , upPressed = value
    , downPressed = joypad.downPressed
    , leftPressed = joypad.leftPressed
    , rightPressed = joypad.rightPressed
    , aPressed = joypad.aPressed
    , bPressed = joypad.bPressed
    , startPressed = joypad.startPressed
    , selectPressed = joypad.selectPressed
    , triggeredInterrupt = joypad.triggeredInterrupt
    }


setDownPressed : Bool -> Joypad -> Joypad
setDownPressed value joypad =
    { selectDirectionKeys = joypad.selectDirectionKeys
    , selectButtonKeys = joypad.selectButtonKeys
    , upPressed = joypad.upPressed
    , downPressed = value
    , leftPressed = joypad.leftPressed
    , rightPressed = joypad.rightPressed
    , aPressed = joypad.aPressed
    , bPressed = joypad.bPressed
    , startPressed = joypad.startPressed
    , selectPressed = joypad.selectPressed
    , triggeredInterrupt = joypad.triggeredInterrupt
    }


setLeftPressed : Bool -> Joypad -> Joypad
setLeftPressed value joypad =
    { selectDirectionKeys = joypad.selectDirectionKeys
    , selectButtonKeys = joypad.selectButtonKeys
    , upPressed = joypad.upPressed
    , downPressed = joypad.downPressed
    , leftPressed = value
    , rightPressed = joypad.rightPressed
    , aPressed = joypad.aPressed
    , bPressed = joypad.bPressed
    , startPressed = joypad.startPressed
    , selectPressed = joypad.selectPressed
    , triggeredInterrupt = joypad.triggeredInterrupt
    }


setRightPressed : Bool -> Joypad -> Joypad
setRightPressed value joypad =
    { selectDirectionKeys = joypad.selectDirectionKeys
    , selectButtonKeys = joypad.selectButtonKeys
    , upPressed = joypad.upPressed
    , downPressed = joypad.downPressed
    , leftPressed = joypad.leftPressed
    , rightPressed = value
    , aPressed = joypad.aPressed
    , bPressed = joypad.bPressed
    , startPressed = joypad.startPressed
    , selectPressed = joypad.selectPressed
    , triggeredInterrupt = joypad.triggeredInterrupt
    }


setAPressed : Bool -> Joypad -> Joypad
setAPressed value joypad =
    { selectDirectionKeys = joypad.selectDirectionKeys
    , selectButtonKeys = joypad.selectButtonKeys
    , upPressed = joypad.upPressed
    , downPressed = joypad.downPressed
    , leftPressed = joypad.leftPressed
    , rightPressed = joypad.rightPressed
    , aPressed = value
    , bPressed = joypad.bPressed
    , startPressed = joypad.startPressed
    , selectPressed = joypad.selectPressed
    , triggeredInterrupt = joypad.triggeredInterrupt
    }


setBPressed : Bool -> Joypad -> Joypad
setBPressed value joypad =
    { selectDirectionKeys = joypad.selectDirectionKeys
    , selectButtonKeys = joypad.selectButtonKeys
    , upPressed = joypad.upPressed
    , downPressed = joypad.downPressed
    , leftPressed = joypad.leftPressed
    , rightPressed = joypad.rightPressed
    , aPressed = joypad.aPressed
    , bPressed = value
    , startPressed = joypad.startPressed
    , selectPressed = joypad.selectPressed
    , triggeredInterrupt = joypad.triggeredInterrupt
    }


setStartPressed : Bool -> Joypad -> Joypad
setStartPressed value joypad =
    { selectDirectionKeys = joypad.selectDirectionKeys
    , selectButtonKeys = joypad.selectButtonKeys
    , upPressed = joypad.upPressed
    , downPressed = joypad.downPressed
    , leftPressed = joypad.leftPressed
    , rightPressed = joypad.rightPressed
    , aPressed = joypad.aPressed
    , bPressed = joypad.bPressed
    , startPressed = value
    , selectPressed = joypad.selectPressed
    , triggeredInterrupt = joypad.triggeredInterrupt
    }


setSelectPressed : Bool -> Joypad -> Joypad
setSelectPressed value joypad =
    { selectDirectionKeys = joypad.selectDirectionKeys
    , selectButtonKeys = joypad.selectButtonKeys
    , upPressed = joypad.upPressed
    , downPressed = joypad.downPressed
    , leftPressed = joypad.leftPressed
    , rightPressed = joypad.rightPressed
    , aPressed = joypad.aPressed
    , bPressed = joypad.bPressed
    , startPressed = joypad.startPressed
    , selectPressed = value
    , triggeredInterrupt = joypad.triggeredInterrupt
    }



-- Utilities


conditionalBitmask : Bool -> Int -> Int
conditionalBitmask condition bitmask =
    if condition then
        bitmask

    else
        0x00
