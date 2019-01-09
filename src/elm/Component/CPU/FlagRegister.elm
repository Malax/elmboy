module Component.CPU.FlagRegister exposing
    ( Flag(..)
    , FlagDelta(..)
    , FlagsRegisterDelta
    , getFlag
    , modifyFlags
    , normalize
    , setAllFlags
    , setFlag
    , toString
    )

import Bitwise


type Flag
    = Zero
    | Subtract
    | HalfCarry
    | Carry


type alias FlagsRegisterDelta =
    Int -> Int


type FlagDelta
    = Unchanged
    | Complemented
    | StaticValue Bool


modifyFlags : FlagDelta -> FlagDelta -> FlagDelta -> FlagDelta -> FlagsRegisterDelta
modifyFlags z n h c flags =
    let
        zValue =
            resolveDelta z zeroMask flags

        nValue =
            resolveDelta n subtractMask flags

        hValue =
            resolveDelta h halfCarryMask flags

        cValue =
            resolveDelta c carryMask flags
    in
    List.foldr Bitwise.or 0x00 [ zValue, nValue, hValue, cValue ]


resolveDelta : FlagDelta -> Int -> Int -> Int
resolveDelta delta mask flags =
    case delta of
        Unchanged ->
            Bitwise.and flags mask

        StaticValue True ->
            mask

        StaticValue False ->
            0x00

        Complemented ->
            if Bitwise.and flags mask == 0x00 then
                mask

            else
                0x00


setAllFlags : Bool -> Bool -> Bool -> Bool -> Int
setAllFlags z n h c =
    [ if z then
        zeroMask

      else
        0x00
    , if n then
        subtractMask

      else
        0x00
    , if h then
        halfCarryMask

      else
        0x00
    , if c then
        carryMask

      else
        0x00
    ]
        |> List.foldl Bitwise.or 0x00


setFlag : Flag -> Bool -> FlagsRegisterDelta
setFlag flag value =
    let
        mask =
            maskByFlag flag
    in
    if value then
        Bitwise.or mask

    else
        Bitwise.and (Bitwise.complement mask)


getFlag : Flag -> Int -> Bool
getFlag flag flags =
    Bitwise.and (maskByFlag flag) flags > 0


normalize : Int -> Int
normalize flagByte =
    Bitwise.and flagByte 0xF0


toString : Flag -> String
toString flag =
    case flag of
        Zero ->
            "Zero"

        Subtract ->
            "Subtract"

        HalfCarry ->
            "HalfCarry"

        Carry ->
            "Carry"



-- Internal


maskByFlag : Flag -> Int
maskByFlag flag =
    case flag of
        Zero ->
            zeroMask

        Subtract ->
            subtractMask

        HalfCarry ->
            halfCarryMask

        Carry ->
            carryMask


zeroMask : Int
zeroMask =
    Bitwise.shiftLeftBy 7 0x01


subtractMask : Int
subtractMask =
    Bitwise.shiftLeftBy 6 0x01


halfCarryMask : Int
halfCarryMask =
    Bitwise.shiftLeftBy 5 0x01


carryMask : Int
carryMask =
    Bitwise.shiftLeftBy 4 0x01
