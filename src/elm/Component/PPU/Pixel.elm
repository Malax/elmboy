module Component.PPU.Pixel exposing
    ( Palette
    , Pixel
    , PixelSource(..)
    , RawPixel
    , bake
    )

import Bitwise
import Component.PPU.Constants exposing (bitsPerPixel)


type PixelSource
    = Background
    | Window
    | ObjectWithPalette0
    | ObjectWithPalette1


type alias RawPixel =
    ( Int, PixelSource )


type alias Pixel =
    Int


type alias Palette =
    Int


bake : Palette -> Palette -> Palette -> RawPixel -> Pixel
bake backgroundPalette objectPalette0 objectPalette1 ( rawPixelData, rawPixelSource ) =
    let
        paletteToUse =
            case rawPixelSource of
                Background ->
                    backgroundPalette

                Window ->
                    backgroundPalette

                ObjectWithPalette0 ->
                    objectPalette0

                ObjectWithPalette1 ->
                    objectPalette1

        mask =
            Bitwise.shiftLeftBy (rawPixelData * bitsPerPixel) 0x03
    in
    paletteToUse
        |> Bitwise.and mask
        |> Bitwise.shiftRightBy (rawPixelData * bitsPerPixel)
