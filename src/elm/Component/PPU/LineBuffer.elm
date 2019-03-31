module Component.PPU.LineBuffer exposing
    ( LineBuffer(..)
    , ObjectPriority(..)
    , init
    , mixEightPixels
    , unpack
    )

import Array exposing (Array)
import Bitwise
import Component.PPU.Pixel exposing (PixelSource(..), RawPixel)


type LineBuffer
    = LineBuffer (Array RawPixel)


type ObjectPriority
    = BehindBackground
    | OverBackground


mixEightPixels : Int -> ( Int, Int ) -> Bool -> PixelSource -> ObjectPriority -> LineBuffer -> LineBuffer
mixEightPixels offset ( highByte, lowByte ) reversed source priority (LineBuffer rawPixels) =
    let
        pixel1 =
            ( Bitwise.and 0x01 highByte * 2 + Bitwise.and 0x01 lowByte, source )

        pixel2 =
            ( Bitwise.and 0x02 highByte // 0x01 + Bitwise.and 0x02 lowByte // 0x02, source )

        pixel3 =
            ( Bitwise.and 0x04 highByte // 0x02 + Bitwise.and 0x04 lowByte // 0x04, source )

        pixel4 =
            ( Bitwise.and 0x08 highByte // 0x04 + Bitwise.and 0x08 lowByte // 0x08, source )

        pixel5 =
            ( Bitwise.and 0x10 highByte // 0x08 + Bitwise.and 0x10 lowByte // 0x10, source )

        pixel6 =
            ( Bitwise.and 0x20 highByte // 0x10 + Bitwise.and 0x20 lowByte // 0x20, source )

        pixel7 =
            ( Bitwise.and 0x40 highByte // 0x20 + Bitwise.and 0x40 lowByte // 0x40, source )

        pixel8 =
            ( Bitwise.and 0x80 highByte // 0x40 + Bitwise.and 0x80 lowByte // 0x80, source )
    in
    let
        defaultPixel =
            ( 4, Background )
    in
    if reversed then
        rawPixels
            |> Array.set (offset + 0) (Array.get (offset + 0) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel1) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 1) (Array.get (offset + 1) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel2) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 2) (Array.get (offset + 2) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel3) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 3) (Array.get (offset + 3) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel4) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 4) (Array.get (offset + 4) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel5) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 5) (Array.get (offset + 5) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel6) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 6) (Array.get (offset + 6) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel7) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 7) (Array.get (offset + 7) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel8) |> Maybe.withDefault defaultPixel)
            |> LineBuffer

    else
        rawPixels
            |> Array.set (offset + 0) (Array.get (offset + 0) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel8) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 1) (Array.get (offset + 1) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel7) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 2) (Array.get (offset + 2) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel6) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 3) (Array.get (offset + 3) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel5) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 4) (Array.get (offset + 4) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel4) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 5) (Array.get (offset + 5) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel3) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 6) (Array.get (offset + 6) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel2) |> Maybe.withDefault defaultPixel)
            |> Array.set (offset + 7) (Array.get (offset + 7) rawPixels |> Maybe.map (\prevPixel -> mixPixel priority prevPixel pixel1) |> Maybe.withDefault defaultPixel)
            |> LineBuffer


unpack : LineBuffer -> List RawPixel
unpack (LineBuffer array) =
    array |> Array.toList


init : List RawPixel -> LineBuffer
init =
    Array.fromList >> LineBuffer



-- Internal


mixPixel : ObjectPriority -> RawPixel -> RawPixel -> RawPixel
mixPixel priority existingPixel newPixel =
    if isTransparentPixel newPixel then
        existingPixel

    else if isObjectPixel existingPixel then
        existingPixel

    else
        case priority of
            BehindBackground ->
                if isTransparentPixel existingPixel then
                    newPixel

                else
                    existingPixel

            OverBackground ->
                newPixel


isObjectPixel : RawPixel -> Bool
isObjectPixel rawPixel =
    let
        source =
            Tuple.second rawPixel
    in
    source == ObjectWithPalette0 || source == ObjectWithPalette1


isTransparentPixel : RawPixel -> Bool
isTransparentPixel rawPixel =
    Tuple.first rawPixel == 0x00
