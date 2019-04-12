module Component.PPU.LineBuffer exposing
    ( ObjectPriority(..)
    , mixPixelLine
    )

import Array exposing (Array)
import Bitwise
import Component.PPU.Pixel exposing (PixelSource(..), RawPixel)


type ObjectPriority
    = BehindBackground
    | OverBackground


mixPixelLine : Int -> ( Int, Int ) -> Bool -> PixelSource -> ObjectPriority -> Array RawPixel -> Array RawPixel
mixPixelLine offset ( highByte, lowByte ) reversed source priority rawPixels =
    let
        pixel1 =
            ( Bitwise.and 0x80 highByte // 0x40 + Bitwise.and 0x80 lowByte // 0x80, source )

        pixel2 =
            ( Bitwise.and 0x40 highByte // 0x20 + Bitwise.and 0x40 lowByte // 0x40, source )

        pixel3 =
            ( Bitwise.and 0x20 highByte // 0x10 + Bitwise.and 0x20 lowByte // 0x20, source )

        pixel4 =
            ( Bitwise.and 0x10 highByte // 0x08 + Bitwise.and 0x10 lowByte // 0x10, source )

        pixel5 =
            ( Bitwise.and 0x08 highByte // 0x04 + Bitwise.and 0x08 lowByte // 0x08, source )

        pixel6 =
            ( Bitwise.and 0x04 highByte // 0x02 + Bitwise.and 0x04 lowByte // 0x04, source )

        pixel7 =
            ( Bitwise.and 0x02 highByte // 0x01 + Bitwise.and 0x02 lowByte // 0x02, source )

        pixel8 =
            ( Bitwise.and 0x01 highByte * 2 + Bitwise.and 0x01 lowByte, source )
    in
    -- Performance Note: By manually unrolling this loop, we gain a nice performance boost at the cost of code duplication.
    -- This is a very 'hot' function and benchmarking proofed this measure has significant impact on overall performance.
    if reversed then
        rawPixels
            |> Array.set (offset + 0)
                (case Array.get (offset + 0) rawPixels of
                    Just value ->
                        mixPixel priority value pixel8

                    Nothing ->
                        pixel8
                )
            |> Array.set (offset + 1)
                (case Array.get (offset + 1) rawPixels of
                    Just value ->
                        mixPixel priority value pixel7

                    Nothing ->
                        pixel7
                )
            |> Array.set (offset + 2)
                (case Array.get (offset + 2) rawPixels of
                    Just value ->
                        mixPixel priority value pixel6

                    Nothing ->
                        pixel6
                )
            |> Array.set (offset + 3)
                (case Array.get (offset + 3) rawPixels of
                    Just value ->
                        mixPixel priority value pixel5

                    Nothing ->
                        pixel5
                )
            |> Array.set (offset + 4)
                (case Array.get (offset + 4) rawPixels of
                    Just value ->
                        mixPixel priority value pixel4

                    Nothing ->
                        pixel4
                )
            |> Array.set (offset + 5)
                (case Array.get (offset + 5) rawPixels of
                    Just value ->
                        mixPixel priority value pixel3

                    Nothing ->
                        pixel3
                )
            |> Array.set (offset + 6)
                (case Array.get (offset + 6) rawPixels of
                    Just value ->
                        mixPixel priority value pixel2

                    Nothing ->
                        pixel2
                )
            |> Array.set (offset + 7)
                (case Array.get (offset + 7) rawPixels of
                    Just value ->
                        mixPixel priority value pixel1

                    Nothing ->
                        pixel1
                )

    else
        rawPixels
            |> Array.set (offset + 0)
                (case Array.get (offset + 0) rawPixels of
                    Just value ->
                        mixPixel priority value pixel1

                    Nothing ->
                        pixel1
                )
            |> Array.set (offset + 1)
                (case Array.get (offset + 1) rawPixels of
                    Just value ->
                        mixPixel priority value pixel2

                    Nothing ->
                        pixel2
                )
            |> Array.set (offset + 2)
                (case Array.get (offset + 2) rawPixels of
                    Just value ->
                        mixPixel priority value pixel3

                    Nothing ->
                        pixel3
                )
            |> Array.set (offset + 3)
                (case Array.get (offset + 3) rawPixels of
                    Just value ->
                        mixPixel priority value pixel4

                    Nothing ->
                        pixel4
                )
            |> Array.set (offset + 4)
                (case Array.get (offset + 4) rawPixels of
                    Just value ->
                        mixPixel priority value pixel5

                    Nothing ->
                        pixel5
                )
            |> Array.set (offset + 5)
                (case Array.get (offset + 5) rawPixels of
                    Just value ->
                        mixPixel priority value pixel6

                    Nothing ->
                        pixel6
                )
            |> Array.set (offset + 6)
                (case Array.get (offset + 6) rawPixels of
                    Just value ->
                        mixPixel priority value pixel7

                    Nothing ->
                        pixel7
                )
            |> Array.set (offset + 7)
                (case Array.get (offset + 7) rawPixels of
                    Just value ->
                        mixPixel priority value pixel8

                    Nothing ->
                        pixel8
                )



-- Internal


mixPixel : ObjectPriority -> RawPixel -> RawPixel -> RawPixel
mixPixel priority existingPixel newPixel =
    -- New pixel is transparent
    if Tuple.first newPixel == 0x00 then
        existingPixel

    else if isSpritePixel existingPixel then
        existingPixel

    else
        case priority of
            BehindBackground ->
                -- Existing pixel is transparent
                if Tuple.first existingPixel == 0x00 then
                    newPixel

                else
                    existingPixel

            OverBackground ->
                newPixel


isSpritePixel : RawPixel -> Bool
isSpritePixel rawPixel =
    let
        source =
            Tuple.second rawPixel
    in
    source == ObjectWithPalette0 || source == ObjectWithPalette1
