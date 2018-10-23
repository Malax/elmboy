module Component.PPU.LineBuffer exposing
    ( LineBuffer(..)
    , init
    , mixPixels
    , unpack
    )

import Component.PPU.Pixel as Pixel exposing (PixelSource(..), RawPixel)


type LineBuffer
    = LineBuffer (List RawPixel)


mixPixels : Int -> List RawPixel -> LineBuffer -> LineBuffer
mixPixels offset pixels ((LineBuffer bufferedPixels) as lineBuffer) =
    let
        clampedOffset =
            clamp 0 lineLength offset

        pixelCount =
            List.length pixels

        dropPixelCount =
            if offset < 0 then
                abs offset

            else
                0

        takePixelCount =
            lineLength - clampedOffset

        sanitizedPixels =
            pixels |> List.drop dropPixelCount |> List.take takePixelCount
    in
    modifySliceSlice (\slice -> zip slice sanitizedPixels |> List.map mixPixel) clampedOffset (clampedOffset + List.length sanitizedPixels) bufferedPixels
        |> LineBuffer


unpack : LineBuffer -> List RawPixel
unpack (LineBuffer array) =
    array


init : List RawPixel -> LineBuffer
init pixels =
    if List.length pixels == lineLength then
        LineBuffer pixels

    else
        pixels
            |> List.take lineLength
            |> List.append (List.repeat (lineLength - List.length pixels) ( 0x00, Background ))
            |> LineBuffer



-- Internal


lineLength : Int
lineLength =
    160


mixPixel : ( RawPixel, RawPixel ) -> RawPixel
mixPixel ( pixel1, pixel2 ) =
    if isObjectPixel pixel1 then
        pixel1

    else if isTransparentPixel pixel2 then
        pixel1

    else
        pixel2


{-| Checks if the given pixel is coming from an object
-}
isObjectPixel : RawPixel -> Bool
isObjectPixel ( _, source ) =
    source == ObjectWithPalette0 || source == ObjectWithPalette1


{-| Checks if the given pixel is transparent or not. Only object pixels can be possibly transparent.
-}
isTransparentPixel : RawPixel -> Bool
isTransparentPixel (( data, source ) as pixel) =
    isObjectPixel pixel && data == 0x00


modifySliceSlice : (List a -> List a) -> Int -> Int -> List a -> List a
modifySliceSlice f start end list =
    let
        prefix =
            List.take start list

        suffix =
            List.drop end list

        slice =
            list |> List.drop start |> List.take (end - start)

        mappedSlice =
            f slice
    in
    List.append prefix (List.append mappedSlice suffix)


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair
