module Component.PPU.LineBuffer exposing
    ( LineBuffer(..)
    , ObjectPriority(..)
    , init
    , mixPixels
    , unpack
    )

import Component.PPU.Pixel as Pixel exposing (PixelSource(..), RawPixel)


type LineBuffer
    = LineBuffer (List RawPixel)


type ObjectPriority
    = BehindBackground
    | OverBackground


mixPixels : Int -> List RawPixel -> ObjectPriority -> LineBuffer -> LineBuffer
mixPixels offset pixels priority ((LineBuffer bufferedPixels) as lineBuffer) =
    let
        clampedOffset =
            clamp 0 (List.length bufferedPixels) offset

        pixelCount =
            List.length pixels

        dropPixelCount =
            if offset < 0 then
                abs offset

            else
                0

        takePixelCount =
            List.length bufferedPixels - clampedOffset

        sanitizedPixels =
            pixels |> List.drop dropPixelCount |> List.take takePixelCount
    in
    modifySliceSlice (\slice -> zip slice sanitizedPixels |> List.map (mixPixel priority)) clampedOffset (clampedOffset + List.length sanitizedPixels) bufferedPixels
        |> LineBuffer


unpack : LineBuffer -> List RawPixel
unpack (LineBuffer array) =
    array


init : List RawPixel -> LineBuffer
init pixels =
    LineBuffer pixels



-- Internal


mixPixel : ObjectPriority -> ( RawPixel, RawPixel ) -> RawPixel
mixPixel priority ( existingPixel, newPixel ) =
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
