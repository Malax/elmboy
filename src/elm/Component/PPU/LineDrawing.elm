module Component.PPU.LineDrawing exposing (drawLine)

import Array exposing (Array)
import Bitwise
import Component.PPU.Constants exposing (..)
import Component.PPU.GameBoyScreen as GameBoyScreen exposing (GameBoyScreen)
import Component.PPU.LineBuffer as LineBuffer exposing (LineBuffer)
import Component.PPU.OAM as OAM
import Component.PPU.Pixel as Pixel exposing (PixelSource(..), RawPixel)
import Component.PPU.Types exposing (Mode, PPU)
import Component.RAM as RAM exposing (RAM)
import Constants
import Types exposing (MemoryAddress)
import Util


drawLine : Int -> PPU -> PPU
drawLine screenY ({ backgroundPalette, objectPalette0, objectPalette1, screen, scrollY, scrollX, windowX, windowY, objects, lcdc, vram } as ppu) =
    let
        objectHeight =
            if Bitwise.and Constants.bit2Mask lcdc == Constants.bit2Mask then
                16

            else
                8

        linePixels =
            viewportBackgroundLinePixels screenY scrollY scrollX windowX windowY lcdc vram
                |> LineBuffer.init
                |> addObjectsToLineBuffer screenY vram objects objectHeight
                |> LineBuffer.unpack
                |> List.map (Pixel.bake backgroundPalette objectPalette0 objectPalette1)
    in
    { ppu | screen = GameBoyScreen.pushPixels screen linePixels }


{-| Reads an amount of pixels, starting from the given coordinate, from a background map. Windows are technically exactly the same as backgrounds,
so this function can be used for both getting pixels from the background or the window map.
-}
backgroundPixels : Int -> Int -> Int -> Int -> MemoryAddress -> RAM -> List RawPixel
backgroundPixels mapY mapX pixelAmount lcdc mapAddress vram =
    let
        -- The line index inside the tiles we need to grab pixels from.
        tileLineIndex =
            remainderBy tileHeight mapY

        -- The offset of the lines of tiles for the line we're getting the pixels from.
        backgroundMapLineOffset =
            (mapY // tileHeight) * backgroundMapWidth

        -- The offset of the first tile we're grabbing pixels from, relative to the start of the line tiles in the background map.
        firstTileOffset =
            mapX // tileWidth

        tileAmount =
            (pixelAmount // tileWidth) + 1

        pixels =
            Util.foldRIndexes tileAmount [] <|
                \index acc ->
                    let
                        -- If we run out of tiles on the right, we just start at the beginning of the line again, instead of
                        -- possibly reading data from the next line. This implements the required screen wrapping.
                        tileIdAddress =
                            mapAddress + backgroundMapLineOffset + remainderBy backgroundMapWidth (firstTileOffset + index)

                        tileId =
                            RAM.readWord8 vram tileIdAddress

                        tileDataAddress =
                            backgroundTileAddress lcdc tileId
                    in
                    List.append (readTileLinePixels tileDataAddress tileLineIndex vram False Background) acc
    in
    pixels
        -- TODO: Not happy with this, maybe we can avoid creating the pixels in the first place?
        |> List.drop (remainderBy tileWidth mapX)
        |> List.take pixelAmount


{-| Reads a whole line of raw pixels from the background map (and referenced tiles) for the given screen line and screen scroll offsets.
-}
viewportBackgroundLinePixels : Int -> Int -> Int -> Int -> Int -> Int -> RAM -> List RawPixel
viewportBackgroundLinePixels screenY scrollY scrollX windowX windowY lcdc vram =
    let
        -- LCDC Data extraction
        backgroundMapMemoryOffset =
            if Bitwise.and Constants.bit3Mask lcdc == Constants.bit3Mask then
                0x1C00

            else
                0x1800

        windowMapMemoryOffset =
            if Bitwise.and Constants.bit6Mask lcdc == Constants.bit6Mask then
                0x1C00

            else
                0x1800

        windowEnabled =
            Bitwise.and Constants.bit5Mask lcdc == Constants.bit5Mask

        windowInLine =
            windowEnabled && screenY >= windowY

        windowScreenX =
            if windowX - 7 < 0 then
                0

            else
                windowX - 7

        windowOffsetX =
            abs (windowX - 7)

        backgroundMapY =
            remainderBy (backgroundMapHeight * tileHeight) (screenY + scrollY)

        requiredBackgroundPixels =
            if windowInLine then
                windowScreenX

            else
                screenWidth

        requiredWindowPixels =
            if windowInLine then
                screenWidth - windowScreenX

            else
                0
    in
    if requiredWindowPixels > 0 then
        backgroundPixels backgroundMapY scrollX requiredBackgroundPixels lcdc backgroundMapMemoryOffset vram
            ++ backgroundPixels (screenY - windowY) windowOffsetX requiredWindowPixels lcdc windowMapMemoryOffset vram

    else
        backgroundPixels backgroundMapY scrollX requiredBackgroundPixels lcdc backgroundMapMemoryOffset vram


{-| Reads an eight pixel line of a tile from memory.
This works for background (and therefore window) as well as for objects tiles - they are encoded the same way.
To support 8x16 tiles, we allow the lineNumber to be larger than 8. Note that a number larger than 8 will actually read the next tile in memory!
-}
readTileLinePixels : MemoryAddress -> Int -> RAM -> Bool -> PixelSource -> List RawPixel
readTileLinePixels tileAddress lineNumber vram readReversed pixelSource =
    let
        tileLineAddress =
            tileAddress + (lineNumber * 2)

        tileLineHighByte =
            RAM.readWord8 vram (tileLineAddress + 1)

        tileLineLowByte =
            RAM.readWord8 vram tileLineAddress
    in
    Util.foldRIndexes 8 [] <|
        \reversedIndex acc ->
            let
                -- TODO: The order seems correct here, but we need to reverse it so that it looks right.
                -- TODO: Maybe another part of the code is reversing it again? Remember that we're building the resuling list with cons, so our order should
                -- be correct.
                index =
                    if readReversed then
                        reversedIndex

                    else
                        7 - reversedIndex

                pixelDataBitmask =
                    Bitwise.shiftLeftBy index 0x01

                highValue =
                    Bitwise.and tileLineHighByte pixelDataBitmask
                        |> Bitwise.shiftRightZfBy index
                        -- We cannot directly reduce the amount shifted to the right, as the index might be zero.
                        |> Bitwise.shiftLeftBy 1

                lowValue =
                    Bitwise.and tileLineLowByte pixelDataBitmask
                        |> Bitwise.shiftRightZfBy index
            in
            ( Bitwise.or highValue lowValue, pixelSource ) :: acc


addObjectsToLineBuffer : Int -> RAM -> Array Int -> Int -> LineBuffer -> LineBuffer
addObjectsToLineBuffer screenY vram objects objectHeight buffer =
    let
        objectIndexes =
            OAM.searchVisibleObjects screenY objectHeight objects
    in
    List.foldl
        (\index line ->
            Maybe.map4 (addObjectToLineBuffer screenY vram objectHeight line)
                (Array.get (index * 4) objects)
                (Array.get (index * 4 + 1) objects)
                (Array.get (index * 4 + 2) objects)
                (Array.get (index * 4 + 3) objects)
                |> Maybe.withDefault line
        )
        buffer
        objectIndexes


{-| Adds pixel data of an object into the given line buffer. This function will not validate if the object is actually visible in the given line and might
read garbage data from unrelated memory or other tiles and render it! This is useful for 8x16 objects as, in that we case, we want to read into the next tile.
-}
addObjectToLineBuffer : Int -> RAM -> Int -> LineBuffer -> Int -> Int -> Int -> Int -> LineBuffer
addObjectToLineBuffer screenY vram objectHeight buffer objectY objectX objectTileId objectFlags =
    let
        normalizedY =
            objectY - 16

        normalizedX =
            objectX - 8

        palette =
            if Bitwise.and Constants.bit4Mask objectFlags == Constants.bit4Mask then
                ObjectWithPalette1

            else
                ObjectWithPalette0

        flipX =
            Bitwise.and Constants.bit5Mask objectFlags == Constants.bit5Mask

        flipY =
            Bitwise.and Constants.bit6Mask objectFlags == Constants.bit6Mask

        line =
            if flipY then
                (objectHeight - 1) - (screenY - normalizedY)

            else
                screenY - normalizedY

        tileAddress =
            objectTileId * 16

        rawPixels =
            readTileLinePixels tileAddress line vram flipX palette
    in
    LineBuffer.mixPixels normalizedX rawPixels buffer



-- Addressing Helpers


backgroundTileAddress : Int -> Int -> MemoryAddress
backgroundTileAddress lcdc tileId =
    if Bitwise.and Constants.bit4Mask lcdc == Constants.bit4Mask then
        tileId * 16

    else
        0x0800 + ((Util.byteToSignedInt tileId + 128) * 16)
