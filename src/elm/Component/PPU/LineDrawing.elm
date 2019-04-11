module Component.PPU.LineDrawing exposing (drawLine)

import Array exposing (Array)
import Bitwise
import Component.PPU.Constants exposing (backgroundMapHeight, backgroundMapWidth, screenWidth, tileHeight, tileWidth)
import Component.PPU.GameBoyScreen as GameBoyScreen
import Component.PPU.LineBuffer as LineBuffer exposing (LineBuffer)
import Component.PPU.OAM as OAM
import Component.PPU.Pixel as Pixel exposing (PixelSource(..), RawPixel)
import Component.PPU.Types as PPUTypes exposing (PPU)
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
    PPUTypes.setScreen (GameBoyScreen.pushPixels screen linePixels) ppu


{-| Reads an amount of pixels, starting from the given coordinate, from a background map. Windows are technically exactly the same as backgrounds,
so this function can be used for both getting pixels from the background or the window map.
-}
backgroundPixels : Int -> Int -> Int -> Int -> MemoryAddress -> RAM -> List RawPixel
backgroundPixels mapY mapX pixelAmount lcdc mapAddress vram =
    let
        -- The line index inside the tiles we need to grab pixels from.
        tileLineIndex =
            remainderBy tileHeight mapY

        tileAmount =
            (pixelAmount // tileWidth) + 1

        pixels =
            fetchTileLines mapAddress lcdc (mapX // tileWidth) (mapY // tileHeight) tileLineIndex tileAmount vram
    in
    pixels
        -- TODO: Not happy with this, maybe we can avoid creating the pixels in the first place?
        |> List.drop (remainderBy tileWidth mapX)
        |> List.take pixelAmount


fetchTileLines : MemoryAddress -> Int -> Int -> Int -> Int -> Int -> RAM -> List RawPixel
fetchTileLines mapAddress lcdc tileMapX tileMapY tileLine amount vram =
    fetchTileLinesHelp mapAddress lcdc ( tileMapX, tileMapY ) tileLine amount vram []


fetchTileLinesHelp : MemoryAddress -> Int -> ( Int, Int ) -> Int -> Int -> RAM -> List RawPixel -> List RawPixel
fetchTileLinesHelp mapAddress lcdc ( tileMapX, tileMapY ) tileLine amount vram acc =
    if amount == 0 then
        acc

    else
        let
            wrappedTileMapX =
                remainderBy backgroundMapWidth (tileMapX + amount - 1)

            tileIdAddress =
                mapAddress + (tileMapY * backgroundMapWidth) + wrappedTileMapX

            tileId =
                RAM.readWord8 vram tileIdAddress

            tileDataAddress =
                if Bitwise.and Constants.bit4Mask lcdc == Constants.bit4Mask then
                    tileId * 16

                else
                    0x0800 + ((Util.byteToSignedInt tileId + 128) * 16)

            ( highByte, lowByte ) =
                readTileLinePixels tileDataAddress tileLine vram

            pixel1 =
                Bitwise.and 0x01 highByte * 2 + Bitwise.and 0x01 lowByte

            pixel2 =
                Bitwise.and 0x02 highByte // 0x01 + Bitwise.and 0x02 lowByte // 0x02

            pixel3 =
                Bitwise.and 0x04 highByte // 0x02 + Bitwise.and 0x04 lowByte // 0x04

            pixel4 =
                Bitwise.and 0x08 highByte // 0x04 + Bitwise.and 0x08 lowByte // 0x08

            pixel5 =
                Bitwise.and 0x10 highByte // 0x08 + Bitwise.and 0x10 lowByte // 0x10

            pixel6 =
                Bitwise.and 0x20 highByte // 0x10 + Bitwise.and 0x20 lowByte // 0x20

            pixel7 =
                Bitwise.and 0x40 highByte // 0x20 + Bitwise.and 0x40 lowByte // 0x40

            pixel8 =
                Bitwise.and 0x80 highByte // 0x40 + Bitwise.and 0x80 lowByte // 0x80

            updatedAcc =
                ( pixel8, Background )
                    :: ( pixel7, Background )
                    :: ( pixel6, Background )
                    :: ( pixel5, Background )
                    :: ( pixel4, Background )
                    :: ( pixel3, Background )
                    :: ( pixel2, Background )
                    :: ( pixel1, Background )
                    :: acc
        in
        fetchTileLinesHelp mapAddress lcdc ( tileMapX, tileMapY ) tileLine (amount - 1) vram updatedAcc


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


readTileLinePixels : MemoryAddress -> Int -> RAM -> ( Int, Int )
readTileLinePixels tileAddress lineNumber vram =
    let
        tileLineAddress =
            tileAddress + (lineNumber * 2)

        tileLineHighByte =
            RAM.readWord8 vram (tileLineAddress + 1)

        tileLineLowByte =
            RAM.readWord8 vram tileLineAddress
    in
    ( tileLineHighByte, tileLineLowByte )


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

        priority =
            if Bitwise.and Constants.bit7Mask objectFlags == Constants.bit7Mask then
                LineBuffer.BehindBackground

            else
                LineBuffer.OverBackground

        line =
            if flipY then
                (objectHeight - 1) - (screenY - normalizedY)

            else
                screenY - normalizedY

        tileAddress =
            objectTileId * 16

        rawPixels =
            readTileLinePixels tileAddress line vram
    in
    LineBuffer.mixEightPixels normalizedX rawPixels flipX palette priority buffer
