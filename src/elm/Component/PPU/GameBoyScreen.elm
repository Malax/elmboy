module Component.PPU.GameBoyScreen exposing
    ( GameBoyScreen
    , empty
    , pushPixel
    , pushPixels
    , serializePixelBatches
    )

import Array exposing (Array)
import Bitwise
import Component.PPU.Pixel exposing (Pixel)


type GameBoyScreen
    = GameBoyScreen (List Int) Int Int


empty : GameBoyScreen
empty =
    GameBoyScreen [] 0 0x00


pushPixel : Pixel -> GameBoyScreen -> GameBoyScreen
pushPixel pixel (GameBoyScreen batchedPixels pixelsInBuffer buffer) =
    let
        updatedBuffer =
            Bitwise.or pixel (Bitwise.shiftLeftBy 2 buffer)

        updatedPixelsInBuffer =
            pixelsInBuffer + 1
    in
    if updatedPixelsInBuffer == 16 then
        GameBoyScreen (updatedBuffer :: batchedPixels) 0 0x00

    else
        GameBoyScreen batchedPixels updatedPixelsInBuffer updatedBuffer


pushPixels : GameBoyScreen -> List Pixel -> GameBoyScreen
pushPixels screen pixels =
    List.foldl pushPixel screen pixels


serializePixelBatches : GameBoyScreen -> List Int
serializePixelBatches (GameBoyScreen batchedPixels pixelsInBuffer buffer) =
    let
        resultAfterFlushing =
            if pixelsInBuffer /= 0 then
                Bitwise.shiftLeftBy (16 - pixelsInBuffer * 2) buffer :: batchedPixels

            else
                batchedPixels

        missingBatchesAmount =
            1440 - List.length resultAfterFlushing

        emptyBatches =
            List.repeat missingBatchesAmount 0x00
    in
    List.append (List.reverse resultAfterFlushing) emptyBatches
