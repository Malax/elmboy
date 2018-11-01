module Component.PPU exposing
    ( emulate
    , getLastCompleteFrame
    , init
    , readBackgroundPalette
    , readLCDC
    , readLCDStatus
    , readLY
    , readLYC
    , readOamRam
    , readObjectPalette0
    , readObjectPalette1
    , readScrollX
    , readScrollY
    , readVRAM
    , readWindowX
    , readWindowY
    , replaceOAMRam
    , writeBackgroundPalette
    , writeLCDC
    , writeLCDStatus
    , writeLY
    , writeLYC
    , writeOAMRam
    , writeObjectPalette0
    , writeObjectPalette1
    , writeScrollX
    , writeScrollY
    , writeVRAM
    , writeWindowX
    , writeWindowY
    )

import Array exposing (Array)
import Bitwise
import Component.PPU.Constants exposing (..)
import Component.PPU.GameBoyScreen as GameBoyScreen exposing (GameBoyScreen)
import Component.PPU.LineBuffer as LineBuffer exposing (LineBuffer)
import Component.PPU.LineDrawing as LineDrawing
import Component.PPU.OAM exposing (searchVisibleObjects)
import Component.PPU.Pixel as Pixel exposing (..)
import Component.PPU.Types as PPUTypes exposing (Mode(..), PPU, PPUInterrupt(..))
import Component.RAM as RAM exposing (RAM)
import Constants
import Types exposing (MemoryAddress)
import Util


init : PPU
init =
    { mode = VBlank
    , vram = RAM.initZero 0x2000
    , line = 0
    , lineCompare = 0
    , scrollX = 0
    , scrollY = 0
    , windowX = 0
    , windowY = 0
    , objects = Array.repeat (40 * 4) 0x00
    , lcdc = 0x91
    , lcdStatus = 0x80
    , backgroundPalette = 0xE4
    , objectPalette0 = 0xFF
    , objectPalette1 = 0xFF
    , screen = GameBoyScreen.empty
    , lastCompleteFrame = GameBoyScreen.empty
    , cyclesSinceLastCompleteFrame = 0
    , triggeredInterrupt = Nothing
    , omitFrame = False
    }


getLastCompleteFrame : PPU -> GameBoyScreen
getLastCompleteFrame =
    .lastCompleteFrame



-- Register Reads


readLCDC : PPU -> Int
readLCDC =
    .lcdc


readLCDStatus : PPU -> Int
readLCDStatus =
    .lcdStatus


readScrollX : PPU -> Int
readScrollX =
    .scrollX


readScrollY : PPU -> Int
readScrollY =
    .scrollY


readWindowX : PPU -> Int
readWindowX =
    .windowX


readWindowY : PPU -> Int
readWindowY =
    .windowY


readLY : PPU -> Int
readLY =
    .line


readLYC : PPU -> Int
readLYC =
    .lineCompare


readObjectPalette0 : PPU -> Int
readObjectPalette0 =
    .objectPalette0


readObjectPalette1 : PPU -> Int
readObjectPalette1 =
    .objectPalette1


readBackgroundPalette : PPU -> Int
readBackgroundPalette =
    .backgroundPalette



-- RAM Reads


readVRAM : PPU -> MemoryAddress -> Int
readVRAM { vram } address =
    RAM.readWord8 vram address


readOamRam : PPU -> MemoryAddress -> Int
readOamRam { objects } address =
    Array.get address objects
        |> Maybe.withDefault 0xFF



-- Register Writes


writeLCDC : Int -> PPU -> PPU
writeLCDC =
    PPUTypes.setLcdc


writeLCDStatus : Int -> PPU -> PPU
writeLCDStatus =
    PPUTypes.setLcdStatus


writeScrollX : Int -> PPU -> PPU
writeScrollX =
    PPUTypes.setScrollX


writeScrollY : Int -> PPU -> PPU
writeScrollY =
    PPUTypes.setScrollY


writeWindowX : Int -> PPU -> PPU
writeWindowX =
    PPUTypes.setWindowX


writeWindowY : Int -> PPU -> PPU
writeWindowY =
    PPUTypes.setWindowY


writeLY : Int -> PPU -> PPU
writeLY _ ppu =
    -- Implement me
    ppu


writeLYC : Int -> PPU -> PPU
writeLYC =
    PPUTypes.setLineCompare


writeBackgroundPalette : Int -> PPU -> PPU
writeBackgroundPalette =
    PPUTypes.setBackgroundPalette


writeObjectPalette0 : Int -> PPU -> PPU
writeObjectPalette0 =
    PPUTypes.setObjectPalette0


writeObjectPalette1 : Int -> PPU -> PPU
writeObjectPalette1 =
    PPUTypes.setObjectPalette1



-- RAM Writes


writeVRAM : MemoryAddress -> Int -> PPU -> PPU
writeVRAM address value ppu =
    PPUTypes.setVram (RAM.writeWord8 address value ppu.vram) ppu


writeOAMRam : MemoryAddress -> Int -> PPU -> PPU
writeOAMRam address value ppu =
    PPUTypes.setOamRam (Array.set address value ppu.objects) ppu


replaceOAMRam : List Int -> PPU -> PPU
replaceOAMRam bytes ppu =
    if List.length bytes == (40 * 4) then
        PPUTypes.setOamRam (Array.fromList bytes) ppu

    else
        ppu



-- Emulation


lcdStatus : Mode -> Int -> Int -> Int -> Int
lcdStatus mode line lineCompare previousLcdStatus =
    let
        modeBits =
            case mode of
                HBlank ->
                    0x00

                VBlank ->
                    0x01

                OamSearch ->
                    0x02

                PixelTransfer ->
                    0x03

        coincidenceBits =
            if line == lineCompare then
                0x04

            else
                0x00
    in
    -- 0x78 == 0b11111000, keeping all interrupt control bits
    Bitwise.and previousLcdStatus 0x78
        + modeBits
        + coincidenceBits


emulate : Int -> PPU -> PPU
emulate cycles ({ mode, cyclesSinceLastCompleteFrame, omitFrame } as ppu) =
    let
        lastEmulatedCycle =
            cyclesSinceLastCompleteFrame + cycles

        currentLine =
            lastEmulatedCycle // cyclesPerLine

        cyclesSinceLastCompleteLine =
            remainderBy cyclesPerLine lastEmulatedCycle

        hBlankInterruptEnabled =
            Bitwise.and ppu.lcdStatus Constants.bit3Mask == Constants.bit3Mask

        oamInterruptEnabled =
            Bitwise.and ppu.lcdStatus Constants.bit5Mask == Constants.bit5Mask

        lineCompareInterruptEnabled =
            Bitwise.and ppu.lcdStatus Constants.bit6Mask == Constants.bit6Mask

        currentMode =
            if currentLine > (screenHeight + vBlankDurationInLines) then
                OamSearch

            else if currentLine >= screenHeight then
                VBlank

            else if cyclesSinceLastCompleteLine < cyclesPerOamSearch then
                OamSearch

            else if cyclesSinceLastCompleteLine < cyclesPerPixelTransfer then
                PixelTransfer

            else
                HBlank

        interrupt =
            if hBlankInterruptEnabled && currentMode == HBlank && mode /= currentMode then
                Just HBlankInterrupt

            else if lineCompareInterruptEnabled && currentLine == ppu.lineCompare && ppu.line /= currentLine then
                Just LineCompareInterrupt

            else if oamInterruptEnabled && currentMode == OamSearch && mode /= currentMode then
                Just OamInterrupt

            else
                Nothing

        modifiedPPUData =
            PPUTypes.setEmulateData
                currentMode
                currentLine
                (lcdStatus currentMode currentLine ppu.lineCompare ppu.lcdStatus)
                (remainderBy cyclesPerFrame lastEmulatedCycle)
                interrupt
                ppu
    in
    case ( mode, currentMode ) of
        ( PixelTransfer, HBlank ) ->
            if not omitFrame then
                LineDrawing.drawLine currentLine modifiedPPUData

            else
                modifiedPPUData

        ( HBlank, VBlank ) ->
            if not omitFrame then
                PPUTypes.setVBlankData modifiedPPUData.screen modifiedPPUData.screen True (Just VBlankInterrupt) modifiedPPUData

            else
                PPUTypes.setVBlankData modifiedPPUData.screen GameBoyScreen.empty False (Just VBlankInterrupt) modifiedPPUData

        _ ->
            modifiedPPUData
