module Component.PPU exposing
    ( emulateClocks
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
import Component.PPU.OAM exposing (foldRIndexes, searchVisibleObjects)
import Component.PPU.Pixel as Pixel exposing (..)
import Component.PPU.Types exposing (Mode(..), PPU, PPUInterrupt(..))
import Component.RAM as RAM exposing (RAM)
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
writeLCDC value ppu =
    { ppu | lcdc = value }


writeLCDStatus : Int -> PPU -> PPU
writeLCDStatus value ppu =
    { ppu | lcdStatus = value }


writeScrollX : Int -> PPU -> PPU
writeScrollX value ppu =
    { ppu | scrollX = value }


writeScrollY : Int -> PPU -> PPU
writeScrollY value ppu =
    { ppu | scrollY = value }


writeWindowX : Int -> PPU -> PPU
writeWindowX value ppu =
    { ppu | windowX = value }


writeWindowY : Int -> PPU -> PPU
writeWindowY value ppu =
    { ppu | windowY = value }


writeLY : Int -> PPU -> PPU
writeLY _ ppu =
    -- Implement me
    ppu


writeLYC : Int -> PPU -> PPU
writeLYC value ppu =
    { ppu | lineCompare = value }


writeBackgroundPalette : Int -> PPU -> PPU
writeBackgroundPalette value ppu =
    { ppu | backgroundPalette = value }


writeObjectPalette0 : Int -> PPU -> PPU
writeObjectPalette0 value ppu =
    { ppu | objectPalette0 = value }


writeObjectPalette1 : Int -> PPU -> PPU
writeObjectPalette1 value ppu =
    { ppu | objectPalette1 = value }



-- RAM Writes


writeVRAM : MemoryAddress -> Int -> PPU -> PPU
writeVRAM address value ({ vram } as ppu) =
    { ppu | vram = RAM.writeWord8 address value vram }


writeOAMRam : MemoryAddress -> Int -> PPU -> PPU
writeOAMRam address value ({ objects } as ppu) =
    { ppu | objects = Array.set address value objects }


replaceOAMRam : List Int -> PPU -> PPU
replaceOAMRam bytes ppu =
    if List.length bytes == (40 * 4) then
        { ppu | objects = Array.fromList bytes }

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


emulateClocks : Int -> PPU -> PPU
emulateClocks cyclesToEmulate ({ mode, cyclesSinceLastCompleteFrame, omitFrame } as ppu) =
    let
        lastEmulatedCycle =
            cyclesSinceLastCompleteFrame + cyclesToEmulate

        currentLine =
            lastEmulatedCycle // cyclesPerLine

        cyclesSinceLastCompleteLine =
            remainderBy cyclesPerLine lastEmulatedCycle

        hBlankInterruptEnabled =
            Bitwise.and ppu.lcdStatus 0x08 == 0x08

        lineCompareInterruptEnabled =
            Bitwise.and ppu.lcdStatus 0x40 == 0x40

        oamInterruptEnabled =
            Bitwise.and ppu.lcdStatus 0x20 == 0x20

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
            { ppu
                | line = currentLine
                , mode = currentMode
                , cyclesSinceLastCompleteFrame = remainderBy cyclesPerFrame lastEmulatedCycle
                , triggeredInterrupt = interrupt
                , lcdStatus = lcdStatus currentMode currentLine ppu.lineCompare ppu.lcdStatus
            }
    in
    case ( mode, currentMode ) of
        ( PixelTransfer, HBlank ) ->
            if not omitFrame then
                LineDrawing.drawLine currentLine modifiedPPUData

            else
                modifiedPPUData

        ( HBlank, VBlank ) ->
            if not omitFrame then
                { modifiedPPUData | lastCompleteFrame = modifiedPPUData.screen, omitFrame = True, triggeredInterrupt = Just VBlankInterrupt }

            else
                { modifiedPPUData | screen = GameBoyScreen.empty, omitFrame = False, triggeredInterrupt = Just VBlankInterrupt }

        _ ->
            modifiedPPUData
