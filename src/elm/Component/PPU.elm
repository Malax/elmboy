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
import Component.PPU.Constants exposing (cyclesPerFrame, cyclesPerLine, cyclesPerOamSearch, cyclesPerPixelTransfer, screenHeight, vBlankDurationInLines)
import Component.PPU.GameBoyScreen as GameBoyScreen exposing (GameBoyScreen)
import Component.PPU.LineDrawing as LineDrawing
import Component.PPU.Types as PPUTypes exposing (Mode(..), PPU, PPUInterrupt(..))
import Component.RAM as RAM
import Constants
import Types exposing (MemoryAddress)


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
    , lastCompleteFrame = Nothing
    , cyclesSinceLastCompleteFrame = 0
    , triggeredInterrupt = None
    , omitFrame = False
    }


getLastCompleteFrame : PPU -> ( PPU, Maybe GameBoyScreen )
getLastCompleteFrame ppu =
    case ppu.lastCompleteFrame of
        Just a ->
            ( PPUTypes.setLastCompleteFrame Nothing ppu, Just a )

        Nothing ->
            ( ppu, Nothing )



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


replaceOAMRam : Array Int -> PPU -> PPU
replaceOAMRam bytes ppu =
    if Array.length bytes == (40 * 4) then
        PPUTypes.setOamRam bytes ppu

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
emulate cycles ppu =
    let
        lastEmulatedCycle =
            ppu.cyclesSinceLastCompleteFrame + cycles

        cyclesSinceLastCompleteLine =
            remainderBy cyclesPerLine lastEmulatedCycle

        updatedLine =
            lastEmulatedCycle // cyclesPerLine

        updatedMode =
            if updatedLine > (screenHeight + vBlankDurationInLines) then
                OamSearch

            else if updatedLine >= screenHeight then
                VBlank

            else if cyclesSinceLastCompleteLine < cyclesPerOamSearch then
                OamSearch

            else if cyclesSinceLastCompleteLine < cyclesPerPixelTransfer then
                PixelTransfer

            else
                HBlank
    in
    -- Mode and line did not change, we can just update essential fields and skip some work for performance reasons
    if updatedMode == ppu.mode && updatedLine == ppu.line then
        { mode = updatedMode
        , vram = ppu.vram
        , line = updatedLine
        , lineCompare = ppu.lineCompare
        , scrollX = ppu.scrollX
        , scrollY = ppu.scrollY
        , windowX = ppu.windowX
        , windowY = ppu.windowY
        , objects = ppu.objects
        , lcdc = ppu.lcdc
        , lcdStatus = ppu.lcdStatus
        , backgroundPalette = ppu.backgroundPalette
        , objectPalette0 = ppu.objectPalette0
        , objectPalette1 = ppu.objectPalette1
        , screen = ppu.screen
        , lastCompleteFrame = ppu.lastCompleteFrame
        , cyclesSinceLastCompleteFrame = lastEmulatedCycle
        , triggeredInterrupt = None
        , omitFrame = ppu.omitFrame
        }

    else
        -- If the mode or line changes, we need to check for possible interrupts, updating LCDSTAT and/or drawing pixels.
        let
            hBlankInterruptEnabled =
                Bitwise.and ppu.lcdStatus Constants.bit3Mask == Constants.bit3Mask

            oamInterruptEnabled =
                Bitwise.and ppu.lcdStatus Constants.bit5Mask == Constants.bit5Mask

            lineCompareInterruptEnabled =
                Bitwise.and ppu.lcdStatus Constants.bit6Mask == Constants.bit6Mask

            interrupt =
                if hBlankInterruptEnabled && updatedMode == HBlank && ppu.mode /= updatedMode then
                    HBlankInterrupt

                else if lineCompareInterruptEnabled && updatedLine == ppu.lineCompare && ppu.line /= updatedLine then
                    LineCompareInterrupt

                else if oamInterruptEnabled && updatedMode == OamSearch && ppu.mode /= updatedMode then
                    OamInterrupt

                else
                    None

            modeChangeEffect =
                if ppu.mode == PixelTransfer && updatedMode == HBlank && not ppu.omitFrame then
                    LineDrawing.drawLine updatedLine

                else if ppu.mode == HBlank && updatedMode == VBlank then
                    if ppu.omitFrame then
                        PPUTypes.setVBlankData Nothing GameBoyScreen.empty False VBlankInterrupt

                    else
                        PPUTypes.setVBlankData (Just ppu.screen) GameBoyScreen.empty True VBlankInterrupt

                else
                    identity
        in
        ppu
            |> PPUTypes.setEmulateData updatedMode updatedLine (lcdStatus updatedMode updatedLine ppu.lineCompare ppu.lcdStatus) (remainderBy cyclesPerFrame lastEmulatedCycle) interrupt
            |> modeChangeEffect
