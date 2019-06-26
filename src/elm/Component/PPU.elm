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
    , vramBank0 = RAM.initZero 0x2000
    , vramBank1 = RAM.initZero 0x2000 -- Size unclear
    , line = 0
    , lineCompare = 0
    , scrollX = 0
    , scrollY = 0
    , windowX = 0
    , windowY = 0
    , sprites = Array.empty
    , lcdc = 0x91
    , lcdStatus = 0x80
    , backgroundPalette = 0xE4
    , objectPalette0 = 0xFF
    , objectPalette1 = 0xFF
    , colorBackgroundPalette0 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorBackgroundPalette1 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorBackgroundPalette2 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorBackgroundPalette3 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorBackgroundPalette4 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorBackgroundPalette5 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorBackgroundPalette6 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorBackgroundPalette7 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorObjectPalette0 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorObjectPalette1 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorObjectPalette2 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorObjectPalette3 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorObjectPalette4 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorObjectPalette5 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorObjectPalette6 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , colorObjectPalette7 = Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
    , screen = GameBoyScreen.empty
    , lastCompleteFrame = Nothing
    , cyclesSinceLastCompleteFrame = 0
    , triggeredInterrupt = NoInterrupt
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


readVRAM : PPU -> Int -> MemoryAddress -> Int
readVRAM { vramBank0, vramBank1 } bank address =
    if bank == 0 then
        RAM.readWord8 vramBank0 address

    else
        RAM.readWord8 vramBank1 address


readOamRam : PPU -> MemoryAddress -> Int
readOamRam ppu address =
    let
        spriteIndex =
            address // 4

        maybeSprite =
            Array.get spriteIndex ppu.sprites
    in
    case maybeSprite of
        Just sprite ->
            case remainderBy 4 address of
                0 ->
                    sprite.y

                1 ->
                    sprite.x

                2 ->
                    sprite.tileId

                _ ->
                    sprite.flags

        Nothing ->
            0xFF



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


writeVRAM : Int -> MemoryAddress -> Int -> PPU -> PPU
writeVRAM bank address value ppu =
    let
        vram =
            if bank == 0 then
                ppu.vramBank0

            else
                ppu.vramBank1
    in
    PPUTypes.setVram (RAM.writeWord8 address value vram) ppu


writeOAMRam : MemoryAddress -> Int -> PPU -> PPU
writeOAMRam address value ppu =
    let
        spriteIndex =
            address // 4

        maybeSprite =
            Array.get spriteIndex ppu.sprites
    in
    case maybeSprite of
        Just sprite ->
            let
                updatedSprite =
                    case remainderBy 4 address of
                        0 ->
                            { y = value, x = sprite.x, tileId = sprite.tileId, flags = sprite.flags }

                        1 ->
                            { y = sprite.y, x = value, tileId = sprite.tileId, flags = sprite.flags }

                        2 ->
                            { y = sprite.y, x = sprite.x, tileId = value, flags = sprite.flags }

                        _ ->
                            { y = sprite.y, x = sprite.x, tileId = sprite.tileId, flags = value }

                updatedSprites =
                    Array.set spriteIndex updatedSprite ppu.sprites
            in
            PPUTypes.setSprites updatedSprites ppu

        Nothing ->
            ppu


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
    -- Mode and line did not change, we can just update essential fields and skip some work, gaining some performance.
    if updatedMode == ppu.mode && updatedLine == ppu.line then
        { mode = updatedMode
        , vramBank0 = ppu.vramBank0
        , vramBank1 = ppu.vramBank1
        , line = updatedLine
        , lineCompare = ppu.lineCompare
        , scrollX = ppu.scrollX
        , scrollY = ppu.scrollY
        , windowX = ppu.windowX
        , windowY = ppu.windowY
        , sprites = ppu.sprites
        , lcdc = ppu.lcdc
        , lcdStatus = ppu.lcdStatus
        , backgroundPalette = ppu.backgroundPalette
        , objectPalette0 = ppu.objectPalette0
        , objectPalette1 = ppu.objectPalette1
        , colorBackgroundPalette0 = ppu.colorBackgroundPalette0
        , colorBackgroundPalette1 = ppu.colorBackgroundPalette1
        , colorBackgroundPalette2 = ppu.colorBackgroundPalette2
        , colorBackgroundPalette3 = ppu.colorBackgroundPalette3
        , colorBackgroundPalette4 = ppu.colorBackgroundPalette4
        , colorBackgroundPalette5 = ppu.colorBackgroundPalette5
        , colorBackgroundPalette6 = ppu.colorBackgroundPalette6
        , colorBackgroundPalette7 = ppu.colorBackgroundPalette7
        , colorObjectPalette0 = ppu.colorObjectPalette0
        , colorObjectPalette1 = ppu.colorObjectPalette1
        , colorObjectPalette2 = ppu.colorObjectPalette2
        , colorObjectPalette3 = ppu.colorObjectPalette3
        , colorObjectPalette4 = ppu.colorObjectPalette4
        , colorObjectPalette5 = ppu.colorObjectPalette5
        , colorObjectPalette6 = ppu.colorObjectPalette6
        , colorObjectPalette7 = ppu.colorObjectPalette7
        , screen = ppu.screen
        , lastCompleteFrame = ppu.lastCompleteFrame
        , cyclesSinceLastCompleteFrame = lastEmulatedCycle
        , triggeredInterrupt = NoInterrupt
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
                    NoInterrupt

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
