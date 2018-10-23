module Component.PPU.Types exposing (Mode(..), PPU, PPUInterrupt(..))

import Array exposing (Array)
import Component.PPU.GameBoyScreen exposing (GameBoyScreen)
import Component.RAM exposing (RAM)


type alias PPU =
    { mode : Mode
    , vram : RAM
    , line : Int
    , lineCompare : Int
    , scrollX : Int
    , scrollY : Int
    , windowX : Int
    , windowY : Int
    , lcdc : Int
    , objects : Array Int
    , screen : GameBoyScreen
    , lastCompleteFrame : GameBoyScreen
    , cyclesSinceLastCompleteFrame : Int
    , backgroundPalette : Int
    , objectPalette0 : Int
    , objectPalette1 : Int
    , triggeredInterrupt : Maybe PPUInterrupt
    , lcdStatus : Int

    {-
       We omit every other frame to increase emulation performance. Omitted frames are still emulated, but no pixels will be
       produced - speeding up the emulation at the cost of halved refresh rate (30fps). Omitted frames will use the same pixels as the
       previous frame.
    -}
    , omitFrame : Bool
    }


type Mode
    = OamSearch
    | PixelTransfer
    | HBlank
    | VBlank


type PPUInterrupt
    = VBlankInterrupt
    | HBlankInterrupt
    | LineCompareInterrupt
    | OamInterrupt
