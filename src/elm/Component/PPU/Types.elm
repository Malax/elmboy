module Component.PPU.Types exposing
    ( Mode(..)
    , PPU
    , PPUInterrupt(..)
    , setBackgroundPalette
    , setClockData
    , setLcdStatus
    , setLcdc
    , setLineCompare
    , setOamRam
    , setObjectPalette0
    , setObjectPalette1
    , setScrollX
    , setScrollY
    , setVBlankData
    , setVram
    , setWindowX
    , setWindowY
    )

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



-- Performance Optimized Setters


setOamRam : Array Int -> PPU -> PPU
setOamRam value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = ppu.windowX
    , windowY = ppu.windowY
    , objects = value
    , lcdc = ppu.lcdc
    , lcdStatus = ppu.lcdStatus
    , backgroundPalette = ppu.backgroundPalette
    , objectPalette0 = ppu.objectPalette0
    , objectPalette1 = ppu.objectPalette1
    , screen = ppu.screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setVram : RAM -> PPU -> PPU
setVram value ppu =
    { mode = ppu.mode
    , vram = value
    , line = ppu.line
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
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setLcdc : Int -> PPU -> PPU
setLcdc value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = ppu.windowX
    , windowY = ppu.windowY
    , objects = ppu.objects
    , lcdc = value
    , lcdStatus = ppu.lcdStatus
    , backgroundPalette = ppu.backgroundPalette
    , objectPalette0 = ppu.objectPalette0
    , objectPalette1 = ppu.objectPalette1
    , screen = ppu.screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setLcdStatus : Int -> PPU -> PPU
setLcdStatus value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = ppu.windowX
    , windowY = ppu.windowY
    , objects = ppu.objects
    , lcdc = ppu.lcdc
    , lcdStatus = value
    , backgroundPalette = ppu.backgroundPalette
    , objectPalette0 = ppu.objectPalette0
    , objectPalette1 = ppu.objectPalette1
    , screen = ppu.screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setScrollX : Int -> PPU -> PPU
setScrollX value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = value
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
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setScrollY : Int -> PPU -> PPU
setScrollY value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = value
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
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setWindowX : Int -> PPU -> PPU
setWindowX value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = value
    , windowY = ppu.windowY
    , objects = ppu.objects
    , lcdc = ppu.lcdc
    , lcdStatus = ppu.lcdStatus
    , backgroundPalette = ppu.backgroundPalette
    , objectPalette0 = ppu.objectPalette0
    , objectPalette1 = ppu.objectPalette1
    , screen = ppu.screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setWindowY : Int -> PPU -> PPU
setWindowY value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = ppu.windowX
    , windowY = value
    , objects = ppu.objects
    , lcdc = ppu.lcdc
    , lcdStatus = ppu.lcdStatus
    , backgroundPalette = ppu.backgroundPalette
    , objectPalette0 = ppu.objectPalette0
    , objectPalette1 = ppu.objectPalette1
    , screen = ppu.screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setLineCompare : Int -> PPU -> PPU
setLineCompare value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = value
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
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setBackgroundPalette : Int -> PPU -> PPU
setBackgroundPalette value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = ppu.windowX
    , windowY = ppu.windowY
    , objects = ppu.objects
    , lcdc = ppu.lcdc
    , lcdStatus = ppu.lcdStatus
    , backgroundPalette = value
    , objectPalette0 = ppu.objectPalette0
    , objectPalette1 = ppu.objectPalette1
    , screen = ppu.screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setObjectPalette0 : Int -> PPU -> PPU
setObjectPalette0 value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = ppu.windowX
    , windowY = ppu.windowY
    , objects = ppu.objects
    , lcdc = ppu.lcdc
    , lcdStatus = ppu.lcdStatus
    , backgroundPalette = ppu.backgroundPalette
    , objectPalette0 = value
    , objectPalette1 = ppu.objectPalette1
    , screen = ppu.screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setObjectPalette1 : Int -> PPU -> PPU
setObjectPalette1 value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
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
    , objectPalette1 = value
    , screen = ppu.screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setVBlankData : GameBoyScreen -> GameBoyScreen -> Bool -> Maybe PPUInterrupt -> PPU -> PPU
setVBlankData lastCompleteFrame screen omitFrame triggeredInterrupt ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
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
    , screen = screen
    , lastCompleteFrame = lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = triggeredInterrupt
    , omitFrame = omitFrame
    }


setClockData : Mode -> Int -> Int -> Int -> Maybe PPUInterrupt -> PPU -> PPU
setClockData currentMode currentLine lcdStatus cyclesSinceLastCompleteFrame interrupt ppu =
    { mode = currentMode
    , vram = ppu.vram
    , line = currentLine
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = ppu.windowX
    , windowY = ppu.windowY
    , objects = ppu.objects
    , lcdc = ppu.lcdc
    , lcdStatus = lcdStatus
    , backgroundPalette = ppu.backgroundPalette
    , objectPalette0 = ppu.objectPalette0
    , objectPalette1 = ppu.objectPalette1
    , screen = ppu.screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = cyclesSinceLastCompleteFrame
    , triggeredInterrupt = interrupt
    , omitFrame = ppu.omitFrame
    }
