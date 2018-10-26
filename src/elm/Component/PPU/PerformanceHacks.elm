module Component.PPU.PerformanceHacks exposing
    ( setBackgroundPalette
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
import Component.PPU.Types exposing (PPU, PPUInterrupt)
import Component.RAM exposing (RAM)


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
