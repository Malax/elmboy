module Component.PPU.Types exposing
    ( Mode(..)
    , PPU
    , PPUInterrupt(..)
    , Sprite
    , setBackgroundPalette
    , setEmulateData
    , setLastCompleteFrame
    , setLcdStatus
    , setLcdc
    , setLineCompare
    , setOamRam
    , setObjectPalette0
    , setObjectPalette1
    , setScreen
    , setScrollX
    , setScrollY
    , setSprites
    , setVBlankData
    , setVram
    , setWindowX
    , setWindowY
    )

import Array exposing (Array)
import Component.PPU.GameBoyScreen exposing (GameBoyScreen)
import Component.RAM exposing (RAM)


type alias Sprite =
    { y : Int
    , x : Int
    , tileId : Int
    , flags : Int
    }


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
    , sprites : Array Sprite
    , screen : GameBoyScreen
    , lastCompleteFrame : Maybe GameBoyScreen
    , cyclesSinceLastCompleteFrame : Int
    , backgroundPalette : Int
    , objectPalette0 : Int
    , objectPalette1 : Int
    , triggeredInterrupt : PPUInterrupt
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
    | NoInterrupt



-- Performance Optimized Setters


parseSprites : Array Int -> Array Sprite -> Array Sprite
parseSprites objectAttributeMemory acc =
    let
        parsedSprite =
            Maybe.map4 Sprite
                (Array.get 0 objectAttributeMemory)
                (Array.get 1 objectAttributeMemory)
                (Array.get 2 objectAttributeMemory)
                (Array.get 3 objectAttributeMemory)
    in
    case parsedSprite of
        Just sprite ->
            parseSprites (Array.slice 4 (Array.length objectAttributeMemory) objectAttributeMemory) (Array.push sprite acc)

        Nothing ->
            acc


setSprites : Array Sprite -> PPU -> PPU
setSprites value ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = ppu.windowX
    , windowY = ppu.windowY
    , sprites = value
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
    , sprites = parseSprites value Array.empty
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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
    , sprites = ppu.sprites
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


setVBlankData : Maybe GameBoyScreen -> GameBoyScreen -> Bool -> PPUInterrupt -> PPU -> PPU
setVBlankData lastCompleteFrame screen omitFrame triggeredInterrupt ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
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
    , screen = screen
    , lastCompleteFrame = lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = triggeredInterrupt
    , omitFrame = omitFrame
    }


setEmulateData : Mode -> Int -> Int -> Int -> PPUInterrupt -> PPU -> PPU
setEmulateData currentMode currentLine lcdStatus cyclesSinceLastCompleteFrame interrupt ppu =
    { mode = currentMode
    , vram = ppu.vram
    , line = currentLine
    , lineCompare = ppu.lineCompare
    , scrollX = ppu.scrollX
    , scrollY = ppu.scrollY
    , windowX = ppu.windowX
    , windowY = ppu.windowY
    , sprites = ppu.sprites
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


setScreen : GameBoyScreen -> PPU -> PPU
setScreen screen ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
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
    , screen = screen
    , lastCompleteFrame = ppu.lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }


setLastCompleteFrame : Maybe GameBoyScreen -> PPU -> PPU
setLastCompleteFrame lastCompleteFrame ppu =
    { mode = ppu.mode
    , vram = ppu.vram
    , line = ppu.line
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
    , screen = ppu.screen
    , lastCompleteFrame = lastCompleteFrame
    , cyclesSinceLastCompleteFrame = ppu.cyclesSinceLastCompleteFrame
    , triggeredInterrupt = ppu.triggeredInterrupt
    , omitFrame = ppu.omitFrame
    }
