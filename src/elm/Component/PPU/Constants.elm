module Component.PPU.Constants exposing
    ( backgroundMapHeight
    , backgroundMapWidth
    , bitsPerPixel
    , cyclesPerFrame
    , cyclesPerHBlank
    , cyclesPerLine
    , cyclesPerOamSearch
    , cyclesPerPixelTransfer
    , screenHeight
    , screenWidth
    , tileHeight
    , tileWidth
    , vBlankDurationInLines
    )


bitsPerPixel : Int
bitsPerPixel =
    2


backgroundMapWidth : Int
backgroundMapWidth =
    32


backgroundMapHeight : Int
backgroundMapHeight =
    32


tileHeight : Int
tileHeight =
    8


tileWidth : Int
tileWidth =
    8


screenHeight : Int
screenHeight =
    144


screenWidth : Int
screenWidth =
    160


cyclesPerOamSearch : Int
cyclesPerOamSearch =
    80


vBlankDurationInLines : Int
vBlankDurationInLines =
    10


cyclesPerPixelTransfer : Int
cyclesPerPixelTransfer =
    172


cyclesPerHBlank : Int
cyclesPerHBlank =
    204


cyclesPerLine : Int
cyclesPerLine =
    cyclesPerOamSearch + cyclesPerPixelTransfer + cyclesPerHBlank


cyclesPerFrame : Int
cyclesPerFrame =
    cyclesPerLine * (screenHeight + vBlankDurationInLines)
