module Component.PPU.GameBoyColorScreen exposing
    ( GameBoyColorScreen
    , empty
    , pushPixel
    , pushPixels
    , serializePixelBatches
    )

import Bitwise
import Component.PPU.Pixel exposing (Pixel)


type GameBoyColorScreen
    = GameBoyColorScreen (List Int)


empty : GameBoyColorScreen
empty =
    GameBoyColorScreen []


pushPixel : Pixel -> GameBoyColorScreen -> GameBoyColorScreen
pushPixel pixel (GameBoyColorScreen pixels) =
    GameBoyColorScreen (pixel :: pixels)


pushPixels : GameBoyColorScreen -> List Pixel -> GameBoyColorScreen
pushPixels screen pixels =
    List.foldl pushPixel screen pixels


serializePixelBatches : GameBoyColorScreen -> List Int
serializePixelBatches (GameBoyColorScreen pixels) =
    List.reverse pixels
