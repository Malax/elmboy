module UI.KeyDecoder exposing (decodeKey)

import Component.Joypad exposing (GameBoyButton(..))
import Json.Decode as Decode


decodeKey : Decode.Decoder (Maybe GameBoyButton)
decodeKey =
    Decode.map mapKey (Decode.field "key" Decode.string)


mapKey : String -> Maybe GameBoyButton
mapKey string =
    case string of
        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        "s" ->
            Just A

        "a" ->
            Just B

        "Enter" ->
            Just Start

        "Shift" ->
            Just Select

        _ ->
            Nothing
