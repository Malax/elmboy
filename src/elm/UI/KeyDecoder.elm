module UI.KeyDecoder exposing (decodeKey)

import Component.Joypad exposing (GameBoyButton(..))
import Json.Decode as Decode


decodeKey : Decode.Decoder GameBoyButton
decodeKey =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ArrowLeft" ->
                        Decode.succeed Left

                    "ArrowRight" ->
                        Decode.succeed Right

                    "ArrowUp" ->
                        Decode.succeed Up

                    "ArrowDown" ->
                        Decode.succeed Down

                    "s" ->
                        Decode.succeed A

                    "a" ->
                        Decode.succeed B

                    "Enter" ->
                        Decode.succeed Start

                    "Shift" ->
                        Decode.succeed Select

                    _ ->
                        Decode.fail "Pressed key is not a GameBoyButton"
            )
