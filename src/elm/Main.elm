module Main exposing (main)

import Array exposing (Array)
import Bootstrap.Modal as Modal
import Browser
import Browser.Events
import Component.Cartridge as Cartridge
import Component.Joypad exposing (GameBoyButton(..))
import Component.PPU as PPU
import Component.PPU.GameBoyScreen as GameBoyScreen
import Emulator
import GameBoy exposing (GameBoy)
import Html exposing (Html)
import Json.Decode as Decode
import Model exposing (Model(..))
import Msg exposing (Msg(..))
import Ports
import UI.KeyDecoder
import View


init : () -> ( Model, Cmd Msg )
init _ =
    ( Idle { errorModal = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Idle idleModel ->
            case msg of
                FileDataReceived data ->
                    case Cartridge.fromBytes data of
                        Just cartridge ->
                            ( Emulation { gameBoy = GameBoy.init cartridge, paused = False, lastFrameTime = 1000 / 60 }, Cmd.none )

                        Nothing ->
                            let
                                errorModal =
                                    Just
                                        { visibility = Modal.shown
                                        , title = "Unsupported ROM"
                                        , body = "Your selected ROM is not yet supported by Elmboy. This is usually the case due to an unsupported memory bank controller required by the ROM you're trying to run. Please select another game and report the issue in the GitHub issue tracker."
                                        }
                            in
                            ( Idle { idleModel | errorModal = errorModal }, Cmd.none )

                FileSelected ->
                    ( model, Ports.requestFileData fileInputId )

                CloseErrorModal ->
                    ( Idle { idleModel | errorModal = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Emulation emulationModel ->
            case msg of
                AnimationFrameDelta time ->
                    let
                        gameBoy =
                            Emulator.emulateClocks (clocksPerSecond // 60) emulationModel.gameBoy
                    in
                    ( Emulation { emulationModel | gameBoy = gameBoy, lastFrameTime = time }
                    , Ports.setPixelsFromBatches { canvasId = canvasId, pixelBatches = GameBoyScreen.serializePixelBatches (PPU.getLastCompleteFrame gameBoy.ppu) }
                    )

                ButtonDown (Just button) ->
                    ( Emulation { emulationModel | gameBoy = GameBoy.setButtonStatus button True emulationModel.gameBoy }
                    , Cmd.none
                    )

                ButtonUp (Just button) ->
                    ( Emulation { emulationModel | gameBoy = GameBoy.setButtonStatus button False emulationModel.gameBoy }
                    , Cmd.none
                    )

                ButtonDown Nothing ->
                    ( model, Cmd.none )

                ButtonUp Nothing ->
                    ( model, Cmd.none )

                Reset ->
                    ( Idle { errorModal = Nothing }, Cmd.none )

                Pause ->
                    ( Emulation { emulationModel | paused = True }, Cmd.none )

                Resume ->
                    ( Emulation { emulationModel | paused = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { view = View.view canvasId fileInputId
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animationFrameSubscription =
            case model of
                Emulation emulationModel ->
                    if not emulationModel.paused then
                        Browser.Events.onAnimationFrameDelta AnimationFrameDelta

                    else
                        Sub.none

                _ ->
                    Sub.none
    in
    Sub.batch
        [ animationFrameSubscription
        , Browser.Events.onKeyDown (Decode.map ButtonDown UI.KeyDecoder.decodeKey)
        , Browser.Events.onKeyUp (Decode.map ButtonUp UI.KeyDecoder.decodeKey)
        , Ports.fileData FileDataReceived
        ]


clocksPerSecond : Int
clocksPerSecond =
    4 * 1024 * 1024


canvasId : String
canvasId =
    "screen"


fileInputId : String
fileInputId =
    "romFileInput"
