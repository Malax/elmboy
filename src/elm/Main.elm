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
import Gamepad exposing (Gamepad)
import Gamepad.Simple
import GamepadPort
import Html exposing (Html)
import Json.Decode as Decode
import Model exposing (EmulationModel, Model(..))
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
                            ( Emulation { gameBoy = GameBoy.init cartridge, paused = False, frameTimes = [] }, Cmd.none )

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
                AnimationFrame { gamepads, dt } ->
                    if not emulationModel.paused then
                        runEmulation dt (applyGamepadControls gamepads emulationModel)
                    else
                        ( model, Cmd.none )

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


applyGamepadControls : List Gamepad -> EmulationModel -> EmulationModel
applyGamepadControls gamepads emulationModel =
    case gamepads of
        [] ->
            emulationModel

        gamepad :: gs ->
            let
                isPressed =
                    Gamepad.isPressed gamepad

                fold ( digital, button ) gameboy =
                    GameBoy.setButtonStatus button (isPressed digital) gameboy
            in
            { emulationModel
                | gameBoy =
                    [ ( Gamepad.A, A )
                    , ( Gamepad.B, B )
                    , ( Gamepad.DpadUp, Up )
                    , ( Gamepad.DpadDown, Down )
                    , ( Gamepad.DpadLeft, Left )
                    , ( Gamepad.DpadRight, Right )
                    , ( Gamepad.Start, Start )
                    , ( Gamepad.Back, Select )
                    ]
                        |> List.foldl fold emulationModel.gameBoy
            }


runEmulation : Float -> EmulationModel -> ( Model, Cmd msg )
runEmulation dt emulationModel =
    let
        time =
            dt

        gameBoy =
            Emulator.emulateClocks (clocksPerSecond // 60) emulationModel.gameBoy
    in
    ( Emulation { emulationModel | gameBoy = gameBoy, frameTimes = time :: List.take 120 emulationModel.frameTimes }
    , Ports.setPixelsFromBatches { canvasId = canvasId, pixelBatches = GameBoyScreen.serializePixelBatches (PPU.getLastCompleteFrame gameBoy.ppu) }
    )


main : Gamepad.Simple.Program () Model Msg
main =
    Gamepad.Simple.element
        { onAnimationFrame = AnimationFrame
        , onBlob = GamepadPort.onBlob
        , saveToLocalStorage = GamepadPort.saveToLocalStorage
        , controls =
            [ ( "Left", Gamepad.DpadLeft )
            , ( "Right", Gamepad.DpadRight )
            , ( "Up", Gamepad.DpadUp )
            , ( "Down", Gamepad.DpadDown )
            , ( "A", Gamepad.A )
            , ( "B", Gamepad.B )
            , ( "Start", Gamepad.Start )
            , ( "Select", Gamepad.Back )
            ]
        }
        { view = View.view canvasId fileInputId
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map ButtonDown UI.KeyDecoder.decodeKey)
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
