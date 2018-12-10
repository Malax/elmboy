module Main exposing (main)

import Array exposing (Array)
import Bootstrap.Dropdown
import Bootstrap.Modal as Modal
import Browser
import Browser.Events
import Bytes
import Bytes.Decode
import Component.Cartridge as Cartridge
import Component.Joypad exposing (GameBoyButton(..))
import Component.PPU as PPU
import Component.PPU.GameBoyScreen as GameBoyScreen
import Emulator
import File
import File.Select
import GameBoy exposing (GameBoy)
import Hex
import Html exposing (Html)
import Json.Decode as Decode
import Model exposing (MemoryArea(..), Model)
import Msg exposing (DebuggerMsg(..), Msg(..))
import Ports
import Task
import UI.KeyDecoder
import Util
import View.Debugger
import View.Emulator


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    if model.debuggerEnabled then
        View.Debugger.view canvasId model

    else
        View.Emulator.view canvasId model


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameBoy = Nothing
      , emulateOnAnimationFrame = False
      , frameTimes = []
      , errorModal = Nothing
      , debuggerEnabled = True
      , debugger = { runToProgramCounter = 0x00, memoryArea = HRAM, memoryAreaDropdownState = Bootstrap.Dropdown.initialState }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrameDelta time ->
            case model.gameBoy of
                Just gameBoy ->
                    let
                        emulatedGameBoy =
                            Emulator.emulateCycles (cyclesPerSecond // 60) gameBoy
                    in
                    ( { model | gameBoy = Just emulatedGameBoy, frameTimes = time :: List.take 120 model.frameTimes }
                    , Ports.setPixelsFromBatches { canvasId = canvasId, pixelBatches = GameBoyScreen.serializePixelBatches (PPU.getLastCompleteFrame emulatedGameBoy.ppu) }
                    )

                Nothing ->
                    ( model, Cmd.none )

        ButtonDown button ->
            ( { model | gameBoy = model.gameBoy |> Maybe.map (GameBoy.setButtonStatus button True) }
            , Cmd.none
            )

        ButtonUp button ->
            ( { model | gameBoy = model.gameBoy |> Maybe.map (GameBoy.setButtonStatus button False) }
            , Cmd.none
            )

        Reset ->
            init ()

        Pause ->
            ( { model | emulateOnAnimationFrame = False }, Cmd.none )

        Resume ->
            ( { model | emulateOnAnimationFrame = True }, Cmd.none )

        OpenFileSelect ->
            ( model, File.Select.file [] FileSelected )

        FileSelected file ->
            file
                |> File.toBytes
                |> Task.map (\bytes -> Bytes.Decode.decode (Util.uint8ArrayDecoder (Bytes.width bytes)) bytes |> Maybe.andThen Cartridge.fromBytes)
                |> Task.perform CartridgeSelected
                |> Tuple.pair model

        CartridgeSelected maybeCartridge ->
            case maybeCartridge of
                Just cartridge ->
                    ( { model | gameBoy = Just (GameBoy.init cartridge), emulateOnAnimationFrame = not model.debuggerEnabled }, Cmd.none )

                Nothing ->
                    let
                        errorModal =
                            { visibility = Modal.shown
                            , title = "Unsupported ROM"
                            , body = "Your selected ROM is not yet supported by Elmboy. This is usually the case due to an unsupported memory bank controller required by the ROM you're trying to run. Please select another game and report the issue in the GitHub issue tracker."
                            }
                    in
                    ( { model | errorModal = Just errorModal }, Cmd.none )

        CloseErrorModal ->
            ( { model | errorModal = Nothing }, Cmd.none )

        Debugger debuggerMsg ->
            let
                debuggerModel =
                    model.debugger
            in
            case debuggerMsg of
                RunToProgramCounterValueChange address ->
                    Hex.fromString address
                        |> Result.map (\parsedAddress -> ( { model | debugger = { debuggerModel | runToProgramCounter = parsedAddress } }, Cmd.none ))
                        |> Result.withDefault ( model, Cmd.none )

                RunToProgramCounter ->
                    ( { model | gameBoy = Maybe.map (Emulator.emulateConditional (\gb -> gb.cpu.pc /= debuggerModel.runToProgramCounter)) model.gameBoy }, Cmd.none )

                RunNextInstruction ->
                    ( { model | gameBoy = Maybe.map Emulator.emulateNextInstruction model.gameBoy }, Cmd.none )

                SelectMemoryArea memoryArea ->
                    ( { model | debugger = { debuggerModel | memoryArea = memoryArea } }, Cmd.none )

                MemoryAreaDropdownStateChange state ->
                    ( { model | debugger = { debuggerModel | memoryAreaDropdownState = state } }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animationFrameSubscription =
            if model.emulateOnAnimationFrame then
                Browser.Events.onAnimationFrameDelta AnimationFrameDelta

            else
                Sub.none
    in
    Sub.batch
        [ animationFrameSubscription
        , Browser.Events.onKeyDown (Decode.map ButtonDown UI.KeyDecoder.decodeKey)
        , Browser.Events.onKeyUp (Decode.map ButtonUp UI.KeyDecoder.decodeKey)
        , Bootstrap.Dropdown.subscriptions model.debugger.memoryAreaDropdownState (MemoryAreaDropdownStateChange >> Debugger)
        ]


cyclesPerSecond : Int
cyclesPerSecond =
    4 * 1024 * 1024


canvasId : String
canvasId =
    "screen"
