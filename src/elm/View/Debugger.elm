module View.Debugger exposing (view)

import Array exposing (Array)
import Bitwise
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Component.CPU as CPU exposing (CPU, Register16(..), Register8(..))
import Component.CPU.FlagRegister as FlagRegister exposing (Flag(..))
import Component.Cartridge as Cartridge
import Component.MMU as MMU
import Component.PPU.Types exposing (Mode(..), PPU)
import Constants
import GameBoy exposing (GameBoy)
import Hex
import Html exposing (Html, h6, text)
import Html.Events
import Model exposing (MemoryArea(..), Model)
import Msg exposing (DebuggerMsg(..), Msg(..))
import Util
import View.Common exposing (romSelector, screen)


view : String -> Model -> Html Msg
view canvasId model =
    let
        -- We need a GameBoy to display most of the UI.
        -- To simplify things, we fall back to a GameBoy with an empty Cartridge here if necessary.
        gameBoy =
            model.gameBoy |> Maybe.withDefault (GameBoy.init Cartridge.empty)

        screenOrRomSelector =
            case model.gameBoy of
                Just _ ->
                    screen canvasId

                Nothing ->
                    romSelector
    in
    Grid.container []
        [ Grid.row [ Row.attrs [ Spacing.mt3 ] ]
            [ Grid.col [ Col.xs6 ] [ screenOrRomSelector ]
            , Grid.col [ Col.xs6 ]
                [ viewRegisters gameBoy.cpu
                , viewFlags gameBoy.cpu
                , viewInterruptData gameBoy.cpu
                , viewPPU gameBoy.ppu
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs3 ] [ runToPCInputGroup model.debugger.runToProgramCounter ]
            ]
        , Grid.row [ Row.attrs [ Spacing.mt3 ] ]
            [ Grid.col [ Col.xs7 ] [ viewMemoryArea model.debugger.memoryArea gameBoy ]
            , Grid.col [ Col.xs5 ] [ buttons, droppy model ]
            ]
        ]



-- Internal


viewMemoryArea : MemoryArea -> GameBoy -> Html msg
viewMemoryArea memoryArea =
    case memoryArea of
        CartridgeROMBank0 ->
            viewMemory 0x00 0x4000

        CartridgeROMBankN ->
            viewMemory 0x4000 0x4000

        VRAM ->
            viewMemory 0x8000 0x1000

        CartridgeRAM ->
            viewMemory 0xA000 0x1000

        WorkRAMBank0 ->
            viewMemory 0xC000 0x1000

        WorkRAMBank1 ->
            viewMemory 0xD000 0x1000

        OAM ->
            viewMemory 0xFE00 0xA0

        IORegisters ->
            viewMemory 0xFF00 0x80

        HRAM ->
            viewMemory 0xFF80 0x70


memoryAreaToString : MemoryArea -> String
memoryAreaToString memoryArea =
    case memoryArea of
        CartridgeROMBank0 ->
            "CartridgeROMBank0"

        CartridgeROMBankN ->
            "CartridgeROMBankN"

        VRAM ->
            "VRAM"

        CartridgeRAM ->
            "CartridgeRAM"

        WorkRAMBank0 ->
            "WorkRAMBank0"

        WorkRAMBank1 ->
            "WorkRAMBank1"

        OAM ->
            "OAM"

        IORegisters ->
            "IORegisters"

        HRAM ->
            "HRAM"


droppy : Model -> Html Msg
droppy model =
    let
        items =
            [ CartridgeROMBank0, CartridgeROMBankN, VRAM, CartridgeRAM, WorkRAMBank0, WorkRAMBank1, OAM, IORegisters, HRAM ]
                |> List.map
                    (\memoryArea ->
                        Dropdown.buttonItem [ Html.Events.onClick (Debugger (SelectMemoryArea memoryArea)) ] [ memoryArea |> memoryAreaToString |> text ]
                    )
    in
    Dropdown.dropdown
        model.debugger.memoryAreaDropdownState
        { options = []
        , toggleMsg = MemoryAreaDropdownStateChange >> Debugger
        , toggleButton =
            Dropdown.toggle [ Button.primary ] [ model.debugger.memoryArea |> memoryAreaToString |> text ]
        , items = items
        }


buttons : Html Msg
buttons =
    ButtonGroup.buttonGroup []
        [ ButtonGroup.button [ Button.secondary, Button.onClick (Debugger RunNextInstruction) ] [ text "Run Next Instruction" ]
        , ButtonGroup.button [ Button.secondary, Button.onClick (Debugger ToggleEmulateOnAnimationFrame) ] [ text "Toggle Auto-Render" ]
        ]


runToPCInputGroup : Int -> Html Msg
runToPCInputGroup address =
    InputGroup.text [ Input.value (address |> Hex.toString), Input.onInput (RunToProgramCounterValueChange >> Debugger) ]
        |> InputGroup.config
        |> InputGroup.predecessors [ InputGroup.span [] [ text "0x" ] ]
        |> InputGroup.successors
            [ InputGroup.button [ Button.secondary, Button.onClick (Debugger RunToProgramCounter) ] [ text "Run" ] ]
        |> InputGroup.view


viewPPU : PPU -> Html msg
viewPPU ppu =
    Table.table
        { options = [ Table.striped, Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Line" ]
                , Table.th [] [ text "Line Compare" ]
                , Table.th [] [ text "Mode" ]
                ]
        , tbody =
            Table.tbody []
                [ Table.tr []
                    [ Table.td [] [ ppu.line |> String.fromInt |> text ]
                    , Table.td [] [ ppu.lineCompare |> String.fromInt |> text ]
                    , Table.td [] [ ppu.mode |> ppuModeToString |> text ]
                    ]
                ]
        }


ppuModeToString : Mode -> String
ppuModeToString mode =
    case mode of
        OamSearch ->
            "OAM Search"

        PixelTransfer ->
            "Pixel Transfer"

        HBlank ->
            "HBlank"

        VBlank ->
            "VBlank"


viewRegisters : CPU -> Html msg
viewRegisters cpu =
    Table.table
        { options = [ Table.striped, Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "AF" ]
                , Table.th [] [ text "BC" ]
                , Table.th [] [ text "DE" ]
                , Table.th [] [ text "HL" ]
                , Table.th [] [ text "PC" ]
                , Table.th [] [ text "SP" ]
                ]
        , tbody =
            Table.tbody []
                [ Table.tr []
                    [ Table.td [] [ viewRegisterValue AF cpu ]
                    , Table.td [] [ viewRegisterValue BC cpu ]
                    , Table.td [] [ viewRegisterValue DE cpu ]
                    , Table.td [] [ viewRegisterValue HL cpu ]
                    , Table.td [] [ viewRegisterValue PC cpu ]
                    , Table.td [] [ viewRegisterValue SP cpu ]
                    ]
                ]
        }


checkboxBitSet : Int -> Int -> Html msg
checkboxBitSet value mask =
    Bitwise.and mask value == mask |> checkbox


viewInterruptData : CPU -> Html msg
viewInterruptData cpu =
    let
        flagCheckboxes =
            \interruptByte ->
                [ Table.td [] [ checkboxBitSet interruptByte Constants.bit0Mask ]
                , Table.td [] [ checkboxBitSet interruptByte Constants.bit1Mask ]
                , Table.td [] [ checkboxBitSet interruptByte Constants.bit2Mask ]
                , Table.td [] [ checkboxBitSet interruptByte Constants.bit3Mask ]
                , Table.td [] [ checkboxBitSet interruptByte Constants.bit4Mask ]
                ]

        rowHeader =
            \string -> Table.th [] [ text string ]
    in
    Table.table
        { options = [ Table.striped, Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "" ]
                , Table.th [] [ text "VBlank" ]
                , Table.th [] [ text "LCD Stat" ]
                , Table.th [] [ text "Timer" ]
                , Table.th [] [ text "Serial" ]
                , Table.th [] [ text "Joypad" ]
                ]
        , tbody =
            Table.tbody []
                [ Table.tr [] (rowHeader "Flagged" :: flagCheckboxes cpu.interruptFlag)
                , Table.tr [] (rowHeader "Enabled" :: flagCheckboxes cpu.interruptEnable)
                , Table.tr [] (rowHeader "Result" :: flagCheckboxes (Bitwise.and cpu.interruptEnable cpu.interruptFlag))
                ]
        }


checkbox : Bool -> Html msg
checkbox value =
    Checkbox.checkbox [ Checkbox.checked value, Checkbox.disabled True ] ""


viewFlags : CPU -> Html msg
viewFlags cpu =
    Table.table
        { options = [ Table.striped, Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Zero" ]
                , Table.th [] [ text "Subtract" ]
                , Table.th [] [ text "Half Carry" ]
                , Table.th [] [ text "Carry" ]
                , Table.th [] [ text "IME" ]
                , Table.th [] [ text "Halted" ]
                ]
        , tbody =
            Table.tbody []
                [ Table.tr []
                    [ Table.td [] [ viewFlagValue Zero cpu ]
                    , Table.td [] [ viewFlagValue Subtract cpu ]
                    , Table.td [] [ viewFlagValue HalfCarry cpu ]
                    , Table.td [] [ viewFlagValue Carry cpu ]
                    , Table.td [] [ checkbox cpu.interruptMasterEnable ]
                    , Table.td [] [ checkbox cpu.halted ]
                    ]
                ]
        }


viewFlagValue : Flag -> CPU -> Html msg
viewFlagValue flag cpu =
    CPU.readRegister8 F cpu |> FlagRegister.getFlag flag |> checkbox


viewMemory : Int -> Int -> GameBoy -> Html msg
viewMemory address length gameBoy =
    let
        rows =
            MMU.readWord8Chunk gameBoy address length
                |> List.map (\byte -> Table.td [] [ byte |> word8ToString |> text ])
                |> Util.chunkList 16
                |> List.indexedMap (\index list -> Table.th [] [ (address + (index * 16)) |> word16ToString |> text ] :: list)
                |> List.map (Table.tr [])
    in
    Table.table
        { options = [ Table.striped, Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "" ]
                , Table.th [] [ text "0" ]
                , Table.th [] [ text "1" ]
                , Table.th [] [ text "2" ]
                , Table.th [] [ text "3" ]
                , Table.th [] [ text "4" ]
                , Table.th [] [ text "5" ]
                , Table.th [] [ text "6" ]
                , Table.th [] [ text "7" ]
                , Table.th [] [ text "8" ]
                , Table.th [] [ text "9" ]
                , Table.th [] [ text "A" ]
                , Table.th [] [ text "B" ]
                , Table.th [] [ text "C" ]
                , Table.th [] [ text "D" ]
                , Table.th [] [ text "E" ]
                , Table.th [] [ text "F" ]
                ]
        , tbody = Table.tbody [] rows
        }


viewRegisterValue : Register16 -> CPU -> Html msg
viewRegisterValue register cpu =
    CPU.readRegister16 register cpu |> word16ToString |> text


word16ToString : Int -> String
word16ToString =
    Hex.toString >> String.toUpper >> String.padLeft 4 '0'


word8ToString : Int -> String
word8ToString =
    Hex.toString >> String.toUpper >> String.padLeft 2 '0'
