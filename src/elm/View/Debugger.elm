module View.Debugger exposing (view)

import Array exposing (Array)
import Bitwise
import Bootstrap.Form.Checkbox as Checkbox
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
import Model exposing (Model)
import Msg exposing (Msg)
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
        , Grid.row [ Row.attrs [ Spacing.mt3 ] ]
            [ Grid.col [ Col.xs12 ] [ viewMemory 0xFF00 0x0100 gameBoy ]
            ]
        ]



-- Internal


viewPPU : PPU -> Html msg
viewPPU ppu =
    Table.table
        { options = [ Table.striped, Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Line" ]
                , Table.th [] [ text "Line Compare" ]
                , Table.th [] [ text "Mode" ]
                , Table.th [] [ text "LCDC" ]
                , Table.th [] [ text "LCD Status" ]
                ]
        , tbody =
            Table.tbody []
                [ Table.tr []
                    [ Table.td [] [ ppu.line |> String.fromInt |> text ]
                    , Table.td [] [ ppu.lineCompare |> String.fromInt |> text ]
                    , Table.td [] [ ppu.mode |> ppuModeToString |> text ]
                    , Table.td [] [ text "a" ]
                    , Table.td [] [ text "a" ]
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
                ]
        , tbody =
            Table.tbody []
                [ Table.tr []
                    [ Table.td [] [ viewFlagValue Zero cpu ]
                    , Table.td [] [ viewFlagValue Subtract cpu ]
                    , Table.td [] [ viewFlagValue HalfCarry cpu ]
                    , Table.td [] [ viewFlagValue Carry cpu ]
                    , Table.td [] [ checkbox cpu.interruptMasterEnable ]
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
                |> List.indexedMap (\index list -> Table.th [] [ address + (index * 16) |> word16ToString |> text ] :: list)
                |> List.map (Table.tr [])
    in
    Table.table
        { options = [ Table.striped, Table.small ]
        , thead = Table.simpleThead []
        , tbody = Table.tbody [] rows
        }


viewRegisterValue : Register16 -> CPU -> Html msg
viewRegisterValue register cpu =
    CPU.readRegister16 register cpu |> word16ToString |> text


word16ToString : Int -> String
word16ToString =
    Hex.toString >> String.toUpper >> String.padLeft 4 '0' >> (++) "0x"


word8ToString : Int -> String
word8ToString =
    Hex.toString >> String.toUpper >> String.padLeft 2 '0' >> (++) "0x"
