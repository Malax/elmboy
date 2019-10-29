module View.Emulator exposing (view)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Spacing as Spacing
import GameBoy
import Html exposing (Html, a, div, em, h1, h4, hr, i, kbd, li, p, small, span, strong, text, ul)
import Html.Attributes exposing (class, href, target)
import Model exposing (Model)
import Msg exposing (Msg(..))
import View.Common exposing (errorModalView, romSelector, screen)


view : String -> Model -> Html Msg
view canvasId model =
    let
        leftContent =
            case model.gameBoy of
                Nothing ->
                    div []
                        [ romSelector
                        , model.errorModal
                            |> Maybe.map errorModalView
                            |> Maybe.withDefault (text "")
                        ]

                Just gameBoy ->
                    div []
                        [ screen canvasId
                        , emulationToolbar model.emulateOnAnimationFrame (GameBoy.isAPUEnabled gameBoy) model.frameTimes
                        ]
    in
    scaffolding leftContent projectDescription



-- Internal


scaffolding : Html Msg -> Html Msg -> Html Msg
scaffolding contentLeft contentRight =
    Grid.container []
        [ Grid.row [ Row.attrs [ Spacing.mt3 ] ]
            [ Grid.col [ Col.xs12 ]
                [ pageHeader
                , hr [] []
                , div [ class "custom-column-layout" ]
                    [ contentLeft
                    , contentRight
                    ]
                ]
            ]
        ]


pageHeader : Html Msg
pageHeader =
    div [ class "page-header" ]
        [ h1 [] [ text "Elmboy", small [ class "text-muted", Display.block ] [ text "A Nintendo™ Game Boy™ Emulator written in Elm" ] ]
        , div [ class "links" ]
            [ a [ href "https://github.com/Malax/elmboy", target "_blank" ] [ i [ class "fab fa-github fa-2x" ] [] ]
            , a [ href "https://twitter.com/malax", target "_blank" ] [ i [ class "fab fa-twitter fa-2x" ] [] ]
            ]
        ]


projectDescription : Html Msg
projectDescription =
    div [ class "project-description" ]
        [ h4 [] [ text "Quick Start" ]
        , p [] [ text "Welcome to ", strong [] [ text "Elmboy" ], text ", a Nintendo™ Game Boy™ Emulator written in ", a [ href "https://elm-lang.org/", target "_blank" ] [ text "Elm" ], text "! Click the cartridge on the left hand side to select a ROM file to emulate." ]
        , p [] [ text "If you do not own a ROM file of a commercial Game Boy game, you can try one of the many great games made by the Game Boy Homebrew community. Maybe you want to try ", a [ href "https://drludos.itch.io/sheep-it-up/purchase", target "_blank" ] [ text "Sheep It Up!" ], text "?" ]
        , p []
            [ text "You can control the game with your Keyboard. "
            , em [] [ text "Start" ]
            , text " is mapped to "
            , kbd [] [ text "Enter" ]
            , text ", "
            , em [] [ text "Select" ]
            , text " to "
            , kbd [] [ text "Shift" ]
            , text ". "
            , em [] [ text "A" ]
            , text " and "
            , em [] [ text "B" ]
            , text " are mapped to "
            , kbd [] [ text "S" ]
            , text " and "
            , kbd [] [ text "A" ]
            , text " respectivly. The Joypad is controlled with the cursor keys."
            ]
        , hr [] []
        , h4 [] [ text "About this Project" ]
        , p [] [ text "This is a work-in-progress version of my side-project which I started out of love for retro games, functional programming and Elm. I always wanted to create my own emulator and this project is the, albeit prelimiary, result. It is the most fun I ever had with a side-project and quite the challenge. Even tough it's unfinished, I already spend a lot of time on the project and wanted to finally get it in the hands of people." ]
        , p [] [ text "It is not intended to be a very accurate emulator (although it passes Blargg's CPU test suite) nor to support every ROM out there." ]
        , p [] [ text "If you have any questions or just want to talk about all things Game Boy, functional programming or Elm, you can find me as ", em [] [ text "Malax" ], text " on the Elm Slack, on Twitter and GitHub." ]
        , h4 [] [ text "Talk Recordings and Podcasts" ]
        , p []
            [ p [] [ text "Since the release of Elmboy, I gave a couple of talks and appeared in a podcast where I talk about this project. If you want to learn more, those resources are your best bet." ]
            , ul []
                [ li [] [ a [ href "https://www.youtube.com/watch?v=vI30OvU3QW0" ] [ text "Talk: Oslo Elm Day 2019 - Emulating the Nintendo Game Boy CPU with Elm" ] ]
                , li [] [ text "Talk: Elm Europe 2019 - Emulating the Nintendo Game Boy Audio Hardware with Elm (Recording not yet released)" ]
                , li [] [ a [ href "https://elmtown.simplecast.fm/a-game-boy-emulator-in-elm" ] [ text "Podcast: Elm Town 40 – A Game Boy Emulator in Elm" ] ]
                ]
            ]
        ]


emulationToolbar : Bool -> Bool -> List Float -> Html Msg
emulationToolbar emulateOnAnimationFrame apuEnabled frameTimes =
    let
        pauseResumeButton =
            if emulateOnAnimationFrame then
                ButtonGroup.button [ Button.secondary, Button.onClick Pause ] [ i [ class "fa fa-pause" ] [] ]

            else
                ButtonGroup.button [ Button.secondary, Button.onClick Resume ] [ i [ class "fa fa-play" ] [] ]

        apuControlButton =
            if apuEnabled then
                Button.button
                    [ Button.secondary
                    , Button.onClick DisableAPU
                    , Button.attrs [ class "audio-controls" ]
                    ]
                    [ i [ class "fa fa-volume-mute" ] [] ]

            else
                Button.button
                    [ Button.secondary
                    , Button.onClick EnableAPU
                    , Button.attrs [ class "audio-controls" ]
                    ]
                    [ i [ class "fa fa-volume-up" ] [] ]

        frameCount =
            toFloat (List.length frameTimes)

        totalTime =
            List.sum frameTimes

        fps =
            frameCount / (totalTime / 1000) |> round |> String.fromInt |> (\value -> value ++ " FPS")
    in
    div [ class "emulation-toolbar" ]
        [ ButtonGroup.buttonGroup []
            [ pauseResumeButton
            , ButtonGroup.button [ Button.secondary, Button.onClick Reset ] [ i [ class "fa fa-power-off" ] [], text " Reset" ]
            ]
        , apuControlButton
        , span [ class "fps-counter" ] [ text fps ]
        ]
