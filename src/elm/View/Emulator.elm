module View.Emulator exposing (view)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html, a, div, em, h1, h4, hr, i, kbd, p, small, span, strong, text)
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

                Just _ ->
                    div []
                        [ screen canvasId
                        , emulationToolbar model.emulateOnAnimationFrame model.frameTimes
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
        , p [] [ text "This is an early and work-in-progress version of my side-project which I started out of love for retro games, functional programming and Elm. I always wanted to create my own emulator and this project is the, albeit prelimiary, result. It is the most fun I ever had with a side-project and quite the challenge. Even tough it's unfinished, I already spend a lot of time on the project and wanted to finally get it in the hands of people." ]
        , p [] [ text "It is not intended to be a very accurate emulator (although it passes Blargg's CPU test suite) nor to support every ROM out there. It is also very slow in it's current form. I plan to continue working on the project, improving compatibility, performance and correctness. The projects ", a [ href "https://github.com/Malax/elmboy/blob/master/README.md", target "_blank" ] [ text "README.md" ], text " has some more technical details about possible improvements. I also have some things in mind about the projects educational value going forward. We will see where it goes in that regard." ]
        , p [] [ text "If you have any questions or just want to talk about all things Game Boy, functional programming or Elm, you can find me as ", em [] [ text "Malax" ], text " on the Elm Slack, on Twitter and GitHub." ]
        ]


emulationToolbar : Bool -> List Float -> Html Msg
emulationToolbar emulateOnAnimationFrame frameTimes =
    let
        pauseResumeButton =
            if emulateOnAnimationFrame then
                ButtonGroup.button [ Button.secondary, Button.onClick Pause ] [ i [ class "fa fa-pause" ] [] ]

            else
                ButtonGroup.button [ Button.secondary, Button.onClick Resume ] [ i [ class "fa fa-play" ] [] ]

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
        , span [ class "fps-counter" ] [ text fps ]
        ]
