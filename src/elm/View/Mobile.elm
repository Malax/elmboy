module View.Mobile exposing (view)

import Component.Joypad exposing (GameBoyButton(..))
import GameBoy
import Html exposing (Html, a, button, div, em, h1, h4, hr, i, kbd, p, small, span, strong, text)
import Html.Attributes exposing (class, href, id, target)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as Decode
import Model exposing (Model)
import Msg exposing (Msg(..))
import View.Common exposing (errorModalView, romSelector, screen)


view : String -> Model -> Html Msg
view canvasId model =
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
                , dpad
                , hr [] []
                , button [ onTouchStart (ButtonDown A), onTouchEnd (ButtonUp A) ] [ text "A" ]
                , button [ onTouchStart (ButtonDown B), onTouchEnd (ButtonUp B) ] [ text "B" ]
                , button [ onTouchStart (ButtonDown Start), onTouchEnd (ButtonUp Start) ] [ text "Start" ]
                , button [ onTouchStart (ButtonDown Select), onTouchEnd (ButtonUp Select) ] [ text "Select" ]
                ]


onTouchStart : msg -> Html.Attribute msg
onTouchStart msg =
    on "touchstart" (Decode.succeed msg)


onTouchEnd : msg -> Html.Attribute msg
onTouchEnd msg =
    on "touchend" (Decode.succeed msg)


dpad : Html Msg
dpad =
    div [ id "virtual-dpad" ] [ text "DPAD" ]
