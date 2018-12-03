module View.Common exposing (errorModalView, romSelector, screen)

import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Html exposing (Html, canvas, div, p, text)
import Html.Attributes exposing (class, height, id, width)
import Html.Events exposing (onClick)
import Model exposing (ErrorModal)
import Msg exposing (Msg(..))


errorModalView : ErrorModal -> Html Msg
errorModalView errorModal =
    Modal.config CloseErrorModal
        |> Modal.large
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text errorModal.title ]
        |> Modal.body [] [ p [] [ text errorModal.body ] ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick CloseErrorModal ]
                ]
                [ text "Close" ]
            ]
        |> Modal.view errorModal.visibility


romSelector : Html Msg
romSelector =
    div [ class "screen-wrapper" ] [ div [ class "rom-selector", onClick OpenFileSelect ] [] ]


screen : String -> Html Msg
screen canvasId =
    div [ class "screen-wrapper" ] [ canvas [ id canvasId, width 160, height 144, class "screen-canvas" ] [] ]
