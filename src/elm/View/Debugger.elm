module View.Debugger exposing (view)

import Html exposing (Html, text)
import Model exposing (Model)
import Msg exposing (Msg)


view : String -> Model -> Html Msg
view canvasId model =
    text "debugger"
