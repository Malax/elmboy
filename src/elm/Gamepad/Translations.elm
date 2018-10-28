module Gamepad.Translations exposing (Translation, allTranslations, pickTranslation)

import Dict exposing (Dict)


type alias Translation =
    { noGamepadsDetected : String
    , remappingGamepadComplete : Int -> String
    , pressAnyButtonToGoBack : String
    , remappingGamepad : Int -> String
    , press : String
    , skipThisAction : String
    , cancelRemapping : String
    , map : String
    , needsMapping : String
    , idle : String
    , receivingSignal : String
    , remap : String
    , standardMapping : String
    , customMapping : String
    , pressTheEscKeyToToggle : ( String, String, String )
    }


pickTranslation : List String -> Dict String Translation -> Translation
pickTranslation languages dict =
    pickTranslation_ (languages ++ List.map getPrimarySubtag languages) dict


pickTranslation_ : List String -> Dict String Translation -> Translation
pickTranslation_ languages translations =
    case languages of
        [] ->
            en

        l :: ls ->
            case Dict.get l translations of
                Just t ->
                    t

                Nothing ->
                    pickTranslation ls translations


getPrimarySubtag : String -> String
getPrimarySubtag tag =
    tag
        |> String.split "-"
        |> List.head
        |> Maybe.withDefault tag


en : Translation
en =
    { noGamepadsDetected = "No gamepads detected"
    , remappingGamepadComplete = \id -> "Remapping Gamepad " ++ String.fromInt id ++ " complete."
    , pressAnyButtonToGoBack = "Press any button to go back."
    , remappingGamepad = \id -> "Remapping Gamepad " ++ String.fromInt id
    , press = "Press:"
    , skipThisAction = "Skip this action"
    , cancelRemapping = "Cancel remapping"
    , map = "Map"
    , needsMapping = "Needs mapping"
    , idle = "idle"
    , receivingSignal = "Receiving signal"
    , remap = "Remap"
    , standardMapping = "Standard mapping"
    , customMapping = "Custom mapping"
    , pressTheEscKeyToToggle = ( "Press the", "Esc", "key to toggle the gamepad configuration menu" )
    }


{-| This is a dictionary of all translations available in the package, indexed by
[language tag](https://www.w3.org/International/questions/qa-choosing-language-tags)
-}
allTranslations : Dict String Translation
allTranslations =
    Dict.fromList
        [ ( "en", en )
        , ( "fr"
          , { noGamepadsDetected = "Aucune manette détectée"
            , remappingGamepadComplete = \id -> "Configuration de la manette " ++ String.fromInt id ++ " terminée."
            , pressAnyButtonToGoBack = "Pressez n'importe quelle touche pour revenir en arrière."
            , remappingGamepad = \id -> "Configuration de la manette " ++ String.fromInt id
            , press = "Pressez :"
            , skipThisAction = "Passer cette action"
            , cancelRemapping = "Annuler la configuration"
            , map = "Attribuer"
            , needsMapping = "Configuration nécessaire"
            , idle = "inactif"
            , receivingSignal = "Réception d'un signal"
            , remap = "Configurer"
            , standardMapping = "Configuration standard"
            , customMapping = "Configuration personnalisée"
            , pressTheEscKeyToToggle = ( "Appuyez sur la touche", "Echap", "pour basculer le menu de configuration du gamepad" )
            }
          )
        ]
