module View exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Model exposing (Model)
import Rule exposing (Rule)


view : Model -> Html msg
view model =
    container <|
        case List.head model of
            Just rule ->
                displayGameRule rule

            Nothing ->
                errorMessage


errorMessage : Element msg
errorMessage =
    text "Oh no, something went wrong. Choose player randomly? ¯\\_(ツ)_/¯"


container : Element msg -> Html msg
container content =
    Element.layout [] <|
        row
            [ centerY, centerX ]
            [ content ]


displayGameRule : Rule -> Element msg
displayGameRule rule =
    column
        [ spacing 30
        , width <| px 800
        ]
        [ paragraph
            [ Font.family
                [ Font.typeface "Roboto Condensed"
                , Font.sansSerif
                ]
            , Font.size 40
            ]
            [ text rule.rule ]
        , link [ Font.underline, Font.italic ]
            { url = rule.url
            , label = text rule.game
            }
        ]
