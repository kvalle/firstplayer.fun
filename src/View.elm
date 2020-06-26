module View exposing (view)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Model exposing (Model, PageState(..))
import Rule exposing (Rule)


view : Model -> Document msg
view model =
    { title = "First Player Fun"
    , body =
        [ container <|
            case model.state of
                Waiting ->
                    text "Loading…"

                AllIsGood rule ->
                    displayGameRule rule

                SomethingIsBad ->
                    errorMessage
        ]
    }


errorMessage : Element msg
errorMessage =
    text "Oh no, something went wrong. Choose player randomly? ¯\\_(ツ)_/¯"


container : Element msg -> Html msg
container content =
    Element.layout
        [ Font.family
            [ Font.typeface "Roboto Condensed"
            , Font.sansSerif
            ]
        ]
    <|
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
            [ Font.size 40 ]
            [ text rule.rule ]
        , link [ Font.underline, Font.italic ]
            { url = rule.url
            , label = text rule.game
            }
        ]
