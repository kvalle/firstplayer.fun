module View exposing (view)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Model exposing (Model, PageState(..))
import Msg exposing (Msg(..))
import Rule exposing (Rule)


view : Model -> Document Msg
view model =
    { title = "First Player Fun"
    , body =
        [ container <|
            case model.state of
                Loading ->
                    text "Loading…"

                ShowRule index rule ->
                    displayGameRule index rule

                Error _ ->
                    errorMessage
        ]
    }


errorMessage : Element Msg
errorMessage =
    text "Oh no, something went wrong. Choose player randomly? ¯\\_(ツ)_/¯"


container : Element Msg -> Html Msg
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


displayGameRule : Int -> Rule -> Element Msg
displayGameRule index rule =
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
        , paragraph [ paddingXY 0 40, Font.color (rgb 0.7 0.7 0.7) ] [ text "—" ]
        , Input.button
            []
            { onPress = Just GetRandomRule
            , label = row [] [ icon "shuffle", text "Random rule" ]
            }
        , Input.button
            []
            { onPress = Just <| GetRuleByIndex (index + 1)
            , label = row [] [ icon "right", text "Next rule" ]
            }
        , Input.button
            []
            { onPress = Just <| GetRuleByIndex (index - 1)
            , label = row [] [ icon "left", text "Previous rule" ]
            }
        ]


icon : String -> Element msg
icon name =
    el [ htmlAttribute <| Html.Attributes.class <| "icon-" ++ name ] Element.none
