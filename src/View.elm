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

                IndexPage ->
                    displayFrontPage

                RulePage index rule ->
                    displayGameRule index rule

                ErrorPage _ ->
                    errorMessage
        ]
    }


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


displayFrontPage : Element Msg
displayFrontPage =
    column
        [ spacing 30
        , width <| px 800
        ]
        [ heading "First Player Fun"
        , paragraph []
            [ text "The most useless rule of any board game is usually the first player rule. But useless dosn't necessarily mean boring. Often these rules are quite quirky and fun!"
            ]
        , paragraph []
            [ text "But they are also usually very static, letting the same player go first every time. And let's be honest, even the funniest rule gets stale rather quickly…"
            ]
        , paragraph [ Font.italic ]
            [ text "So, what if you could use a new rule every time you played?"
            ]
        , Input.button
            []
            { onPress = Just RedirectToRandomRule
            , label = row [ Font.bold ] [ icon "shuffle", text "Show me a random rule!" ]
            }
        ]


errorMessage : Element Msg
errorMessage =
    text "Oh no, something went wrong. Choose player randomly? ¯\\_(ツ)_/¯"


displayGameRule : Int -> Rule -> Element Msg
displayGameRule index rule =
    column
        [ spacing 30
        , width <| px 800
        ]
        [ heading rule.rule
        , link [ Font.underline, Font.italic ]
            { url = rule.url
            , label = text rule.game
            }
        , paragraph [ paddingXY 0 40, Font.color (rgb 0.7 0.7 0.7) ] [ text "—" ]
        , Input.button
            []
            { onPress = Just RedirectToRandomRule
            , label = row [] [ icon "shuffle", text "Random rule" ]
            }
        , Input.button
            []
            { onPress = Just <| RedirectToIndexRule <| Ok (index + 1)
            , label = row [] [ icon "right", text "Next rule" ]
            }
        , Input.button
            []
            { onPress = Just <| RedirectToIndexRule <| Ok (index - 1)
            , label = row [] [ icon "left", text "Previous rule" ]
            }
        ]


icon : String -> Element msg
icon name =
    el [ htmlAttribute <| Html.Attributes.class <| "icon-" ++ name ] Element.none


heading : String -> Element msg
heading textContent =
    paragraph
        [ Font.size 40 ]
        [ text textContent ]
