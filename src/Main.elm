module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Json.Decode as JD
import Rule exposing (Rule)


type alias Model =
    List Rule


initModel : Model
initModel =
    Rule.allTheRules


main : Program JD.Value Model msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


update : msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html msg
view model =
    model
        |> List.map .rule
        |> List.head
        |> Maybe.withDefault "yolo"
        |> text
