module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Json.Decode as JD
import Model exposing (Model)
import Rule exposing (Rule)
import View exposing (view)


main : Program JD.Value Model msg
main =
    Browser.element
        { init = \_ -> ( Model.init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


update : msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )
