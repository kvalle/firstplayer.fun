module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Json.Decode as JD exposing (Value)
import Model exposing (Model(..))
import Random
import Rule exposing (Rule)
import View exposing (view)


type Msg
    = NewRandomRule Rule


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : Value -> ( Model, Cmd Msg )
init flags =
    case Rule.ruleGenerator of
        Nothing ->
            ( Model.SomethingIsBad, Cmd.none )

        Just generator ->
            ( Model.Waiting, Random.generate NewRandomRule generator )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandomRule rule ->
            ( AllIsGood rule
            , Cmd.none
            )
