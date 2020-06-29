module Main exposing (main)

import Browser
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, text)
import Json.Decode as JD exposing (Value)
import Model exposing (Model, PageState(..))
import Msg exposing (Msg(..))
import Random
import Rule exposing (Rule)
import Url exposing (Url)
import View exposing (view)


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            Model.init key
    in
    ( model, Rule.getRandom NewRandomRule )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandomRule (Ok ( index, rule )) ->
            ( { model | state = ShowRule index rule }
            , Nav.pushUrl model.key ("/" ++ String.fromInt index)
            )

        NewRandomRule (Err error) ->
            ( { model | state = Error error }
            , Cmd.none
            )

        GetRandomRule ->
            ( { model | state = Loading }
            , Rule.getRandom NewRandomRule
            )

        UrlChanged url ->
            ( model, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )
