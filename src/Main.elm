module Main exposing (main)

import Browser
import Browser.Events
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
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    Model.init key
        |> resolveUrl url


resolveUrl : Url -> Model -> ( Model, Cmd Msg )
resolveUrl url model =
    case url.path of
        "/" ->
            ( model, Rule.getRandomIndex RedirectToIndexRule )

        "/random" ->
            ( model, Rule.getRandomIndex RedirectToIndexRule )

        path ->
            case path |> String.dropLeft 1 |> String.toInt of
                Just index ->
                    ( model, Rule.getByIndex index ShowRule )

                Nothing ->
                    ( { model | state = ErrorPage "Page not found" }, Cmd.none )


subscriptions model =
    case model.state of
        RulePage index rule ->
            JD.field "key" JD.string
                |> JD.andThen
                    (\key ->
                        case key of
                            "ArrowLeft" ->
                                JD.succeed <| RedirectToIndexRule <| Ok (index - 1)

                            "ArrowRight" ->
                                JD.succeed <| RedirectToIndexRule <| Ok (index + 1)

                            "r" ->
                                JD.succeed <| RedirectToRandomRule

                            "R" ->
                                JD.succeed <| RedirectToRandomRule

                            other ->
                                JD.fail <| "Ignored key press: " ++ other
                    )
                |> Browser.Events.onKeyDown

        _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowRule (Ok ( index, rule )) ->
            ( { model | state = RulePage index rule }
            , Cmd.none
            )

        ShowRule (Err error) ->
            ( { model | state = ErrorPage error }
            , Cmd.none
            )

        RedirectToRandomRule ->
            ( { model | state = Loading }
            , Rule.getRandomIndex RedirectToIndexRule
            )

        RedirectToIndexRule (Ok index) ->
            ( { model | state = Loading }
            , Nav.pushUrl model.key ("/" ++ String.fromInt index)
            )

        RedirectToIndexRule (Err error) ->
            ( { model | state = ErrorPage error }
            , Cmd.none
            )

        UrlChanged url ->
            let
                _ =
                    Debug.log "url changed" url
            in
            resolveUrl url model

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
