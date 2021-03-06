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
import Screen
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
    let
        device =
            flags
                |> JD.decodeValue (JD.field "width" JD.int)
                |> Result.map Screen.classify
                |> Result.withDefault Screen.Wide
    in
    Model.init key device
        |> resolveUrl url


resolveUrl : Url -> Model -> ( Model, Cmd Msg )
resolveUrl url model =
    case url.path of
        "/" ->
            ( { model | state = IndexPage }, Cmd.none )

        "/random" ->
            ( model, Rule.getRandomIndex RedirectToIndexRule )

        path ->
            case path |> String.dropLeft 1 |> String.toInt of
                Just index ->
                    ( model, Rule.getByIndex index ShowRule )

                Nothing ->
                    ( { model | state = ErrorPage "Page not found" }, Cmd.none )


subscriptions model =
    Sub.batch
        [ Browser.Events.onResize
            (\width _ -> WindowResized <| Screen.classify width)
        , case model.state of
            RulePage index rule ->
                JD.map2 Tuple.pair
                    (JD.field "key" JD.string)
                    (JD.map4
                        (\meta shift alt ctrl ->
                            [ meta, shift, alt, ctrl ]
                        )
                        (JD.field "metaKey" JD.bool)
                        (JD.field "shiftKey" JD.bool)
                        (JD.field "altKey" JD.bool)
                        (JD.field "ctrlKey" JD.bool)
                    )
                    |> JD.andThen
                        (\( key, modifiers ) ->
                            if List.any identity modifiers then
                                JD.fail <| "Ignored key because of modifier: " ++ key

                            else
                                case key of
                                    "ArrowLeft" ->
                                        JD.succeed <| RedirectToIndexRule <| Ok (index - 1)

                                    "ArrowRight" ->
                                        JD.succeed <| RedirectToIndexRule <| Ok (index + 1)

                                    "r" ->
                                        JD.succeed <| RedirectToRandomRule

                                    other ->
                                        JD.fail <| "Ignored irrelevant key: " ++ other
                        )
                    |> Browser.Events.onKeyDown

            _ ->
                Sub.none
        ]


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

        WindowResized screen ->
            ( { model | screen = screen }
            , Cmd.none
            )

        UrlChanged url ->
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
