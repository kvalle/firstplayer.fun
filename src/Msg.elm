module Msg exposing (Msg(..))

import Browser
import Rule exposing (Rule)
import Url exposing (Url)


type Msg
    = NewRandomRule (Result String Rule)
    | GetRandomRule
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
