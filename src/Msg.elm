module Msg exposing (Msg(..))

import Browser
import Rule exposing (Rule)
import Url exposing (Url)


type Msg
    = ShowRule (Result String ( Int, Rule ))
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | RedirectToRandomRule
    | RedirectToIndexRule (Result String Int)
