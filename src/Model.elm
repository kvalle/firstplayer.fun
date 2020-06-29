module Model exposing (Model, PageState(..), init)

import Browser.Navigation as Nav
import Rule exposing (Rule)


type alias Model =
    { state : PageState
    , key : Nav.Key
    }


type PageState
    = Loading
    | ShowRule Int Rule
    | Error String


init : Nav.Key -> Model
init key =
    { state = Loading
    , key = key
    }
