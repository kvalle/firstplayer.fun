module Model exposing (Model, PageState(..), init)

import Browser.Navigation as Nav
import Rule exposing (Rule)


type alias Model =
    { state : PageState
    , key : Nav.Key
    }


type PageState
    = Waiting
    | AllIsGood Rule
    | SomethingIsBad


init : Nav.Key -> Model
init key =
    { state = Waiting
    , key = key
    }
