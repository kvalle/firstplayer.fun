module Model exposing (Model, PageState(..), init)

import Browser.Navigation as Nav
import Rule exposing (Rule)
import Screen exposing (Screen)


type alias Model =
    { state : PageState
    , key : Nav.Key
    , screen : Screen
    }


type PageState
    = Loading
    | IndexPage
    | RulePage Int Rule
    | ErrorPage String


init : Nav.Key -> Screen -> Model
init key screen =
    { state = Loading
    , key = key
    , screen = screen
    }
