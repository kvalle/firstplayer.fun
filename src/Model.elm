module Model exposing (Model, init)

import Rule exposing (Rule)


type alias Model =
    List Rule


init : Model
init =
    Rule.allTheRules
