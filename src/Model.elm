module Model exposing (Model(..), init)

import Rule exposing (Rule)


type Model
    = Waiting
    | AllIsGood Rule
    | SomethingIsBad


init : Model
init =
    Waiting
