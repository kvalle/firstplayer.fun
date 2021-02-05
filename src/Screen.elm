module Screen exposing (Screen(..), classify)


type Screen
    = Small
    | Medium
    | Wide


classify : Int -> Screen
classify pixels =
    if pixels < 650 then
        Small

    else if pixels < 1000 then
        Medium

    else
        Wide
