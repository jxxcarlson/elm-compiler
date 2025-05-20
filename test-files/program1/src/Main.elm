module Main exposing (main)

import Html exposing (text)

inc : Int -> Int
inc x =
    x + 1

main : Html.Html msg
main =
    text "Hello, World!" 