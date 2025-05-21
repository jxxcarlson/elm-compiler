module Main exposing (main)

import Html exposing (text)

type Agent = Human String | AI String | Alien

agent = AI "Claude"

inc : Int -> Int
inc x =
  let
    delta = 2
  in
    x + delta

main : Html.Html msg
main =
    text "Hello, World!" 

