module Main exposing (main, inc, Maybe(..), Result(..))

import Html exposing (text)

type Maybe a
    = Just a
    | Nothing

type Result error value
    = Ok value
    | Err error

type Agent = Human String | AI String | Alien

agent = AI "Claude"

inc : Int -> Int
inc x =
    let
        delta =
            2
    in
    x + delta

main : Html.Html msg
main =
    text "Hello, World!" 

