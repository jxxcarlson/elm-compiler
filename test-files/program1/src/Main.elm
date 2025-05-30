module Main exposing (main, inc, Agent(..))

import Html exposing (text)


type Agent = Human String | AI String | Alien

{-| The agent who will work with you-}
agent = AI "Claude"

{-| A weird inc function -}
inc : Int -> Int
inc x =
    let
        delta =
            2
    in
    x + delta

{-| Entry point for the program -}
main : Html.Html msg
main =
    text "Hello, World!" 
