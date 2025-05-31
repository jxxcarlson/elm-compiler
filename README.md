#  Fork: AST output

This is a fork of the Elm compiler, an experiment in producing both human and machine-readble 
output of the raw AST of an Elm program. Below is an example.  To run it, proceed as follows:

  - clone this repo
  - cd into the root of the project
  - Build the compiler: run `cabal build`
  - Make an alias of the binary. You will find the binary at `dist-newstyle/build/aarch64-osx/ghc-9.6.7/elm-0.19.1/x/elm/build/elm/elm`.
    Let's call the alias `elma`
  - `cd test-files/program1 && elma make --ast src/Main.elm`

You should get the output listed in the next section.  For other command-line options
say `elma make --help`.

This is an experimental project. Comments and suggestions of all kinds are welcome, including issues and pull requests.

## Example

### Output for test-files/program1

```
Success! Compiled 1 module.
Module: Main

Imports:
  Platform.Sub
  Platform.Cmd
  Platform
  Tuple
  Char
  String
  Result
  Maybe
  List
  Debug
  Basics
  Html

Values:
[Function] main with patterns:  = [23:5-23:61] [23:5-23:9] text <| [23:13-23:35] "Hello, World! Test: " ++ [23:39-23:60] [23:39-23:53] String.fromInt [23:55-23:60] [23:55-23:58] inc [23:59-23:60] 1

[Function] inc with patterns: [13:5-13:6] x = [14:5-18:14] let [15:9-16:14] delta  = [16:13-16:14] 2 in [18:5-18:14] [18:5-18:6] x + [18:9-18:14] delta

[Function] agent with patterns:  = [9:9-9:20] [9:9-9:11] AI [9:12-9:20] "Claude"

Types:
[Type] [6:6-6:11] Agent
  [6:14-6:19] Human String
  [6:29-6:31] AI String
  [6:41-6:46] Alien

Type Aliases:
Ports:
```
### test-files/program1/src/Main.elm

```
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
    text <| "Hello, World! Test: " ++ String.fromInt (inc 1)
```

**NOTES.** The `rag`, `rag-json`, and `rag-json-pretty` options produce output in a format specifically designed for RAG applications. They follow this structure:_

```
{
  "type": "function",                 // "function", "class", "module", etc.
  "name": "inc",                     // Identifier
  "code": "inc : Int -> Int\n...",   // The full code snippet
  "language": "elm",                 // Code language
  "filePath": "/path/to/file.elm",   // Absolute or relative path
  "startLine": 10,
  "endLine": 15,
  "calls": ["add", "log"],           // Optional: function calls within the snippet
  "imports": ["Html"],               // Optional: imported modules
  "docstring": "Increments a number",// Optional: extracted comment or summary
  "embedding": [0.123, -0.456, ...]  // Optional: precomputed vector embedding
}
```


# Elm

A delightful language for reliable webapps.

Check out the [Home Page](http://elm-lang.org/), [Try Online](http://elm-lang.org/try), or [The Official Guide](http://guide.elm-lang.org/)


<br>

## Install

✨ [Install](https://guide.elm-lang.org/install/elm.html) ✨

For multiple versions, previous versions, and uninstallation, see the instructions [here](https://github.com/elm/compiler/blob/master/installers/README.md).

<br>

## Help

If you are stuck, ask around on [the Elm slack channel][slack]. Folks are friendly and happy to help with questions!

[slack]: http://elmlang.herokuapp.com/
