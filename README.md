#  Fork: AST output

This is a fork of the Elm compiler, an experiment in producing human-readble output of the raw AST.
To play with it, 

  - Clone this repo
  - cd into it and run `cabal build`
  - Find the path to the binary and make an alias ls something like this,
    `cd test-files/program1 && ../../dist-newstyle/build/aarch64-osx/ghc-9.6.7/elm-0.19.1/x/elm/build/elm/elm make --raw-ast src/Main.elm && cd -`
    where you use the correct path to the compiler
  - Let's call the alias `test-ast-raw`.  Then cd to the the root of the repo and run `test-ast-raw` to see the output of the AST.

## Here is an example of what we can do at present:

```
➜  elm-compiler git:(master) ✗ test-ast-raw
Success!
Module: Main
[Function] main with patterns:  = [27:5-27:25] [27:5-27:9] text [27:10-27:25] "Hello, World!"

[Function] inc with patterns: [18:5-18:6] x = [19:5-23:14] let [20:9-21:14] delta  = [21:13-21:14] 2 in [23:5-23:14] [23:5-23:6] x + [23:9-23:14] delta

[Function] agent with patterns:  = [15:9-15:20] [15:9-15:11] AI [15:12-15:20] "Claude"

[Type] [13:6-13:11] Agent
  [13:14-13:19] Human String
  [13:29-13:31] AI String
  [13:41-13:46] Alien

[Type] [9:6-9:12] Result error value
  [10:7-10:9] Ok value
  [11:7-11:10] Err error

[Type] [5:6-5:11] Maybe a
  [6:7-6:11] Just a
  [7:7-7:14] Nothing

```

This output was for the input

```
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
```

### Note that errors are handled in the usual way:

```
➜  elm-compiler git:(master) test-ast-raw
Detected problems in 1 module.
-- NAMING ERROR --------------------------------------------------- src/Main.elm

I cannot find a `Str` type:

5| type Agent = Human Str | AI String | Alien
                      ^^^
These names seem close though:

    Sub
    Char
    Cmd
    Int

Hint: Read <https://elm-lang.org/0.19.1/imports> to see how `import`
declarations work in Elm.
```


At the moment (May 20, this repo is little more than a skeleton of what we are aiming for. See TODO list below).  


See NOTES.md for additional details.m See also [RAG REPO](https://github.com/jxxcarlson/github_repo_rag/blob/ragChunker/README.md)

## RAG output

I've started a new branch (will push and document late this evening) that gives output in a form that RAG serverse like (apparently).
This is a first try:

```
{
  "aliases": [],
  "name": "Main",
  "ports": [],
  "type": "Module",
  "unions": [
    {
      "constructors": [
        {
          "name": {
            "region": {
              "start": {
                "line": 13,
                "column": 14
              },
              "end": {
                "line": 13,
                "column": 19
              }
            },
            "value": "Human"
          },
          "types": [
            {
              "region": {
                "start": {
                  "line": 13,
                  "column": 20
                },
                "end": {
                  "line": 13,
                  "column": 26
                }
              },
              "type": {
                "name": "String",
                "type": "Type",
                "types": []
              }
            }
          ]
        },
        {
          "name": {
            "region": {
              "start": {
                "line": 13,
                "column": 29
              },
              "end": {
                "line": 13,
                "column": 31
              }
            },
            "value": "AI"
          },
          "types": [
            {
              "region": {
                "start": {
                  "line": 13,
                  "column": 32
                },
                "end": {
                  "line": 13,
                  "column": 38
                }
              },
              "type": {
                "name": "String",
                "type": "Type",
                "types": []
              }
            }
          ]
        },
        {
          "name": {
            "region": {
              "start": {
                "line": 13,
                "column": 41
              },
              "end": {
                "line": 13,
                "column": 46
              }
            },
            "value": "Alien"
          },
          "types": []
        }
      ],
      "name": {
        "region": {
          "start": {
            "line": 13,
            "column": 6
          },
          "end": {
            "line": 13,
            "column": 11
          }
        },
        "value": "Agent"
      },
      "type": "Type",
      "vars": []
    }
```

## TODO

- Add much more detail to the output of --raw-ast. At the moment much of the output is given by placeholders.
- Add location data, including range of line numbers so that the relevant source code can be accessed)
- Add json output
- Maybe do the canonical AST

### Files added

compiler/src/
- AST/Pretty/Raw.hs
- AST/Pretty/Json.hs

## Files changed

terminal/src/Make.hs

## Files referenced

- Data/Name.hs -- use `toElmString`
- Reporting/Annotation.hs -- `toValue` which extracts the vlaue from a `Located` type:

  ```
  data Located a =
    At Region a
  ```
**Note.** I need to check and update the File data above.

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
