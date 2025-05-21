#  Fork: AST output

This is a fork of the Elm compiler, an experiment in producing human-readble output of the raw AST.
To play with it, 

  - Clone this repo
  - cd into it and run `cabal build`
  - Find the path to the binary and make an alias ls something like this,
    `cd test-files/program1 && ../../dist-newstyle/build/aarch64-osx/ghc-9.6.7/elm-0.19.1/x/elm/build/elm/elm make --raw-ast src/Main.elm && cd -`
    where you use the correct path to the compiler
  - Let's call the alias `test-ast`.  Then cd to the the root of the repo and run `test-ast` to see the output of the AST.

## Here is an example of what we can do at present:

```
➜  elm-compiler git:(master) ✗ test-ast-raw
Success!
Module: Main
[Function] main with patterns:  = [14:5-14:25] [14:5-14:9] text [14:10-14:25] "Hello, World!"

[Function] inc with patterns: [6:5-6:6] x = [7:3-10:14] let [8:5-8:14] delta  = [8:13-8:14] 2 in [10:5-10:14] [10:5-10:6] x + [10:9-10:14] delta
```

This output was for the input

```
module Main exposing (main)

import Html exposing (text)

inc : Int -> Int
inc x =
  let
    delta = 2
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


See NOTES.md for additional details.

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
