#  Fork: AST output

This is a fork of the Elm compiler, an experiment in producing human-readble output of the raw AST.
To play with it, 

  - Clone this repo
  - cd into it and run `cabal build`
  - Find the path to the binary and make an alias ls something like this,
    `cd test-files/program1 && ../../dist-newstyle/build/aarch64-osx/ghc-9.6.7/elm-0.19.1/x/elm/build/elm/elm make --raw-ast src/Main.elm && cd -`
    where you use the correct path to the compiler
  - Let's call the alias `test-ast`.  Then cd to the the root of the repo and run `test-ast` to see the output of the AST.

At the moment (May 21), this repo is little more than a skeleton of what we are aiming for. See TODO list below)

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
