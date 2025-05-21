# Key Components

- `terminal/src/Make.hs`: Contains the core compilation logic and flag handling
- `terminal/src/Main.hs`: Defines the command-line interface and flag definitions
- `compiler/src/AST/Source.hs`: Contains the raw AST data structures
- `compiler/src/Parse/Module.hs`: Handles parsing source code into the raw AST 

## Command to test raw ast output

Run the below from the root of the project:

```bash
cd test-files/program1 && ../../dist-newstyle/build/aarch64-osx/ghc-9.6.7/elm-0.19.1/x/elm/build/elm/elm make --raw-ast src/Main.elm && cd -
```

Or better, alias this to `test-ast` and run it from the root of the project (after running `source ./zshrc` or the like)

```bash



# Filling in the Gaps in AST.Pretty.Raw

**Example**

program1 is now

```elm
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
and the output of the command is

```elm
Success! Compiled 1 module.
Module: Main
[Function] main with patterns:  = text "Hello, World!"

[Function] inc with patterns: x = let delta  = 2 in x + delta
```



In `compiler/src/AST/Pretty/Raw.prettyExpr`, we made this change

```haskell
        Src.Let defs expr ->
            "let " ++ intercalate "; " defStrings ++ " in " ++ prettyExpr expr
            where
                defStrings = map prettyDef defs
                prettyDef (A.At _ def) = case def of
                    Src.Define name patterns e _ ->
                        ES.toChars (Name.toElmString (A.toValue name)) ++ " " ++ unwords (map prettyPattern patterns) ++ " = " ++ prettyExpr e
                    Src.Destruct pat e ->
                        prettyPattern pat ++ " = " ++ prettyExpr e
```

# Next: show ranges