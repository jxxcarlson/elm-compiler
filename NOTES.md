# Key Components

- `terminal/src/Make.hs`: Contains the core compilation logic and flag handling
- `terminal/src/Main.hs`: Defines the command-line interface and flag definitions
- `compiler/src/AST/Source.hs`: Contains the raw AST data structures
- `compiler/src/Parse/Module.hs`: Handles parsing source code into the raw AST 

## Command to test raw ast output

Run the below from the root of the project:

cd test-files/program1 && ../../dist-newstyle/build/aarch64-osx/ghc-9.6.7/elm-0.19.1/x/elm/build/elm/elm make src/Main.elm --raw-ast