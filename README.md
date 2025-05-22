#  Fork: AST output

This is a fork of the Elm compiler, an experiment in producing human-readble output of the raw AST.
To play with it, 

  - Clone this repo
  - cd into it and run `cabal build`
  - Find the path to the binary and make an alias e.g.,
    `alias elmx='/Users/carlson/dev/elm/hacking-ast-json/elm-compiler/dist-newstyle/build/aarch64-osx/ghc-9.6.7/elm-0.19.1/x/elm/build/elm/elm`
    where you use the correct path to the compiler
  - Make a small Elm test project somewhere.  The repo has one at `test-files/program1`. Now cd into your project folder and run `elmx make --help`.
    You should see this:

     ```
     ...
     
     --raw-ast
        Print the raw AST of the Elm file.

    --rag-json
        Print the raw AST of the Elm file in a JSON format suitable for RAG
        applications.

    --rag-json-pretty
        Print the raw AST of the Elm file in a pretty-printed JSON format.
     ```

  Try these options.  For example, in `test-files/project1`, do `elmx make --rag-json-pretty src/Main.elm`   

**NOTE.** _The `rag-json` and `rag-json-pretty` options are intended to produce output for RAG servers.
However, at the moment, they are just JSON versions of what you get with --raw-ast._

**Further Note.** _Here is what ChatGPT tells me is a typical RAG chunk:_
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
The docstring should be useful for the intenden application of talking to your codebase.

**Final Note.** _At the moment (May 23), this repo is little more than a skeleton of what we are aiming for. Many elm constructs
are rendered by placeholders.  These "holes" need to be filled in. Etc._



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
