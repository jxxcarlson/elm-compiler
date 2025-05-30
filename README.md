#  Fork: AST output

This is a fork of the Elm compiler, an experiment in producing both human and machine-readble 
output of the raw AST. Below is an example.  To run it, proceed as follows:

  - clone this repo
  - cd into the root of the project
  - Build the compiler: run `cabal build`
  - Make an alias of the binary. You will find the binary at `dist-newstyle/build/aarch64-osx/ghc-9.6.7/elm-0.19.1/x/elm/build/elm/elm`.
    Let's call the alias `elmx`
  - `cd test-files/program1 && elmx make --ast src/Main.elm`

You should get output as below.  For other command-line options
say `elmx make --help`.

This is an experimental project. At the moment, only a small number of elements
of the AST are handled.  Much work needed!

**Output for test-files/program1**

```
➜  program1 git:(master) elmx make --ast src/Main.elm
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
[Function] main with patterns:  = [30:5-30:25] [30:5-30:9] text [30:10-30:25] "Hello, World!"

[Function] inc with patterns: [20:5-20:6] x = [21:5-25:14] let [22:9-23:14] delta  = [23:13-23:14] 2 in [25:5-25:14] [25:5-25:6] x + [25:9-25:14] delta

[Function] agent with patterns:  = [16:9-16:20] [16:9-16:11] AI [16:12-16:20] "Claude"

Types:
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

Type Aliases:
Ports:
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
