# Port of the Elm Compiler to Elm

<br>


## What Is It?

A port of the Elm compiler from Haskell to Elm.

For more information see the recording of the August 2024 Elm Online Meetup:  
https://www.youtube.com/watch?v=OK9S_HUdReA.

This repository contains only the compiler backend, but no UI.
As such, it is intended to be embedded into other apps.

<br>


## How to Use It?


#### Embed the Code

One possibility to embed the compiler backend is to add it as a Git subtree or a Git submodule into your project.

The following two repositories embed the compiler backend as a Git subtree:

* [elm-compiler-in-elm-ui](https://github.com/pithub/elm-compiler-in-elm-ui) provides a simple `elm reactor`-like user interface

* [elm-repl-worker](https://github.com/pithub/elm-repl-worker) implements an Elm `Platform.Worker` internally running `elm repl` that communicates with your app via ports

<br>


#### Compile the Code

To test whether the code compiles you can run one of the following commands:

```sh
make
```

or

```sh
elm make src/Test/Main.elm --output /dev/null
```

<br>
