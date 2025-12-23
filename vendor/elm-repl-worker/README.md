# Elm REPL Worker

<br>


## What Is It?

An Elm `Platform.worker` that runs `elm repl` internally.
It communicates with your app via ports.

The REPL uses a [port of the Elm compiler](https://github.com/pithub/elm-compiler-in-elm)
from Haskell to Elm.

For more information see the recording of the August 2024 Elm Online Meetup:  
https://www.youtube.com/watch?v=OK9S_HUdReA.

<br>


## How to Use It?


#### Compile the Worker

```sh
make
```

or

```sh
elm make src/Repl/Worker.elm --output dist/worker.js
```

<br>


#### Add It to Your App

In branch `clients` there are examples for adding the REPL worker to an Elm app:

* Loading the REPL Worker at Startup

  * [eager-tea.html](https://github.com/pithub/elm-repl-worker/blob/clients/dist/eager-tea.html) -
    Adds the REPL worker to a normal TEA app -
    [Live demo](https://pithub.github.io/elm-repl-worker/eager-tea.html)

  * [eager-io.html](https://github.com/pithub/elm-repl-worker/blob/clients/dist/eager-io.html) -
    Adds the REPL worker to an app using the IO monad of the Elm compiler port -
    [Live demo](https://pithub.github.io/elm-repl-worker/eager-io.html)

* Loading the Repl Worker on Demand

  * [lazy-tea.html](https://github.com/pithub/elm-repl-worker/blob/clients/dist/lazy-tea.html) -
    Adds the REPL worker to a normal TEA app -
    [Live demo](https://pithub.github.io/elm-repl-worker/lazy-tea.html)

  * [lazy-io.html](https://github.com/pithub/elm-repl-worker/blob/clients/dist/lazy-io.html) -
    Adds the REPL worker to an app using the IO monad of the Elm compiler port -
    [Live demo](https://pithub.github.io/elm-repl-worker/lazy-io.html)


<br>
