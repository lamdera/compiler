# Lets Build Lamdera

## Get the code and compare with the original:

Get the Lamdera code, and required git submodules.
    
    mkdir -p lamdera/lamdera
    git clone https://github.com/lamdera/compiler.git lamdera/lamdera/

    cd lamdera/lamdera
    git submodule init && git submodule update

Get the Elm compiler code (because it will be informative to compare):

    mkdir -p elm/compiler
    git clone https://github.com/elm/compiler.git elm/compiler/

Compare the codebases:

    kdiff3 lamdera/lamdera elm/compiler

Find alternative implementations:

    find builder/ compiler/ -name '*.hs' | xargs grep 'alternativeImplementation'
        
## Build it with Stack:

Haskell has build systems called Cabal and Stack. Stack is more modern and builds on top of Cabal and is the recommended way to build Lamdera. The original Elm compiler only has a Cabal build.

On Debian I was able to install Stack through the package manager:
    
    sudo apt-get install haskell-stack
    sudo stack upgrade

Alternatively there are install scripts:

    curl -sSL https://get.haskellstack.org/ | sh

One thing I noticed is that when running `stack ghci` it downloaded ghc-tinfo6-9.8.4, which must be the ghc compiler itself, but what is the tinfo6 bit? Stack overflow answer: "It is a GHC build variant that links to libtinfo.so.6 (as opposed to linking to some version of libncurses)". So seems like Stack itself knows which compiler version it needs and takes care of fetching it.

Creating a compiler binary is as simple as:

    stack install

# Build it with Cabal:

Mario advises against building with Cabal as managing dependencies and upgrades to them is a pain. But for completeness lets try it.

These build steps were extracted from the `distribution/docker/x86_64-musl.dockerfile`. There are already scripts under `distribution/` folder to build for many different platforms. This is probably the easiest way
of doing it. But here is a more epxlicit step by step way for a Linux box.

Not that compared with the Dockerfile I had to remove static linking, it has `CABALOPTS="-f-export-dynamic -fembed_data_files --enable-executable-static -j4"`. Making a more portable statically linked binary is a bit tricky with Haskell, and that is why the Dockerfile builds on Alpine Linux to do that since it uses musl libc
which is a more standalone libc.

    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    export PATH="${PATH}:~/.ghcup/bin"

    ghcup install ghc 9.2.8 --set
    ghcup install cabal 3.10.1.0 --set    
    cabal update

    export CABALOPTS="-f-export-dynamic -fembed_data_files -j4"
    export GHCOPTS="-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread"

    cabal build $CABALOPTS --ghc-options="$GHCOPTS" --only-dependencies
    cabal build $CABALOPTS --ghc-options="$GHCOPTS"