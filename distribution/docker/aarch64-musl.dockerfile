FROM registry.gitlab.b-data.ch/ghc/ghc4pandoc:8.10.7 as bootstrap

# @TODO having issues on subsequent versions of GHC, retry in future
# FROM registry.gitlab.b-data.ch/ghc/ghc4pandoc:9.0.2 as bootstrap

RUN cabal update

# Install packages
WORKDIR /elm
COPY elm.cabal ./

ENV CABALOPTS="-f-export-dynamic -fembed_data_files --enable-executable-static -j4"
ENV GHCOPTS="-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread"

RUN cabal build $CABALOPTS --ghc-options="$GHCOPTS" --only-dependencies

# Import source code
COPY builder builder
COPY compiler compiler
COPY reactor reactor
COPY terminal terminal
COPY LICENSE ./

RUN cabal build $CABALOPTS --ghc-options="$GHCOPTS"

RUN cp dist-newstyle/build/aarch64-linux/ghc-8.10.7/lamdera-1.1.0/x/lamdera/build/lamdera/lamdera ./lamdera
# Once we're on a newer cabal, we can drop hardcoding the previous command
# RUN cp `cabal list-bin .` ./elm

RUN strip lamdera
