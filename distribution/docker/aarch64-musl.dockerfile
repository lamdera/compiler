# FROM registry.gitlab.b-data.ch/ghc/ghc4pandoc:8.10.7 as bootstrap

# @TODO having issues on subsequent versions of GHC, retry in future
FROM registry.gitlab.b-data.ch/ghc/ghc4pandoc:9.0.2 as bootstrap

# Install packages
WORKDIR /lamdera
COPY elm.cabal ./
COPY cabal.project ./
COPY cabal.project.freeze ./
COPY vendor/elm-format vendor/elm-format

RUN cabal update
ENV CABALOPTS="--allow-newer -f-export-dynamic -fembed_data_files --enable-executable-static -j4"
ENV GHCOPTS="-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread"
RUN cabal build $CABALOPTS --ghc-options="$GHCOPTS" --only-dependencies

# Import source code
COPY builder builder
COPY compiler compiler
COPY reactor reactor
COPY terminal terminal
COPY LICENSE ./

COPY ext-common ext-common
COPY ext-elm-pages ext-elm-pages
COPY ext-sentry ext-sentry
COPY extra extra
COPY test test
COPY .git .git

RUN cabal build $CABALOPTS --ghc-options="$GHCOPTS"

# RUN cp dist-newstyle/build/aarch64-linux/ghc-9.0.2/lamdera-1.1.0/x/lamdera/build/lamdera/lamdera ./lamdera
# Once we're on a newer cabal, we can drop hardcoding the previous command
RUN cp `cabal list-bin .` ./lamdera

RUN strip lamdera
