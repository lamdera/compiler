# FROM registry.gitlab.b-data.ch/ghc/ghc4pandoc:8.10.7 as bootstrap

# @TODO having issues on subsequent versions of GHC, retry in future
# https://gitlab.b-data.ch/ghc/ghc4pandoc
FROM registry.gitlab.b-data.ch/ghc/ghc4pandoc:9.4.3 as bootstrap

# Install packages
WORKDIR /lamdera
COPY elm.cabal ./
COPY cabal.project ./
# COPY cabal.project.freeze ./
COPY vendor/elm-format vendor/elm-format

RUN cabal update

ENV CABALOPTS="--allow-newer -f-export-dynamic -fembed_data_files --enable-executable-static -j4"
ENV GHCOPTS="-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread"
# RUN cabal build $CABALOPTS --ghc-options="$GHCOPTS" --only-dependencies
# Manually parse & compile non-elm/elm-format deps first to save time and cut out baseline issues
RUN cabal build --dry-run | grep ' - ' | grep -v 'elm-' | cut -d' ' -f3 | sed 's/-[^-]*$//' | xargs cabal build $CABALOPTS --ghc-options="$GHCOPTS"

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

RUN cp `cabal list-bin .` ./lamdera
RUN ./lamdera --version-full
RUN strip lamdera
