# FROM registry.gitlab.b-data.ch/ghc/ghc4pandoc:8.10.7 as bootstrap

# @TODO having issues on subsequent versions of GHC, retry in future
# https://gitlab.b-data.ch/ghc/ghc4pandoc
FROM registry.gitlab.b-data.ch/ghc/ghc4pandoc:9.2.7 as bootstrap

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
# Manually parse & compile non-elm/elm-format deps first to save time and cut out baseline issues, previous line has the issue of busting cache very time because of elm-format deps whose folder contents change with cache files
RUN cabal build --dry-run | grep ' - ' | grep -v 'elm-' | grep -v 'avh4' | cut -d' ' -f3 | sed 's/-[^-]*$//'
RUN cabal build --dry-run | grep ' - ' | grep -v 'elm-' | grep -v 'avh4' | cut -d' ' -f3 | sed 's/-[^-]*$//' | xargs -I{} cabal build {} --only-dependencies $CABALOPTS --ghc-options="$GHCOPTS"

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
