FROM alpine:3.15 as build

RUN apk add --no-cache \
        alpine-sdk \
        autoconf \
        gcc \
        gmp \
        gmp-dev \
        libffi \
        libffi-dev \
        llvm10 \
        make \
        musl-dev \
        ncurses-dev \
        ncurses-static \
        tree \
        wget \
        zlib-dev \
        zlib-static \
        curl

RUN curl https://downloads.haskell.org/~ghcup/0.1.18.0/x86_64-linux-ghcup-0.1.18.0 -o /usr/local/bin/ghcup && chmod a+x /usr/local/bin/ghcup

# Setup GHC
RUN ghcup install ghc 9.0.2 --set
RUN ghcup install cabal 3.6.2.0 --set

ENV PATH="${PATH}:/root/.ghcup/bin"


# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
# Use the next line to debug the right file source if this area starts failing in future
# RUN tree /usr/lib/gcc/x86_64-alpine-linux-musl
# @TODO is there a sure-fire way of getting this path?
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/10.3.1/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
RUN cp crtend.o crtend.o.orig
RUN cp crtendS.o crtend.o

RUN cabal update

# # Install packages
# WORKDIR /lamdera
# COPY elm.cabal ./
# RUN cabal build --ghc-option=-optl=-static --ghc-option=-split-sections -O2 --only-dependencies

# # Import source code
# COPY builder builder
# COPY compiler compiler
# COPY reactor reactor
# COPY terminal terminal
# COPY LICENSE ./
# COPY vendor/elm-format vendor/elm-format

# RUN cabal build --ghc-option=-optl=-static --ghc-option=-split-sections -O2
# RUN cp `cabal list-bin .` ./lamdera
# RUN strip lamdera

# Install packages
WORKDIR /lamdera
COPY elm.cabal ./
COPY cabal.project ./
COPY cabal.project.freeze ./
COPY vendor/elm-format vendor/elm-format

ENV CABALOPTS="-f-export-dynamic -fembed_data_files --enable-executable-static -j4"
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
