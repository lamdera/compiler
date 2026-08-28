FROM alpine:3.15 AS build

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

# @LAMDERA ghcup 0.1.19.5 predates GHC 9.8.4 and can't find its bindist; 0.1.40.0 knows it.
RUN curl https://downloads.haskell.org/~ghcup/0.1.40.0/x86_64-linux-ghcup-0.1.40.0 -o /usr/local/bin/ghcup && chmod a+x /usr/local/bin/ghcup

# Setup GHC
# @LAMDERA 0.19.2 needs GHC 9.8+ (ExtendedLiterals); keep in sync with stack.yaml resolver (lts-23.28).
# NOTE: keep the alpine base <= 3.17 — alpine >= 3.18 ships musl >= 1.2.4 which drops sendfile64,
# which snap-server hard-codes (see elm/compiler 8a941d56). 3.15 is safely below that.
RUN ghcup install ghc 9.8.4 --set
RUN ghcup install cabal 3.12.1.0 --set

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

# Install packages
WORKDIR /lamdera
COPY elm.cabal ./
COPY cabal.project ./
COPY cabal.project.freeze ./
COPY vendor/elm-format vendor/elm-format

RUN cabal update

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
COPY vendor/elm-repl-worker vendor/elm-repl-worker

ARG GITHUB_ACTIONS
ENV GITHUB_ACTIONS=${GITHUB_ACTIONS}

RUN cabal build $CABALOPTS --ghc-options="$GHCOPTS"

RUN cabal list-bin exe:lamdera | grep -v HEAD
RUN cp `cabal list-bin exe:lamdera | grep -v HEAD` ./lamdera
RUN ./lamdera --version-full
RUN strip lamdera
