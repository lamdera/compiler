FROM alpine:3.15 AS build

RUN apk add --no-cache \
        bash \
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
        curl \
        nodejs \
        npm

# @LAMDERA ghcup 0.1.19.5 predates GHC 9.8.4 and can't find its bindist; 0.1.40.0 knows it.
RUN curl https://downloads.haskell.org/~ghcup/0.1.40.0/x86_64-linux-ghcup-0.1.40.0 -o /usr/local/bin/ghcup && chmod a+x /usr/local/bin/ghcup

# Setup GHC
# @LAMDERA 0.19.2 needs GHC 9.8+ (ExtendedLiterals); keep in sync with stack.yaml resolver (lts-23.28).
# NOTE: keep the alpine base <= 3.17 — alpine >= 3.18 ships musl >= 1.2.4 which drops sendfile64,
# which snap-server hard-codes (see elm/compiler 8a941d56). 3.15 is safely below that.
RUN ghcup install ghc 9.8.4 --set
RUN ghcup install cabal 3.12.1.0 --set

ENV PATH="${PATH}:/root/.ghcup/bin"

# @LAMDERA the compiler build runs Template Haskell that compiles the reactor
# frontend and the elm-repl-worker at build time (extra/Lamdera/Repl.hs,
# terminal/src/Develop/StaticFiles.hs), which shell out to `esbuild` (found via
# PATH). The dev's host has it; the container needs it explicitly.
RUN npm install -g esbuild

# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
# Use the next line to debug the right file source if this area starts failing in future
# RUN tree /usr/lib/gcc/x86_64-alpine-linux-musl
# @TODO is there a sure-fire way of getting this path?
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/10.3.1/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
RUN cp crtend.o crtend.o.orig
RUN cp crtendS.o crtend.o

WORKDIR /root/compiler
