# @LAMDERA arm64 musl base, mirroring x86_64-musl-base.dockerfile.
# Alpine 3.17 is the sweet spot for a static aarch64 build:
#  - musl 1.2.3 still provides sendfile64 (dropped in musl 1.2.4 / alpine >= 3.18),
#    which snap-server hard-codes (see elm/compiler 8a941d56); and
#  - gcc 12.2 ships the aarch64 outline-atomics helpers (__aarch64_*_sync) in libgcc
#    that the GHC RTS references (alpine 3.15's gcc 10.3 lacked them).
FROM alpine:3.17 AS build

RUN apk add --no-cache \
        bash \
        alpine-sdk \
        autoconf \
        gcc \
        gmp \
        gmp-dev \
        libffi \
        libffi-dev \
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

# aarch64 ghcup; 0.1.40.0 knows GHC 9.8.4 (0.1.19.5 predates it).
RUN curl https://downloads.haskell.org/~ghcup/0.1.40.0/aarch64-linux-ghcup-0.1.40.0 -o /usr/local/bin/ghcup && chmod a+x /usr/local/bin/ghcup

# Setup GHC (keep in sync with stack.yaml resolver lts-23.28 / GHC 9.8.4)
RUN ghcup install ghc 9.8.4 --set
RUN ghcup install cabal 3.12.1.0 --set

ENV PATH="${PATH}:/root/.ghcup/bin"

# reactor + repl-worker are compiled at TH build time and shell out to esbuild.
RUN npm install -g esbuild

# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734 (PIE crt objects).
# Glob the gcc version dir so this survives the alpine/gcc version.
RUN cd /usr/lib/gcc/aarch64-alpine-linux-musl/*/ && \
    cp crtbeginT.o crtbeginT.o.orig && cp crtbeginS.o crtbeginT.o && \
    cp crtend.o crtend.o.orig && cp crtendS.o crtend.o

WORKDIR /root/compiler
