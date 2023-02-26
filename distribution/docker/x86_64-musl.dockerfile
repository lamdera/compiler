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

# Install packages
WORKDIR /elm
COPY elm.cabal ./
RUN cabal build --ghc-option=-optl=-static --ghc-option=-split-sections -O2 --only-dependencies


# Import source code
COPY builder builder
COPY compiler compiler
COPY reactor reactor
COPY terminal terminal
COPY LICENSE ./

RUN cabal build --ghc-option=-optl=-static --ghc-option=-split-sections -O2
RUN cp `cabal list-bin .` ./lamdera
RUN strip lamdera
