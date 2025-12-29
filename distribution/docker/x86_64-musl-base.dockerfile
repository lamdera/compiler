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
        curl

RUN curl https://downloads.haskell.org/~ghcup/0.1.19.5/x86_64-linux-ghcup-0.1.19.5 -o /usr/local/bin/ghcup && chmod a+x /usr/local/bin/ghcup

# Setup GHC
RUN ghcup install ghc 9.2.8 --set
RUN ghcup install cabal 3.10.1.0 --set

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

WORKDIR /root/compiler
