FROM arm32v7/alpine:3.15 as build

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
        curl \
        libc6-compat \
        ncurses

RUN curl https://downloads.haskell.org/~ghcup/0.1.18.0/armv7-linux-ghcup-0.1.18.0 -o /usr/local/bin/ghcup && chmod a+x /usr/local/bin/ghcup


# Things work up until here – but we can't actually run this binary in this environment:
#7 [ 4/23] RUN ghcup install ghc 9.0.2 --set
#7 sha256:18069fe75b6218dab4c6eca40c9f61dce485402e2213a82a3c2d57673393ec32
#7 0.357 Error loading shared library libtinfo.so.6: No such file or directory (needed by /usr/local/bin/ghcup)
#7 0.358 Error relocating /usr/local/bin/ghcup: gnu_dev_makedev: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: gnu_dev_major: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: gnu_dev_minor: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: set_curterm: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: tigetnum: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: tigetstr: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: setupterm: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: fcntl64: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: __xstat: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: __fxstat: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: __lxstat: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: __fxstatat: symbol not found
#7 0.358 Error relocating /usr/local/bin/ghcup: del_curterm: symbol not found
#7 ERROR: executor failed running [/bin/sh -c ghcup install ghc 9.0.2 --set]: exit code: 127

# libtinfo.so.6 doesn't appear to be available in the alipine environment
RUN find . | grep -E ".so$"


# Setup GHC
RUN ghcup install ghc 9.0.2 --set
RUN ghcup install cabal 3.6.2.0 --set

ENV PATH="${PATH}:/root/.ghcup/bin"


# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
# Use the next line to debug the right file source if this area starts failing in future
RUN tree /usr/lib/gcc/
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
