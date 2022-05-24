FROM alpine:3.15 as build

ENV GHC_VERSION 9.0.2

ENV LANG C.UTF-8

# Install ghc and cabal-isntall
RUN apk add --no-cache curl
RUN curl https://downloads.haskell.org/~ghcup/0.1.17.3/x86_64-linux-ghcup-0.1.17.3 > /usr/local/bin/ghcup && \
    chmod +x /usr/local/bin/ghcup
RUN ghcup install cabal recommended
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
        zlib-static
RUN ghcup install ghc $GHC_VERSION && \
    ghcup set ghc $GHC_VERSION
ENV PATH $PATH:/root/.ghcup/bin


# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
# Use the next line to debug the right file source if this area starts failing in future
# RUN tree /usr/lib/gcc/x86_64-alpine-linux-musl
# @TODO is there a sure-fire way of getting this path?
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/10.3.1/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
RUN cp crtend.o crtend.o.orig
RUN cp crtendS.o crtend.o


WORKDIR /lamdera

# Install lamdera dependencies
COPY cabal.project ./
COPY cabal.project.freeze ./

COPY elm.cabal ./

RUN cabal v2-update

# RUN cabal v2-install --lib shake
# RUN cabal v2-build --only-dependencies
# RUN cabal v2-build --only-dependencies --enable-tests
# RUN cabal v2-install ShellCheck
RUN cabal v2-build --ghc-option=-optl=-static --ghc-option=-split-sections -O2 --only-dependencies

# Build lamdera
COPY ./ .

ARG ELM_FORMAT_VERSION="unknown"
RUN mkdir generated && echo -e "module Build_lamdera where\n\ngitDescribe :: String\ngitDescribe = \"$ELM_FORMAT_VERSION\"\n" > generated/Build_lamdera.hs
RUN cabal v2-build --ghc-option=-optl=-static --ghc-option=-split-sections -O2
RUN cp dist-newstyle/build/x86_64-linux/ghc-*/elm-*/x/lamdera/*opt/build/lamdera/lamdera ./
RUN strip -s ./lamdera


FROM scratch as artifact
COPY --from=build /lamdera/lamdera /lamdera
