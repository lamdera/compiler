# @LAMDERA musl base for the static Linux release builds (build-linux-*-musl.sh).
#
# GHC comes from benz0li's ghc-musl image, not from a ghcup bindist. GHC's own alpine
# bindists (what ghcup installs, both aarch64-alpine3_18 and x86_64-alpine3_12) ship
# their boot libraries without -split-sections: libHSbase.a for 9.8.4 has 286 .text
# sections and not one .text.<symbol> section. The static link's --gc-sections can then
# drop nothing from base, text, containers, bytestring, template-haskell... and every
# module the binary touches comes in whole. ghc-musl builds GHC with the
# perf+split_sections+llvm flavour: its libHSbase.a has a section per symbol (33882
# .text.* sections in the 9.2.8 image). The 1.4.0 linux-arm64 release was built on
# ghc-musl:9.2.8 and was 28.6MB; 1.5.0 on the ghcup bindist was 74MB.
#
# This image's alpine (3.24, musl 1.2.5) no longer lets new code link against
# sendfile64, which snap-server's foreign import hard-codes (see elm/compiler
# 8a941d56). The build scripts alias it to sendfile at link time
# (-Wl,--defsym=sendfile64=sendfile), which is the same function on musl, where off_t is
# always 64 bit. Template Haskell is unaffected: it dlopens the dependencies' shared
# objects, and musl's dynamic linker still resolves the LFS64 names.
#
# Pinned by digest because the 9.8.4 tag is rebuilt in place. This digest is the
# multi-arch index of the 2026-10-23 build (alpine 3.24.3, cabal 3.10.3.0); docker picks
# the arm64 or amd64 image from it. To move to a newer build:
#   docker buildx imagetools inspect glcr.b-data.ch/ghc/ghc-musl:9.8.4   (the top Digest)
FROM glcr.b-data.ch/ghc/ghc-musl:9.8.4@sha256:ffdc665c9d82bc886bb4e0a2693c7044a8e00000e6f2d39425a4a1ee2ce52279

# The compiler build runs Template Haskell that compiles the reactor frontend and the
# elm-repl-worker at build time (extra/Lamdera/Repl.hs, terminal/src/Develop/StaticFiles.hs),
# which shell out to `esbuild`. The image has git, gcc, and the static gmp/zlib/ncurses/ffi.
RUN apk add --no-cache nodejs npm
RUN npm install -g esbuild

WORKDIR /root/compiler
