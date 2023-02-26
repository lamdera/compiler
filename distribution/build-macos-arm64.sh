#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

arch="arm64"
buildTag="lamdera-1.1.0-macos-$arch"
bin=distribution/dist/$buildTag

ghcVersion=9.0.2
cabalVersion=3.6.2.0

scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
isolate=$scriptDir/ghcup/macos-$arch

ghc=$isolate/bin/ghc-$ghcVersion
cabal="$isolate/cabal --with-compiler=$ghc"


if ! uname -a | grep "Darwin" && uname -a | grep "arm64"; then
  echo "This build can only be run on an Apple M-series chipset build host."
  exit 1;
fi
if ! ghcup --version; then
  echo "This build requires ghcup: https://www.haskell.org/ghcup/"
  exit 1;
fi

                                                          # Ensure correct arch toolchain is installed, or install it
                                                          # Hopefully in future ghcup has better multi-arch support
if ! ls -alh "$ghc"; then
  ghcup install ghc "$ghcVersion" --isolate "$isolate" --force
fi
if ! $cabal --version | grep $cabalVersion; then
  ghcup install cabal "$cabalVersion" --isolate "$isolate" --force
fi

opt --version                                             # The arm64 build currently requires llvm until we get to GHC 9.4+


cd "$scriptDir/.."                                        # Move into the project root

ffiLibs="$(xcrun --show-sdk-path)/usr/include/ffi"        # Workaround for GHC9.0.2 bug until we can use GHC9.2.3+
export C_INCLUDE_PATH=$ffiLibs                            # https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_436353

export PATH="/opt/homebrew/opt/llvm@12/bin:$PATH"         # The arm64

$cabal update
$cabal build -j4                                          # Build with concurrency 4

mkdir -p distribution/dist                                # Ensure the dist directory is present

cp "$($cabal list-bin .)" $bin                            # Copy built binary to dist
strip $bin                                                # Strip symbols to reduce binary size (90M -> 56M)
