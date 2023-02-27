#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

arch="arm64"
buildTag="lamdera-1.1.0-macos-$arch"
dist=distribution/dist
mkdir -p $dist
bin=$dist/$buildTag

stackVersion=2.9.1

scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
isolate=~/.ghcup/macos-$arch
mkdir -p $isolate

stack="$isolate/stack"


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
if [ ! -f $stack ]; then
  ghcup install stack "$stackVersion" --isolate "$isolate" --force
fi

# /opt/homebrew/opt/llvm/bin/opt --version                  #The arm64 build currently requires llvm until we get to GHC 9.4+


cd "$scriptDir/.."                                        # Move into the project root
git submodule init && git submodule foreach --recursive git pull && git submodule update

ffiLibs="$(xcrun --show-sdk-path)/usr/include/ffi"        # Workaround for GHC9.0.2 bug until we can use GHC9.2.3+
export C_INCLUDE_PATH=$ffiLibs                            # https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_436353

export PATH="/opt/homebrew/opt/llvm@12/bin:$PATH"         # The arm64

$stack install --local-bin-path $dist

cp $dist/lamdera $bin                                     # Copy built binary to dist
strip $bin                                                # Strip symbols to reduce binary size (90M -> 56M)
