#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

version="1.1.0"
os="macos"
arch="x86_64"

buildTag="lamdera-$version-$os-$arch"
dist=distribution/dist
mkdir -p $dist
bin=$dist/$buildTag
scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

stackVersion=2.9.3
isolate=~/.ghcup/$os-$arch
mkdir -p $isolate
stack="$isolate/stack"

                                                          # Ensure correct arch toolchain is installed, or install it
                                                          # Hopefully in future ghcup has better multi-arch support
if [ ! -f "$stack" ]; then
  ghcup install stack "$stackVersion" --isolate "$isolate" --force -p x86_64-apple-darwin
fi

# /opt/homebrew/opt/llvm/bin/opt --version                  #The arm64 build currently requires llvm until we get to GHC 9.4+


cd "$scriptDir/.."                                        # Move into the project root
git submodule init && git submodule update


$stack install --local-bin-path $dist

cp $dist/lamdera $bin                                     # Copy built binary to dist
strip $bin                                                # Strip symbols to reduce binary size (90M -> 56M)
ls -alh $bin
file $bin
ls -alh $bin
