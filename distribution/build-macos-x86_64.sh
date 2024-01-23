#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

source "common.sh"
os="macos"
arch="x86_64"

buildTag="lamdera-$version-$os-$arch"
dist=distribution/dist
mkdir -p $dist
bin=$dist/$buildTag
scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )






stackVersion=2.11.1
isolate=~/.ghcup/$os-$arch
mkdir -p $isolate
stack="$isolate/stack"

                                                          # Ensure correct arch toolchain is installed, or install it
                                                          # Hopefully in future ghcup has better multi-arch support
if [ ! -f "$stack" ]; then
  ghcup install stack "$stackVersion" --isolate "$isolate" --force -p x86_64-apple-darwin
fi

cd "$scriptDir/.."                                        # Move into the project root
git submodule init && git submodule update

$stack install --local-bin-path $dist

cp $dist/lamdera $bin                                     # Copy built binary to dist
strip $bin                                                # Strip symbols to reduce binary size (90M -> 56M)
file $bin
ls -alh $bin
echo "put $bin next/lamdera-next-$os-$arch" | sftp -i ~/.ssh/id_ed25519 -P 22 github@apps.lamdera.com
