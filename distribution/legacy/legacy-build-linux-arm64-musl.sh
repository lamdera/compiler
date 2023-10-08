# This build script runs entirely within docker, which makes using the caches effectively stupidly tricky,
# invalidates them often, and makes it really slow and painful to debug any build issues as we're always
# going from scratch. So this is here for posterity, but we're now using a "run bash in the slim Docker
# base environment with a mounted volume for files & caches" method instead. Builds down from 20m to 60s.


#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

source "common.sh"
os="linux"
arch="arm64"

buildTag="lamdera-$version-$os-$arch"
dist=distribution/dist
mkdir -p $dist
bin=$dist/$buildTag
scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd "$scriptDir/.."                                        # Move into the project root
git submodule init && git submodule update

                                                          # Build in Docker
docker -H ssh://root@lamdera-falkenstein-arm64-1 build --progress=plain --platform linux/$arch \
  -t "$buildTag:latest" \
  -f distribution/legacy/$arch-musl.dockerfile .

mkdir -p distribution/dist                                # Ensure the dist directory is present


                                                          # Copy built binary to dist
docker run --rm --entrypoint cat $buildTag /lamdera/lamdera > $bin
chmod a+x $bin
file $bin
ls -alh $bin
echo "put $bin next/lamdera-next-$os-$arch" | sftp -i ~/.ssh/id_ed25519 -P 22 github@apps.lamdera.com
