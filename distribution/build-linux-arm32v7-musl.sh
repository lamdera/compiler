#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

buildTag="lamdera-1.1.0-linux-arm32v7-musl"

scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd "$scriptDir/.."                                        # Move into the project root

                                                          # Build in Docker
docker build --progress=plain --platform linux/arm32v7 \
  -t $buildTag \
  -f distribution/docker/arm32v7-musl.dockerfile .

mkdir -p distribution/dist                                # Ensure the dist directory is present


bin=distribution/dist/$buildTag                           # Copy built binary to dist
docker run --rm --entrypoint cat $buildTag /lamdera/lamdera > $bin
chmod a+x $bin
