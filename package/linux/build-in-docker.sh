#!/bin/bash

set -euo pipefail

if [[ $(docker --version) == *" 19.0"[0-2]* ]]; then
    echo "ERROR: Docker >= 19.03 is required"
    docker --version
    exit 1
fi

set -x

export DOCKER_BUILDKIT=1

SHA="${1-}"
if [ -z "$SHA" ]; then
    CONTEXT="."
    # VERSION="$(git describe --abbrev=8)"
    DEST="_build/docker/local/linux-x64"
else
    CONTEXT="-"
    # VERSION="$(git describe --abbrev=8 "$SHA")"
    DEST="_build/docker/$SHA/linux-x64"
fi

VERSION="0.19.1-20-gd87b2f0d"

echo "Building with SHA: $SHA"
echo "Building with Context: $CONTEXT"
echo "Building with Dest: $DEST"

rm -Rf "$DEST"

CACHE_IMAGE=lamdera/compiler-linux

# First time to build the docker image so we have something to cache
git ls-files | tar -cf - -T - | docker build - \
    --tag $CACHE_IMAGE:latest \
    --cache-from $CACHE_IMAGE:latest \
    --build-arg "ELM_FORMAT_VERSION=$VERSION" \
    --build-arg "BUILDKIT_INLINE_CACHE=1"

# Second time to get the artifacts
git ls-files | tar -cf - -T - | docker build - \
    --tag $CACHE_IMAGE:latest \
    --cache-from $CACHE_IMAGE:latest \
    --build-arg "ELM_FORMAT_VERSION=$VERSION" \
    --build-arg "BUILDKIT_INLINE_CACHE=1" \
    --target artifact \
    --output type=local,dest="$DEST/"

"$DEST/lamdera" --help
