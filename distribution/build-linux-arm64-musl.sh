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
compilerRoot="$scriptDir/.."

if [ "$GITHUB_ACTIONS" == "true" ]; then
    mountRoot="$compilerRoot"
    rsyncCompilerPath=""
    dockerHost=""
else
    mountRoot="/root/compiler"
    rsyncCompilerPath="root@lamdera-falkenstein-arm64-1:/root/compiler/"
    dockerHost="-H ssh://root@lamdera-falkenstein-arm64-1"
fi


cd "$compilerRoot"                                                   # Move into the project root
git submodule init && git submodule update

if [ -z "$rsyncCompilerPath" ]; then
    echo "rsyncCompilerPath is empty, skipping"
else
    # GOAL: syncronise all relevant files to the remote host
    rsync -avz \
        --exclude="ext-package-cache" \
        --exclude="distribution/dist" \
        --exclude="extra/npm" \
        --exclude="node_modules" \
        --exclude="elm-stuff" \
        ./ $rsyncCompilerPath
fi


build_binary_docker() {
    set -ex
    local bin="$1"
    local compilerRoot="/root/compiler"
    cd $compilerRoot

    git config --global --add safe.directory /root/compiler

    # GOAL: get the cabal caches into the mounted folder so they persist outside the Docker run lifetime and we don't needlessly rebuild hundreds of super expensive deps repeatedly forever
    # This is documented but doesn't seem to work https://cabal.readthedocs.io/en/3.6/installing-packages.html#environment-variables
    export CABAL_DIR=$compilerRoot/distribution/cabal-caches/linux-arm64
    mkdir -p distribution/cabal-caches/linux-arm64 || true
    ln -sf $compilerRoot/distribution/cabal-caches/linux-arm64 ~/.cabal

    # GOAL: pin our dependencies so we can build them one by one
    cabal update
    # We have to freeze the deps to get a cohesive deps set, otherwise `cabal build <dep>` will install the latest version instead of the one we need
    rm cabal.project.freeze || true # Remove the freeze file so we can update the deps
    cabal freeze

    # Our options required for static linking
    CABALOPTS="--allow-newer -f-export-dynamic -fembed_data_files --enable-executable-static -j4"
    GHCOPTS="-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread"

    # GOAL: build non-elm/elm-format deps first to save time and cut out baseline issues
    # cabal build --dry-run | grep ' - ' | grep -v 'elm-' | grep -v 'avh4' | cut -d' ' -f3 | sed 's/-[^-]*$//' | xargs -I{} cabal build {} --only-dependencies $CABALOPTS --ghc-options="$GHCOPTS"
    cabal build --only-dependencies $CABALOPTS --ghc-options="$GHCOPTS"

    # GOAL: build the Lamdera binary statically
    cabal build $CABALOPTS --ghc-options="$GHCOPTS"

    cp "$(cabal list-bin .)" "$bin"
    strip "$bin"

}
declare -f build_binary_docker

# GOAL: get a suitable build environment with GHC & Cabal build for arm64 in an Alpine container using MUSL instead of GLIBC, so we can build portable static binaries

# Use it without the bash injection for manual testing
# docker -H ssh://root@lamdera-falkenstein-arm64-1 run \
#     -v /root/compiler:/root/compiler \
#     -it registry.gitlab.b-data.ch/ghc/ghc4pandoc:9.2.7 \
#     /bin/bash

[ "$GITHUB_ACTIONS" == "true" ] && runMode="-i" || runMode="-it"

docker $dockerHost run \
    -v "$mountRoot:/root/compiler" \
    $runMode registry.gitlab.b-data.ch/ghc/ghc4pandoc:9.2.7 \
    bash -c "$(declare -f build_binary_docker); build_binary_docker '$bin'"


if [ -z "$rsyncCompilerPath" ]; then
    echo "rsyncCompilerPath is empty, skipping"
else
    # GOAL: get the remote binary locally
    scp $rsyncCompilerPath/"$bin" "$bin"
fi

ls -alh "$bin"
chmod a+x "$bin"
file "$bin"
ls -alh "$bin"
echo "put $bin next/lamdera-next-$os-$arch" | sftp -i ~/.ssh/id_ed25519 -P 22 github@apps.lamdera.com
