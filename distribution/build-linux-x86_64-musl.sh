#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

source "common.sh"
os="linux"
arch="x86_64"

buildTag="lamdera-$version-$os-$arch"
dist=distribution/dist
bin=$dist/$buildTag

scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
compilerRoot="$scriptDir/.."

if [ "$GITHUB_ACTIONS" == "true" ]; then
    mountRoot="$compilerRoot"
    cacheRoot="$GITHUB_WORKSPACE/user-build-cache/linux-x86_64"
    dockerHost=""
elif [ "$LOCAL_DOCKER" == "true" ]; then
    mountRoot="$compilerRoot"
    cacheRoot="$HOME/.docker/user-build-cache/linux-x86_64"
    dockerHost=""
else
    mountRoot="$compilerRoot"
    cacheRoot="$HOME/.docker/user-build-cache/linux-x86_64"
    dockerHost=""
fi

cd "$compilerRoot"                                        # Move into the project root
git submodule init && git submodule update
# Fetch origin/master in package-replacement submodules so the TH code can diff against it.
# Docker has no SSH so this must happen on the host where git+ssh works.
for dir in extra/package-replacements/*/*; do
    [ -d "$dir" ] && git -C "$dir" fetch origin master:refs/remotes/origin/master
done

mkdir -p "$cacheRoot" || true

# GOAL: build the base image with GHC, Cabal, and system deps (this layer caches well)
docker build --progress=plain --platform linux/amd64 \
    -t "lamdera-x86_64-musl-base:latest" \
    -f distribution/docker/x86_64-musl-base.dockerfile .


build_binary_docker() {
    set -ex
    local bin="$1"
    local actions="$2"
    local userId="$3"
    local groupId="$4"
    local compilerRoot="/root/compiler"
    cd $compilerRoot
    export GITHUB_ACTIONS=$actions

    cleanup() {
        echo "trap cleanup: build failed with exit code $?"
        # Work around ownership issues that prevent GH actions from managing the files later
        [ "$actions" == "true" ] && chown -R "$userId:$groupId" ./* || true
    }
    trap cleanup EXIT

    git config --global --add safe.directory /root/compiler
    # Also mark submodule directories as safe (they're mounted from the host with different ownership)
    for submodule in $(git -C /root/compiler submodule foreach --quiet 'echo $toplevel/$sm_path'); do
        git config --global --add safe.directory "$submodule"
    done

    # GOAL: get the cabal caches into the mounted folder so they persist outside the Docker run lifetime and we don't needlessly rebuild hundreds of super expensive deps repeatedly forever
    # This is documented but doesn't seem to work https://cabal.readthedocs.io/en/3.6/installing-packages.html#environment-variables
    export CABAL_DIR=/root/cache/cabal
    export STACK_ROOT=/root/cache/stack
    export STACK_WORK=/root/cache/stack-work

    mkdir -p /root/cache/cabal || true
    ln -sf /root/cache/cabal ~/.cabal

    cabal update

    # GOAL: pin our dependencies so we can build them one by one
    # We do this once outside of the build script, see distribution/sync-cabal-freeze.sh
    # We have to freeze the deps to get a cohesive deps set, otherwise `cabal build <dep>` will install the latest version instead of the one we need

    # Our options required for static linking
    CABALOPTS="-f-export-dynamic -fembed_data_files --enable-executable-static -j4"
    GHCOPTS="-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread"

    # GOAL: build non-elm/elm-format deps first to save time and cut out baseline issues
    # cabal build --dry-run | grep ' - ' | grep -v 'elm-' | grep -v 'avh4' | cut -d' ' -f3 | sed 's/-[^-]*$//' | xargs -I{} cabal build {} --only-dependencies $CABALOPTS --ghc-options="$GHCOPTS"
    cabal build --only-dependencies $CABALOPTS --ghc-options="$GHCOPTS"

    # GOAL: build the Lamdera binary statically
    cabal build $CABALOPTS --ghc-options="$GHCOPTS" || true

    # GOAL: catch silly cache failures that work on a second build
    GITHUB_ACTIONS=true cabal build $CABALOPTS --ghc-options="$GHCOPTS"

    cp "$(cabal list-bin .)" "$bin"
    strip "$bin"

    # Work around ownership issues that prevent GH actions from managing the files later
    [ "$actions" == "true" ] && chown -R "$userId:$groupId" ./* || true
}
declare -f build_binary_docker

# GOAL: get a suitable build environment with GHC & Cabal build for x86_64 in an Alpine container using MUSL instead of GLIBC, so we can build portable static binaries

# For manual testing drop a `bash` line wherever you'd like within build_binary_docker and re-run this script

mkdir -p $dist

[ "$GITHUB_ACTIONS" == "true" ] && runMode="--rm -i" || runMode="-it"
docker $dockerHost run \
    --platform linux/amd64 \
    -v "$mountRoot:/root/compiler" \
    -v "$cacheRoot:/root/cache" \
    $runMode lamdera-x86_64-musl-base:latest \
    bash -c "$(declare -f build_binary_docker); build_binary_docker '$bin' '$GITHUB_ACTIONS' '$(id -u)' '$(id -g)'"

ls -alh "$bin"
chmod a+x "$bin"
file "$bin"
ls -alh "$bin"
echo "put $bin next/lamdera-next-$os-$arch" | sftp -i ~/.ssh/id_ed25519 -P 22 github@apps.lamdera.com
