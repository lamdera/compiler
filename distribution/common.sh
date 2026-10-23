#!/usr/bin/env bash

# Parse version from the canonical source in Version.hs
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
export version=$(grep -E '^raw = \(' "$REPO_ROOT/extra/Lamdera/Version.hs" | sed 's/raw = (\([0-9]*\),\([0-9]*\),\([0-9]*\))/\1.\2.\3/')


# @LAMDERA `lamdera --version-full` ends in "-dirty" when `git status --porcelain` lists
# anything at the moment extra/Lamdera/Version.hs compiles (githash's giDirty). That
# counts untracked files, and a submodule with untracked files shows up as modified,
# so build leftovers dirty every release binary. What the main repo's build leaves
# behind is in .gitignore; what lands inside a submodule can't be ignored from here,
# so it goes in that submodule's own info/exclude (local to the clone, never committed).
#
# vendor/elm-format: its library stanza lists its Main module, and building the library
# (stack or cabal) links a stray executable at vendor/elm-format/src/Main (.exe on
# Windows). Upstream elm-format's .gitignore doesn't cover it.
excludeBuildLeftovers() {
    local root exclude
    root="$(git rev-parse --show-toplevel)"
    [ -e "$root/vendor/elm-format/.git" ] || return 0
    exclude="$(cd "$root/vendor/elm-format" && cd "$(git rev-parse --git-dir)" && pwd)/info/exclude"
    mkdir -p "$(dirname "$exclude")"
    for pattern in /src/Main /src/Main.exe; do
        grep -qxF "$pattern" "$exclude" 2>/dev/null || echo "$pattern" >> "$exclude"
    done
}

# Print what githash will see. Anything listed here makes the binary's version -dirty.
gitTreeReport() {
    echo "== git status --porcelain (anything listed here stamps the version -dirty)"
    git status --porcelain
    git submodule foreach --quiet --recursive 'echo "== submodule $sm_path"; git status --porcelain'
    echo "== end of git status"
}

# After the build: show the stamp, and flag a -dirty one as a GitHub Actions warning.
checkVersionStamp() {
    local bin="$1" stamp
    stamp="$("$bin" --version-full 2>/dev/null)" || return 0   # e.g. a Linux binary on a macOS host
    echo "$stamp"
    case "$stamp" in
        *-dirty*) echo "::warning::$bin is stamped -dirty: the tree had changes when Version.hs compiled, see the git status printed before the build" ;;
    esac
}
