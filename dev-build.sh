#!/usr/bin/env bash

# Dev build wrapper for the lamdera binary.
#
# live.js and the LocalDev runtime harness are embedded into the binary via
# Template Haskell. cabal only hashes .hs file contents to decide whether to
# invoke ghc at all, so editing those embedded assets alone leaves
# `cabal build` saying "Up to date" and the binary serving stale copies
# (addDependentFile only helps once ghc actually runs).
#
# This script stamps a hash of the embedded assets into a comment inside the
# two embedding modules: when the assets change, the .hs contents change, and
# cabal recompiles exactly those modules.

set -e
cd "$(dirname "$0")"

hash=$(cat extra/dist/live.js $(find extra/LocalDev/runtime-src -name '*.elm' | sort) | shasum | cut -d' ' -f1)
stamp="-- embed-stamp: $hash"

for f in extra/Lamdera/Live.hs extra/Lamdera/CLI/Live.hs; do
  if ! grep -qF "$stamp" "$f"; then
    if grep -q "^-- embed-stamp: " "$f"; then
      perl -pi -e "s|^-- embed-stamp: .*|$stamp|" "$f"
    else
      printf '\n%s\n' "$stamp" >> "$f"
    fi
  fi
done

exec cabal build exe:lamdera "$@"
