#!/usr/bin/env bash

# Parse version from the canonical source in Version.hs
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
export version=$(grep -E '^raw = \(' "$REPO_ROOT/extra/Lamdera/Version.hs" | sed 's/raw = (\([0-9]*\),\([0-9]*\),\([0-9]*\))/\1.\2.\3/')
