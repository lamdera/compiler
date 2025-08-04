#!/bin/bash

# Usage: elm-compile-tree-shake.sh <elm-file> <output-file>

if [ $# -ne 2 ]; then
    echo "Usage: $0 <elm-file> <output-file>"
    exit 1
fi

ELM_FILE="$1"
OUTPUT_FILE="$2"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
COMPILER_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
COMPILER="$COMPILER_DIR/.stack-work/install/x86_64-linux-tinfo6/c15ff6a5d083c6e061a1d54e6c3841c8be647aecbdae79225f229a5189eed1b3/9.2.8/bin/lamdera"
TREE_SHAKER="$SCRIPT_DIR/elm-tree-shake.js"

# Compile and tree shake in a pipeline
# Use tail to skip the "Success!" line
"$COMPILER" make "$ELM_FILE" --export-all-functions --output=- 2>/dev/null | tail -n +2 | node "$TREE_SHAKER" - "$OUTPUT_FILE"