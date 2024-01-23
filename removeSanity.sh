#!/bin/bash
set -ex

if [ "$OS" != 'Darwin' ]; then
  echo "This script only works on OS X! See implementation for notes on how to run."
fi

# @TODO this should be used but xargs cannot call a local function without weird jumps, so left out for now...
# $(replace_in_file pattern file)
function replace_in_file() {
  OS=`uname`
  if [ "$OS" = 'Darwin' ]; then
    # for MacOS
    sed -i '' -e "$1" "$2"
  else
    # for Linux and Windows
    sed -i'' -e "$1" "$2"
  fi
}

export -f replace_in_file

find builder -type f -name "*.hs" -print0 | xargs -0 -I {} bash -c 'replace_in_file "s/import Sanity ((!), debugFind) --/import/g" "$@"' _ {}
find compiler -type f -name "*.hs" -print0 | xargs -0 -I {} bash -c 'replace_in_file "s/import Sanity ((!), debugFind) --/import/g" "$@"' _ {}
