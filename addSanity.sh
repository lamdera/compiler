#!/bin/bash
set -ex

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

find builder -type f -print0 | xargs -0 -I {} bash -c 'replace_in_file "s/import Data.Map ((!))/import Sanity ((!), debugFind) -- Data.Map ((!))/g" "$@"' _ {}
find compiler -type f -print0 | xargs -0 -I {} bash -c 'replace_in_file "s/import Data.Map ((!))/import Sanity ((!), debugFind) -- Data.Map ((!))/g" "$@"' _ {}

find builder -type f -print0 | xargs -0 -I {} bash -c 'replace_in_file "s/import Data.Map.Strict ((!))/import Sanity ((!), debugFind) -- Data.Map.Strict ((!))/g" "$@"' _ {}
find compiler -type f -print0 | xargs -0 -I {} bash -c 'replace_in_file "s/import Data.Map.Strict ((!))/import Sanity ((!), debugFind) -- Data.Map.Strict ((!))/g" "$@"' _ {}
