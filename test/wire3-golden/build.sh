#!/bin/sh
# Builds the golden app (app/) with $LAMDERA in dev, --optimize and --optimize-legible,
# each with the direct codecs (the default) and the Elm codecs (LAMDERA_WIRE3_DIRECT=0),
# into build/<codecs>-<mode>.js. Uses $ELM_HOME as given (set a fresh one per compiler).
set -e
cd "$(dirname "$0")"
L=${LAMDERA:?set LAMDERA to the compiler binary under test}
mkdir -p build
cd app
for mode in dev opt legible; do
  flag=""; [ $mode = opt ] && flag="--optimize"; [ $mode = legible ] && flag="--optimize-legible"
  rm -rf elm-stuff; LAMDERA_WIRE3_DIRECT=0 $L make src/Main.elm $flag --output=../build/base-$mode.js >/dev/null
  rm -rf elm-stuff; env -u LAMDERA_WIRE3_DIRECT LAMDERA_WIRE3_DIRECT_DEBUG=1 $L make src/Main.elm $flag --output=../build/direct-$mode.js > ../build/direct-$mode.log 2>&1 || { cat ../build/direct-$mode.log; exit 1; }
done
rm -rf elm-stuff
echo "built with $L"
