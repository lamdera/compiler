#!/bin/sh
# Build the test app with this worktree's compiler, flag off (baseline) and on (direct),
# in --optimize and dev mode, and expose gen/encode/decode for check-compiler.js.
set -e
cd "$(dirname "$0")"
L=${LAMDERA:-$(ls -t ../../.stack-work/install/*/*/*/bin/lamdera | head -1)}
mkdir -p build
for mode in opt dev; do
  flag=""; [ $mode = opt ] && flag="--optimize"
  rm -rf elm-stuff; $L make src/Main.elm $flag --output=build/c-base-$mode.js
  rm -rf elm-stuff; LAMDERA_WIRE3_DIRECT=1 $L make src/Main.elm $flag --output=build/c-direct-$mode.js
done
for f in build/c-base-opt.js build/c-direct-opt.js build/c-base-dev.js build/c-direct-dev.js; do
  python3 - "$f" <<'PY'
import sys
p = sys.argv[1]
s = open(p).read()
i = s.rindex('}(this));')
s = s[:i] + "\nscope.W3X = { gen: $author$project$Main$gen, edge: $author$project$Main$edge, encode: $author$project$Main$encode, decode: $author$project$Main$decode, eq: _Utils_eq };\n" + s[i:]
open(p.replace('.js', '-x.js'), 'w').write(s)
PY
done
