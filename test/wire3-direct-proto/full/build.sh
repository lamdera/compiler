#!/bin/sh
# Build with this worktree's compiler, flag off (base) and on (direct), --optimize and dev.
# Uses an isolated ELM_HOME so package artifacts are built by this compiler (with IR),
# never touching the shared ~/.elm cache.
set -e
cd "$(dirname "$0")"
L=${LAMDERA:-$(ls -t ../../../.stack-work/install/*/*/*/bin/lamdera | head -1)}
export ELM_HOME=${ELM_HOME:-${W3BENCH:-$(git rev-parse --show-toplevel)/../lamdera-wire3-bench}/elm-home-direct}
mkdir -p build
# The IR format is internal to this prototype: rebuild package artifacts when asked.
[ -n "$WIPE_ARTIFACTS" ] && find "$ELM_HOME" -name "artifacts*.dat" -delete
for mode in opt dev; do
  flag=""; [ $mode = opt ] && flag="--optimize"
  rm -rf elm-stuff; $L make src/Main.elm $flag --output=build/base-$mode.js >/dev/null
  rm -rf elm-stuff; LAMDERA_WIRE3_DIRECT=1 $L make src/Main.elm $flag --output=build/direct-$mode.js >/dev/null
done
for f in build/base-opt.js build/direct-opt.js build/base-dev.js build/direct-dev.js; do
  python3 - "$f" <<'PY'
import sys
p = sys.argv[1]
s = open(p).read()
i = s.rindex('}(this));')
api = "{ gen: $author$project$Main$gen, edge: $author$project$Main$edge, encode: $author$project$Main$encode, decode: $author$project$Main$decode, encodeHasFn: $author$project$Main$encodeHasFn, decodeHasFn: $author$project$Main$decodeHasFn, hasFns: $author$project$Main$hasFns, streamModel: (typeof _W3_streamEncode === 'function' && $author$project$Types$w3_encode_Model.w3e) ? function (m, size, onChunk) { return _W3_streamEncode($author$project$Types$w3_encode_Model.w3e, m, size, onChunk); } : null }"
s = s[:i] + "\nscope.W3X = " + api + ";\n" + s[i:]
open(p.replace('.js', '-x.js'), 'w').write(s)
PY
done
echo "built with $L"
