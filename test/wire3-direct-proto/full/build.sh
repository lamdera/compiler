#!/bin/sh
# Build with this worktree's compiler: base = the Elm codecs (LAMDERA_WIRE3_DIRECT=0, the
# escape hatch) and direct = the default (no env var), in dev, --optimize and
# --optimize-legible.
# Uses an isolated ELM_HOME so package artifacts are built by this compiler (with IR),
# never touching the shared ~/.elm cache.
set -e
cd "$(dirname "$0")"
L=${LAMDERA:-$(ls -t ../../../.stack-work/install/*/*/*/bin/lamdera | head -1)}
export ELM_HOME=${ELM_HOME:-${W3BENCH:-$(git rev-parse --show-toplevel)/../lamdera-wire3-bench}/elm-home-direct}
# The binary must embed the current direct-runtime.js (it is a Template Haskell embed).
marker=$(grep -o 'W3_RUNTIME_VERSION [0-9.-]*' ../../../extra/Lamdera/Wire3/direct-runtime.js)
if ! grep -q "$marker" "$L"; then echo "compiler binary does not embed $marker: rebuild it" >&2; exit 1; fi
mkdir -p build
# The IR format is internal to this prototype: rebuild package artifacts when asked.
[ -n "$WIPE_ARTIFACTS" ] && find "$ELM_HOME" -name "artifacts*.dat" -delete
# dev, --optimize, and --optimize-legible (what prod builds the backend with: --optimize
# without field-name shortening)
for mode in opt dev legible; do
  flag=""; [ $mode = opt ] && flag="--optimize"; [ $mode = legible ] && flag="--optimize-legible"
  rm -rf elm-stuff; LAMDERA_WIRE3_DIRECT=0 $L make src/Main.elm $flag --output=build/base-$mode.js >/dev/null
  rm -rf elm-stuff; env -u LAMDERA_WIRE3_DIRECT LAMDERA_WIRE3_DIRECT_DEBUG=1 $L make src/Main.elm $flag --output=build/direct-$mode.js > build/direct-$mode.log
  # Every codec in the app is direct, validated decoders and the non-validating
  # (w3_unsafe_decode_) chain included: no fallbacks to Elm codecs.
  if ! grep -q "; 0 codecs fall back to Elm" build/direct-$mode.log; then grep "wire3-direct" build/direct-$mode.log >&2; echo "direct-$mode: some codecs fell back to Elm" >&2; exit 1; fi
done
for f in build/base-opt.js build/direct-opt.js build/base-dev.js build/direct-dev.js build/base-legible.js build/direct-legible.js; do
  python3 - "$f" <<'PY'
import sys
p = sys.argv[1]
s = open(p).read()
i = s.rindex('}(this));')
api = "{ depth: $author$project$Main$depthProbes, decodeSNaN: $author$project$Main$decodeSNaN, encodeSNaN: $author$project$Main$encodeSNaN, gen: $author$project$Main$gen, edge: $author$project$Main$edge, encode: $author$project$Main$encode, decode: $author$project$Main$decode, encodeHasFn: $author$project$Main$encodeHasFn, decodeHasFn: $author$project$Main$decodeHasFn, hasFns: $author$project$Main$hasFns, modelEncoder: $author$project$Types$w3_encode_Model, wireBytesEncode: $lamdera$codecs$Lamdera$Wire3$bytesEncode, streamModel: (typeof _W3_streamEncode === 'function' && $author$project$Types$w3_encode_Model.w3e) ? function (m, size, onChunk) { return _W3_streamEncode($author$project$Types$w3_encode_Model.w3e, m, size, onChunk); } : null, vgen: $author$project$Main$vgen, vencode: $author$project$Main$vencode, vdecode: $author$project$Main$vdecode, vdecodeUnsafe: $author$project$Main$vdecodeUnsafe, vdecoder: $author$project$Main$vdecoder, vdecoderUnsafe: $author$project$Main$vdecoderUnsafe, vtrace: [] }"
# Every validator is wrapped (inside the IIFE, where both the Elm codecs and the direct
# codecs look it up by name at call time), so check.js can compare the sequence of
# validator calls, their arguments and results, between the two builds.
import re
validators = sorted(set(re.findall(r'^var (\$author\$project\$Validated\$w3_validate_\w+) = ', s, re.M)))
wrap = ''.join("%s = (function (f, name) { return function (v) { var r = f(v); scope.W3X.vtrace.push([name, v, r]); return r; }; })(%s, '%s');\n" % (g, g, g.split('$')[-1]) for g in validators)
s = s[:i] + "\nscope.W3X = " + api + ";\n" + wrap + "scope.W3X.validators = " + str(len(validators)) + ";\n" + s[i:]
open(p.replace('.js', '-x.js'), 'w').write(s)
PY
done
echo "built with $L"
