#!/bin/sh
# Builds the review app with this worktree's compiler: flag off (base) and on (direct),
# --optimize and dev, then exposes every e_T / d_T as scope.W3R for fuzz.js.
# ELM_HOME defaults to a private copy (never the shared ~/.elm).
set -e
cd "$(dirname "$0")"
L=${LAMDERA:-$(ls -t $(pwd)/../../../.stack-work/install/*/*/*/bin/lamdera | head -1)}
: "${ELM_HOME:?set ELM_HOME to an isolated copy, e.g. a copy of ../lamdera-wire3-bench/elm-home-direct}"
export ELM_HOME
EXCLUDE=${EXCLUDE:-TMA} python3 gen.py
OUT=${OUT:-build}
mkdir -p $OUT
cd app
for mode in opt dev leg; do
  flag=""; [ $mode = opt ] && flag="--optimize"; [ $mode = leg ] && flag="--optimize-legible"
  rm -rf elm-stuff; $L make src/Main.elm $flag --output=../$OUT/base-$mode.js >/dev/null
  rm -rf elm-stuff; LAMDERA_WIRE3_DIRECT=1 LAMDERA_WIRE3_DIRECT_DEBUG=1 $L make src/Main.elm $flag --output=../$OUT/direct-$mode.js
done
cd ..
for f in $OUT/base-opt.js $OUT/direct-opt.js $OUT/base-dev.js $OUT/direct-dev.js $OUT/base-leg.js $OUT/direct-leg.js; do
  python3 - "$f" <<'PY'
import sys, json
p = sys.argv[1]
s = open(p).read()
names = json.load(open('names.json'))
i = s.rindex('}(this));')
api = '{ ' + ', '.join('%s: { e: $author$project$Main$e_%s, d: $author$project$Main$d_%s, u: $author$project$Main$u_%s, w: typeof $author$project$Types$w3_encode_%s !== "undefined" ? $author$project$Types$w3_encode_%s : $author$project$Params$w3_encode_%s }' % (n, n, n, n, n, n, n) for n in names) + ' }'
extra = "scope.W3R = " + api + ";\nscope.W3R.TAll.s = $author$project$Main$s_TAll;\nscope.W3R.TVModel.s = $author$project$Main$s_TVModel;\nscope.W3readBytesPast = $author$project$Main$readBytesPast;\nscope.W3R_direct = typeof _W3_runEncode === 'function';\nscope.W3S = typeof _W3_streamEncode === 'function' ? _W3_streamEncode : null;\n"
# Every validator (Validated.elm, and the generated ones in Params.elm) is wrapped inside
# the IIFE, where both the Elm and the direct codecs look it up by name at call time, so
# fuzz.js can compare the sequence of validator calls (validator, argument, result).
import re
validators = sorted(set(re.findall(r'^var (\$author\$project\$(?:Validated|Params)\$w3_validate_\w+) = ', s, re.M)))
extra += "scope.W3VT = [];\nscope.W3VN = %d;\n" % len(validators)
extra += ''.join("%s = (function (f, name) { return function (v) { var r = f(v); scope.W3VT.push([name, v, r]); return r; }; })(%s, '%s');\n" % (g, g, g.split('$')[-1]) for g in validators)
s = s[:i] + "\n" + extra + s[i:]
open(p.replace('.js', '-x.js'), 'w').write(s)
PY
done
grep -c "function \$author\$project\$Types\$w3e_\|var \$author\$project\$Types\$w3_encode_.* = _W3_encWrap" $OUT/direct-opt.js || true
echo "built with $L"
