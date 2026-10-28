#!/bin/sh
# Build the special-types oracle (src/Types.elm) with this worktree's compiler: base = the
# Elm codecs (LAMDERA_WIRE3_DIRECT=0) and direct = the default, in dev, --optimize and
# --optimize-legible. LAMDERA_DIRECT=<binary> builds the direct side with another compiler
# (e.g. a release, to tell a regression from a pre-existing behaviour).
# Uses an isolated ELM_HOME (see ../full/build.sh).
set -e
cd "$(dirname "$0")"
L=${LAMDERA:-$(ls -t ../../../.stack-work/install/*/*/*/bin/lamdera | head -1)}
LD=${LAMDERA_DIRECT:-$L}
export ELM_HOME=${ELM_HOME:-${W3BENCH:-$(git rev-parse --show-toplevel)/../lamdera-wire3-bench}/elm-home-direct}
mkdir -p build
for mode in opt dev legible; do
  flag=""; [ $mode = opt ] && flag="--optimize"; [ $mode = legible ] && flag="--optimize-legible"
  rm -rf elm-stuff; LAMDERA_WIRE3_DIRECT=0 $L make src/Main.elm $flag --output=build/base-$mode.js >/dev/null
  rm -rf elm-stuff; env -u LAMDERA_WIRE3_DIRECT LAMDERA_WIRE3_DIRECT_DEBUG=1 $LD make src/Main.elm $flag --output=build/direct-$mode.js > build/direct-$mode.log
  # Item and every type it reaches is direct: no fallbacks to Elm codecs in this app's
  # own modules (package codecs that are never reached may fall back).
  if grep "fall back to Elm:" build/direct-$mode.log | grep -q "Types\.\|Main\."; then grep "wire3-direct" build/direct-$mode.log >&2; echo "direct-$mode: some app codecs fell back to Elm" >&2; exit 1; fi
done
for f in build/base-opt.js build/direct-opt.js build/base-dev.js build/direct-dev.js build/base-legible.js build/direct-legible.js; do
  python3 - "$f" <<'PY'
import sys
p = sys.argv[1]
s = open(p).read()
i = s.rindex('}(this));')
names = ['encode', 'decode', 'decodeUnsafe', 'items', 'injKey', 'injEffKey', 'injFile', 'injFiles', 'injEffFile', 'injTexture', 'injProcessId', 'injInt', 'injFloat', 'injBytes', 'injString']
api = "{ " + ", ".join("%s: $author$project$Main$%s" % (n, n) for n in names) + " }"
s = s[:i] + "\nscope.W3X = " + api + ";\n" + s[i:]
open(p.replace('.js', '-x.js'), 'w').write(s)
PY
done
echo "built with $L (direct: $LD)"
