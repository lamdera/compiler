#!/bin/sh
# The green bar for a commit of the direct codecs:
#   1. full/: build (dev, --optimize, --optimize-legible), static scope check, coverage
#      oracles (incl. cold depth, sNaN, retention) on Node 22;
#   2. the adversarial reviewer's differential suite (review/: fuzz, strings, targeted,
#      dictsizes, depth) in all three modes, run from a scratch copy so its own build/
#      directory is never touched.
# Usage: bar.sh [scratchDir]   (default ../lamdera-wire3-bench/review-bar)
# ELM_HOME is used by the full/ build; ELM_HOME_REVIEW (default
# ../lamdera-wire3-bench/elm-home-review, made as a copy of ELM_HOME, or of elm-home-direct,
# when missing) by the review build. Caches are keyed by the version string alone, so
# never point either at an ELM_HOME written by a different 1.5.0 build (for example one
# without the validator chains, whose package artifacts lack w3_unsafe_decode_*).
set -u
cd "$(dirname "$0")"
here=$(pwd)
L=${LAMDERA:-$(ls -t "$(cd ../../.. && pwd)"/.stack-work/install/*/*/*/bin/lamdera | head -1)}
N=${NODE:-/nix/store/92igwjsd6av0pg8dvygirhdwahwq2sh7-nodejs-22.21.1/bin/node}
R=${1:-${W3BENCH:-$(git rev-parse --show-toplevel)/../lamdera-wire3-bench}/review-bar}
fail=0
step() { echo "== $*"; }
run() { "$@" || { echo "FAILED: $*"; fail=1; }; }

step full build; run ./build.sh
step scope check
run $N scopecheck.js build/direct-opt.js build/base-opt.js build/direct-dev.js build/base-dev.js build/direct-legible.js build/base-legible.js
step full oracles; run $N --expose-gc --stack-size=60000 check.js

step review suite build
rm -rf "$R"; mkdir -p "$R"
(cd ../review && tar --exclude='./build' --exclude='./build-bridge' --exclude='./app/elm-stuff' -cf - .) | (cd "$R" && tar xf -)
# The review suite's build.sh predates the default flip: its base build must now opt out
# (LAMDERA_WIRE3_DIRECT=0), and its direct build uses the default (no env var). Patched in
# the scratch copy only.
sed -i '' 's|rm -rf elm-stuff; \$L make|rm -rf elm-stuff; LAMDERA_WIRE3_DIRECT=0 $L make|; s|LAMDERA_WIRE3_DIRECT=1 LAMDERA_WIRE3_DIRECT_DEBUG=1 \$L make|env -u LAMDERA_WIRE3_DIRECT LAMDERA_WIRE3_DIRECT_DEBUG=1 $L make|' "$R/build.sh"
grep -q 'LAMDERA_WIRE3_DIRECT=0 $L make' "$R/build.sh" || { echo "could not patch the review build.sh"; exit 1; }
EHR=${ELM_HOME_REVIEW:-${W3BENCH:-$(git rev-parse --show-toplevel)/../lamdera-wire3-bench}/elm-home-review}
[ -d "$EHR" ] || cp -R "${ELM_HOME:-${W3BENCH:-$(git rev-parse --show-toplevel)/../lamdera-wire3-bench}/elm-home-direct}" "$EHR"
(cd "$R" && LAMDERA=$L ELM_HOME=$EHR sh build.sh > "$R/build.log" 2>&1); grep -v '^\[wire3-direct\]' "$R/build.log" | tail -1
# Every codec of the review app is direct in all three direct builds, validated decoders
# and the non-validating chain included.
n0=$(grep -c "; 0 codecs fall back to Elm" "$R/build.log")
[ "$n0" = 3 ] || { grep "codecs fall back\|NOT LOWERED" "$R/build.log" | cut -c1-400; echo "review build: codecs fell back to Elm"; fail=1; }
cd "$R"
log="$R/bar.log"; : > "$log"
# The review scripts report problems in their output (and not always in the exit code),
# so the log is scanned for their markers as well.
rrun() { "$@" >> "$log" 2>&1 || { echo "FAILED: $*"; fail=1; }; }
for mode in opt dev leg; do
  step "review fuzz $mode"; rrun $N fuzz.js ${FUZZ_ITERS:-2000} ${FUZZ_SEED:-1} $mode
  step "review targeted $mode"; rrun $N targeted.js $mode
  step "review depth $mode"; rrun $N depth.js $mode
done
step "review strings (all modes)"; rrun $N strings.js ${STR_ITERS:-100000} 1
for mode in opt dev; do step "review dictsizes $mode"; rrun $N dictsizes.js $mode; done
# Failure markers. "<-- DIVERGES" on "read N bytes from a decoded ..." lines is the one
# intentional difference (decoded Bytes are copies, _W3_STRICT_BYTES): expected, not a failure.
if grep -E "DIVERGENCE |SHALLOWER|matches elm: false|INVALID|not valid|DIVERGES" "$log" | grep -v "^read [0-9]* bytes from a decoded" | head -20 | grep .; then
  echo "review suite reported problems (see $log)"; fail=1
fi
n=$(grep -c "^NO DIVERGENCES" "$log")
[ "$n" -ge 4 ] || { echo "expected NO DIVERGENCES from fuzz x3 and strings, got $n"; fail=1; }
tail -40 "$log"
cd "$here"
[ $fail = 0 ] && echo "GREEN BAR" || echo "BAR FAILED"
exit $fail
