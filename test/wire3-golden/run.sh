#!/bin/sh
# Wire3 goldens recorded with Lamdera 1.5.0 on Elm 0.19.2 (see golden.js, fullapp.js and
# test/Test/WireGolden.hs). Exit code 0 only if every golden matches.
#
#   LAMDERA=<compiler> ELM_HOME=<fresh dir> [LAMDERA_TESTS=<lamdera-tests binary>] run.sh
#
# Use a fresh ELM_HOME per compiler so package artifacts are built by the compiler under
# test. LAMDERA_TESTS (the stack test binary) adds the type hash and Evergreen snapshot
# goldens, which run from the repository root.
set -u
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
N=${NODE:-node}
: "${LAMDERA:?set LAMDERA}" "${ELM_HOME:?set ELM_HOME}"
fail=0
echo "== golden app build"; sh "$here/build.sh" || fail=1
echo "== golden app bytes"; $N "$here/golden.js" || fail=1
echo "== full app build"; sh "$root/test/wire3-direct-proto/full/build.sh" >/dev/null || fail=1
echo "== full app bytes"; $N --stack-size=60000 "$here/fullapp.js" || fail=1
if [ -n "${LAMDERA_TESTS:-}" ]; then
  echo "== type hashes and Evergreen snapshot"
  (cd "$root" && "$LAMDERA_TESTS" Test.WireGolden) || fail=1
fi
[ $fail = 0 ] && echo "WIRE3 GOLDENS OK" || echo "WIRE3 GOLDENS FAILED"
exit $fail
