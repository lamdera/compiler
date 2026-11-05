#!/usr/bin/env bash
# Runs the community Elm test suite (https://github.com/eco-lang/elm-test-suite) against a
# lamdera build. On demand only: nothing else calls this, and it needs node 22+ and the network
# (to clone the suite and fetch the Elm packages its tests use).
#
# The suite is about 1,400 real Elm programs: each is compiled, run on node, and its output
# checked against the expected output in its comments, or it must be rejected with a given error.
# It is cloned into test/elm-test-suite/ (ignored by git) at SUITE_REV below.
#
#   test/elm-test-suite.sh                         # this checkout's stack build
#   test/elm-test-suite.sh --lamdera ~/bin/lamdera # another binary
#   test/elm-test-suite.sh --compare elm-0.19.1,elm-0.19.3,lamdera-1.5.0
#                                                  # also run released compilers (pnpm installs
#                                                  # them) and report where outcomes differ
#   test/elm-test-suite.sh --latest                # the suite's main branch, not SUITE_REV
#   test/elm-test-suite.sh -- --project elm-core --filter Dict
#                                                  # anything after -- goes to the suite's runner
#
# The exit status is 0 when every failure is listed in test/elm-test-suite.expected, so it can
# gate a release. A test listed there that now passes is reported, so the list can shrink.
# Results: test/elm-test-suite/.work/results/lamdera-dev.json (and .work/report.md with --compare).
set -euo pipefail

SUITE_REPO=https://github.com/eco-lang/elm-test-suite.git
SUITE_REV=9b63f2882d46bb00b466462190821b90b1a248e7

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
compilerRoot="$(dirname "$here")"
suite="$here/elm-test-suite"
expected="$here/elm-test-suite.expected"

lamdera=""
compare=""
latest=0
runnerArgs=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --lamdera) lamdera="$2"; shift 2 ;;
    --compare) compare="$2"; shift 2 ;;
    --latest) latest=1; shift ;;
    --) shift; runnerArgs=("$@"); break ;;
    -h|--help) sed -n '2,23p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown argument: $1 (see --help)" >&2; exit 2 ;;
  esac
done

if [[ -z "$lamdera" ]]; then
  lamdera="$(cd "$compilerRoot" && stack path --local-install-root)/bin/lamdera"
fi
lamdera="$(cd "$(dirname "$lamdera")" && pwd)/$(basename "$lamdera")"
[[ -x "$lamdera" ]] || { echo "no lamdera binary at $lamdera (stack build first, or pass --lamdera)" >&2; exit 2; }

nodeMajor="$(node -p 'process.versions.node.split(".")[0]')"
if (( nodeMajor < 22 )); then
  echo "warning: node $(node --version); the suite's case-mapping tests expect node 22+ (Unicode 17)" >&2
fi

# The suite, at the pinned commit (or its main branch with --latest).
if [[ ! -d "$suite/.git" ]]; then
  git clone --quiet "$SUITE_REPO" "$suite"
fi
if (( latest )); then
  git -C "$suite" fetch --quiet origin main
  rev="$(git -C "$suite" rev-parse FETCH_HEAD)"
else
  rev="$SUITE_REV"
  git -C "$suite" cat-file -e "$rev^{commit}" 2>/dev/null || git -C "$suite" fetch --quiet origin
fi
git -C "$suite" checkout --quiet --force "$rev"
echo "elm-test-suite at $(git -C "$suite" log -1 --format='%h %cs %s' "$rev")"

# Add this lamdera to the suite's compilers.json as "lamdera-dev". checkout --force above put back
# the suite's own file, so this starts from it each run.
LAMDERA_BIN="$lamdera" LAMDERA_DESC="lamdera $("$lamdera" --version-full 2>/dev/null | head -1) at $lamdera" \
  node -e '
    const fs = require("fs");
    const file = process.argv[1];
    const cfg = JSON.parse(fs.readFileSync(file, "utf8"));
    cfg.compilers["lamdera-dev"] = {
      description: process.env.LAMDERA_DESC,
      command: process.env.LAMDERA_BIN,
      target: "js",
    };
    fs.writeFileSync(file, JSON.stringify(cfg, null, 2) + "\n");
  ' "$suite/compilers.json"

cd "$suite"
results=".work/results/lamdera-dev.json"

if [[ -n "$compare" ]]; then
  # The released compilers come from npm through the suite's package.json. It asks for pnpm, but
  # its dependencies are plain npm aliases (elm-0.19.3 -> elm@0.19.3-0), which npm installs too.
  npm install --no-save --no-package-lock --no-audit --no-fund --silent
  node runner/run-all.js --compilers "lamdera-dev,$compare" ${runnerArgs[@]+"${runnerArgs[@]}"} || true
  echo
  echo "Comparison report: $suite/.work/report.md"
else
  node runner/run.js --compiler lamdera-dev --json "$results" ${runnerArgs[@]+"${runnerArgs[@]}"} || true
fi

[[ -f "$results" ]] || { echo "the runner wrote no results ($results)" >&2; exit 2; }

# Compare the failures with the expected ones. A filtered run (-- --project/--filter) only
# checks the expected failures it ran.
node -e '
  const fs = require("fs");
  const [resultsFile, expectedFile] = process.argv.slice(1);
  const report = JSON.parse(fs.readFileSync(resultsFile, "utf8"));
  const expected = new Map();
  for (const line of fs.readFileSync(expectedFile, "utf8").split("\n")) {
    const m = line.match(/^\s*([^#\s]+)\s*(?:#\s*(.*))?$/);
    if (m) expected.set(m[1], m[2] || "");
  }
  const ran = new Map();
  for (const [project, tests] of Object.entries(report.projects)) {
    for (const [name, r] of Object.entries(tests)) ran.set(project + "/" + name, r);
  }
  const unexpected = [], fixed = [];
  for (const [test, r] of ran) {
    const failed = r.status !== "pass" && r.status !== "skip";
    if (failed && !expected.has(test)) unexpected.push(test + ": " + r.reason);
    if (r.status === "pass" && expected.has(test)) fixed.push(test + " (expected to fail: " + expected.get(test) + ")");
  }
  const known = [...expected.keys()].filter((t) => ran.has(t) && !["pass", "skip"].includes(ran.get(t).status));
  console.log();
  console.log("Expected failures that failed: " + known.length);
  if (fixed.length) console.log("\nNow passing, take them out of test/elm-test-suite.expected:\n  " + fixed.join("\n  "));
  if (unexpected.length) console.log("\nUnexpected failures:\n  " + unexpected.join("\n  "));
  process.exit(unexpected.length ? 1 : 0);
' "$results" "$expected"
