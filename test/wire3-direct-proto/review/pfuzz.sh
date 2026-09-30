#!/bin/sh
# Random parameterised-type sets (gen.py, PSEED): build each into build-p<seed>/, check
# scoping with ../full/scopecheck.js, and fuzz only the generated types in dev/opt/leg.
#   ELM_HOME=... sh pfuzz.sh "1 2 3" [iterations]
cd "$(dirname "$0")"
ITER=${2:-800}
for seed in $1; do
  out=build-p$seed
  if ! PSEED=$seed OUT=$out sh build.sh > $out.log 2>&1; then echo "seed $seed: BUILD FAILED"; grep -v Verifying $out.log | head -5; continue; fi
  grep -q "NOT LOWERED" $out.log && echo "seed $seed: some codecs not lowered: $(grep -c 'NOT LOWERED' $out.log)"
  node ../full/scopecheck.js $out/direct-opt.js $out/base-opt.js $out/direct-dev.js $out/base-dev.js $out/direct-leg.js $out/base-leg.js | grep -v "no unbound" 
  only=$(node -e 'console.log(require("./names.json").filter(n=>/^(IP|PModel|THolder|TPRec)/.test(n)).join(","))')
  echo "seed $seed: $(BUILD=$out ONLY=$only node fuzz.js $ITER $seed 2>&1 | grep -E 'DIVERGENCE|NO DIV|divergences:' | cut -c1-300)"
done
