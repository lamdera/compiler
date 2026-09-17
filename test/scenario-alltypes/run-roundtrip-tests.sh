#!/bin/bash

# Wire codec roundtrip fuzz tests.
#
# Generates random Elm type shapes (extensible record chains, alias chains,
# cross-module refs, overlapping fields, many-variant unions...), has the Lamdera
# compiler generate their Wire codecs, then asserts decode(encode(x)) == Just x
# over fuzzed values of each type.
#
# This complements the golden-file codec expectations in Test.Wire: those pin the
# exact generated codec for a hand-curated corpus, this checks the actual roundtrip
# property over shapes nobody thought to write down.
#
# SEEDS may list several seeds; each is generated and run in turn, so one CI run
# covers more of the shape space while staying fully deterministic.

SEEDS=${SEEDS:-${SEED:-42}}
COUNT=${COUNT:-5}
COMPILER=${COMPILER:-lamdera}

echo "Wire codec roundtrip fuzz tests"
echo "  seeds=$SEEDS  modules=$COUNT  compiler=$COMPILER"
echo ""

# Ensure deps and codegen bindings are present
if [ ! -d node_modules ]; then
    echo "Installing dependencies..."
    npm install
fi
if [ ! -d codegen/Gen ]; then
    echo "Generating elm-codegen bindings..."
    npx elm-codegen install
fi

failed_seeds=""

for seed in $SEEDS; do
    echo "=============================================================="
    echo "seed=$seed"
    echo "=============================================================="

    echo "Phase 1: Generating types and tests..."
    rm -rf tests/Generated tests/WireRoundtripTests.elm
    if ! npm run generate -- --seed "$seed" --count "$COUNT" 2>&1; then
        echo ""
        echo "GENERATION FAILED (seed=$seed)"
        failed_seeds="$failed_seeds $seed"
        continue
    fi

    type_count=$(grep -c "test_" tests/WireRoundtripTests.elm 2>/dev/null || echo 0)
    echo "Generated $COUNT modules with $type_count tests"
    echo ""

    echo "Phase 2: Running fuzz tests..."
    if npx elm-test --compiler="$COMPILER" 2>&1; then
        echo ""
        echo "PASSED (seed=$seed)"
    else
        echo ""
        echo "FAILED (seed=$seed)"
        failed_seeds="$failed_seeds $seed"
    fi
    echo ""
done

if [ -n "$failed_seeds" ]; then
    echo "=============================================================="
    echo "FAILED seeds:$failed_seeds"
    echo ""
    echo "To reproduce a failure:"
    for seed in $failed_seeds; do
        echo "  cd $(pwd)"
        echo "  SEED=$seed COUNT=$COUNT COMPILER=$COMPILER bash run-roundtrip-tests.sh"
    done
    echo ""
    echo "Generated files for the last seed are in tests/ for inspection."
    exit 1
fi

echo "ALL PASSED (seeds=$SEEDS, modules=$COUNT)"
