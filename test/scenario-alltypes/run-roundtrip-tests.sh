#!/bin/bash

SEED=${SEED:-42}
COUNT=${COUNT:-5}
COMPILER=${COMPILER:-lamdera}

echo "Wire codec roundtrip fuzz tests"
echo "  seed=$SEED  modules=$COUNT  compiler=$COMPILER"
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

echo "Phase 1: Generating types and tests..."
rm -rf tests/Generated tests/WireRoundtripTests.elm
if ! npm run generate -- --seed "$SEED" --count "$COUNT" 2>&1; then
    echo ""
    echo "GENERATION FAILED"
    echo "  seed=$SEED  modules=$COUNT"
    exit 1
fi

type_count=$(grep -c "test_" tests/WireRoundtripTests.elm 2>/dev/null || echo 0)
echo "Generated $COUNT modules with $type_count tests"
echo ""

echo "Phase 2: Running fuzz tests..."
if npx elm-test --compiler="$COMPILER" 2>&1; then
    echo ""
    echo "ALL PASSED (seed=$SEED, modules=$COUNT)"
else
    exit_code=$?
    echo ""
    echo "FAILED (seed=$SEED, modules=$COUNT, compiler=$COMPILER)"
    echo ""
    echo "To reproduce:"
    echo "  cd $(pwd)"
    echo "  SEED=$SEED COUNT=$COUNT COMPILER=$COMPILER bash run-roundtrip-tests.sh"
    echo ""
    echo "Generated files are in tests/Generated/ for inspection."
    exit $exit_code
fi
