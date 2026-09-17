# scenario-alltypes

The Wire codec test corpus. Two layers, checking different things:

**1. Golden codec expectations** (`src/Test/Wire_*.elm`, driven by `Test.Wire` in the
Haskell suite, runs under `stack test`). A hand-curated set of tricky type shapes;
each pins the exact codec the compiler should generate. Catches unintended codegen
changes.

**2. Roundtrip property fuzz** (`run-roundtrip-tests.sh`, local only). Generates
*random* type shapes, has the compiler generate their codecs, and asserts
`decode(encode(x)) == Just x` over fuzzed values. Catches shapes nobody thought to
add to the corpus - which is how most Wire bugs have historically been found
(parameterized alias chains, extensible records reached through alias arguments,
Holey aliases whose own parameters looked like free tvars).

Layer 2 is deliberately **not** in CI: it needs node plus a compiler binary and adds
several minutes per run. Run it locally when touching `Lamdera/Wire3/`.

## Running the roundtrip fuzz tests

```bash
# against whatever `lamdera` is on PATH
bash run-roundtrip-tests.sh

# against YOUR build of the compiler (what you usually want)
stack install elm:exe:lamdera
COMPILER="$HOME/.local/bin/lamdera" bash run-roundtrip-tests.sh

# wider sweep
SEEDS="1 7 42 99" COUNT=8 bash run-roundtrip-tests.sh
```

| var | default | meaning |
| --- | --- | --- |
| `SEEDS` | `42` | space-separated seeds; each generated and run in turn |
| `SEED` | - | single-seed alias for `SEEDS`, kept for reproduction lines |
| `COUNT` | `5` | modules generated per seed (more modules = more cross-module refs) |
| `COMPILER` | `lamdera` | compiler binary elm-test shells out to |

On failure the script prints the exact command to reproduce that seed, and leaves the
generated Elm in `tests/` for inspection.

## How it works

`script/src/Generate/` is an elm-pages script run via elm-codegen:

- `Types.elm` - the shape generator. 15 scenarios covering extensible record chains,
  two-level extensible bases, cross-module refs, overlapping fields, alias chains,
  many-variant unions, large records.
- `Fuzzers.elm` - a `Fuzz.Fuzzer` per generated type.
- `Tests.elm` - the `elm-test` module asserting the roundtrip property.

Generated output (`tests/`, `generate.mjs`, `codegen/Gen/`) is gitignored.

## Tool versions are pinned exactly

`package.json` pins `elm-pages`, `elm-codegen` and `elm-test` to exact versions, and
`script/elm.json` pins the `dillonkearns/elm-pages` **Elm** package. These must stay in
lockstep: elm-pages checks a `compatibilityKey` shared between its npm and Elm halves,
and a floating `^` range silently drifted them apart (npm key 29 vs Elm key 26), which
broke the generator with "Incompatible NPM and Elm package versions". Current pairing
is npm `3.3.2` with Elm `12.1.0` (both key 26). If you bump one, bump the other.
