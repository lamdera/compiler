# Adversarial review: direct JS Wire3 codecs (C2)

**Latest verdict (54c72486): no divergences found; see "Re-review of 54c72486".**

**Update (465a60cb):** see "Re-review of 465a60cb" below.
- Findings 1-3 are fixed.
- Finding 4 is now behind `_W3_STRICT_BYTES`, which I verified restores exact elm/bytes behaviour.
- **One new high-severity bug:** parameterised types containing a closed container argument fail to decode, with `ReferenceError: d0 is not defined`.

Reviewer's differential tests, run against the Elm combinator codecs of the same compiler
(flag off = "Elm", `LAMDERA_WIRE3_DIRECT=1` = "direct"). Results are against commit
a5830545 ("Stop the direct Wire3 runtime pinning decoded inputs"), which is the branch head at the time of the final runs (see "Commit and build" at the end).

Bottom line: I found no divergence in the bytes produced, in the decoded values, or in
which inputs fail, for valid or invalid input, across every shape I could think of, in
dev, `--optimize` and `--optimize-legible`, on Node 24 and 22, with and without
`Buffer`/`TextDecoder`/`TextEncoder`. That covers several million decode comparisons, with
consumed offsets compared too. I did find five other things, in order of severity:

1. **The direct decoder can overflow the stack at a SMALLER depth than the Elm decoder.**
   This is a restore-failure risk. The design doc says the opposite.
2. **The compiler binary could silently embed a stale `direct-runtime.js`.** This is now
   fixed in a5830545 (`addDependentFile`). Results from binaries built before that commit
   may have tested an older runtime.
3. **Signalling-NaN payloads in `Dict`/`Set` keys or `Dict` values come out quieted**, so
   re-encoding changes one bit.
4. **Decoded `Bytes` are copies, and this is observable.** Elm code that reads past the
   end of a decoded `Bytes` with `Bytes.Decode.bytes n` now gets `Nothing`, where it used
   to get `Just`.
5. Aside: two limitations that already exist in Wire3 and also affect the released
   `lamdera`, so they are not caused by C2.

The two known issues the lead listed are not repeated here: the pinned `_W3_ru8a`/`_W3_ru8aFor`,
and no package replacements in the worktree.

---

## 1. The direct decoder is shallower than Elm for recursive types with many locals (medium)

**What.** Each direct decoder is one JS function per type. It holds a `var rN` for every
value that any branch of the union decodes: every ctor argument and every inlined
anonymous-record field. In V8's interpreter every `var` is a register in the frame, so
the frame of a recursive `w3d_T` grows with the whole type's width. The Elm decoder uses
several small frames per level. Once a recursive type has more than about 50-60 locals,
the direct decoder runs out of stack at a smaller depth than the Elm one. So a value
that restores today can fail to restore with direct: `bytesDecode` returns `Nothing` on
the RangeError, and the snapshot is rejected.

**Repro** (`review/depth.js`, cold: a fresh process per probe, the way a restore runs;
default Node stack; types are in `review/gen.py`):

| Type (recursive position) | Locals in `w3d_T` | Node 24 opt: Elm / direct max depth | Node 22 opt: Elm / direct |
| --- | --- | --- | --- |
| `Fat3 = Fat3 { c001..c200 : Int, next : Fat3 } \| Fat3End` | 203 | 1680 / **552** | 1472 / **488** |
| `Big` (300 ctors, u16 tags, recursion via `BigK251 String (Maybe Big)`) | 188 | 1280 / **592** | 592 / **520** |
| `Fat = Fat { a01..a60 : Int, next : Fat } \| FatEnd` | 63 | 1680 / **1600** | 1559 / **1503** (warm) |
| `Hist` (10-field record + next) | 19 | 2592 / 3296 | 2240 / 2912 |
| `Tree`, `Mixed`, `Nest`, `Fat2` (small frames) | 3-43 | direct 3x to 30x deeper | same |

The `--optimize-legible` and dev results are the same as opt (Fat3 1680 / 552, Big 1296 / 592).

Example bytes for `Fat3` at depth n: `([0x00] ++ 200 x [0x00]) * n ++ [0x01]`. Depth 600
decodes to `Just` with the Elm codecs and to `Nothing` with direct.

Encoding is always deeper with direct (`review/encdepth.js`: Fat3 3504 / 10368, Big
2112 / 8832). So **direct can now write a snapshot it cannot read back**: for Fat3,
depths 553 to about 10000 encode but don't decode. This gap already exists with the Elm
codecs (3504 vs 1680), but direct makes it much wider.

**Real exposure.** None of the four real apps has a recursive type with a direct decoder:
I scanned `direct-builds/*/on` and found 0 decoders on a call cycle. So this is latent. It
matters for linked-list or AST-like union types with many constructors or wide inline
records, at depths in the hundreds.

**Fix.** Keep the frame of a recursive decoder small, and add a cold-depth oracle, "direct
decode depth >= Elm decode depth" for shapes like the ones above, to `full/check.js`. Some
options:
- Decode each union branch in a block with `let` (Ignition reuses registers across
  sibling blocks), or hoist each non-trivial branch into its own function. The frame then
  grows with the widest branch, not the sum of all branches. This fixes `Big`.
- For records, decode straight into the object literal (`{ a: _W3_rint(), b: _W3_rstr() }`).
  Wire order equals Elm's literal key order (both are the sorted field names). Only fields
  that need statements (Maybe/List/Dict/union) would go through small hoisted helpers.
  This fixes `Fat3`.
- As belt and braces, run the restore decode with a bigger stack. That changes behaviour
  in the "more tolerant" direction only.

## 2. `direct-runtime.js` edits didn't trigger a rebuild, so a stale runtime shipped (fixed in a5830545; no wire impact)

`Direct.hs` embeds the runtime with `$(bsToExp =<< runIO (Lamdera.Relative.readByteString
".../direct-runtime.js"))`. It never calls `addDependentFile`, so neither stack nor GHC
knows about the dependency. GHC also recompiles on source hashes, so a `touch` doesn't help.

Evidence: the installed binary was built at 23:13, and 1a30c912 (which changes only
`direct-runtime.js` in `extra/`) was committed at 23:20. The binary did not contain the
string "hand the grown buffer", but did contain the old `var out = _W3_buf.slice(0,
_W3_pos)`. `stack build` said it was up to date. Only deleting
`.stack-work/.../Lamdera/Wire3/Direct.{hi,o,dyn_hi,dyn_o}` rebuilt it. Any test or
benchmark run after 1a30c912 with that binary exercised the OLD runtime.

(Building also rewrites the tracked `extra/dist/repl-worker.js`. I restored it with
`git checkout`.)

**Status.** Fixed in a5830545, which adds `addDependentFile` in the splice. I checked that
the rebuilt binary embeds a5830545's runtime strings. Numbers the author measured between
1a30c912 and a5830545 with a binary built before 1a30c912 may still reflect the old
runtime, and are worth re-running.

## 3. Signalling NaNs are quieted in Dict/Set keys and Dict values (low: needs hand-crafted input)

**What.** `WDict`/`WSet` decoding collects keys and values in JS arrays (`ks.push`,
`vs.push`) before `_W3_dictFromPairs`. When an array holds only numbers, V8 stores them
in an unboxed double backing store, and that store silences NaNs: it sets the quiet bit
of a signalling NaN and keeps the payload. The Elm decoder keeps values in tuple and cons
objects, which preserve the bits. Nothing in Elm can see a NaN payload except
re-encoding, so the effect is that `encode (decode bytes) /= bytes` for such inputs.
With the Elm codecs it is the identity.

**Repro** (`fuzz.js` reports it as "sNaN quieted by direct decode"; every mode, both Node
versions):

| Type | Input bytes | Elm decode, then encode (either encoder) | Direct decode, then encode (either encoder) |
| --- | --- | --- | --- |
| `Set Float` | `02 0100000000 00f07f` | `02010000000000f07f` | `02010000000000f87f` |
| `Dict Int Float` | `02 00 edceca7f0c69f0ff` | `0200edceca7f0c69f0ff` | `0200edceca7f0c69f8ff` |

The same happens for `Dict Float _` keys and for values of any `Dict k Float`, e.g.
`Dict (Int, String) Float`. `List`, `Array`, `SeqDict`, records and ctor arguments are
not affected: `Array` quiets in the Elm codec too (`JsArray` storage), and the others
never pass through a numbers-only array.

**Real exposure.** JS arithmetic never produces a signalling NaN. One can only get in by
decoding raw bytes, through elm/bytes `float64` or a hand-crafted Wire3 message or
snapshot. So this is essentially exotic input only. It does break the "decode then
encode is byte-identical" property that the snapshot oracles rely on.

**Fix.** Don't push raw doubles into a fresh array:
- keep generic elements (`var ks = [null]; ks.length = 0;`, which is V8-specific), or
- collect `_Utils_Tuple2(k, v)` pairs (objects keep the bits), or
- build from a cons list.

Also add an sNaN case to the oracles.

## 4. Decoded `Bytes` are copies, and that is observable (low)

**What.** elm/bytes `_Bytes_read_bytes` returns `new DataView(input.buffer, offset, len)`.
Later `Bytes.Decode.bytes n` reads on that value are bounds-checked against the whole
underlying buffer, not the view. So Elm code can read past the end of a decoded `Bytes`
into the rest of the input, which is an elm/bytes quirk. `_W3_rbytes` copies, so the same
read now fails. The doc says "the copy is invisible". It isn't, although only
out-of-bounds reads can tell.

**Repro** (`targeted.js`, section 2; every mode). Type `TBytes = Bytes`. Input
`06 010203 09090909090909090909` (a 3-byte `Bytes` followed by 10 more bytes in the same
buffer). Then Elm code runs `Bytes.Decode.decode (Bytes.Decode.bytes n) decoded`:

| n | Elm | direct |
| --- | --- | --- |
| 3 | `Just` (3 bytes) | `Just` |
| 4, 10, 13 | `Just` (reads the trailing input bytes) | **`Nothing`** |
| 14 | `Nothing` | `Nothing` |

**Real exposure.** This needs app code that reads past the end of its own `Bytes`, for
example a nested Wire3 or elm/bytes decode of a stored `Bytes` whose length prefix is
corrupt. Elm "succeeds" there with garbage from the neighbouring message or snapshot, and
direct fails cleanly. Arguably an improvement, but it is a behaviour change.

**Fix.** Keep the copy (the memory win is real) and correct the doc and the "semantics
frozen" claim. Or, if strict identity is required, return a view over a copy of
`[offset, end of input)`. That keeps both the bounds and the result, but it defeats the
purpose.

## 5. Aside: limitations that already exist in Wire3 (not C2; the released `lamdera` behaves the same)

- A non-regular recursive type, e.g. `type NonReg a = NonRegEnd a | NonReg a (NonReg (List a))`:
  the generated `w3_encode_NonReg` fails to type-check ("needs the 1st argument to be
  `a -> Encoder`").
- Mutually recursive parameterised types through a list, e.g. `type MA x = MA x (Maybe (MB x))`
  and `type MB x = MB (List (MA x)) (Dict Int x)` with `type alias TMA = MA (Result String Float)`:
  the compiler crashes with `Map.!: given key is not an element in the map`. The same
  happens with `~/.local/bin/lamdera` on a fresh ELM_HOME.

---

## Re-review of 465a60cb

Setup:
- Compiler rebuilt at 465a60cb. The binary embeds `W3_RUNTIME_VERSION 2026-09-30.1` and
  the new `_W3_rbool`.
- A fresh ELM_HOME with all package artifacts deleted, so every package is rebuilt with
  IR and with the package replacements (now real submodules). The Lamdera elm/core is in
  the bundles, which I checked by finding the `SeqDict_elm_builtin` equality in `_Utils_eq`.
- New types to attack the expression-based decoder rendering:
  - `Order1`: fields `zz, aa, mm, b, a10, a2, a1, aB, ab, aZ`, declared in an order
    unrelated to how they sort, with mixed case and digits;
  - `TOrderedTuple`: a tuple of records containing a nested inline record and a list of
    records;
  - `CtorOrder`: ctor args made of an inline record, a tuple, a list of inline records,
    and a record alias;
  - `TExtOrder = Ext { zed, ant }`: an extensible record;
  - `THolder = Holder Bool` and `TPRec = PRec Char`: parameterised types whose fields are
    closed containers passed to other parameterised types.

### Earlier findings

1. **Depth: fixed.** Cold decode depth (`depth.js`), Elm / direct:

   | Type | Node 24 opt | Node 22 opt |
   | --- | --- | --- |
   | Fat3 | 1696 / 8448 | 1472 / 7488 |
   | Big | 1296 / 4544 | 592 / 4032 |
   | Fat | 1680 / 8448 | 1424 / 7488 |
   | Hist | 2592 / 7424 | 2208 / 6528 |
   | Nest | 480 / 3968 | 388 / 3488 |
   | Tree | 712 / 8448 | 584 / 7488 |
   | Mixed | 768 / 4736 | 624 / 4224 |
   | Fat2 | 73 / 8448 | 64 / 7488 |

   dev and `--optimize-legible` give the same numbers within noise.

   Both claims hold for every shape:
   - **direct decode depth >= Elm decode depth**;
   - **direct decode depth >= Elm encode depth** (`encdepth.js`). Elm encode maxima:
     Fat 3440, Fat3 3504, Hist 5728, Big 2160, Mixed 1728, Tree 3424, Nest 968,
     Fat2 1568 (worst of dev/opt on Node 24; Node 22 is lower).

   Residual, not a regression: direct *encode* can still go deeper than direct decode
   (for example Fat3 10368 vs 8448, Nest 9216 vs 3968). So a value between those depths
   could be written but not restored. Elm has the same property today, at much lower depths.
2. **Stale embedded runtime: fixed.** `addDependentFile` is in the splice, and the
   binary contains the current `W3_RUNTIME_VERSION`.
3. **sNaN quieting: fixed.** `Set Float` `02010000000000f07f` and `Dict Int Float`
   `0200edceca7f0c69f0ff` now re-encode byte-identically in opt, dev and leg. The byte
   fuzzer reports no sNaN divergences.
4. **Decoded Bytes copy: default unchanged (documented), and the strict switch works.**
   `strictbytes.js` sets `_W3_STRICT_BYTES = true` in the direct bundle. Reading 0..40
   bytes past the end of a decoded `Bytes` then matches elm/bytes exactly in opt, dev and
   leg, including:
   - a plain input;
   - an input that is a view at offset 5 inside a 40-byte buffer (reads reach the
     buffer's tail, as in Elm);
   - a zero-length `Bytes`;
   - re-encoding.

   A control run with the switch off shows 129 divergences, so the test does detect the
   difference.

### NEW: parameterised types can fail to decode at all, with `ReferenceError: d0 is not defined` (high)

**What.** The new renderer puts statement-shaped decoding (List, Maybe, Result, Dict,
Set) into hoisted helpers `w3dh_T_n(d0, ...)`, and every helper call passes all of the
codec's decoder parameters. The problem is a *closed* codec argument (one containing no
type variable) passed to another parameterised type. `decFnExpr` hoists it to module
level as `var w3da_T_n = function () { ... }` (on `isClosed w`). But when its expression
contains a helper call, that call still passes `d0`, which isn't in scope at module
level. Generated code (`--optimize`):

```js
var $author$project$Types$w3da_Holder_1 = function () { return $author$project$Types$w3dh_Holder_0(d0); };
...
case 0: return $author$project$Types$Holder$(d0(), $author$project$Other$w3d_Box($author$project$Types$w3da_Holder_1), ...
```

**Effect.** Every value that reaches such an argument fails to decode: the ReferenceError
is caught by `bytesDecode` and becomes `Nothing`. The same happens in wire messages and on
restore. For a `BackendModel` this means the snapshot is rejected, i.e. data loss on
deploy. It happens in dev, opt and leg, with or without Buffer. Encoders are not affected.

**Trigger.** The type must have a type variable, and contain a closed List, Maybe,
Result, Dict, Set, Array or SeqDict (or a Pair/Record containing one) passed as an
argument to another parameterised type. For example
`type alias Paginated a = { items : List a, pages : Box (List Int) }`, or
`Holder a = Holder a (Box (List Int)) ...`.

**Repro.**

```elm
type Box a = Box a                      -- Other.elm
type alias PRec a = { zeta : a, alpha : Box (Dict String Int), mid : List a, bB : Box ( a, Maybe Int ) }
type alias TPRec = PRec Char
```

Bytes `00 00 00 00 00 00 00` (alpha = Box empty Dict, bB = Box ('\0', Nothing),
mid = [], zeta = '\0'): Elm gives `Just` (7 bytes consumed), direct gives `Nothing`
(`ReferenceError: d0 is not defined`). `THolder = Holder Bool`: bytes `0000000000000000`
give Elm `Just` and direct `Nothing`. `fuzz.js` shows it on every seed:
"THolder/TPRec decode outcome ... direct=Nothing (ReferenceError)".

**Real apps.** I rebuilt the four real apps at 465a60cb (in a scratch copy, with
`--optimize-legible`) and grepped for module-level `w3da_` closures that reference a
`dN` parameter: there are none. So these four aren't affected. The pattern is ordinary,
though, and the current oracles didn't catch it.

**Fix.** A hoisted closed closure must not pass the enclosing codec's parameters. Either:
- give helpers created under a closed `decFnExpr` an empty parameter list (their body
  can't reference a tvar), or
- don't hoist when the expression contains a helper call, or
- hoist the helper with the parameters it actually uses.

Add `PRec`/`Holder`-like types to `full/`.

### Everything else re-run at 465a60cb: held

- `fuzz.js`, 82 types:
  - Node 24, seeds 501/502 at 2000, dev/opt/leg;
  - Node 22, seed 503;
  - `NO_BUFFER=1`, seed 504;
  - `NO_BUFFER=1 NO_TEXTCODEC=1` on Node 22, seed 505;
  - seed 511 with every type except THolder/TPRec: `NO DIVERGENCES`.

  The only divergences are THolder and TPRec (above).
- Wire-order evaluation: `Order1`, `TOrderedTuple`, `CtorOrder`, `TExtOrder`, `Person`,
  `TNamedField`, `Nested`, `Reserved`, `TAll`, `Multi`, `Mixed` and the Big union all
  match Elm exactly: values with record key order, offsets, and failure points under
  truncation and mutation. JS evaluates object-literal values and call arguments left to
  right, and the IR's field order is the wire order, which is also Elm's literal key order.
- `strings.js`: Node 24 (150k), Node 22 (80k), `NO_BUFFER` (80k), and
  `NO_BUFFER NO_TEXTCODEC` on Node 22 (80k), dev/opt/leg: no divergences.
- `dictsizes.js` opt/dev/leg: all 4071 sizes are valid LLRB with the right contents.
- `targeted.js` opt/dev/leg: the streaming state after a throwing or re-entrant `onChunk`
  is correct, and odd scalars are byte-identical. The only difference is the documented
  Bytes-copy behaviour with the switch off.

---

## Re-review of 54c72486

**Verdict: no divergences found at 54c72486.** The `d0` regression is fixed. Across
every mode, Node version and platform-feature combination, and across a randomised
generator of parameterised types, the direct codecs matched the Elm codecs in bytes,
values, failures and consumed offsets. The only intentional exception is the documented
Bytes-copy behaviour, and `_W3_STRICT_BYTES = true` removes it. The residuals are the
pre-existing limitations listed at the end of this section, which the released `lamdera`
also has.

Setup:
- Compiler rebuilt at 54c72486. The binary embeds `W3_RUNTIME_VERSION 2026-09-30.1`.
  54c72486 changes only `Direct.hs`, so the runtime marker is correctly unchanged.
- Fresh ELM_HOME with package IR and the package replacements.
- Every check in dev, `--optimize` and `--optimize-legible`.

### Attack on parameter threading

`gen.py` now also writes `Params.elm`, a random set of parameterised types chosen by
`PSEED`/`NPARAMS`, and `pfuzz.sh` builds, scopechecks and fuzzes each set. Each set
has 14-20 types with:
- 0-3 tvars, including phantom tvars and tvars used only in some ctors;
- ctor args mixing tvar-using and tvar-free List, Maybe, Dict, Set, Array, Result,
  tuple, triple and inline records, nested 1-3 deep;
- references to earlier generated types with their args partially closed and partially
  tvar, i.e. codec arguments that are themselves applied codecs, as in
  `Box (Two Int a)` and `P0 (List (Maybe (Box Float)))`;
- self-recursion through Maybe;
- cross-module parameterised types (`Other.Box/Two/Wrap`);
- every type instantiated at three different closed arguments, plus a `PModel` record
  holding all the instantiations of the same types at once.

`fuzz.js` seeds `PModel` by concatenating valid field encodings, so it decodes non-trivially.

Hand-written additions:
- `THolder`/`TPRec`, the regression repros;
- `MX/MY/MZ`: a 3-cycle mutually recursive parameterised group with closed containers
  (`Box (List Int)`, `List (Box Int)`, `Maybe (Box (Maybe String))`) inside the cycle,
  in an inline record;
- `MP a b` / `MQ a b`: a mutual pair that swaps its type arguments across the cycle
  (`MP a b` contains `MQ b a`).

Results:
- **PSEED 1-3, 5-10, 12-16** (14 sets, about 20 types and about 55 instantiations each),
  600 iterations per type per phase, dev/opt/leg: **NO DIVERGENCES**. PSEED 4 and 11
  don't compile, even with the flag off; see the limitations below.
- `THolder`, `TPRec`, `TMX`, `TMP` and `PModel`: 5000 iterations, dev/opt/leg, Node 24:
  **NO DIVERGENCES**. `TMX` and `TMP` decode to `Just` on about 23% of inputs, so the
  cycles are exercised. `TMX`/`TMP` 3000 iterations on Node 22, and with
  `NO_BUFFER=1 NO_TEXTCODEC=1`: **NO DIVERGENCES**.
- The generated helpers take exactly the parameters their own sub-type uses. In the `MX`
  group: `w3dh_MX_0(d0)`, `w3dh_MX_1()`, `w3dh_MY_1()`, `w3dh_MZ_1()`. The formerly
  broken hoisted closure is now `w3da_Holder_1 = function () { return w3dh_Holder_0(); }`.

### `scopecheck.js` sanity check

- Against my saved 465a60cb bundles it reports the real bug: `d0 (line 4076)` in opt and dev.
- Against mutants of those bundles it also flags:
  - a helper referencing a different missing parameter (`d3`);
  - a misspelt runtime function (`_W3_dictFromPair`);
  - a loop variable left undeclared (an implicit global `n`);
  - a closure referencing a parameter that doesn't exist where it is used (`d1`);
  - a `typeof`-guarded unknown global (`Deno`). This one is conservative, so only the
    feature-detected names in its allowlist are exempt.
- Against all 54c72486 bundles (review app, all 14 passing PSEED sets, all three modes)
  it reports **no unbound identifiers**.

Its limits, by design:
- It can't catch a reference that resolves to the *wrong* binding. That means a
  parameter that exists in the enclosing codec but is the wrong one; the differential
  fuzz covers that.
- It masks names that are unbound in the Elm-codec build too.
- It says nothing about initialisation order, e.g. a module-level `var` closure called
  before its assignment runs. Loading the bundles, as every fuzz run does, covers that.

### Full suite at 54c72486 (review app with PSEED=1)

- `fuzz.js`, 82+ types plus the generated ones:
  - Node 24, seeds 601/602 at 2000;
  - Node 22, seed 603;
  - `NO_BUFFER` on Node 24, seed 604;
  - `NO_BUFFER NO_TEXTCODEC` on Node 22, seed 605;
  - `NO_TEXTCODEC` on Node 24, seed 606.

  All dev/opt/leg: **NO DIVERGENCES**, including no sNaN reports.
- `strings.js`: Node 24 (150k), Node 22 (80k), `NO_BUFFER` (80k), and
  `NO_BUFFER NO_TEXTCODEC` on Node 22 (80k): no divergences.
- `dictsizes.js` opt/dev/leg, on Node 24 and 22: all 4071 sizes valid.
- `strictbytes.js` on Node 24 and 22: identical to elm/bytes.
- `targeted.js` opt/dev/leg, on Node 24 and 22: streaming state and odd scalars
  identical. The only difference is the documented Bytes copy.
- Depth, cold (`depth.js` / `encdepth.js`):

  | Type | Node 24 opt: Elm decode / Elm encode / direct decode | Node 22 opt: Elm decode / Elm encode / direct decode |
  | --- | --- | --- |
  | Fat3 | 1680 / 3504 / 8448 | 1472 / 3072 / 7488 |
  | Big | 1312 / 2096 / 4544 | 600 / 1304 / 4032 |
  | Hist | 2592 / 4704 / 7424 | 2208 / 5280 / 6528 |
  | Nest | 480 / 976 / 3968 | 388 / 756 / 3488 |
  | Mixed | 776 / 1744 / 4736 | 624 / 1304 / 4224 |
  | Tree | 720 / 3232 / 8448 | 584 / 2608 / 7488 |
  | Fat2 | 73 / 1592 / 8448 | 64 / 1240 / 7488 |
  | Fat | 1696 / 3232 / 8448 | 1424 / 3072 / 7488 |

  dev and leg are the same within noise; the largest Elm encode depth in any mode is
  Hist 5888 (dev), still below direct decode 7424. **Both claims hold everywhere: direct
  decode depth >= Elm decode depth, and >= Elm encode depth**, so anything the Elm codecs
  could write, direct can read back. The residual from the 465a60cb section is unchanged:
  direct encode can still go deeper than direct decode.

### Existing Wire3 limitations found on the way (not C2; the released `lamdera` also fails)

These are in addition to non-regular recursion and the `Map.!` crash listed earlier.

- **Mutually recursive parameterised types can only be used at one instantiation.**
  Minimal repro:

  ```elm
  type A a = A0 a | A1 (List (B a))
  type B a = B0 a | B1 (List (A a))
  type alias X = A Int
  type alias Y = A String
  ```

  This gives `TYPE MISMATCH` in the generated `w3_decode_A`/`w3_encode_A`, with both
  compilers. It also happens with `type C = C (A Char) (B Float)`, and it is why random
  sets PSEED 4 and 11 don't compile. The same group used at a single instantiation
  compiles, and its direct codecs match.
- The generator must avoid `List ()`-like zero-width elements when fuzzing with random
  lengths, or both builds run out of memory building a list of about 2^32 units. This is
  harmless, and the same in both builds.

---

## What I tested that held (at a5830545)

All of these are differential (Elm codecs vs direct, same compiler, same mode), in dev,
`--optimize` and `--optimize-legible` unless noted. Every decode compares:
- the outcome (`Just`/`Nothing`, including any JS exception, as `bytesDecode` does);
- the **consumed offset** (strict vs non-strict decode, and trailing bytes);
- the value, with a NaN-aware and -0-aware comparison that respects record key order
  (hidden classes and SeqDict FNV hashing depend on key order), compares Dicts by
  contents, and compares Bytes by contents.

Every successful decode also checks that:
- both encoders produce identical bytes on both the Elm-decoded and the direct-decoded value;
- the streamed encode (random chunk sizes 1-20) is byte-identical;
- every Dict the direct decoder built satisfies elm/core's left-leaning red-black
  invariants, unless the Elm-built one fails them too (NaN keys).

Coverage:

- **Byte fuzzing** (`fuzz.js`). 80 types, including every builtin, container, key kind and
  union shape. Inputs:
  - random bytes biased toward tag and varint boundaries (0-5, 107/108, 215/216,
    250-255), UTF-8 lead bytes and zero-tag prefixes;
  - mutations of valid encodings: flip, insert, delete, truncate, splice two encodings,
    duplicate a span, rewrite a small varint in a non-canonical 252/253/254 form, rewrite
    a byte as a 255 float form holding NaN, ±Infinity, -0, 0.5, 2^53+2 or subnormals;
  - 20% of inputs placed at an offset inside a larger buffer.

  Seeds 1-7, 11-13, 21-22, 31-32, 41-42, 51, 61, 71, 101-107 and 201, at 300 to 20000
  iterations per type and phase. The only divergences are the sNaN ones (section 3).
  Also run on Node 22.21.1, with `NO_BUFFER=1`, and with `NO_BUFFER=1 NO_TEXTCODEC=1`.
- **Decode failure semantics that held**:
  - bad union tag, out-of-range tag, u8 vs u16 tags (255, 256 and 300 ctors; 300 with args);
  - Bool/Maybe/Result/Order tags ≥ 2 or 3;
  - Char from out-of-range, negative, NaN-free 255-form and surrogate codes;
  - varints of every width, non-canonical long forms, 255 with NaN/Infinity (fails), -0
    (stays -0), and fractional values (floored);
  - reading past the end at every position;
  - string lengths past the end, and negative lengths (`""`, offset unchanged);
  - negative list lengths (empty);
  - negative `Bytes` lengths (fail);
  - `Never`/`failDecode`, and `Maybe Never` / `Result Never Int` / `List Never`.
- **Dict/Set from non-sorted and duplicate keys** of type Int, Float (NaN, -0), String
  (including astral and surrogate order), Char, (Int, String), (Float, String),
  (Int, Int, Int) and List: identical to `fromList`, last value wins.
  `dictsizes.js` checks every fast-path size 0..4000, plus 2^k±2 up to 2^20 and 3^k±2 up
  to 3^12: all valid LLRB with the right contents.
- **Strings** (`strings.js`, 200k+ per mode): well-formed text, BOM, literal U+FFFD,
  CESU-8 and lone surrogates, overlong, out of range (F4 90 / F8 / FE / FF), stray
  continuations, truncated sequences, length prefixes off by -3..+3, and inputs at
  offsets. The encoder is fed random UTF-16 including lone surrogates at lengths 0-70,
  which crosses the 4/8/32-char fast-path thresholds. All identical, including the
  offset after truncated sequences and all browser paths.
- **Encode of odd values**:
  - Ints 1.5, -1.5, 0.3, 215.5, 9431.5, 65535.5, 4294967295.5, NaN, ±Infinity, -0,
    ±(2^53+2), -(2^31)-1, 1e300 and 5e-324;
  - strings with lone surrogates at start, middle and end, around the thresholds;
  - Chars `'\uD800'`, `''`, `'ab'` and astral.

  Tested at top level in every mode, and injected into decoded values of every type
  (dev), or of every type with no enum, Array or SeqDict internals (prod). All
  byte-identical, and the same exceptions for garbage (`null` string gives a TypeError in both).
- **Shapes**:
  - records with JS-reserved field names (`class`, `default`, `new`, `this`, `var`,
    `delete`, `function`, `return`, `null`, `a`, `b`), which the legible mode mangles to
    `_class`;
  - extensible records (`Named { age : Int }`, `Ext { w, a }`, fields of extensible type);
  - nested anonymous records, unbox-of-unbox (`Wrapped (Id String)`), single ctor with
    several args;
  - ctors declared in non-alphabetical order;
  - recursive (`Tree`, `Mixed`), mutually recursive (`Forest`/`TreeB`), and recursive
    across modules through a type parameter (`Thing = Thing (Other.Rose Thing)`);
  - parameterised types from another module applied at depth
    (`Box (Box (List (Maybe Int)))`, `Two String (Two Int Bool)`, `Swap a b = Two b (Maybe a)`,
    `Box (Deep3 (Wrap Bool))`), and phantoms;
  - `Time.Posix`, `Random.Seed`;
  - `SeqDict` with record, `Dict` and `Float` keys (FNV hashes depend on record key
    order, which matches), `SeqSet (Maybe Int)`;
  - function fields (`failEncode`: nothing written, decode fails).
- **Bridges**: I built against package artifacts compiled by the released `lamdera`, so
  there is no IR and `Random.Seed` is bridged in both directions. Fuzzed with `TSeed`,
  `TAll`, `TPosix` and `TSeqDict`, including streaming. Identical.
- **Streaming** (`targeted.js`):
  - an `onChunk` that throws on the 2nd chunk leaves the writer usable, and the next
    encode is correct;
  - an `onChunk` that re-enters (runs another encode during the callback) gets a correct
    inner result, and the outer stream is still byte-identical;
  - an encode that throws mid-way leaves the writer usable;
  - chunks are never empty.
- **Direct-call naming**: `makeArgLookup` runs on the modified graph, so no Elm caller
  emits a `w3_*$(...)` direct call to a replaced codec.
- **Package replacements**: the Lamdera elm/core replacement only changes
  SeqDict/SeqSet equality, so these results carry over to the stock-core bundles.
- **The author's `full/check.js`**, rebuilt with the fresh binary: all 272 checks pass.

## How to run

```sh
cd test/wire3-direct-proto/review
cp -r ../lamdera-wire3-bench/elm-home-direct /some/scratch/elm-home   # never the shared ~/.elm
ELM_HOME=/some/scratch/elm-home sh build.sh            # base/direct x opt/dev/leg into build/
node fuzz.js 2000 <seed> [opt|dev|leg ...]             # NO_BUFFER=1, NO_TEXTCODEC=1; ONLY=T1,T2; BUILD=dir
node strings.js 200000 <seed>
node targeted.js opt|dev|leg                            # Bytes view, streaming state, odd scalars
node dictsizes.js opt|dev
node depth.js opt|dev|leg [Type ...]                    # cold decode depth (fresh process per probe)
node encdepth.js opt|dev [Type ...]                     # cold encode depth
node strictbytes.js                                     # _W3_STRICT_BYTES = true vs elm/bytes
ELM_HOME=... sh pfuzz.sh "1 2 3" 600                  # random parameterised type sets (PSEED), scopecheck + fuzz
```

`fuzz.js` exits non-zero on the known sNaN divergence. Filter it with
`grep -v "sNaN quieted"`.

## Commit and build

- Compiler: `~/dev/projects/lamdera-compiler-wire3` at a5830545, `stack build --fast`. I
  verified that the binary embeds that commit's runtime by grepping it for "The outermost
  decode is over".
- ELM_HOME: a private copy of `../lamdera-wire3-bench/elm-home-direct`, with package IR
  built by this compiler. For the bridge build, a copy whose artifacts the released
  `lamdera` rebuilt.
- Final runs at a5830545:
  - `fuzz.js` seeds 301-305 (dev/opt/leg, Node 24; 303 on Node 22.21.1; 304 with
    `NO_BUFFER=1 NO_TEXTCODEC=1`): only sNaN divergences;
  - `strings.js` (100k, plus 50k with `NO_BUFFER`): none;
  - `dictsizes.js` opt/dev: pass;
  - `targeted.js leg`: only the Bytes-view divergence;
  - `depth.js`/`encdepth.js`: the table in section 1 is unchanged.

  Earlier seeds ran at 1a30c912, whose codec code a5830545 does not change.
