# Where we differ from upstream Elm

Places where Lamdera changes how upstream's own code behaves: bugs we fix ahead of
upstream, and behaviour we keep on purpose. Lamdera's additions (`extra/`, Wire3,
Evergreen, `lamdera live`) are not listed.

Each change is in its own commit, and the code carries an `@LAMDERA` comment saying why.

Upstream as of 2026-11-06: the `0.19.4` branch of elm/compiler is still at the
`0.19.4-beta` tag (0b0d3f8d), and `main` is at 0.19.3. None of the bugs below is fixed
upstream yet.


## At each upstream release

For each entry below:

1. Merge the release.
2. Check whether upstream changed the code our fix touches. If it did, read their fix.
3. Take our fix out, build, and run `stack test --test-arguments Test.Upstream0194`.
   If the entry's case still passes, upstream's fix covers it: drop ours and update this
   file. If it fails, keep ours and tell upstream if they don't know.

Entries marked "no test" need checking by hand, as described in the entry.

Then run the community Elm test suite with `test/elm-test-suite.sh` (add `--compare
elm-0.19.x` for the release, once it is on npm, to see where we differ from it), and update
`test/elm-test-suite.expected`.


## Elm 0.19.4-beta bugs we fix

| Bug | Our commit | Upstream knows | Test case |
|-----|------------|----------------|-----------|
| Recursive top-level functions are missing from the JS output, `List.foldl` included (`optRecDefs` folds over `defs`, not `def : defs`) | 5906932a | Reported by someone else, with this fix | recursive top-level functions are in the JS output |
| A port module can't expose its ports (`toInterface` leaves the ports out) | 6662f240 | Reported by someone else, no cause given | a port module can expose its ports |
| AMBIGUOUS NAME for a type exposed by two imports, so elm-geometry 3.11.0 and 4.0.0 don't build. We bring back 0.19.1's rule for types exposed by name: the later import wins. Possibly intended upstream | a2761c82 | Reported by someone else (the `Json.Decode.Value` / `Json.Encode.Value` case) | a type exposed by two imports...; Json.Decode.Value and Json.Encode.Value both exposed |
| A `.elmi` with an unannotated record function reads back corrupt, or the read hangs (`eOrder` writes no order for unordered fields) | aaba6a09 | Reported by someone else, cause given, no fix | an unannotated record function's .elmi reads back... |
| `ident x = x` is exported as `a -> b` (`getFlex` stores the unnamed variable back) | 18f564dd | Being reported by us (Elm Slack) | an unannotated identity function is exported as a -> a |
| `Map.!` crash on unannotated mutually recursive functions (`toUniques` never clears its marks) | 18f564dd | Being reported by us (Elm Slack) | unannotated mutually recursive functions |
| `initState` writes 16 bytes into a 5 byte array. It can crash the GC when the array ends a nursery block | 18f564dd | Being reported by us (Elm Slack) | No test: an Elm build can't show it. Build `Type/Type.hs` with `-fcheck-prim-bounds` and `initState` NOINLINE: compiling `ident x = x` must not stop with "out of bounds array access" |
| Two extensible records that each have a field the other lacks: INFINITE TYPE, or an Int/String mismatch let through (`unifyExtExt` unifies each row with its own record's extra fields) | ad2e9e83 | Reported by someone else, cause suspected, no fix | two extensible records...; the same field used as an Int and a String... |
| An alias of a function type in an annotation gives the arguments the alias's own variables: wrong TYPE MISMATCH, or a bus error (`canTypedArgs` doesn't fill in the alias's arguments) | a35403ea | Reported by someone else (both symptoms), cause suspected | an alias of a function type in an annotation |
| Crash in the hint for 2 or 3 unexpected record fields (`problemToHint` lists `expected`, which is empty) | 9ccf2406 | Reported by someone else, unsure of the fix | the hint for two unexpected record fields |
| Segfault or bus error type checking deeply nested lets (20 is enough) (`nearestSquare` returns the bit length, not the next power of two) | see `Type/Pools.hs` | Reported by someone else, with this fix | deeply nested lets |
| `_ = ...` let bindings run in reverse source order, so `Debug.log` lines in a let print bottom to top (`toNodes` numbers destructures first to last, and `Graph.toSCC` emits unconnected nodes highest name first). We number them last to first, so destructures (`_`, tuples, records) run in source order among themselves, as `_` ones did up to 0.19.3 | see `Canonicalize/Expression.hs` | Not reported | let destructures run in source order; and elm-test-suite's elm/DebugLogOrderingTest |
| `()` is comparable (`unifyFlexWithStructure` merges `Unit1` with a comparable flex). We reject it, as 0.19.3 did | see `Type/Unify.hs` | Reported by someone else | () is not comparable |
| Docs for an extensible alias given an extensible record (`type alias B a = A { a \| x : Int }`, `A` itself `{ a \| ... }`) never finish and use memory until the machine runs out. Docs are built for every dependency, so any app depending on such a package does too (canceraiddev/elm-form-builder: 73GB). `extend` in `I.dealias` looked an already substituted record's variable up again. We do what `Type.Type`'s `goExtRec` does | see `Elm/Interface.hs` | Reported by someone else (as a `--docs` hang) | docs for an extensible alias given an extensible record |
| Type variable indexes past 254 wrap in `.elmi` files: `E.array8` escapes the count but the indexes are written as a bare byte. Plain Elm needs over 255 type variables in one annotation to reach it, Lamdera's generated Wire3 codecs reach 510 | 727dcb9a | Not reported | No test. The commit message describes the case |


## Older upstream bugs we fix

| Bug | Our commit | Upstream knows | Check |
|-----|------------|----------------|-------|
| Unifying two records with the same flexible row tail and fields of their own on both sides never finishes (0.19.1 too). We report a TYPE MISMATCH, as a rigid row tail already did. Wire3's codecs for such types ran `lamdera make` out of memory | c4e3909d | Not reported | The repro in the commit message must be a TYPE MISMATCH within a second |


## Behaviour we keep on purpose

| What | Our commit | Why |
|------|------------|-----|
| Applications at any Elm 0.19.x up to the compiler's own version build. Upstream 0.19.4 accepts only 0.19.4 | c7ecddec | Existing apps and packages keep building without editing elm.json. Test.MultiVersion |
| The type checker and builder performance work (deferred alias bodies, closed types outside the pools, the interface type table, the build scheduler, and so on) | from 1e5b0301 on | Large Lamdera apps compile much faster. It changes upstream's `Type/`, `Build`, and the `.elmi` format, so every upstream merge in those files needs care. Test.CompilePerf and Test.ElmiTypeTable cover it, and `.elmi`/`.elmo` should match the previous release byte for byte on real apps |


## Not yet fixed by us

Still present in 1.6.0. We leave these to upstream, unless an app turns out to need one
before then. Reported upstream by someone else, unless the entry says otherwise:

- A module's name isn't checked against its file path.
- Two ports with the same name, or a port and a value with the same name, compile.
- A record alias with no fields has no constructor (0.19.1 had one).
- Errors print `List.List Int` instead of `List Int`.
- BAD TYPE ANNOTATION counts one too many arguments ("3 arguments" for `f x y`).
