# `lamdera live --memory`: an in-memory build for live

Branch `live-memory` (worktree `~/dev/projects/lamdera-compiler-live-memory`), on top of
`elmi-type-table` (1ab97db3) with `wire3-typed-decoders` (8844fef4) merged. Not pushed.


## Summary

- **What works.** `lamdera live --memory` keeps every project build artifact in memory as
  decoded values: per module the interface and the objects, plus the project `Details`
  with the package interfaces and objects resolved. After the first build no rebuild
  reads, writes, encodes or decodes anything under `elm-stuff/lamdera-*`, and the JS of
  definitions whose module did not change is reused instead of rendered again. Everything
  else is the normal live build (Wire3 injection, the post-compile and type hash checks,
  `Generate.dev`, the harness, the REPL endpoint). Default `lamdera live` and every other
  command are unchanged. On Ctrl-C what was compiled in memory is written to elm-stuff, so
  the next start, `lamdera make` or plain `lamdera live` is warm.
- **Correctness.** `Test.LiveMemory` builds two copies of a Lamdera app side by side, one in
  memory and one from disk, through a dozen edits (body change, interface change, an arity
  change behind an unchanged interface, a new constructor in Types with new Wire3 codecs,
  a type error and its fix, touching elm.json, flush, restart): byte identical JS and
  exactly the same set of recompiled modules after every step. On the large app every measured
  rebuild (88 of them, both modes, both compilers) gave the same JS sha1 at the same step.
  Full `stack test`: 141 passed, 7 pending (the pending count elmi-type-table had), no
  failures.
- **Hash-consed interfaces are essential.** A fresh interface is the unshared tree type
  inference produces. Holding 920 of them cost 4.5GB of live heap on the large app with the base
  compiler (1.6GB with compile-perf's alias change). `I.share` (new) rebuilds each with
  every distinct type node allocated once: 0.8GB (0.9GB).
- **Numbers (the large app, 920 modules).** What a rebuild costs now depends mostly on
  compile-perf. With compile-perf merged (cd2ce8cb), a body edit to a leaf module takes
  2.6s in memory mode against 3.2-3.4s by default, and an interface change to Types.elm
  (391 modules) about 5s in both. On the base compiler the leaf edit is 2.8-3.0s against
  3.8-4.3s, and the Types change is ~50s in both, because that build is ~90% GC (`-qg`).
  The memory mode costs ~1.1GB of resident heap (49MB without it).
- **Where the time goes now** (memory mode, compile-perf, leaf edit, 2.6s): about 2.0s is
  `Lamdera.Wire3.Direct.replaceCodecs` reading every module's Wire3 IR back with the
  derived `Read` instance, on every build, in both modes. compile-perf has a replacement
  reader in progress; if it takes that 2.0s near zero, the same edit lands around 0.8s in
  memory mode and 1.5s by default (an estimate, not measured). The rest is the JS traversal
  (0.15-0.25s), concatenating the cached JS (0.3s; 0.6-0.75s after a Types change) and the
  type hash check (0.1s).
- **Found on the way:** a Ctrl-C that lands during the `performMajorGC` `Ext.Sentry` runs
  after every rebuild is lost (pre-existing, both modes; the window is the length of that
  GC). Memory mode now skips that forced GC, which made the first Ctrl-C right after a
  rebuild work again there; default mode is unchanged.
- **Gaps.** SIGTERM/SIGHUP do not flush; the store is dropped (not repaired) if a build
  throws; ~1.1GB resident on the large app. See Known limitations.


## elm-dev: what is there and how it got there

elm-dev (https://github.com/mdgriffith/elm-dev, BSD-3-Clause, the Elm compiler's license)
is a fork of the Elm 0.19.1 compiler (`ElmDevElmVersion.elmVersion = 0.19.1`) with an
editor/LSP/MCP daemon around it. Cloned at d9395458 (2026-10-12).

### Mario's contribution (71 commits, 2021-02 to 2024-01; the build modes are the 33 of 2022-06/07)

- 2021-02, "elmx": `Ext.Sentry` (an MVar holding the last compile result), `Ext.Filewatch`
  (fsnotify `watchTree` plus `fold-debounce`), a websocket server, and "compile on file
  change, serve from cache". This is the same Sentry/Filewatch pair `lamdera live` uses today
  (`ext-sentry/`).
- 2022-06/07, compile modes (`d4d1c2c0` .. `79112b42`):
  - `Ext.CompileMode`: `Disk | Memory | Race`, chosen by the `MODE` env var.
  - `Ext.FileProxy`: every `File.*` call in builder/ (writeBinary, readBinary, readUtf8,
    getTime, exists, ...) routed by mode, either to the real `File` or to `Ext.FileCache`.
  - `Ext.FileCache`: a global `CuckooHashTable FilePath (Time, ByteString)`, a virtual file
    system. `writeBinary` still `Binary.encode`s into it and `readBinary` still decodes out
    of it; only the disk I/O is gone. The `Time` of an entry is its insertion time.
    `handleIfChanged` re-reads the given paths, compares bytes, and re-inserts (bumping the
    time) only what changed, so builder's normal mtime-based invalidation keeps working on
    virtual times.
  - `Ext.MemoryCached.{Build,Details,Generate,BackgroundWriter}`: Build.hs, Details.hs,
    Generate.hs vendored and pointed at FileCache, plus MVar caches of the whole `Details`
    and the whole `Artifacts`.
  - `Ext.CompileProxy`: one entry point per operation that runs the chosen mode, with
    per-mode timing aggregates; `Race` runs both and logs which won.
  - "Dumbest cache invalidation strategy: clobber everything on file change" (`3e00b3a5`):
    the Details and Artifacts MVars are busted on every compile.
  - "Second artifacts cache" (`3742a1d1`): all package interfaces/objects cached in an MVar.
  - Memory statistics from `GHC.Stats` (live bytes, slop, mem in use).
  - JS generation was dropped from the memory path (type checking only, for the editor).

### What changed since (Matt Griffith, 793 commits from 2022-11 to 2026-10)

The build core was not re-engineered; the work went into a daemon around it:

- 2022-11 `8f0bd480` "default to Disk mode for now"; the daemon (`Watchtower.Server.Daemon`)
  and `elm-dev` main later switch to Memory explicitly with `setModeMemory`.
  (`getMode` is a NOINLINE CAF over an MVar, so it is read once; set it before first use.)
- 2025-05/06: `CompileProxy.compile` as the single compile entry; `Watchtower.State.Compile`
  as "the blessed way to compile"; one file watch per project.
- 2025-06 to 2025-09: virtual-file projects (`elm-stuff/virtual`), LSP edits forwarded into
  the VFS (`FileCache.edit`, unsaved buffers compile from memory), `allowWrites = False`
  (memory mode keeps elm-stuff writes in the VFS only; package writes still go to disk).
- 2025-08: per-file analysis captured during compile in `Ext.MemoryCached.Build.compile`:
  warnings, docs, canonical AST, localizer, hover index (kept as Haskell values, not JSON).
- 2025-09: JS generation in the daemon again, hot-reload injection, per-project flags,
  package docs/readme read into memory.
- 2025-10: RTS nursery `-A128m` lowered to `-A32m`; memory stats in the dev server.
- 2025-11: VFS versions (`fsVersion`/`compileVersion`); `updateVfsFromFs` re-reads every
  source file of the project on each compile and content-compares it.
- 2026-05: multiple entrypoints per project (only the entrypoint groups affected by the
  changed files are compiled); incremental `-O2`/`-O3`.
- 2026-08: "Reduce memory usage for cached hover type information", "Remove expensive daemon
  and docs cache hashing", "Isolate test runs to bound memory usage".

So the in-memory build in elm-dev today is still the 2022 shape: a byte-level VFS under a
vendored builder. Each compile still decodes `d.dat` and, for every module it needs,
the `.elmi`/`.elmo` bytes out of the VFS; still encodes every artifact it writes; still
decodes the package `i.dat`/`o.dat` blobs; and keeps both the bytes and (during a
compile) the decoded values. It saves the disk I/O and the syscalls, not the codec work,
and it does not share any decoded value across compiles.

### Differences from our tree

Our compiler is Elm 0.19.3 plus Lamdera: `File.readBytes`/`writeBytes` with hand-written
`Bytes.Encode`/`Decode` codecs instead of `Data.Binary`, a `File.Writer` threaded through
builder, `Root`/`R.Stuff` instead of `Stuff`, `ThreadSafe.Fork` instead of raw MVars, the
interface type table (each distinct `Can.Type` written once per interface, decoded shared),
Wire3 injection inside `Compile.compile`, and the Lamdera live harness compiled as an
outside root. None of elm-dev's vendored builder files apply as-is, and vendoring 2,500
lines of builder again would double the merge surface we just reconciled for 0.19.3.

### What was ported, and the license

The idea (an in-memory elm-stuff behind the unchanged build algorithm, with the builder's
own invalidation deciding what to recompile) and the shape of the live loop (Sentry +
Filewatch, which Lamdera already had from elmx). No elm-dev code was copied: the store
holds decoded values instead of bytes and the builder is hooked instead of vendored.
elm-dev is BSD-3-Clause like this compiler, so either way it is compatible;
`Ext.MemoryCache` credits it in its module comment.

### Improvements over the naive version

| elm-dev memory mode | here |
| --- | --- |
| bytes in a VFS: every artifact still encoded on write and decoded on read | decoded values: no codec work after the first touch |
| `d.dat`, `i.dat`, `o.dat` decoded on every compile | `Details` kept with package artifacts resolved, decoded once per session |
| the decoded values are rebuilt every compile, nothing shared | interfaces hash-consed (`I.share`); unchanged interfaces keep their old shared value |
| (Mario's version) caches clobbered on every change | builder's own mtime/BuildID invalidation, exactly as on disk |
| elm-stuff not written at all (`allowWrites = False`) | flushed on exit in an order a later disk build can trust |
| JS fully regenerated (when generated at all) | JS of unchanged definitions reused |
| vendored copies of Build/Details/Generate | a dozen hooks in the real ones, no cost when unused |


## Design

### Goal

`lamdera live --memory` (opt-in; without the flag nothing changes) keeps a project's build
artifacts in memory as decoded Haskell values across rebuilds:

- per module: its interface (`I.Interface`) and its objects (`Opt.LocalGraph`),
- the project `Details`, with the package interfaces and objects resolved
  (`ArtifactsFresh`), so `i.dat` and `o.dat` are decoded once per session, not per rebuild,
- the rendered JS of each top-level definition from the last build.

A rebuild then never encodes, writes, reads or decodes anything under
`elm-stuff/lamdera-*`. What still happens per rebuild: the crawl (one `stat` per module, a
read and parse of each changed file), compiling what changed and what depends on a changed
interface, and generating the JS (in part from the kept statements).

### Shape: elm-stuff in memory, keyed by the stuff directory

Instead of vendoring builder (elm-dev's route), a small store, `Ext.MemoryCache`, is
registered for a project's stuff directory (`R.Stuff`, canonicalised). The handful of
places in builder that touch project artifacts consult it:

| site | disk | registered store |
| --- | --- | --- |
| `Elm.Details.load` | decode `d.dat` | in-memory `Details` (buildID+1) while `elm.json` is unchanged; otherwise the disk path, with `i.dat`/`o.dat` decoded once into `ArtifactsFresh` |
| `Build.writeDetails` (via `Details.write`) | encode `d.dat` | keep the new `Details` |
| `Build.compile`: old `.elmi` | decode `.elmi` | store (else disk once, kept) |
| `Build.compile`: write `.elmo`/`.elmi` | encode + write | store; a changed interface is hash-consed first |
| `Build.loadInterface` | decode `.elmi` | store (else disk once, kept) |
| `Generate.loadObject` | decode `.elmo` | store (else disk once, kept) |
| `Generate.loadTypesHelp`, `Ext.Query.Interfaces.cachedHelp` | decode `.elmi` | store (else disk once, kept) |
| `Generate.dev` | `JS.generate` | `JS.generateCached` with the store's JS cache (see below) |

Every build of that project in the process goes through the same store: the live harness
build, a manual compile of another `.elm` file from the browser (`serveElm_`), and the nested
`Interfaces.all` build that `Lamdera.TypeHash.buildCheckHashes` runs. So the semantics are
exactly those of elm-stuff, minus the codecs. Nothing above the builder changes:
`Develop.compile` (Details.load, Build.fromPaths, PostCompile.check, TypeHash, Generate.dev)
runs as before, with Wire3 injection inside `Compile.compile` as before. Multiple entry
points need nothing special: the store is per module, whichever root reaches it.

The hooks cost one `IORef` read and one lookup in an empty `Map` when no store is
registered, which is every compile outside `lamdera live --memory`.

A store entry missing for a module means "not touched in memory yet": the disk artifact is
read (and kept). That is consistent because memory mode never writes project artifacts
during the session: a module compiled in memory is always in the store, and a module never
compiled in memory still has the disk artifacts that the disk `d.dat` (the base of the
in-memory `Details`) describes.

The `Details` and the JS cache are kept as `Dynamic`s in the store because `Elm.Details`
itself imports `Ext.MemoryCache` (for the load/write hooks).

### Keep the retained interfaces small: hash-consing

The interface type table branch showed that type inference stores every alias fully
expanded, so a fresh interface is a tree with the same subtrees repeated thousands of
times (the large app: 707MB of `.elmi` before the table, 90MB after). Decoding a table-format
`.elmi` already rebuilds shared values, but an interface fresh from `I.fromModule` is the
unshared tree.

So `I.share` (new, in `Elm.Interface`) rebuilds an interface with each distinct type node
allocated once, using the same node keys as the type table encoder, without going through
bytes. A changed interface (`RNew`) is shared before it is stored and before dependents in
the same build use it; an unchanged one (`RSame`) keeps the already-shared old value and
the fresh tree is dropped. Measured after a cold `--memory` build of the large app (920 fresh
interfaces held; `LAMDERA_MEMORY_SHARE=0` turns it off):

| | without `I.share` | with |
| --- | --- | --- |
| base compiler: live heap after the build | 4,493MB | 771MB |
| base compiler: peak in use / build time | 20.8GB / 75s | 13.2GB / 58s |
| with compile-perf: live heap after the build | 1,594MB | 934MB (incl. the JS cache) |
| with compile-perf: peak in use / build time | 6.5GB / 9.2s | 5.1GB / 7.2s |

### Reusing the JS of unchanged definitions

In memory an unchanged module's objects are the same values from one build to the next.
The JS statement of a top-level definition depends only on its `Opt.Node`, the mode, and
(through Lamdera's direct calls, `makeArgLookup`) the arities of the globals it calls, all
of which are in its deps. `Generate.JavaScript.generateCached` keeps, per global, the node,
the arities of its deps and the rendered bytes, and reuses the bytes when the node is
pointer equal (a check that can only miss, never be wrong) and the arities are unchanged.
The traversal still runs in full, so the output order is identical; only the rendering is
skipped. `Define`, `DefineTailFunc` and `Cycle` statements are cached; kernel nodes,
including the Wire3 codecs that `replaceCodecs` renders every build, are emitted as before.
Only `Generate.dev` (Mode.Dev, not ESM) uses it. The arity check is covered by a test whose
dependency changes arity behind an unchanged interface (the importer is not recompiled but
its JS must change); with the check disabled that test fails. On the large app (with compile-perf)
it costs ~165MB and takes a leaf edit from 2.85-2.9s to 2.56-2.65s (rendering 0.3s instead
of ~0.7s); on a 391 module rebuild the gain is within noise.

### Disk

- Package artifacts (`~/.elm/0.19.3/packages/.../artifacts.*.dat`) stay on disk, written and
  read as before; they are decoded once per session via `ArtifactsFresh`.
- On `elm.json` changes `Details.load` takes the normal path (verify, possibly build
  packages, write `d.dat`/`i.dat`/`o.dat`), and the module store is reset so nothing in it
  can disagree with the reloaded `Details`.
- Project artifacts are not written during the session. On a clean exit (Ctrl-C) the dirty
  modules are flushed under the root lock: all `.elmo`, then all `.elmi`, then `d.dat`.
  That order keeps a partly written elm-stuff safe for a later normal build: no `.elmi` is
  ever newer than the `.elmo` files compiled against it, and the old `d.dat` makes the
  normal build re-check every module that changed. If the process dies instead, elm-stuff
  is simply the stale but consistent state of the last disk build, and the next start
  recompiles what changed since. Flush on the large app: 0.2s after Types edits (391 `.elmo`,
  1 `.elmi`), 4.4-6.8s after a cold start (920 + 920).
- The REPL worker in the live UI does not read Lamdera's project elm-stuff (it compiles the
  project itself under its own `elm-stuff/0.19.3` in an in-browser file system, and reads
  only the package artifacts from `~/.elm`), so it is unaffected.
- Background write-behind was considered and left out: it keeps elm-stuff warm across a
  crash, but costs encoding work after every rebuild for a benefit (a warm start after
  SIGKILL) the exit flush mostly gives already.

### Failure handling

If a build throws (as opposed to reporting compile errors) the store, the in-memory
`Details` and the JS cache are dropped (`withMemoryReset`), and the next build starts from
disk again. A module that fails to compile leaves its store entry and its `Local`
untouched, exactly like elm-stuff.

### Concurrency

Live already serialises rebuilds (`Ext.Sentry` takes the JS MVar) and builds take the root
lock. Within a build modules compile in parallel; the store is an `IORef (Map ...)` updated
with `atomicModifyIORef'` per module.

### Live loop

`Develop.runWithRoot` registers the store when `--memory` is given, wraps each rebuild in
`withMemoryReset` (and `timedRebuild` for `LAMDERA_LIVE_TIMING`), wraps the server in
`withMemoryFlush`, and asks `Ext.Sentry` not to force a major GC after each rebuild (that GC
would mostly copy the kept artifacts, and loses a Ctrl-C that arrives during it, see Known
limitations).

### Not changed

- The crawl still `stat`s every module per rebuild. elm-dev content-compares every source
  file instead (more robust to mtime games, more I/O); stats are cheap and also catch edits
  the watcher misses (git checkout, editors that rename).
- `lamdera make`, `check`, `deploy` and plain `lamdera live` are untouched.

### Timing output

`LAMDERA_LIVE_TIMING=1` (either mode) prints per rebuild the time to fresh JS split into
`details` / `build` / `postcompile` / `typehash` / `objects` / `rest`, the modules
compiled, the GC time and collections during it, what the store did (hits, disk reads,
entries held) and, with `+RTS -T`, the heap after a major GC. By default `objects` is
loading the objects and `rest` all of the JS generation; in memory mode `objects` also
includes building the global graph and walking it (`replaceCodecs` included), and `rest`
is rendering. The exit flush prints what it wrote and how long it took.


## Measurements (the large app, 920 modules)

Machine: Apple Silicon, 16 cores, 128GB. Each run (`scratchpad/lm/livebench.py`) starts
`lamdera live` with `LAMDERA_LIVE_TIMING=1 +RTS -T`, waits for the first build, then twice:
an interface change to `src/Types.elm` (an unused top-level value appended; 391 modules
recompile) and a body-only change to `src/Palette/Alert.elm` (a CSS class string in an
unexposed value; the module and the harness recompile). It then reverts both, lets it
rebuild, and stops it with SIGINT. Times are what the server reports from the start of the
rebuild to the finished HTML; the wall time from the file write to that line is 0.0-0.7s
more (watcher latency, debounce) and is in the raw results. Warm runs start from the
previous run's elm-stuff, cold runs with none (package artifacts warm). The compile-perf
agent ran the large app builds of its own during several runs (the script waits for any other
`lamdera make`/`live` before each build, but not for `stack build`), so the 1-minute load
average is given. Every run's JS was compared by sha1: both modes give byte identical JS
at every step.

### With compile-perf merged (the state this would ship in)

Branch `live-memory-perf-trial` (worktree `~/dev/projects/lamdera-compiler-live-memory-perf`),
a throwaway merge of this branch with compile-perf at cd2ce8cb (parallel GC back on, alias
real types built lazily). The only conflict was `Build.compile`, where compile-perf wraps
the same `.elmo`/`.elmi` calls this branch replaces in its `Lamdera.Perf` timing (kept both).
Three warm runs per mode (load 3.5-7.4), the last with this branch's final commit.

| | default `lamdera live` | `lamdera live --memory` |
| --- | --- | --- |
| first build, cold (921 modules) | 7.1s, peak in use 5.3GB | 7.2-7.5s, peak in use 5.1-5.3GB |
| first build, warm elm-stuff | 3.0, 3.0, 3.1s | 3.0, 3.5, 3.0s |
| Types.elm interface change, 391 modules | 5.1, 5.4; 5.0, 5.2; 4.9, 5.2 | 5.4, 4.7; 5.0, 4.9; 4.9, 4.9 |
| of which compiling (`build`) | 1.8-2.1s | 1.7-2.4s |
| leaf body change, 2 modules | 3.2, 3.4; 3.2, 3.3; 3.2, 3.3 | 2.6, 2.6; 2.6, 2.6; 2.6, 2.6 |
| live heap after GC | 49MB | 1.1GB |
| RTS peak in use over a Types rebuild | 4.6-4.9GB | 6.4-6.8GB |
| exit | | flush 0.2s, first Ctrl-C |

A leaf edit in memory mode (2.6s): `objects` 2.15s, of which about 2.0s is
`replaceCodecs` (measured separately: `graphModifications` alone takes 2.0s every build)
and 0.15-0.25s the graph walk; rendering 0.3s; type hash check 0.1s; compiling 0.03s.
By default the same edit spends 0.2s in the type hash check (a second, nested build of
`src/Types.elm` and a second load of the package artifacts), 0.05s loading 920 `.elmo`,
and 2.9s generating the JS (2.0s of it the same `replaceCodecs`).

### Base compiler (this branch; RTS `-N -qg -A128m`), before the JS cache

| | default `lamdera live` | `lamdera live --memory` |
| --- | --- | --- |
| first build, cold (921 modules) | 62.8s (load 4.8), peak RSS 15.2GB | 57.7s (load 4.4), peak RSS 13.3GB |
| first build, warm elm-stuff | 3.9s | 3.8s |
| Types.elm interface change, 391 modules | 54.0, 54.5 (load 7.9, 6.6); 49.0, 52.5 (7.2, 5.8) | 54.6, 50.9 (3.2, 2.9); 50.5, 52.1 (2.8, 5.6) |
| of which GC | 43.7-46.8s | 45.0-49.5s |
| leaf body change, 2 modules | 3.8, 4.1; 3.9, 4.3 | 2.9, 3.0; 3.0, 3.0 |
| live heap after GC | 49MB | 0.8-1.0GB |
| RTS peak in use over a Types rebuild | 11.6-12.9GB | 11.9-13.2GB |

With the JS cache on the base compiler (one run): leaf edit 2.8, 2.9s; Types change 55.6, 56.0s.

Reading the tables:

- The memory mode removes what it set out to remove: `details` and `objects` loading drop
  to (near) zero, the type hash check halves, and nothing is read from disk once warm
  (`store: 0 disk reads`). On the large app that, plus the JS cache, is worth 0.6-0.8s on a leaf
  edit with compile-perf and ~1.1s on the base compiler; on a 391 module rebuild the
  difference is within the noise of these runs.
- On the base compiler a big rebuild is ~90% GC (sequential GC), and holding ~1GB more makes
  every major collection more expensive, which cancels the saving on the Types change.
  With compile-perf's parallel GC, GC is under 1.1s per rebuild and that effect is gone.
- With compile-perf, JS generation is the largest part of every rebuild in both modes, and
  most of it is `replaceCodecs` re-reading the Wire3 IR (compile-perf's next fix).


## Tests

- `Test.ElmiTypeTable`: new case for `I.share` (50 annotations each with their own Model
  tree come back equal, encode to the same bytes, and all 100 Model occurrences are one
  value).
- `Test.LiveMemory` (new, 17 checks): two copies of a Lamdera app built the way live builds
  them, one with a registered store; byte identical JS and identical recompiled module
  sets (and the expected ones) after: first build; no change; leaf body change; leaf
  interface change (importer recompiles, its interface stays, its importer does not); a
  dependency's arity changing behind an unchanged interface (importer not recompiled, its
  JS changes, via the JS cache's arity check) and back; new constructor in Types (Wire3
  codecs change); a type error (both fail) and its fix; touching elm.json (details
  reloaded, store reset, full rebuild like disk); more in-memory rebuilds. Then: no `.elmo`
  written while registered, a flush that a disk build finds up to date (only the harness
  compiles), and a new store starting from it that reads nothing from disk once warm.
- Full `stack test` (scratch `ELM_HOME`) on the final branch: 141 passed, 7 pending, no
  failures. (elmi-type-table had 97 passed and 7 pending; the typed-decoders merge adds its
  Wire3 cases and this branch the 18 above. Pending: the two TestMigrationGenerator e2e
  scopes, two Test.Check scopes, Test.Ext.ElmPages.Check isWireCompatible, Test.Lamdera,
  Test.WebGL.)
- Manually: `lamdera live --memory` on a fresh `lamdera init` project and on the large app; edits
  to Frontend (body) and Types (interface) update the served page; after Ctrl-C a plain
  `lamdera live` on the flushed elm-stuff compiles only the harness and serves a
  byte-identical page.


## Known limitations

- **A Ctrl-C during a forced major GC is lost** (GHC 9.8 threaded RTS, macOS; pre-existing).
  `Ext.Sentry` forces one after every rebuild, so a Ctrl-C right after a rebuild did nothing
  (a second one was needed): every time in memory mode, where that GC copies ~1GB, and in
  about half of the default runs. Isolated with a build of `Ext.Sentry` without the GC:
  SIGINT at the moment the rebuild finishes was lost 2/2 with the GC and handled 2/2
  without, on both compilers; sent 0.3s or more later it was handled every time. Memory mode now skips the
  forced GC (fb3e3fd3); default mode keeps it and keeps the (short) window.
  `LAMDERA_LIVE_TIMING` with `+RTS -T` forces a major GC of its own for the heap figure,
  which reopens the window, so the benchmark waits 3s before stopping the server.
- Ctrl-C is the only exit that flushes. SIGTERM, SIGHUP (closing the terminal) or a crash
  skip it; elm-stuff is then the stale but consistent state of the last disk build, and
  the next start recompiles what changed since (same as the backend snapshot `withEnd`
  saves today).
- The flush takes the root lock, so it waits for a rebuild in progress.
- A build that throws drops the whole store: correct, but the next rebuild reloads from an
  elm-stuff that may be far behind.
- A concurrent `lamdera make` on the same project in another terminal is safe (root lock,
  and memory mode never trusts disk over its own state), but memory mode does not see its
  results, may redo its work, and the exit flush overwrites what it wrote with the same
  content.
- Modules that disappear keep their store entry for the session; it is only read again if
  a module of that name comes back, and that one is recompiled because its time differs.
- Memory: ~1.1GB resident on the large app for the session, objects being roughly half, the JS
  cache ~165MB.


## Next steps

1. **`replaceCodecs`** (2.0s of every rebuild on the large app, both modes): compile-perf's IR
   reader is the fix and should land first. In memory mode it could go further and keep
   each module's parsed IR (or the rendered codec nodes) next to its objects, so unchanged
   modules' codecs are neither read nor rendered again.
2. **Smaller retained objects.** Objects are about half of the ~1.1GB held. Names and
   `Opt.Global`s are allocated per occurrence; interning them when objects enter the store
   (like `I.share` does for types) would cut that.
3. **Share interfaces in normal builds too.** During any build, results hold every fresh
   interface until the build ends. `RSame` could return the old (shared, already decoded)
   interface and `RNew` the `I.share`d one in `lamdera make` as well; on the base compiler
   that is the 4.5GB vs 0.8GB difference above for a cold build, and with compile-perf
   1.6GB vs 0.9GB. Worth measuring as part of compile-perf.
4. **Compact regions for the store** (`GHC.Compact`): retained data would no longer be
   copied by major GCs. Blocked today by the Wire3 IR, which is stored as pinned
   `ByteString`s in `Opt.Kernel` nodes (not compactable); with the parallel GC the gain is
   small anyway.
5. **Flush on SIGTERM/SIGHUP.** And look at the lost SIGINT in default mode: either drop
   the forced GC there too, or report it upstream with the small reproduction above.
6. **Optional background write-behind** if warm starts after crashes matter.

### Relation to the compile-perf work

compile-perf (`~/dev/projects/lamdera-compiler-perf`, branch `compile-perf`) and this
branch touch the same three lines of `Build.compile` (both wrap the `.elmo`/`.elmi`
reads and writes); the trial merge resolved that by keeping compile-perf's `perf` labels
around this branch's `MemoryCache` calls. Nothing else overlaps: compile-perf changes the
type checker, the RTS options and (in progress, uncommitted) the Wire3 IR reader in
`Lamdera.Wire3.Direct`; this branch changes how artifacts are stored and adds the JS cache
in `Generate.JavaScript`. They combine cleanly and measurably (the first table): together
a Types change goes from ~50s to ~5s and a leaf edit from ~4s to 2.6s, and once the IR
reader lands the leaf edit should be well under a second in memory mode.
