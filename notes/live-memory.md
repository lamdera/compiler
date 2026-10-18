# `lamdera live --memory`: an in-memory build for live

Branch `live-memory` (worktree `~/dev/projects/lamdera-compiler-live-memory`), rebased onto
`compile-perf` f9b569c4, which includes `elmi-type-table` and the Wire3 typed decoders. Not
pushed. The tips before each rebase are kept as branches: `live-memory-pre-rebase`
(14aace8d, on elmi-type-table) and `live-memory-pre-rebase2` (4e7987f4, on compile-perf
1957416e).


## Summary

- **What it is.** `lamdera live --memory` keeps a project's build in memory between
  rebuilds as decoded values: every module's interface and objects, the `Details` with the
  package artifacts resolved, the sources (bytes, mtimes, parsed modules), the rendered JS
  of every definition and the rendered Wire3 codecs. After the first build a rebuild reads,
  writes, encodes or decodes nothing under `elm-stuff/lamdera-*` and stats almost nothing,
  and only renders what changed. In freeze mode the page then gets a hot patch (just the
  changed definitions and the values that use them, run in the running page's scope) instead
  of evaluating the whole new bundle. Ctrl-C writes elm-stuff, so the next start, `lamdera
  make` or plain `lamdera live` is warm. Default `lamdera live` and every other command are
  unchanged.
- **Numbers (the large app, 920 modules, final branch on compile-perf f9b569c4).**

  | | default `lamdera live` | `--memory` |
  | --- | --- | --- |
  | first build, cold | 3.95s | 3.73s |
  | first build, warm elm-stuff | 1.30s | 1.48-1.51s |
  | Types.elm interface change (391 modules) | 2.38-2.55s | 1.92-2.05s (2.34-2.38 the first time after a warm start) |
  | leaf body change (2 modules) | 1.26-1.29s | 0.44-0.49s |
  | leaf body change, slow file system (300us per stat/open; on 1957416e) | 2.11s | 0.52s |
  | view edit to visible in the page (freeze mode) | 2.03, 2.05s (full hot reload, 49MB) | 0.56, 0.58, 0.61s (hot patch, 1.9KB) |
  | live heap over 22 rebuilds | 50-134MB | levels off at 1.4-1.5GB |

  JS identical in both modes at every step (sha1, with the hot patch hook and build number
  taken out).
- **Where the leaf edit's 0.46s goes** (memory): compiling 0.06s, type hash check 0 (skipped,
  nothing it hashes changed), JS generation 0.37-0.41s (building the 58,000 entry global
  graph, walking the 34,000 reachable definitions, the cache lookups, the listing), putting
  the page together 0.01s. By default the same edit is 0.05s compiling, 0.2s for the type
  hash check's second build of Types, and 1.0s generating the JS.
- **The heap levels off** now: over 22 rebuilds (ten Types changes, ten leaf edits) the
  memory mode stays at 1.4-1.5GB and the default at 50-134MB. Before two fixes made after
  the second rebase it grew without end, 0.7GB to 3.3GB in memory mode and 57MB to 313MB by
  default, ~85MB per Types change from compile-perf's type checker tables (never emptied in a
  long-running process) and ~120MB per Types change from the hot patch history (trimmed
  lazily, so never trimmed). Both are emptied or forced now; see Known limitations.
- **The steps of this round**, each a commit:
  1. Rebased onto compile-perf 1957416e, then again onto f9b569c4 (see Relation to the
     compile-perf work). The old trial merge with cd2ce8cb (a buggy base) and its numbers
     are gone, worktree and branch removed.
  2. Sources in memory (Mario's `FileCache` idea, done on the source table): stats and reads
     from a table updated by watcher events, with full resets on directory/rename events
     and `elm.json`, a reconcile every 5s and on page load, and parsed modules kept. Worth
     0.1-0.5s on a Types change locally and the whole crawl on a slow file system
     (0.55s to 0.06s of `build` per leaf edit at 300us per stat).
  3. Re-measured. The fast Wire3 IR reader (b353e45f) took `replaceCodecs` from 2.0s to
     ~0.35s (reading 75-125ms, rendering the reachable codecs 200-300ms); those are now kept
     between builds too. The type hash check uses the main build's interfaces and is skipped
     when they are unchanged.
  4. Hot patches for Simon's freeze mode hot reload, with the full hot reload as the
     fallback (kernel/Wire3 codec changes, ports, effect managers, constructors, exports,
     cycles of values, big patches, errors).
  5. A plan for Matt Griffith's MCP and LSP daemon work from elm-dev on this store.
- **Tests.** `Test.LiveMemory` (24 checks) builds a Lamdera app in memory and from disk
  through a dozen edits: identical JS, identical recompiled modules, sources missed by the
  watcher found by a reconcile, a directory event resetting the table, the type hashes
  equal to a separate build's. `Test.HotPatch` (8 checks) builds a worker app across edits
  and checks the patches (and full reloads where needed), and in node that the first build
  plus the patches answers like the latest. Full `stack test`: 158 passed, 7 pending (the
  same 7 as compile-perf), no failures.
- **Gaps.** JS generation (global graph and walk, 0.38s) is now the largest part of a
  memory rebuild; the memory mode holds 1.4-1.5GB; a Ctrl-C is still occasionally lost
  (the major GC window); the large app's own FrontendModel does not decode
  with its codecs, so its freeze mode reloads the page after every hot reload, patch or
  full (pre-existing, both paths). See Known limitations.


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

### Credit, and what came from where

The in-memory compile mode in elm-dev (`Ext.FileCache`, `Ext.FileProxy`,
`Ext.CompileMode`, `Ext.MemoryCached`, the Sentry/Filewatch live loop) is Mario's own work,
so this branch revisits it rather than ports it: the idea (an in-memory elm-stuff and
source tree behind the unchanged build algorithm, with the builder's own invalidation
deciding what to recompile) is the same, the implementation is new (decoded values
instead of bytes, the builder hooked instead of vendored). What is taken from Matt
Griffith's later work in elm-dev is credited where it is used: the full-tree reconcile of
the source table (his `updateVfsFromFs`, 2025-11) and the plan for the MCP/LSP daemon
(below). elm-dev is BSD-3-Clause like this compiler.

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
`elm-stuff/lamdera-*`. What still happens per rebuild: the crawl (from the source table, see
Sources in memory; a read and parse of each changed file), compiling what changed and what
depends on a changed interface, and generating the JS (from the kept statements and Wire3
codecs where nothing changed).

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
`Interfaces.all` build of the type hash check where it still runs (with `--memory` the check
takes its interfaces from the main build instead, see The type hash check). So the
semantics are exactly those of elm-stuff, minus the codecs. Nothing above the builder changes:
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
| final branch (compile-perf 1957416e): live heap after the build | 1,640MB | 984MB (incl. the JS and codec caches) |
| final branch: peak in use / build time | 6.1GB / 4.9s | 4.3GB / 5.0s |

(The base compiler rows are from before the rebase, on the compiler without compile-perf.)

### Reusing the JS of unchanged definitions

In memory an unchanged module's objects are the same values from one build to the next.
The JS statement of a top-level definition depends only on its `Opt.Node`, the mode, and
(through Lamdera's direct calls, `makeArgLookup`) the arities of the globals it calls, all
of which are in its deps. `Generate.JavaScript.generateCached` keeps, per global, the node,
the arities of its deps and the rendered bytes, and reuses the bytes when the node is
pointer equal (a check that can only miss, never be wrong) and the arities are unchanged.
The traversal still runs in full, so the output order is identical; only the rendering is
skipped. Every node kind goes through it, kernel chunks included (the Wire3 codecs are
kernel nodes that `replaceCodecsCached` keeps the same between builds, see Wire3 codecs),
and each build also lists what it emitted, for hot patches (see Hot patches). Only
`Generate.dev` (Mode.Dev, not ESM) uses it. The arity check is covered by a test whose
dependency changes arity behind an unchanged interface (the importer is not recompiled but
its JS must change); with the check disabled that test fails.

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
  recompiles what changed since. Flush on the large app: 0.24s after Types edits (391 `.elmo`,
  1 `.elmi`), 2.2s after a cold start (920 + 920).
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
`withMemoryFlush`, and rebuilds with `Sentry.updateJsOutputNow`: the JS output is taken
before the clients are told (so a hot patch asked for right away waits for the build), and
no major GC is forced after each rebuild (that GC would mostly copy the kept artifacts, and
loses a Ctrl-C that arrives during it, see Known limitations). It also tells the source
table what the watcher saw before each build, runs the background reconcile, reconciles
when the page is loaded, and serves `/_patch`.

### Not changed

- Builder's invalidation: the source table only decides what `stat` and `readUtf8` return
  (see Sources in memory); what to recompile is still decided by comparing times and
  interfaces, as on disk.
- `lamdera make`, `check`, `deploy` and plain `lamdera live` are untouched.

### Timing output

`LAMDERA_LIVE_TIMING=1` (either mode) prints per rebuild the time to fresh JS split into
`details` / `build` / `postcompile` / `typehash` / `objects` / `rest`, the modules
compiled, the GC time and collections during it, what the store did (hits, disk reads,
entries held) and, with `+RTS -T`, the heap after a major GC. By default `objects` is
loading the objects and `rest` all of the JS generation; in memory mode `objects` is all of
the JS generation (building the global graph, the Wire3 codecs, the walk, rendering what
changed and the listing for hot patches), and `rest` putting the page together. The exit
flush prints what it wrote and how long it took.


## Sources in memory

With `--memory` the crawl's per-module source `stat`s and reads (`Build.crawlModule`,
`crawlFile`, `checkModule`) go through a table in the store instead of the disk
(`Ext.MemoryCache` SOURCES): `path -> Present mtime (Maybe bytes) (Maybe parsed module) |
Absent`. An entry is filled the first time a path is looked at and then only changes when
something says it may have:

- **Watcher events** (`sourcesChanged`, from `Develop`'s rebuild, before the build): an
  `.elm` path is re-stat'ed; same mtime and bytes never read means "read them now, no
  change"; same mtime with different bytes, or a new mtime, replaces the entry (the time
  becomes `File.zeroTime` when only the bytes moved, so the builder's own time check sees
  a change). Everything else the builder does is unchanged: it still compares times with
  `d.dat`'s and decides what to recompile.
- **A directory event, a rename, or a path that vanished** (moving a folder, `git
  checkout`, some editors' atomic saves): the whole table is forgotten and the next build
  looks at the disk again, like the default.
- **`elm.json`**: the store (and with it the table) is reset with the Details.
- **The live harness** (`elm-stuff/lamdera`, written by `prepareLocalDev` before every build,
  ignored by the watcher): the files it writes are reported to the table every build
  (`Live.localDevFiles`). It is an extra source directory where every module is looked for
  first and not found, so leaving it to the disk would cost a stat per module per build.
- **A reconcile** (`reconcileSources`) stats every entry and refreshes those that differ: in
  the background every 5s, and when the page is loaded (so a browser reload always shows
  what is on disk, even after an edit the watcher missed, e.g. in an ignored directory or a
  dropped event). This is Matt Griffith's `updateVfsFromFs` idea from elm-dev, as a safety
  net rather than on every build. `.git/HEAD` needs no case of its own: the watcher filters
  `.git`, but a branch switch shows up as file and directory events in `src`.
- **Parsed modules** are kept next to the bytes (`sourceParseFor`): a module recompiled only
  because a dependency's interface changed is not parsed again (389 of the 391 modules of a
  Types.elm change on the large app).
- `Build.makeScheduler` (compile-perf) stats every cached module's source for its size on
  every build, in the calling thread; with `--memory` that size comes from the table
  (`sourceSizeFor`), kept per mtime.
- `LAMDERA_MEMORY_SOURCES=0` turns all of this off.

What it saves on the large app: on a local SSD almost nothing per leaf edit (the crawl's 920 stats
are ~10ms), and 0.1-0.5s on a Types.elm change (the parses kept). On a slow file system it
is the whole crawl. Measured with a `DYLD_INSERT_LIBRARIES` shim that adds 300us to every
`stat`/`lstat`/`fstatat`/`open`/`openat` under the project (a VM shared folder or a network
mount), leaf body edit:

| | rebuild | `build` (crawl + compile) | `typehash` |
| --- | --- | --- | --- |
| default | 2.1-2.2s | 0.53-0.56s | 0.53-0.64s |
| `--memory`, sources from disk | 1.47s | 0.53-0.55s | 0.35s |
| `--memory`, sources in memory, first version | 1.18-1.19s | 0.40-0.41s | 0.21-0.22s |
| `--memory`, final (harness kept, scheduler sizes, hashes from the build) | 0.52s | 0.06s | 0 (skipped) |
| `--memory`, final, local SSD for comparison | 0.45-0.46s | 0.05-0.06s | 0 (skipped) |

The first version still paid ~1,200 stats per build: the harness directory (fixed by keeping
it), compile-perf's scheduler sizes (920, fixed by `sourceSizeFor`) and the type hash
check's second build of Types (fixed below). Found with the shim logging every slowed path
with a timestamp (and a C backtrace for chosen paths), then a `HasCallStack` trace in
`File.getTime`/`File.exists`.


## The type hash check

`Lamdera.TypeHash.buildCheckHashes`, run after every live build, builds `src/Types.elm` a
second time (`Interfaces.all`: a whole second crawl, check and interface load of everything
Types needs, 250 modules on the large app) to hash the Lamdera core types, and then throws the
result away (it only forces the errors). With `--memory`, `buildCheckHashesFrom` takes the
same interfaces from the build that just ran (Types' import closure, from the Details it
wrote) and skips the hashing entirely when every one of them is the very value the last
hashes came from (an interface that compiled the same stays the same value in memory,
`MemoryCache.keepSame`). `Test.LiveMemory` checks the hashes equal a separate build's after a
Types change. On the large app: 0.2s by default, 0.1s with the first version of memory mode,
0.09-0.12s when a Types interface changed and 0ms otherwise. The default is unchanged (the
same change would save it ~0.2s per rebuild there too).


## Wire3 codecs (`replaceCodecs`)

compile-perf's IR reader (b353e45f) took the 2.0s of `replaceCodecs` reading every type's
Wire3 IR with the derived `Read` instance down to about 0.1s. Measured separately on the
rebased branch (a temporary patch around `Generate.dev`, the large app, leaf edit): reading all
IR and working out which codecs can be replaced (the spine) 75-125ms, rendering the JS of
the 4,271 reachable `w3_` codecs 200-300ms, every build, in both modes.

With `--memory`, `Lamdera.Wire3.Direct.replaceCodecsCached` keeps both between builds: the
IR read from each `w3_ir_`/`w3_iru_` node (reused while the node's bytes are the same) and
each rendered codec node (reused while its `Codec` and the direct arity of every type it
refers to are the same, the only things rendering looks up). A codec rendered fresh gets a
`_direct` map cut down to those types, so a kept node that is never rendered (codecs are
rendered lazily, only the reachable ones) does not hold on to a whole build's map. The JS
cache carries it from build to build. Same output (`Test.LiveMemory`, and the large app's JS sha1
at every step). Leaf edit on the large app: the rendering part of the rebuild went from ~300ms to
~120ms, the whole rebuild from 0.72s to 0.56s. A Types change gains little (within the
noise of these runs): the types whose codecs it changes are rendered again.


## Hot patches (lamdera live, freeze mode)

### What there was

Simon Lydell's hot reload (095173c1 "First step towards hot reloading", 0153bf45 "Only do
hot reloads in freeze mode", 0ff02583/237627fe refresh when the Backend/FrontendModel
changed, c83e76d9, and the error dialog commits): in freeze mode, on the websocket's `r`
message, `extra/live.js` `hotReload()` fetches `/`, cuts the Elm JS out between the
`// lamdera-elm-js-start`/`-end` markers, evaluates it into a fresh scope
(`new Function(js).call(newScope)`), and calls `Elm.hot.reload(newScope)`. The Platform
kernel (`extra/package-replacements/elm/core/.../Platform.js`, `_Platform_export__DEBUG`,
`app.hotReload`) then swaps the running app's `impl` (init, update, view, subscriptions) for
the new scope's, keeps the model, effect managers and ports, redraws and resubscribes.
Afterwards the harness checks the stored BackendModel/FrontendModel bytes still decode
with the new codecs (`verify*ModelDecodableAfterHotReload`) and reloads the page if not.
Outside freeze mode every rebuild reloads the page.

So each edit still evaluates the whole new bundle: 49MB on the large app, which also has to be
generated and sent.

### What the page gets now

Elm's output is one scope (`(function(scope){ 'use strict'; ... }(this))`) of `var`
definitions in dependency order: the F/A helpers, then every kernel chunk, then every
definition (`$author$project$Module$name`, plus `$fn` direct-call names for functions of
two or more arguments), then `_Platform_export(...)` and Lamdera's injections. A function
looks up the definitions it uses each time it runs, so redefining one is seen by every
caller; a value (anything worked out when the page loads, `main` included) holds on to
what it was worked out from; kernel code comes before every definition, so it can only use
them when called.

With `--memory`:

- Every build lists what it emitted, in output order (`Generate.JavaScript.Listing`: per
  statement its global, kind, rendered bytes and deps; the kernel chunks; the exports and
  injections), as part of the JS cache, which now handles every node kind (and kernel
  chunks), not just definitions. The store keeps the last 10 listings, numbered
  (`MemoryCache.recordJsBuild`).
- The bundle has one more line inside its scope, `scope['__lamdera_patch'] =
  function(code) { eval(code); }` (`patchHook`), and the page is told which build it runs
  (`this.__lamderaJsVersion = N`, after the Elm JS).
- In freeze mode `live.js` first asks `/_patch?from=N`. `Lamdera.HotPatch.diff` compares the
  listings: the statements whose bytes changed or are new, plus every value that uses one
  of them directly or not (one pass in output order), are sent with each `var NAME =` at
  the start of a line turned into `NAME =` (and a name the page never had first made a
  global, so later patches can assign it), then the programs are exported again into a
  scratch scope and `Elm.hot.reload` from there, exactly the call the full hot reload
  makes. The patch runs through the hook, so in the page's own scope. A page a few builds
  behind gets the patches since its build, one after the other.
- It answers `full <reason>` instead when a patch is not safe or not worth it, and `live.js`
  then does the full hot reload as before (also on any error running a patch):
  - kernel code changed or appeared (the Wire3 codecs of a changed type, a newly used
    package's kernel);
  - the exports or Lamdera's injected code changed;
  - a constructor, port or effect manager changed, or a port or manager would have to be
    worked out again (they register themselves when they run);
  - a cycle of values would have to be worked out again;
  - the patch would be more than half the page;
  - the page's build is no longer kept, or there is no newer build (the last one failed:
    the full path shows the error dialog as before; the next good build's patch removes it).
- After a full hot reload the page takes the new scope's hook and build number, since the
  app now runs the new scope's definitions.
- The model checks after a hot reload run as before, so a model whose encoding changed
  still reloads the page.
- With `--memory` the watcher takes the JS output before telling the clients, so a patch
  request made right away waits for the build.

Not in freeze mode nothing changes: the page reloads.

### Measured

Save (file write) to the change visible in the page (a `MutationObserver` in the page,
Playwright), freeze mode, a view function edit:

| | counter example app | the large app login page (920 modules) |
| --- | --- | --- |
| `--memory`, hot patch | 41-169ms (patch 0.9-1.4KB gzipped, 4-6 statements) | 557, 584, 614ms (patch 1.9KB, 5 statements, 8.6KB); 560, 605ms on 1957416e |
| default, full hot reload | | 2,030, 2,046ms (49MB, 8.4MB gzipped); 2,085ms on 1957416e |
| after a ToFrontend change (kernel codecs changed, full) | 166ms | |

On the large app the 0.56-0.6s is the rebuild (0.43-0.46s), working out the patch on the server
(40ms, diffing two 34,000 statement listings) and applying it. State was kept in every case
(counter value, form contents, the backend model).

the large app itself always reloads the page right after any hot reload, patch or full: its stored
FrontendModel does not decode with its own codecs (checked in the page by decoding the
`localStorage` copy through the hook: `Nothing`, with no patch applied), so the harness's
model check reloads. That is the same with the full hot reload, and is the large app's (or the
harness's) to look at; the times above are to the change being visible, before that reload.


## Measurements (the large app, 920 modules, final branch)

Machine: Apple Silicon, 16 cores, 128GB. Each run (`scratchpad/lm/livebench.py`) starts
`lamdera live` with `LAMDERA_LIVE_TIMING=1 +RTS -T`, waits for the first build, then twice:
an interface change to `src/Types.elm` (an unused top-level value appended; 391 modules
recompile) and a body-only change to `src/Palette/Alert.elm` (a CSS class string; the
module and the harness recompile). It then reverts both, lets it rebuild, and stops it with
SIGINT. Times are what the server reports from the start of the rebuild to the finished
page; the wall time from the file write to that line is 0.02-0.15s more. Warm runs start
from the previous run's elm-stuff, cold runs with none (package artifacts warm). The JS of
every step was compared by sha1 (without the hot patch hook and the build number): the
same in both modes at every step. Load average 4-6.

| | default `lamdera live` | `lamdera live --memory` |
| --- | --- | --- |
| first build, cold (921 modules) | 3.95s, peak in use 3.3GB | 3.73s, peak in use 3.7GB |
| first build, warm elm-stuff | 1.30s | 1.48-1.51s |
| Types.elm interface change, 391 modules (cold run; warm run) | 2.39, 2.50; 2.38, 2.55s | 1.93, 1.93; 2.38, 1.93s, then 1.92-2.05s over 20 more |
| of which compiling (`build`) | 1.20-1.32s | 0.95-1.06s (1.37 the first time) |
| of which type hash check | 0.19-0.21s | 0.09-0.10s |
| of which JS (`objects` + `rest`) | 0.98-1.03s | 0.85-0.91s |
| leaf body change, 2 modules (cold run; warm run) | 1.28, 1.92; 1.26, 1.29s | 0.44, 0.45; 0.67, 0.45s, then 0.45-0.49s over 20 more |
| of which compiling / type hash / JS | 0.04-0.06 / 0.19-0.21 / 1.0-1.03s | 0.06 / 0 / 0.37-0.43s |
| the same, slow file system (300us per stat/open; measured on 1957416e) | 2.11s (0.56 / 0.55 / 1.0s) | 0.52s (0.06 / 0 / 0.45s) |
| live heap after GC, over 22 rebuilds (with the two heap fixes) | 50-134MB | 1.29-1.51GB |
| RTS peak in use over 22 rebuilds | | 5.4GB |
| exit | | flush 0.2s (warm), 1.75s after a cold start (920 + 920 files) |

The 1.92s and 0.67s are one-off outliers (load 5-6). Against compile-perf 1957416e the same
table was: cold 4.9 / 5.1s, Types change 2.8-3.0 / 2.25-2.65s, leaf edit 1.2-1.26 /
0.45-0.46s.

The numbers of the earlier trial merge with compile-perf at cd2ce8cb are discarded: that
commit alone is buggy (fixed by 5952a85d).

How the memory mode's leaf edit came down on the rebased branch (one run each, `scratchpad/lm`
binaries):

| | leaf body change | of which JS |
| --- | --- | --- |
| rebased, before this round's changes (`rb1`) | 0.70-0.73s | 0.57s |
| sources in memory (`src1`/`src2`) | 0.71-0.78s | 0.58-0.63s |
| scheduler sizes and type hashes from the table and the build (`src3`) | 0.71-0.74s | 0.55-0.61s |
| Wire3 codecs kept (`src4`) | 0.56-0.57s | 0.41s |
| type hashes skipped when unchanged (`src5`) | 0.44-0.50s | 0.38-0.43s |
| hot patch listing (final) | 0.45-0.46s | 0.39-0.40s |


## Tests

- `Test.ElmiTypeTable`: a case for `I.share` (50 annotations each with their own Model tree
  come back equal, encode to the same bytes, and all 100 Model occurrences are one value).
- `Test.LiveMemory` (24 checks): two copies of a Lamdera app built the way live builds
  them, one with a registered store; identical JS (but for the hot patch hook) and identical
  recompiled module sets (and the expected ones) after: first build; no change; leaf body
  change; leaf interface change; a dependency's arity changing behind an unchanged
  interface, and back; a new constructor in Types (new Wire3 codecs), with the importers not
  parsed again and the type hashes equal to a separate build of Types; a type error and its
  fix; an edit the watcher did not report, not seen until a reconcile, then seen by both; a
  directory event forgetting every kept source, after which the disk is looked at again;
  touching elm.json; more rebuilds; nothing written to elm-stuff while registered; a flush
  that a disk build finds up to date; a new store starting from it that reads nothing from
  disk once warm.
- `Test.HotPatch` (8 checks): a Platform.worker app with ports, built in memory after each
  edit, the patch from the build before checked for what it holds and run in node on top
  of the first build (which then must answer like the latest): a changed value (patched,
  with the values using it worked out again, functions left alone), a changed function and
  a new definition, patches from two builds back, an empty patch for an unchanged build,
  and full reloads for a new port, new kernel code (Bitwise) and a build no longer kept.
- Full `stack test` (scratch `ELM_HOME`): 158 passed, 7 pending (the same 7 as
  compile-perf: the two TestMigrationGenerator e2e scopes, two Test.Check scopes,
  Test.Ext.ElmPages.Check isWireCompatible, Test.Lamdera, Test.WebGL), no failures.
- Manually, with Playwright: the counter example app and the large app in freeze mode, edits to a
  value, a view function, a new constructor with a compile error and its fix (error dialog,
  then removed by the next patch), a ToFrontend change (full hot reload, then patches into
  the new scope), clicks working after each patch, state kept throughout.


## Known limitations

- **JS generation is now most of a memory rebuild** (0.38s of 0.46s on the large app): every build
  still builds the global graph of 58,000 globals from the per-module graphs, runs
  `graphModificationsCached` over it, and walks all 34,000 reachable definitions (cache
  lookups and arity checks for each). See Next steps.
- **Memory**: the live heap is 0.7GB after a warm start, 1.3GB once the first Types change
  has filled the store, and levels off at 1.4-1.5GB (measured over 22 rebuilds). Two leaks
  were found on the way, with `+RTS -hT` heap profiles by closure type: compile-perf's type
  checker keeps two process-wide tables keyed by the identity of type objects
  (`sharedBodies`, and `canonicals` from 871997c5), which a long-running live never emptied
  (~85MB per Types change, both modes); and the hot patch history was trimmed with a lazy
  `take`, so every build held on to all earlier ones (and each kept Dynamic its whole JS
  cache) until a page asked for a patch (~120MB per Types change). Live now empties the
  tables before each rebuild (`Type.Type.resetSharedTables`; rebuild times unchanged) and the
  history is forced when a build is recorded.
- **A Ctrl-C is sometimes lost.** Seen once in about 20 normal runs of this round, idle, 3s
  after the sixth rebuild (the second one worked, and the flush ran), and every time with
  `+RTS -hT -i2` (which forces a major GC every 2 seconds); five idle trials after one build
  were all handled. So it is still the major GC window: `LAMDERA_LIVE_TIMING`'s heap figure
  forces one after every rebuild too.
- **the large app's FrontendModel does not decode with its own codecs** (the copy the harness keeps
  in `localStorage`, decoded in the page through the hook, gives `Nothing` before any patch),
  so in freeze mode the harness reloads the page after every hot reload, patch or full.
  Pre-existing and the same on both paths; the counter app keeps its state.
- **Hot patches only in freeze mode**, as Simon's hot reload is: outside it the page reloads
  as before.
- **What forces the full hot reload**: any kernel change (on the large app every change to a type
  that crosses the wire, since its codecs are kernel nodes), ports, effect managers, changed
  constructors, cycles of values. Each of those could be patched with more care (codecs are
  plain functions underneath, a constructor change is safe when no value of it is alive).
- **The source table** trusts the watcher plus a 5s reconcile and the page-load reconcile;
  an edit both miss is not seen until one of them runs. On a slow file system the reconcile
  costs about as much as a default build's crawl, every 5s (1.2-1.6s at 300us per stat, in
  the background).
- Ctrl-C is the only exit that flushes. SIGTERM, SIGHUP or a crash skip it; elm-stuff is
  then the stale but consistent state of the last disk build.
- A build that throws drops the whole store; the next rebuild reloads from elm-stuff.
- A concurrent `lamdera make` on the same project is safe but not seen by the memory mode.


## Next steps

1. **Incremental global graph.** Keep the merged graph (and the graph after
   `graphModificationsCached`) in the store and update it per changed module, instead of
   rebuilding 58,000 entries every build; then walk from the changed globals only (the
   listing gives the order). That is most of the remaining 0.38s.
2. **Patch more**: Wire3 codec changes (send the changed kernel chunks for codecs, which are
   self-contained functions), new ports (register them through the Platform kernel's hot
   reload path), constructor changes when the model check passes.
3. **Skip the full page render** when every connected page takes patches: the page is
   rendered and kept every build (cheap now, 10ms, but 49MB held per version).
4. **Bring the type hash change to the default** (`buildCheckHashesFrom` without the skip
   would save the default ~0.2s per rebuild).
5. **Smaller retained objects** (intern names and globals as `I.share` does types), and
   compact regions once the Wire3 IR is no longer pinned bytes.
6. **Flush on SIGTERM/SIGHUP**, and find the remaining lost Ctrl-C window.
7. **The MCP/LSP daemon** below.

### Relation to the compile-perf work

This branch is rebased onto compile-perf f9b569c4. compile-perf's `perf` labels wrap the
memory mode's `.elmo`/`.elmi` calls in `Build.compile`; its scheduler (`Lamdera.Schedule`)
gets its module sizes from the source table in memory mode; its IR reader is what the codec
cache builds on.

The second rebase (from 1957416e, 8 commits: the registry skip 8402515d, `-maxN12`
e5e27a68, the generalisation and unify fixes, the type table hash table 0bdc1936, and
4396f9a7) had one conflict, in `Build.compile`. 4396f9a7 decides whether a recompiled
module's interface changed by encoding it and comparing with the `.elmi` file's bytes. In
memory mode there is no old file to compare with (the kept interface is a value, and the
file on disk can be any number of builds old), so `Build.compile` now has two paths: with
a registered store, the kept interface is compared as a value and kept when equal (as
before), and the new one is hash-consed into the store when not; without one, compile-perf's
byte comparison and write, unchanged. `Test.LiveMemory` builds the same edits both ways and
checks that the same modules recompile after every step (an interface change whose
importer's interface stays the same included).


## Plan: an MCP and LSP daemon on the memory store (from Matt Griffith's elm-dev work)

The point of a fast in-memory build is low latency for agentic edit/check loops (and
editors): "does this compile, and what are the errors and types" in tens of milliseconds
for a body edit, without generating 49MB of JS. elm-dev (Matt Griffith, BSD-3-Clause, at
d9395458) has the daemon, MCP server and LSP around Mario's 2022 memory mode. What follows
is what it exposes, what maps onto this branch's store, and what it would take. Nothing of
it is implemented here.

### What elm-dev exposes

- **One global daemon** (`Watchtower.Server.Daemon`, state in `~/.elm/daemon/state.json`,
  started on demand by `elm-dev mcp`/`elm-dev lsp`), with three local TCP ports (LSP, MCP,
  the dev HTTP/websocket server). Editors and agents talk stdio to a small Rust proxy that
  frames JSON-RPC onto the TCP port. Projects are discovered by walking the workspace for
  `elm.json`; each has an id, and MCP keeps a focused project per connection.
- **MCP** (`Watchtower.Server.MCP`, ~4.5k lines with protocol, docs rendering and guides):
  8 tools, each taking an optional project `dir`: `check` (diagnostics for the project,
  a file or a module, with warnings; also starts and polls background test runs), `docs`
  (batched queries: project, modules, module, file, value, search, package), `install`,
  `dependencies` (install, uninstall, unused, tree, size, upgrade), `test_install`,
  `generate_scaffold_app`, `generate_scaffold_package`, `add` (pages, stores, effects from
  elm-dev's app templates). Resources: an overview, the projects, architecture and style
  guides, file/module/value/package docs, and a module graph (imports, exports,
  dependents). No prompts. No tool for the type at a position, definitions or references.
- **LSP** (`Watchtower.Server.LSP` and helpers, ~3.9k lines): what works is document sync
  (incremental changes kept in memory, unsaved buffers compile), push and pull diagnostics
  (errors, warnings such as unused imports and variables, test compile errors, unused
  modules), hover (types and docs), definition, references (global ones by scanning every
  module's canonical AST), a code lens to add a missing type signature, and formatting
  (the `elm-format` binary). Completion, signature help, rename and a few more answer with
  placeholder data; symbols, semantic tokens and folding are not offered.
- **Per-file analysis captured during compile** (2025-08/09, in the vendored
  `Ext.MemoryCached.Build.compile`): per module a `FileInfo` with the warnings (which stock
  Elm drops), the module's `Docs`, its `Localizer`, its canonical AST, and a hover index
  (region to type annotation) from a second constraint solving pass with a patched
  `Type.Solve.runTypeAt`, lazily. Kept in one `TVar (Map FilePath FileInfo)`; a module that
  was not recompiled gets no new entry, so the first compile recompiles everything to fill
  them. Package docs come from the package cache's `docs.json`.
- **Freshness**: per project an `fsVersion` (bumped by edits, watcher events and a full
  rescan of the sources, `updateVfsFromFs`) and a `compileVersion`; MCP requests rescan
  and compile only when stale. Editor buffers live in the file cache and are not
  overwritten from disk while open.

### What maps onto this branch

| elm-dev | here |
| --- | --- |
| byte VFS for sources and elm-stuff (Mario's `FileCache`/`FileProxy`) | the store: decoded interfaces and objects, `Details`, the source table |
| unsaved editor buffers in the VFS | a source overlay in the source table: `Present` with the buffer's bytes and a bumped time, marked open so neither the watcher nor the reconcile replaces it (new, small) |
| `fsVersion`/`compileVersion` | the source table's changes and the build numbers (`jsVersion`) already there; make the source side a counter too |
| canonical AST, docs, localizer, warnings per file | not kept yet: `Compile.compile` has the canonical module, annotations and (once kept) warnings for every fresh module; the parsed module is already in the source table, so the localizer and imports are free |
| the hover index (second solve pass) | needs the same pass on compile-perf's solver, which differs a lot from 0.19.1's |
| the dev server, websocket and hot reload | `lamdera live` itself (and its hot patches) |
| the daemon and project discovery | `lamdera live --memory` is already a long-running server per project, with the store |

Two things are better here than in elm-dev: an unchanged module keeps its analysis from
the build that made it (elm-dev drops it and recompiles everything to fill it), and the
store already knows exactly which modules a check recompiled.

### What it would take

1. **A check-only build** on the store: crawl and compile (with overlays), no JS, no type
   hash unless Types changed. On the large app that is the `build` phase: 0.05s for a body edit,
   1.2-1.7s for a Types interface change. A check of proposed content that is not saved
   puts it in the table as an overlay and, after the check, puts the disk version back with
   a bumped time, so the next real build recompiles that module from disk. 1 day.
2. **Per-module analysis kept with each module's store entry**: canonical module,
   annotations, warnings, docs, built from `Compile.compile`'s results for fresh modules and
   on demand for a module that has none (re-check it from its kept parse, no codegen). Cost
   to measure: canonical ASTs for 920 modules could be several hundred MB on the large app; keep
   them lazily or only for files an editor or agent has asked about. 2-3 days.
3. **MCP over HTTP on the live server** (`/_mcp`, JSON-RPC 2.0 with the streamable HTTP
   transport, so an agent connects to the running `lamdera live` with no proxy). Tools:
   `check` (project, file or proposed content; errors as Lamdera's JSON reports, compact),
   `type_at`, `definition`, `references`, `docs` (project modules from the kept docs,
   packages from `docs.json`), `module_graph` (from the `Details` locals), and the Lamdera
   ones elm-dev cannot have: Wire/Evergreen status (the type hashes and what `lamdera check`
   would say), evaluating an expression (the REPL endpoint is already there), the backend
   model and the live log. Port from elm-dev: the protocol module, the docs rendering and
   the definition/references lookups; leave out scaffolding, templates, the dependency
   manager and the test runner. 3-4 days on top of 1 and 2.
4. **The type at a position**: port `runTypeAt` to compile-perf's solver (it records the
   type of every region while solving). 3-5 days, the riskiest part. Top-level and annotated
   values work from the annotations alone, so `type_at` can start with those (half a day).
5. **LSP** as `lamdera lsp` (stdio): if a `lamdera live --memory` is running for the
   project, bridge to it (the live server would serve LSP over its websocket or a local
   socket); otherwise run the store in process. Document sync into overlays, diagnostics
   (push and pull), hover, definition, references, the add-type-signature code lens. Port
   the LSP protocol types and helpers and `Watchtower.AST.{Lookup,Definition,References}`
   to 0.19.3's canonical AST. 4-6 days.
6. **Tests** (elm-dev runs them in a node subprocess, as background jobs): later.

So a useful MCP for agents (1-3, with the annotation-only `type_at`) is about a week and a
half; full hover and an LSP about two weeks more. What to watch: the memory cost of keeping
canonical ASTs, compile-perf's solver changes for the hover pass, and keeping the
store the single source of truth when an editor, an agent and the file watcher all change
sources (one overlay per path, the newest wins, the watcher never overwrites an open
buffer).
