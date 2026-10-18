# `lamdera live --memory`: an in-memory build for live

Status: design written before implementation; results and numbers are filled in below as
they land.


## Summary

(filled in at the end)


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


## Design

### Goal

`lamdera live --memory` (opt-in; without the flag nothing changes) keeps a project's build
artifacts in memory as decoded Haskell values across rebuilds:

- per module: its interface (`I.Interface`) and its objects (`Opt.LocalGraph`),
- the project `Details`, with the package interfaces and objects resolved
  (`ArtifactsFresh`), so `i.dat` and `o.dat` are decoded once per session, not per rebuild.

A rebuild then never encodes, writes, reads or decodes anything under
`elm-stuff/lamdera-*`. What still happens per rebuild: the crawl (one `stat` per module, a
read and parse of each changed file), compiling what changed and what depends on a changed
interface, and generating the JS.

### Shape: elm-stuff in memory, keyed by the stuff directory

Instead of vendoring builder (elm-dev's route), a small store, `Ext.MemoryCache`, is
registered for a project's stuff directory (`R.Stuff`). The handful of places in builder that
touch project artifacts consult it:

| site | disk | registered store |
| --- | --- | --- |
| `Elm.Details.load` | decode `d.dat` | in-memory `Details` (buildID+1) while `elm.json` is unchanged; otherwise the disk path, with `i.dat`/`o.dat` decoded once into `ArtifactsFresh` |
| `Build.writeDetails` | encode `d.dat` | keep the new `Details` |
| `Build.compile`: old `.elmi` | decode `.elmi` | store (else disk once) |
| `Build.compile`: write `.elmo`/`.elmi` | encode + write | store; a changed interface is hash-consed first (below) |
| `Build.loadInterface` | decode `.elmi` | store (else disk once, kept) |
| `Generate.loadObject` | decode `.elmo` | store (else disk once, kept) |
| `Generate.loadTypesHelp`, `Ext.Query.Interfaces.cachedHelp` | decode `.elmi` | store (else disk once, kept) |

Every build of that project in the process goes through the same store: the live harness
build, a manual compile of another `.elm` file from the browser (`serveElm_`), and the nested
`Interfaces.all` build that `Lamdera.TypeHash.buildCheckHashes` runs. So the semantics are
exactly those of elm-stuff, minus the codecs. Nothing above the builder changes:
`Develop.compile` (Details.load, Build.fromPaths, PostCompile.check, TypeHash, Generate.dev)
runs as before, with Wire3 injection inside `Compile.compile` as before.

The hooks cost one `IORef` read and one lookup in an empty `Map` when no store is
registered, which is every compile outside `lamdera live --memory`.

A store entry missing for a module means "not touched in memory yet": the disk artifact is
read (and kept). That is consistent because memory mode never writes project artifacts
during the session: a module compiled in memory is always in the store, and a module never
compiled in memory still has the disk artifacts that the disk `d.dat` (the base of the
in-memory `Details`) describes.

### Keep the retained interfaces small: hash-consing

The interface type table branch showed that type inference stores every alias fully
expanded, so a fresh interface is a tree with the same subtrees repeated thousands of
times (the large app: 707MB of `.elmi` before the table, 90MB after). Decoding a table-format
`.elmi` already rebuilds shared values, but an interface fresh from `I.fromModule` is the
unshared tree. Keeping 900 of those for a session would cost gigabytes.

So `I.share` (new, in `Elm.Interface`) rebuilds an interface with each distinct type node
allocated once, using the same node keys as the type table encoder. A changed interface
(`RNew`) is shared before it is stored and before dependents in the same build use it; an
unchanged one (`RSame`) keeps the already-shared old value and the fresh tree is dropped.

### Disk

- Package artifacts (`~/.elm/0.19.3/packages/.../artifacts.*.dat`) stay on disk, written and
  read as before; they are decoded once per session via `ArtifactsFresh`.
- On `elm.json` changes `Details.load` takes the normal path (verify, possibly build
  packages, write `d.dat`/`i.dat`/`o.dat`), and the module store is reset so nothing in it
  can disagree with the reloaded `Details`.
- Project artifacts are not written during the session. On a clean exit (Ctrl-C) the dirty
  modules are flushed: all `.elmo`, then all `.elmi`, then `d.dat`. That order keeps a
  partly written elm-stuff safe for a later normal build: a dependent's `.elmo` is never
  older than an interface written next to it, and the old `d.dat` makes the normal build
  re-check everything that changed. If the process dies instead, elm-stuff is simply the
  stale but consistent state of the last disk build, and the next start recompiles what
  changed since.
- The REPL worker in the live UI does not read Lamdera's project elm-stuff (it compiles the
  project itself under its own `elm-stuff/0.19.3` in an in-browser file system, and reads
  only the package artifacts from `~/.elm`), so it is unaffected.

### Failure handling

If a build throws (as opposed to reporting compile errors) the store and the in-memory
`Details` are dropped, and the next build starts from disk again. A module that fails to
compile leaves its store entry and its `Local` untouched, exactly like elm-stuff.

### Concurrency

Live already serialises rebuilds (`Ext.Sentry` takes the JS MVar) and builds take the root
lock. Within a build modules compile in parallel; the store is an `IORef (Map ...)` updated
with `atomicModifyIORef'` per module.

### Not changed

- The crawl still `stat`s every module per rebuild. elm-dev content-compares every source
  file instead (more robust to mtime games, more I/O); stats are cheap and also catch edits
  the watcher misses (git checkout, editors that rename).
- JS generation still builds the global graph from all objects and walks it from the mains.
- `lamdera make`, `check`, `deploy` and plain `lamdera live` are untouched.

### Relation to the compile-perf work

compile-perf (`~/dev/projects/lamdera-compiler-perf`) is profiling the same functions this
hooks (`Build.compile`, `loadInterface`, `Generate.loadObject`, `Details.loadObjects`); its
current diff is SCC annotations on those lines, so the two touch the same few lines and
merge by hand trivially. Anything it makes cheaper inside `Compile.compile` (canonicalize,
type check, Wire3, optimize) helps both modes equally; what this removes is the
per-rebuild codec and I/O work around it, which compile-perf's profiles should show
separately (`phase_elmi_load`, `phase_elmo_load`, `phase_pkg_*_load`, `phase_elmo_write`,
`phase_elmi_write`).
