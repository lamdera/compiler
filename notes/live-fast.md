# `lamdera live` fast by default, `--cores`, and Elm 0.19.3 upstream compared

Branch `live-fast` (worktree `~/dev/projects/lamdera-compiler-live-fast`), from
`live-memory` 1454522b (which is on `compile-perf` 4d4e7b72). Not pushed, not merged.
Read `notes/live-memory.md` first for the store, the JS cache, the codec cache, the type
hash skip and the hot patches this builds on.


## Summary

- **Plain `lamdera live` now gets every live-memory win.** The store that `--memory` used
  is now registered by every `lamdera live`, with a backing: `OnDisk` (the default) writes
  elm-stuff exactly as before and keeps the decoded values too, each with the stamp
  (mtime to the nanosecond, size, inode) of the file it was written to or read from;
  `InMemory` (`--memory`) keeps artifacts and sources only in memory and writes elm-stuff
  on exit, as before. The JS cache, the Wire3 codec cache, the type hashes from the build
  (skipped when unchanged), the resolved package Details and the hot patches only need the
  long-running process, so they work in both.
- **Numbers (the large app, 920 modules, two runs each, load 6-15 from other work on the
  machine).**

  | | default before (live-memory tip) | default now | `--memory` now |
  | --- | --- | --- | --- |
  | first build, cold | 3.69s | 3.87-3.93s | 4.05-4.30s |
  | first build, warm elm-stuff | 1.41s | 1.56-1.60s | 1.59s |
  | Types.elm interface change (391 modules) | 2.55-2.71s | 2.00-2.19s (2.44-2.51 the first after a warm start) | 2.10-2.17s (2.47-2.51 the first) |
  | leaf body change (2 modules) | 1.33-1.40s | 0.45-0.49s | 0.49-0.54s |
  | view edit to visible, freeze mode | 2.14-2.19s (full hot reload) | 0.51-0.55s (hot patch; 0.71 the first) | 0.55-0.56s (0.62 the first) |
  | live heap after a GC | 50MB | 0.69-1.0GB | 0.98-1.35GB |
  | peak RSS over the run | 3.0-3.35GB | 4.35-4.6GB | 4.6-5.1GB |
  | idle CPU (40s, after the first build) | (0% on live-memory) | 1.4-1.5% | 2.0-2.2% |
  | exit | 0.07-0.13s | 0.12s | 0.34-0.39s warm, 2.05-2.2s cold (the flush) |

  The JS is identical in every mode and step (sha1 with the hot patch hook and build
  number taken out, and `Test.LiveMemory`).
- **So `--memory` is no longer faster than the default on a local SSD**; it is slightly
  slower and holds more (the source table and parses). What it still has over the
  default: no elm-stuff writes while running, and no stats on a slow file system (a VM
  shared folder, a network mount: 0.52s vs 2.1s per leaf edit at 300us per stat, measured
  on live-memory). The type hash check, codec cache and JS reuse, which were most of the
  gap, are now in both.
- **Another process changing elm-stuff is noticed** (a `lamdera make` in another terminal,
  a checkout, `rm -rf elm-stuff`): d.dat's stamp is checked at the start of every build
  and, when it moved, everything kept is dropped and the build starts from disk; a single
  artifact whose stamp moved is read again. Either way the hot patch history is dropped,
  so open pages get the full reload. `LAMDERA_LIVE_SESSION=0` turns the session off
  (the old default behaviour, for comparison or as an escape hatch).
- **`--cores N`** on live, make, check, deploy, install, repl, annotate and eval, and
  `LAMDERA_CORES=N`; the flag wins. Default unchanged (all cores up to 12), but on Linux
  lowered to the cgroup CPU quota (v2 `cpu.max`, v1 `cfs_quota_us`/`cfs_period_us`) when
  that is smaller. Lower OS priority for live's rebuilds was measured and left out (see
  Cores).
- **Against official Elm 0.19.3** on five open-source plain Elm apps (33 to 183 modules),
  see Upstream. In short: on ordinary apps compile-perf is at parity
  with official Elm in compute (0.13-0.31s from scratch offline against 0.15-0.34s,
  incremental 0.12-0.30s against 0.15-0.28s), not ahead of it; what it beats upstream on
  is the registry request on a fresh elm-stuff (about 1.1s, skipped), and 0.5GB less
  memory (RTS options; official Elm run with them matches). Against the v1.5.0 base it
  removes Lamdera's overhead (the base was 1.2-2.7x slower than official Elm). Two of the
  five apps do not build with `lamdera make` because of pre-existing Wire3 bugs (a
  phantom type parameter in elm-fontawesome; a runaway type check, 22-47GB, on nested
  extensible record aliases in concourse); both build with `--no-wire`.
- **Tests.** `Test.LiveMemory` (31 checks, was 24) builds a third copy with an OnDisk
  store; `Test.HotPatch` runs for both backings (16, was 8); `Test.Cores` (14) is new.
  Full `stack test` (scratch `ELM_HOME`): 187 passed, 7 pending (the same 7 as
  live-memory and compile-perf), no failures. `perf/verify.sh` with the final binary
  (package artifacts from scratch, the large app, camp, testapp, bug, errs: 2,088 files)
  identical to compile-perf 4d4e7b72's (`scope1`).
- **Credit.** The store and the source table revisit Mario's own in-memory compile mode
  (elmx's FileCache, later in elm-dev); the hot patches build on Simon Lydell's freeze
  mode hot reload; the MCP/LSP plan below comes from Matt Griffith's later elm-dev work
  (see live-memory's notes for what came from where).
- **Commits** (on 1454522b): 88f8eac7 the session by default; e759e622 `--cores`;
  4d757b1b comments; this note.


## The live session in both modes

### What changed

`Ext.MemoryCache.Store` gets a `Backing`:

| site | no store (make, check, ...) | `OnDisk` (plain `lamdera live`) | `InMemory` (`--memory`) |
| --- | --- | --- | --- |
| `Elm.Details.load` | decode d.dat | the kept Details while elm.json's time and d.dat's stamp are unchanged, else from disk (store reset) | the kept Details while elm.json is unchanged |
| `Details.write` (d.dat) | write | write, then keep with the new stamp | keep, write on exit |
| `Build.compile` `.elmo` | write | write, keep with stamp | keep, write on exit |
| `Build.compile` `.elmi`, interface changed | write | write, keep `I.share`d with stamp, the shared value goes on in the build | keep shared, write on exit |
| `Build.compile` `.elmi`, interface the same (bytes equal) | nothing | the kept value when its stamp still matches (so it stays the very same value), else keep this one shared | the kept value (`keepSame`) |
| `.elmi`/`.elmo` reads (`loadInterface`, `loadObject`, `loadTypesHelp`, `cachedHelp`) | decode | stat; the kept value when the stamp matches, else decode and keep | the kept value, else decode once and keep |
| sources (crawl, scheduler sizes) | disk | disk | the source table |

Everything above the store is the same in both: `Generate.dev` runs `generateCached`
(JS of unchanged definitions reused by pointer equality of their nodes, and the Wire3
codecs kept by content), the type hashes come from the build and are skipped when every
interface they read is the same value as last time, every build is listed for hot
patches, and the page gets the patch hook and its build number. Develop starts the
session (`Live.startSession`) unless `LAMDERA_LIVE_SESSION=0`, and with one rebuilds go
through `Sentry.updateJsOutputNow` (no forced major GC, output taken before clients are
told). Only `InMemory` runs the 5s source reconcile and the exit flush.

### Why the stamps are enough

OnDisk the store only ever holds what a file decodes to. A kept value is used only while
the file still has the stamp it had right after this process wrote it (writes are
synchronous, `File.writeBytes`, under the root lock) or right before it read it. A
writer that is not this process changes the stamp: APFS and ext4 keep nanosecond
mtimes, and an in-place rewrite keeps the inode but moves the mtime, a replace changes
the inode. Everything derived from a kept value is self-validating anyway: the JS cache
compares nodes by pointer, the codec cache by content, the hot patch diff by the emitted
bytes. So a stale value can only come from a stamp that did not move, which needs a
rewrite within the same nanosecond with the same size.

Per rebuild that costs one stat per artifact read (on the large app about 920 `.elmo` for the JS
and ~250 `.elmi` for the type hashes): about 10ms locally, part of the "objects" phase.

What is checked where:

- d.dat, once per build in `Details.load` (`detailsStillOnDisk`): a moved stamp means
  someone else built (or removed elm-stuff). The store is reset (modules, Details, JS
  cache, codec cache) and the build starts from disk as a fresh process would. Counted
  (`externalChanges`) and the hot patch history dropped.
- each artifact, on read (`readKept`): a moved stamp means a writer that did not get as
  far as d.dat (an interrupted `lamdera make`); that artifact is read again. Counted
  (`_staleReads`, and `externalChanges`) and the hot patch history dropped.
- elm.json's time, as before (store reset).

A known gap: package artifacts in `~/.elm` are not stamped. They are only rewritten when a
package is built again, which happens together with a d.dat rewrite in the project that
asked for it, but another project rebuilding a shared package is not seen. Same as
before this branch (the Details have always been reused by `--memory`), and a package
artifact for a given version is the same bytes whoever builds it.

### Tests

- `Test.LiveMemory`: three copies of the Lamdera app (InMemory store, OnDisk store, no
  store), the same edits to each, identical JS (no store vs. the sessions modulo the hook,
  InMemory vs OnDisk byte for byte) and identical recompiled modules after every step.
  New checks for OnDisk: elm-stuff written as usual; a rebuild with nothing changed reads
  nothing from disk; the type hashes taken from the build equal a separate build's;
  another process's build (the store suspended while the same project is built in the
  same process) is noticed, the session's next build compiles only the harness (it finds
  Leaf compiled by the other build) and a page on the earlier build gets `full`; an
  `.elmo` rewritten behind its back is read again (exactly one stale read); elm-stuff
  removed: the next build compiles everything. 31 checks.
- `Test.HotPatch`: the same 8 checks for each backing.

### Measurements

`scratchpad/lf/livebench.py` (from live-memory): starts `lamdera live` with
`LAMDERA_LIVE_TIMING=1 +RTS -T`, waits for the first build, then twice: a new top-level
value in `src/Types.elm` (391 modules recompile) and a class string in
`src/Palette/Alert.elm` (2 modules), then reverts, rebuilds and stops with SIGINT. Cold
runs start without elm-stuff (package artifacts warm). Apple Silicon, 16 cores, 128GB;
the machine was shared with other work (load averages 6-15 during the runs, given per
run in `scratchpad/lf/livebench-results.log`). Each configuration ran twice, the modes
alternating.

| run | first build | Types change | leaf change | heap after GC | peak RSS |
| --- | --- | --- | --- | --- | --- |
| default, cold, 1 | 3.93s | 2.11, 2.10s | 0.49, 0.45s | 0.72-0.81GB | 4.40GB |
| default, cold, 2 | 3.87s | 2.19, 2.08s | 0.45, 0.46s | 0.72-0.81GB | 4.42GB |
| `--memory`, cold, 1 | 4.05s | 2.12, 2.12s | 0.49, 0.51s | 0.98-1.06GB | 4.62GB |
| `--memory`, cold, 2 | 4.30s | 2.16, 2.11s | 0.53, 0.52s | 0.98-1.06GB | 4.58GB |
| default, warm, 1 | 1.60s | 2.44, 2.12s | 0.47, 0.46s | 0.69-0.99GB | 4.57GB |
| default, warm, 2 | 1.56s | 2.51, 2.00s | 0.49, 0.49s | 0.69-0.99GB | 4.52GB |
| `--memory`, warm, 1 | 1.59s | 2.47, 2.10s | 0.54, 0.53s | 0.70-1.35GB | 5.14GB |
| `--memory`, warm, 2 | 1.59s | 2.51, 2.17s | 0.53, 0.52s | 0.70-1.35GB | 5.12GB |
| live-memory tip, default, cold | 3.69s | 2.55, 2.71s | 1.33, 1.36s | 50MB | 3.35GB |
| live-memory tip, default, warm | 1.41s | 2.55, 2.70s | 1.40, 1.36s | 50MB | 3.07GB |
| this branch, `LAMDERA_LIVE_SESSION=0`, warm | 1.40s | 2.63, 2.65s | 1.35, 1.36s | 50MB | 3.00GB |

Where the time goes now, leaf edit, default: compiling 0.045s, type hashes 0.002s
(skipped), JS 0.39-0.42s (the global graph and the walk, as in memory mode), the page
0.015s. Before: 0.05 / 0.21 / 1.05s.

Notes on the numbers:

- The first Types change after a warm start is ~0.35s slower than later ones in both
  modes: the 389 importers recompiled for it have interfaces that were never read in this
  process, so they are hash-consed (`I.share`) the first time (OnDisk) or read and kept
  (InMemory). Later Types changes find them kept.
- `--memory` spends less in `build` on a Types change (1.03-1.13s against 1.37-1.43s: no
  encoding and writing of 391 `.elmo`) but more in the JS (0.89-0.99s against
  0.58-0.62s). Encoding forces the optimized objects in the parallel build; in memory
  they stay lazy until the single threaded JS generation forces them. The totals come out
  the same. Forcing them in the build (a deep `seq` of the local graph in `writeElmo`)
  would likely make `--memory` the faster of the two again on a Types change.
- The first warm build is 0.15-0.2s slower than before: the session resolves the package
  interfaces and objects once (`ArtifactsFresh`) and keeps them.
- Memory is the cost: the default `lamdera live` now holds 0.7-1.0GB live on the large app
  (every module's objects and the interfaces it read, the JS cache, the listings of the
  last 10 builds) where it held 50MB, and its peak RSS is 1.3GB higher. `--memory` holds
  0.3GB more again. `LAMDERA_LIVE_SESSION=0` gives the old footprint back.
- Idle (`idle.py`, 40s after the first build, no browser): 1.4-1.5% of a core by default
  and 2.0-2.2% with `--memory`, one major GC in the 40s (the `-Iw30` idle GC of a larger
  heap). Before, the default was 0% with its 50MB heap.

View edit to visible: the large app's login page in freeze mode (Playwright; a
`MutationObserver` in the page timestamps the edited text appearing; a route answers the
page's own reload with 204 so the page stays, because the large app's stored FrontendModel does
not decode with its codecs and the harness reloads after every hot reload, see
live-memory's notes). Save (file write) to visible:

| | per edit |
| --- | --- |
| default, hot patch (5 statements, 8.6KB, 1.9KB sent) | 706, 530, 513, 544, 532, 550ms |
| `--memory`, hot patch | 624, 556, 551, 558, 560ms |
| `LAMDERA_LIVE_SESSION=0`, full hot reload (49MB) | 2143, 2148, 2172, 2191ms |


## The seam for an MCP / LSP layer

Not built. The point of it: an agent or an editor asks "does this compile, what are the
errors, what is the type of this" against the same long-running build that `lamdera
live` already keeps, in either mode, without knowing which.

### Where the seam is

Today the session is spread over three places: `Ext.MemoryCache.Store` (artifacts, Details,
JS cache, listings, sources), `Develop` (the watcher, the rebuild, `Sentry` serialising
builds) and `Lamdera.CLI.Live` (start, flush, reconcile, hot patches). The layer above
should see one value, say `Lamdera.Session` (new module), with the store and the build
serialisation inside it:

```haskell
data Session  -- the store (either backing), a build lock, a sources version, the last results

open        :: FilePath -> Backing -> IO Session   -- what Live.startSession does now
close       :: Session -> IO ()                    -- InMemory: flush; OnDisk: nothing

-- what changed (the watcher, an editor's save, an agent's write)
changed     :: Session -> [FilePath] -> IO ()
-- unsaved content (an editor buffer, an agent's proposal); Nothing drops it
overlay     :: Session -> FilePath -> Maybe ByteString -> IO ()

-- crawl and compile only: problems (as Reporting values, rendered by the caller as
-- JSON or terminal text), which modules were recompiled, and the build's Artifacts
check       :: Session -> [FilePath] -> IO Checked
-- check, then the type hashes and the JS: what `lamdera live` serves today
build       :: Session -> FilePath -> IO Built
-- the hot patch from build N to the latest (Lamdera.HotPatch)
patchSince  :: Session -> Int -> IO Patch

-- read-only questions, answered from the kept values (no build)
interfaceOf :: Session -> ModuleName.Raw -> IO (Maybe I.Interface)
annotation  :: Session -> ModuleName.Raw -> Name -> IO (Maybe Can.Annotation)
moduleGraph :: Session -> IO (Map ModuleName.Raw [ModuleName.Raw])  -- from the kept Details
versions    :: Session -> IO (Int, Int)  -- sources version, build number
```

`Develop` becomes one client of it (watcher -> `changed` -> `build` -> tell the pages);
an MCP endpoint in the same process (`/_mcp`, JSON-RPC over the streamable HTTP
transport) another (`check`, the queries, `build` for "is the page up to date"); a
`lamdera lsp` (stdio) a third, either talking to a running `lamdera live` of the project
over a local socket or opening its own `Session` when none runs. Nothing in the
interface says which backing is used: that is only `open`'s argument.

### What the backings mean for it

- Same answers in both: everything is computed by the same builder with the same
  invalidation; the backing only decides where artifacts live (the tests check the JS and
  the recompiled modules are identical).
- Overlays need care OnDisk. An overlay's artifacts must not be written to elm-stuff,
  where `lamdera make`, `annotate` or `check` would take them for the file on disk. So a
  build with overlays runs on a scratch layer: an `InMemory` view over the `OnDisk` store
  (writes kept in a per-check map and in nothing else, reads falling through to the OnDisk
  entries), dropped when the overlay is. That is one more `Backing`-like case in the
  store's write sites, the same ones this branch touches. InMemory, overlays go into the
  source table as entries the watcher and the reconcile do not overwrite (live-memory's
  plan).
- External changes: a session client can see `externalChanges` move and say so ("elm-stuff
  was rebuilt by another process; rechecked"), instead of serving stale results.
- One build at a time: `check` and `build` take the session's lock (today `Sentry`'s
  MVar and the root lock); a check asked for during a rebuild waits and then usually finds
  nothing to recompile.

### What it builds on, and what it still needs

live-memory's plan for the daemon (from Matt Griffith's elm-dev MCP and LSP) stands: the
check-only build, per-module analysis kept next to each module's store entry (canonical
module, annotations, warnings, docs), the type at a position (port `runTypeAt` to
compile-perf's solver), MCP tools, then LSP. This branch removes the "only with
`--memory`" condition from it: those land on whichever backing the user runs.


## Cores

### Behaviour

`lamdera make|live|check|deploy|install|repl|annotate|eval --cores N`, or `LAMDERA_CORES=N`:

1. `--cores N` (1 to 1024) wins; it is also put into `LAMDERA_CORES` for lamdera
   processes started by this one.
2. Else `LAMDERA_CORES=N` (an invalid value is ignored).
3. Else the default: what the RTS started with (`-maxN12`: every core up to 12, or `+RTS
   -N` if given), lowered on Linux to the CPU quota of the process's cgroup when that is
   smaller: the smallest of cgroup v2 `cpu.max` over the process's cgroup and its
   ancestors, or cgroup v1 `cpu.cfs_quota_us`/`cpu.cfs_period_us` (rounded up, at least 1).
   GHC's RTS counts the cores it may run on (affinity), not a quota, so a container with 2
   CPUs on a 64 core host ran 12 capabilities, each with its own 128MB nursery.

`setNumCapabilities` applies it before the command runs (`Lamdera.Cores.withCores` wraps
each command in `Main`). `LDEBUG=1` prints what was chosen. The help of each of those
commands has `--cores=<number-of-cores>` with this explained. Checked on macOS
(`--cores=3` gives 3, `LAMDERA_CORES=5` 5, both given the flag, `+RTS -N4` 4); the cgroup
reading is tested against fake `/proc` and `/sys` trees (v1, v2, nested, namespace root,
no limits, no cgroups), in the layout the NixOS test VM shows for a `systemd-run -p
CPUQuota=150%` scope (`0::/system.slice/run-...scope` and `150000 100000` in its
`cpu.max`). Not run inside a real container here (no Linux build of this branch).

### Lower priority for live's rebuilds: measured, left out

`nice -n 10` on the whole `lamdera live` (a background thread's priority cannot be
lowered alone in a useful way: the rebuild runs on all capabilities), the large app, default
mode, two Types and two leaf edits each:

| | Types change | leaf change |
| --- | --- | --- |
| normal priority, machine as it was (load 6-7) | 2.42, 2.09s | 0.49, 0.48s |
| nice 10, same | 2.42, 2.16s | 0.49, 0.48s |
| normal priority, 16 busy loops besides | 4.28, 3.70s | 0.59, 0.67s |
| nice 10, 16 busy loops besides | 4.68, 4.28s | 0.75, 0.82s |

No difference on an idle machine, 10-25% slower rebuilds when the machine is busy, which
is the only time it would do anything. The rebuild is what the developer waits for, so
it stays at normal priority; `nice lamdera live` does it for anyone who wants it.


## Upstream: official Elm 0.19.3 compared

Question: what of the compiler work is a real improvement over upstream Elm on ordinary
apps, rather than shaped by the large app (920 modules, big Lamdera types, Wire3 everywhere)?

### Setup

- Compilers: official Elm 0.19.3 (github.com/elm/compiler release `elm-0.19.3-mac-arm.gz`,
  `elm --version` 0.19.3, GHC 9.10.3, RTS options `-N -qg -A128m`, sha256 `aab23c83...f137d`);
  `lamdera` at the v1.5.0 base b0153727 (`base`, `-N -qg -A128m` like upstream); `lamdera`
  at compile-perf 4d4e7b72 (`perf`, `-maxN12 -A128m` with the parallel GC). Both lamdera
  binaries rebuilt in their worktrees and checked with `--version-full`. `lamdera make`
  is the same code on live-fast as on compile-perf (`--cores` aside).
- To tell RTS tuning from compiler work, also: `elm-rts` and `base-rts` (the same
  binaries run with `+RTS -maxN12 -qg0`, compile-perf's options), and `perf-nowire`
  (`perf --no-wire`).
- Apps (open source, plain Elm, no Lamdera features), modules reachable from main:
  rtfeldman/elm-spa-example (33), gingko/client (62), concourse/concourse `web/elm` (116),
  NoRedInk/noredink-ui `component-catalog` (160), Lattyware/massivedecks `client` with its
  pinned elm-material (183). Each compiler builds its own copy with its own `ELM_HOME`.
  The same edits to every `elm.json`: `"elm-version": "0.19.3"` (official 0.19.3 refuses
  0.19.1 apps), and elm/core 1.0.5, elm/browser 1.0.2, elm/html 1.0.1, elm/virtual-dom
  1.0.5 (lamdera replaces those four with its own versions and needs exactly those;
  elm-spa-example pinned 1.0.0s).
- Steps (`scratchpad/upstream/scripts/upbench.py`), after a warm-up build that downloads
  and builds the packages: `scratch` (rm -rf elm-stuff, make, JS written), `scratch-offline`
  (the same with the network refused by a dead proxy, see below), `noop`, `leaf` (a string
  literal in the largest module only Main imports), `iface` (a new exposed value in the
  module with the most transitive dependents: 22 to 130 of them; recompiles its direct
  importers). Medians of 3 (scratch-offline: 5), the compilers alternating in each round.
  Wall time; peak RSS from `/usr/bin/time -l`. Load averages 5-15 (other work on the
  machine).

### Results

Medians; decks and concourse are built by both lamdera binaries with `--no-wire` (see
Lamdera-specific differences).

| app (modules) | step | elm | base | perf | elm-rts | perf-nowire | base-rts |
| --- | --- | --- | --- | --- | --- | --- | --- |
| spa (33) | scratch | 1.25s | 1.34s | 0.15s | 1.28s | 0.13s | 1.19s |
| spa (33) | scratch-offline | 0.15s | 0.20s | 0.13s | 0.12s | 0.13s | 0.18s |
| spa (33) | noop | 0.15s | 0.19s | 0.12s | 0.09s | 0.09s | 0.14s |
| spa (33) | leaf | 0.15s | 0.21s | 0.12s | 0.10s | 0.10s | 0.15s |
| spa (33) | iface | 0.16s | 0.20s | 0.12s | 0.09s | 0.10s | 0.14s |
| gingko (62) | scratch | 1.43s | 1.59s | 0.32s | 1.32s | 0.23s | 1.56s |
| gingko (62) | scratch-offline | 0.25s | 0.55s | 0.31s | 0.24s | 0.24s | 0.45s |
| gingko (62) | noop | 0.19s | 0.37s | 0.17s | 0.13s | 0.14s | 0.33s |
| gingko (62) | leaf | 0.20s | 0.41s | 0.20s | 0.14s | 0.16s | 0.35s |
| gingko (62) | iface | 0.28s | 0.57s | 0.30s | 0.20s | 0.22s | 0.45s |
| nri (160) | scratch | 1.25s | 1.58s | 0.26s | 1.33s | 0.24s | 1.53s |
| nri (160) | scratch-offline | 0.27s | 0.60s | 0.27s | 0.24s | 0.24s | 0.47s |
| nri (160) | noop | 0.17s | 0.45s | 0.17s | 0.16s | 0.17s | 0.39s |
| nri (160) | leaf | 0.17s | 0.46s | 0.17s | 0.16s | 0.17s | 0.42s |
| nri (160) | iface | 0.19s | 0.47s | 0.19s | 0.18s | 0.20s | 0.41s |
| decks (183) | scratch | 1.40s | 1.45s | 0.30s | 1.30s | n/a | 1.55s |
| decks (183) | scratch-offline | 0.34s | 0.42s | 0.31s | 0.32s | n/a | 0.36s |
| decks (183) | noop | 0.15s | 0.19s | 0.13s | 0.14s | n/a | 0.17s |
| decks (183) | leaf | 0.15s | 0.20s | 0.13s | 0.13s | n/a | 0.17s |
| decks (183) | iface | 0.20s | 0.23s | 0.16s | 0.17s | n/a | 0.21s |
| concourse (116) | scratch | 1.27s | 1.40s | 0.21s | 1.21s | n/a | 1.34s |
| concourse (116) | scratch-offline | 0.25s | 0.32s | 0.22s | 0.23s | n/a | 0.30s |
| concourse (116) | noop | 0.15s | 0.22s | 0.13s | 0.13s | n/a | 0.20s |
| concourse (116) | leaf | 0.16s | 0.24s | 0.14s | 0.13s | n/a | 0.21s |
| concourse (116) | iface | 0.19s | 0.26s | 0.17s | 0.17s | n/a | 0.24s |

Peak RSS, from scratch: official 2.14-2.17GB, base 2.15-2.28GB, perf 1.61-1.68GB,
elm-rts 1.62-1.70GB, base-rts 1.62-1.75GB. JS output: lamdera's is 2-14% larger than
official Elm's, from the replaced packages and Lamdera's injections (the Wire3 codecs of
a plain app are not reachable from main, so `--no-wire` gives byte identical JS); base
and perf JS identical on every app.

### What it says

- **From scratch, the 1.1s is the network.** Official Elm and the base both ask the
  package server for the registry when there is no elm-stuff (1.0-1.2s here); with the
  network refused they fall back to the cached registry and take 0.15-0.34s (official) and
  0.20-0.60s (base). compile-perf's registry skip (8402515d, ask only when the cached
  registry cannot answer) is why `perf` is 0.15-0.32s either way. That is a real
  improvement over upstream for everyone, worth about a second per fresh elm-stuff (CI, a
  new checkout, after `rm -rf elm-stuff`), more on a slow network.
- **Compute: compile-perf is at parity with official Elm on ordinary apps, not ahead.**
  Offline from scratch perf is 0.13 / 0.31 / 0.27 / 0.31 / 0.22s against official's 0.15 /
  0.25 / 0.27 / 0.34 / 0.25s; incremental builds are 0.12-0.30s against 0.15-0.28s. Given
  compile-perf's options, official Elm is as fast or a little faster (elm-rts: noop and
  leaf 0.09-0.16s, against perf's 0.12-0.20s). So the type checker, interface and
  scheduler work of compile-perf shows on large-app-shaped code (huge records and aliases,
  hundreds of modules recompiled at once), not on these.
- **Against the base, compile-perf removes Lamdera's overhead**: base was 1.2-2.7x slower
  than official Elm on the same apps (gingko and nri most, both Wire3 heavy; noop on nri
  0.45s against 0.17s), perf is back at official's times. Of what is left, Wire3 is the
  part that differs: perf with wire against `--no-wire` 0.31 vs 0.24s from scratch on
  gingko, 0.30 vs 0.22s for its iface step, little on spa and nri (0.13 vs 0.13s, 0.27 vs
  0.24s from scratch).
- **Memory**: the 0.5GB less peak RSS of perf is the RTS options (official Elm with
  them uses the same 1.6-1.7GB): `-N` with a 128MB nursery per core on 16 cores is 2GB of
  nursery for any build.
- **RTS options alone** (elm vs elm-rts, base vs base-rts): 5-40% faster incremental
  builds and 0.5GB less memory, for free. Upstream would get that by changing its
  `-with-rtsopts`; for Lamdera it is already in compile-perf.

### Lamdera-specific differences on plain Elm apps

- `lamdera make` generates Wire3 codecs for every type of every module, packages
  included, even with no Lamdera features in sight. Two of the five apps do not build
  with it, on the base and on compile-perf alike, and build with `--no-wire`:
  - **massivedecks**: the package lattyware/elm-fontawesome 6.0.0 fails to build (`TYPE
    MISMATCH` in `FontAwesome.Internal`: the generated `w3_unsafe_decode_Icon` takes a
    decoder for the phantom parameter of `type Icon hasId = Icon {...}` and is given
    `w3_unsafe_decode_WithId`). Official Elm builds it.
  - **concourse**: the type check of the generated code in `SideBar.SideBar` (extensible
    record aliases nested three deep, `Tooltip.Model (Favorites.Model { m | ... })`) runs
    away: 22GB after 60s on compile-perf, 47GB and killed after 400s on the base. Found
    with `LAMDERA_PERF_LOG` (the one module with `wire3_generate` and no `typecheck`).
    Official Elm builds the app in 0.25s.
  Both are pre-existing Wire3 bugs, not from this or the compile-perf work; worth fixing
  on their own (a non-Lamdera user of `lamdera make` hits them, and so would a Lamdera app
  with such types).
- `lamdera make` needs elm/core 1.0.5, elm/browser 1.0.2, elm/html 1.0.1 and
  elm/virtual-dom 1.0.5 exactly (it substitutes its own copies); apps pinned to older
  ones fail with "PROBLEM BUILDING DEPENDENCIES" naming elm/browser.
- Its JS is larger (Lamdera's injections, the replaced packages).


## Known limitations and next steps

- Memory: the default `lamdera live` now holds 0.7-1.0GB on the large app. Smaller kept objects
  (names and globals interned as `I.share` does types) would help both modes.
- `--memory` forcing its objects in the parallel build (see Measurements) is untried.
- Package artifacts in `~/.elm` are not stamped (see Why the stamps are enough).
- The incremental global graph (live-memory's next step 1) is still the largest part of a
  leaf rebuild, now in both modes: 0.39-0.42s of 0.45-0.49s.
