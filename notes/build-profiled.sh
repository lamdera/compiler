#!/bin/bash
# Build a cost centre profiling lamdera binary (branch compile-perf, for measurement only).
#
# usage: notes/build-profiled.sh <dir>
#
# `stack build --profile` fails on this repo as it is: the whole compiler is one
# executable, and some of its modules use Template Haskell. This copies
# the working tree's tracked files to <dir>, turns the executable into a library plus a
# one module executable (Cabal builds a library the normal way before the profiled way,
# which leaves Template Haskell the objects it needs), and builds it with profiling and
# late cost centres (-fprof-late: cost centres go in after optimisation, so the
# profiled code is optimised like the real thing).
#
# Run it again after changing sources: only files that differ are copied, so the build
# is incremental. Times on a 16 core M-series Mac: the first run also builds profiled
# versions of all dependencies into the stack snapshot (~8 minutes, once per snapshot);
# then ~6.5 minutes for a new copy (the vendored elm-format packages included), ~1.5
# minutes after a change to a few type checker modules, seconds when nothing changed.
#
# Then: <dir>/.stack-work-prof/install/.../bin/lamdera make ... +RTS -pj -RTS writes
# lamdera.prof (JSON) in the current directory. A profiled build of the 920 module app
# takes ~10x as long as with the normal binary.

set -euo pipefail

if [ $# -ne 1 ]; then
  echo "usage: $0 <dir>" >&2
  exit 2
fi

SRC=$(cd "$(dirname "$0")/.." && pwd)
DEST=$1
mkdir -p "$DEST"
DEST=$(cd "$DEST" && pwd)

python3 - "$SRC" "$DEST" <<'PYEOF'
import os, re, shutil, subprocess, sys, filecmp
src, dest = sys.argv[1], sys.argv[2]

# Tracked files of the working tree and its submodules, as they are on disk.
files = subprocess.run(['git', '-C', src, 'ls-files', '-z', '--recurse-submodules'],
                       check=True, capture_output=True).stdout.decode().split('\0')
copied = 0
for f in files:
    if not f or f == 'elm.cabal':
        continue
    a, b = os.path.join(src, f), os.path.join(dest, f)
    if not os.path.isfile(a):
        continue
    if os.path.isfile(b) and filecmp.cmp(a, b, shallow=False):
        continue
    os.makedirs(os.path.dirname(b), exist_ok=True)
    shutil.copy2(a, b)
    copied += 1
print('copied %d changed files' % copied)

# elm.cabal: the executable becomes a library (same modules, exposed), and a new
# executable holds only Main, from prof-main/.
s = open(os.path.join(src, 'elm.cabal')).read()
start = s.index('Executable lamdera')
end = s.index('Test-Suite lamdera-tests')
exe = s[start:end]
rts = re.search(r'ghc-options: -O2 -rtsopts -threaded "-with-rtsopts=[^"]*"', exe).group(0)
lib = exe.replace('Executable lamdera', 'Library', 1)
lib = lib.replace(rts, 'ghc-options: -O2', 1)
lib, n = re.subn(r'\n    main-is:\n        Main.hs\n', '\n', lib)
assert n == 1, 'main-is not found'
lib = lib.replace('\n    other-modules:\n',
                  '\n    autogen-modules:\n        Paths_elm\n\n    exposed-modules:\n', 1)
deps = re.search(r'\n    build-depends:\n(.*?)\n\n    hs-source-dirs:', exe, re.S).group(1)
newexe = '''Executable lamdera
    -- PROFILING COPY (notes/build-profiled.sh): the compiler is the library above, so
    -- that Cabal builds it the normal way (with the .dyn_o files Template Haskell
    -- needs) before the profiled way.
    %s
    build-depends:
        elm,
%s

    hs-source-dirs:
        prof-main

    main-is:
        Main.hs

''' % (rts, deps)
cabal = s[:start] + lib + '\n' + newexe + s[end:]
path = os.path.join(dest, 'elm.cabal')
if not os.path.isfile(path) or open(path).read() != cabal:
    open(path, 'w').write(cabal)
    print('wrote split elm.cabal')

os.makedirs(os.path.join(dest, 'prof-main'), exist_ok=True)
main_src = os.path.join(src, 'terminal/src/Main.hs')
main_dest = os.path.join(dest, 'prof-main/Main.hs')
if not os.path.isfile(main_dest) or not filecmp.cmp(main_src, main_dest, shallow=False):
    shutil.copy2(main_src, main_dest)
PYEOF

# Template Haskell reads git at compile time. Lamdera.PackageReplacementsTH runs
# `git diff origin/master... --name-only` and `git rev-parse HEAD` in each package
# replacement submodule (commit to commit, read only), so the copied submodules point at
# the original submodule repositories. Lamdera.Version (githash) also asks whether the
# top level is dirty, which can rewrite an index, so the copy's top level is its own
# repository rather than the original's. It is committed once, on the first run only:
# a new commit (or index) on every run would recompile Lamdera.Version and the modules
# whose Template Haskell depends on it.
if [ ! -d "$DEST/.git" ]; then
  git -C "$DEST" init -q
  printf '/.stack-work-prof/\n' > "$DEST/.git/info/exclude"
  git -C "$SRC" submodule status | awk '{print $2}' | while read -r sub; do
    echo "gitdir: $(git -C "$SRC/$sub" rev-parse --absolute-git-dir)" > "$DEST/$sub/.git"
    printf '/%s/\n' "$sub" >> "$DEST/.git/info/exclude"
  done
  git -C "$DEST" add -A
  git -C "$DEST" -c user.name=profiling -c user.email=profiling@localhost \
    commit -q -m "profiling copy of $(git -C "$SRC" rev-parse --short HEAD)"
fi

cd "$DEST"
stack build --work-dir .stack-work-prof \
  --library-profiling --executable-profiling --ghc-options=-fprof-late \
  elm:exe:lamdera

echo "profiled binary: $(ls "$DEST"/.stack-work-prof/install/*/*/*/bin/lamdera)"
