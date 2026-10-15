#!/bin/sh
# Lamdera on Elm 0.19.3 builds applications whose elm.json says 0.19.1, 0.19.2 or
# 0.19.3, against packages whose own elm-version bound starts at 0.19.0, 0.19.1,
# 0.19.2 or 0.19.3, or even excludes the compiler (bburdette/toop 1.2.0 declares
# "0.19.0 <= v < 0.19.2"). See Elm.Details.isCompatibleAppElmVersion (applications)
# and Elm.Constraint.interchangeableElm (packages).
#
# This builds a plain Elm app (test/scenario-empty-elm-init) and a Lamdera app
# (test/scenario-empty-lamdera-init) at each application version, then rewrites the
# bounds of their packages in a scratch ELM_HOME and builds them all again. Two
# negative controls make sure the rewrites are really read: an application at 0.19.4
# and a package bound that admits none of the interchangeable versions both fail.
#
#   LAMDERA=<lamdera binary> test/multi-version/run.sh
#
# The scratch ELM_HOME is seeded from SEED_ELM_HOME (default ~/.elm), which is only
# read: packages in its 0.19.3 tree (else its 0.19.2 tree) are copied in, without
# their build artifacts, and anything missing is downloaded. Exit status 0 = all pass.
set -u

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
L=${LAMDERA:-lamdera}
seed=${SEED_ELM_HOME:-$HOME/.elm}
work=$(mktemp -d "${TMPDIR:-/tmp}/lamdera-multi-version.XXXXXX")
[ -n "${KEEP:-}" ] || trap 'rm -rf "$work"' EXIT

export ELM_HOME="$work/elm-home"
packages="$ELM_HOME/0.19.3/packages"
mkdir -p "$packages"
unset LAMDERA_APP_NAME

echo "lamdera: $(command -v "$L" || echo "$L") ($("$L" --version-full 2>/dev/null || "$L" --version))"
echo "scratch: $work"

# Every package the two fixtures use, at the version their elm.json pins.
deps="elm/browser/1.0.2 elm/bytes/1.0.8 elm/core/1.0.5 elm/file/1.0.5 elm/html/1.0.0
elm/http/2.0.0 elm/json/1.1.3 elm/time/1.0.0 elm/url/1.0.0 elm/virtual-dom/1.0.3
lamdera/codecs/1.0.0 lamdera/core/1.0.0"

for p in $deps; do
  for tree in "$seed/0.19.3/packages" "$seed/0.19.2/packages"; do
    if [ -d "$tree/$p/src" ]; then
      mkdir -p "$packages/$p"
      cp -R "$tree/$p/." "$packages/$p/"
      break
    fi
  done
done
find "$packages" -name 'artifacts.*' -delete
# Only a registry in the 0.19.3 encoding is any use; 0.19.2's would read as corrupt.
[ -f "$seed/0.19.3/packages/registry.dat" ] && cp "$seed/0.19.3/packages/registry.dat" "$packages/"

fail=0
pass=0

# build <label> <fixture dir> <app elm-version> <files...>
build() {
  label=$1; src=$2; version=$3; shift 3
  dir="$work/$label"
  rm -rf "$dir"
  mkdir -p "$dir"
  cp -R "$src/elm.json" "$src/src" "$dir/"
  sed "s/\"elm-version\": \"[^\"]*\"/\"elm-version\": \"$version\"/" "$src/elm.json" > "$dir/elm.json"
  grep -q "\"elm-version\": \"$version\"" "$dir/elm.json" || { echo "could not set elm-version in $dir/elm.json"; exit 2; }
  (cd "$dir" && "$L" make "$@" --output=/dev/null) > "$dir.log" 2>&1
}

expect_ok() {
  label=$1
  if build "$@"; then
    pass=$((pass + 1)); echo "ok    $label"
  else
    fail=1; echo "FAIL  $label (expected success)"; tail -30 "$work/$label.log"
  fi
}

# expect_error <text the error must contain> <build args...>
expect_error() {
  text=$1; shift
  label=$1
  if build "$@"; then
    fail=1; echo "FAIL  $label (expected an error, it built)"
  elif grep -q "$text" "$work/$label.log"; then
    pass=$((pass + 1)); echo "ok    $label (refused: $text)"
  else
    fail=1; echo "FAIL  $label (failed, but without \"$text\")"; tail -30 "$work/$label.log"
  fi
}

# bound <author/project/version> <elm-version constraint>
bound() {
  f="$packages/$1/elm.json"
  [ -f "$f" ] || { echo "package $1 is not in the scratch ELM_HOME"; exit 2; }
  sed "s/\"elm-version\": \"[^\"]*\"/\"elm-version\": \"$2\"/" "$f" > "$f.new" && mv "$f.new" "$f"
  grep -q "\"elm-version\": \"$2\"" "$f" || { echo "could not set the bound of $1"; exit 2; }
}

plain="$root/test/scenario-empty-elm-init"
lamdera="$root/test/scenario-empty-lamdera-init"

all_apps() {
  for v in 0.19.1 0.19.2 0.19.3; do
    expect_ok "$1-plain-$v" "$plain" "$v" src/Main.elm
    expect_ok "$1-lamdera-$v" "$lamdera" "$v" src/Frontend.elm src/Backend.elm
  done
}

echo "== applications at each elm-version, packages as published"
all_apps published

echo "== packages bounded at each style"
bound elm/json/1.1.3        "0.19.0 <= v < 0.20.0"
bound elm/time/1.0.0        "0.19.1 <= v < 0.20.0"
bound elm/url/1.0.0         "0.19.2 <= v < 0.20.0"
bound elm/virtual-dom/1.0.3 "0.19.3 <= v < 0.20.0"
bound elm/html/1.0.0        "0.19.0 <= v < 0.19.2"
all_apps bounds

echo "== negative controls"
expect_error "ELM VERSION MISMATCH" "app-0.19.4" "$plain" 0.19.4 src/Main.elm
bound elm/html/1.0.0 "0.19.0 <= v < 0.19.1"
expect_error "INCOMPATIBLE DEPENDENCIES" "bound-excludes-all" "$plain" 0.19.3 src/Main.elm
# ...and it was that bound: put it back and the same app builds.
bound elm/html/1.0.0 "0.19.0 <= v < 0.20.0"
expect_ok "bound-restored" "$plain" 0.19.3 src/Main.elm

echo "$pass passed"
if [ $fail = 0 ]; then echo "MULTI-VERSION OK"; else echo "MULTI-VERSION FAILED"; fi
exit $fail
