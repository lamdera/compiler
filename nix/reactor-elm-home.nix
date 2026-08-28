# A prefilled ELM_HOME package cache for the Template Haskell reactor build.
#
# Compiling the `lamdera` executable runs the compiler-under-construction on
# reactor/src at build time (Develop.StaticFiles). That build normally
# downloads its Elm dependencies; the nix sandbox has no network, so this
# derivation provides the complete package cache up front, in the layout
# builder/src/Stuff.hs expects:
#
#   <ELM_HOME>/0.19.1/packages/<author>/<pkg>/<version>/...
#   <ELM_HOME>/0.19.1/packages/registry.dat
#
# registry.dat is a package.elm-lang.org registry snapshot (borrowed from
# nixpkgs' elm packaging, pinned by commit). It only has to parse and to list
# the elm/* packages used here; the compiler injects the lamdera/* packages
# itself (Deps.Registry.registryAddLamderaCoreDeps). With a registry present
# and the network unreachable, Solver.initEnv falls back to Offline mode and
# resolves everything from this cache.
{
  lib,
  stdenvNoCC,
  fetchurl,
  unzip,
  # The Elm version keying the package cache layout. MUST match elm.cabal's
  # `version:` - Stuff.getCacheDir puts packages at $ELM_HOME/<compilerVersion>/,
  # and compilerVersion is that cabal version. A mismatch means the compiler
  # looks in a directory this derivation never created, and every build-time Elm
  # compile fails as if the packages were missing.
  elmVersion,
}:

let
  srcs = import ./elm-srcs.nix;

  # Elm's "details" phase VERIFIES dependencies against this registry, not just
  # against what is unpacked in ELM_HOME. If a package is absent from it, Elm
  # tries to reach package.elm-lang.org, which a nix sandbox has no route to, and
  # the build dies with:
  #
  #   I could not connect to https://package.elm-lang.org to get the latest list
  #   of packages, and I was unable to verify your dependencies with the
  #   information I have cached locally.
  #
  # Lamdera.Repl discards that message and reports only "details phase", so the
  # cause is invisible from the build log.
  #
  # This used to fetch nixpkgs' registry.dat. That one predates several packages
  # vendor/elm-repl-worker depends on (agu-z/elm-zip, miniBill/elm-unicode,
  # folkertdev/elm-flate, ...), so it worked for the reactor and failed for the
  # repl worker. We ship our own instead, as nixpkgs itself does.
  #
  # To regenerate (needs network): run any `lamdera make` against a project using
  # these dependencies with ELM_HOME set to a scratch dir, then copy out
  # $ELM_HOME/0.19.1/packages/registry.dat. It is a snapshot of the public
  # package list, so refreshing it only ever adds packages.
  registryDat = ./registry.dat;

  fetchPkg =
    name: info:
    if info.lamderaZip or false then
      fetchurl {
        url = "https://static.lamdera.com/r/${name}/pack-${info.version}.zip";
        inherit (info) sha256;
      }
    else
      fetchurl {
        url = "https://github.com/${name}/archive/${info.version}.tar.gz";
        inherit (info) sha256;
      };

  # A package may need to be seeded at MORE THAN ONE version: the build-time Elm
  # projects are applications, so their dependency versions are exact, and
  # reactor/elm.json and vendor/elm-repl-worker/elm.json do not always agree.
  # An entry is therefore either one {version; sha256;} or a list of them.
  toList = v: if builtins.isList v then v else [ v ];

  unpackCmds = lib.concatStrings (
    lib.concatLists (
      lib.mapAttrsToList (name: infos: map (info: ''
      dest="$packages/${name}/${info.version}"
      mkdir -p "$dest"
      tmp=$(mktemp -d)
      ${
        if info.lamderaZip or false then
          ''unzip -q ${fetchPkg name info} -d "$tmp"''
        else
          ''tar -xzf ${fetchPkg name info} -C "$tmp"''
      }
      # Both archive styles wrap the package in a single root directory.
      cp -R "$tmp"/*/. "$dest"/
      rm -rf "$tmp"
    '') (toList infos)) srcs
    )
  );
in
stdenvNoCC.mkDerivation {
  name = "lamdera-reactor-elm-home";
  nativeBuildInputs = [ unzip ];
  dontUnpack = true;
  buildCommand = ''
    packages=$out/${elmVersion}/packages
    mkdir -p "$packages"
    ${unpackCmds}
    cp ${registryDat} "$packages/registry.dat"
  '';
}
