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
}:

let
  srcs = import ./elm-srcs.nix;

  registryDat = fetchurl {
    url = "https://raw.githubusercontent.com/NixOS/nixpkgs/01d14d1f0cf4747e41c772f7c526c44d5f5285c5/pkgs/development/compilers/elm/registry.dat";
    sha256 = "18y9m922s1zrj4c7hblsslmq2b9rhcksmvyzr7mvfp1bd37z9b1a";
  };

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
    packages=$out/0.19.1/packages
    mkdir -p "$packages"
    ${unpackCmds}
    cp ${registryDat} "$packages/registry.dat"
  '';
}
