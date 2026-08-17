{
  # Builds the Lamdera compiler from source with nix.
  #
  #   nix build '.?submodules=1#lamdera'
  #   ./result/bin/lamdera --version
  #
  # or, pinned from another flake (this is the intended use: downstream NixOS
  # configs pin a compiler git SHA instead of fetching a mutable prebuilt
  # binary):
  #
  #   inputs.lamdera-compiler.url = "git+ssh://git@github.com/lamdera/compiler-internal?ref=<branch>&submodules=1";
  #   ...
  #   inputs.lamdera-compiler.packages.${system}.lamdera
  #
  # The '?submodules=1' is required: vendor/elm-format and the
  # extra/package-replacements/* packages are git submodules and are part of
  # the build.
  #
  # How it works: stacklock2nix reads stack.yaml + stack.yaml.lock, so the nix
  # build follows exactly the package versions of the official stack build
  # (resolver lts-20.26 = GHC 9.2.8, the extra-deps, and the language-glsl git
  # dependency), building them with nixpkgs' ghc928. The vendored elm-format
  # packages (local extra-deps, not covered by the lock file) are added
  # explicitly below.
  #
  # (haskell.nix was tried first: current haskell.nix has dropped GHC < 9.6,
  # and the last revision supporting GHC 9.2.8 requires either the
  # cache.iog.io binary cache or bootstrap-building several GHCs from source.
  # The nixpkgs ghc928 route has no such exotic requirements: GHC comes
  # straight from cache.nixos.org.)
  #
  # Three things the stack build takes from the surrounding environment have
  # to be provided differently under nix (no .git, no network in the build
  # sandbox):
  #
  #   1. extra/Lamdera/Version.hs reads the git commit via Template Haskell.
  #      Patched (nix/patches/version-no-githash.patch) to embed the flake
  #      source rev instead.
  #   2. extra/Lamdera/PackageReplacementsTH.hs asks git which files each
  #      package-replacement submodule changes relative to upstream, and for
  #      the submodule commits. Those answers are snapshotted in
  #      nix/package-replacements/ (regenerate with
  #      nix/gen-package-replacements-lock.sh; the build fails loudly if the
  #      snapshot disagrees with extra/package-replacements/lock.txt).
  #   3. terminal/src/Develop/StaticFiles.hs compiles the reactor frontend at
  #      build time, which needs an Elm package cache; nix/reactor-elm-home.nix
  #      provides it offline.
  description = "Lamdera compiler, built from source";

  inputs = {
    # nixos-24.11, the same pin as nix/devbox/flake.nix, so the ghc928 build
    # is shared with the devbox environment.
    nixpkgs.url = "github:nixos/nixpkgs/107d5ef05c0b1119749e381451389eded30fb0d5";
    stacklock2nix = {
      url = "github:cdepillabout/stacklock2nix/550797146fb57220319614a62f7f791e4fdc2631";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      stacklock2nix,
    }:
    let
      lib = nixpkgs.lib;

      # aarch64-darwin is the primary development platform; x86_64-linux is the
      # deployment target.
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = f: lib.genAttrs systems f;

      gitRev = self.rev or self.dirtyRev or "unknown";

      lamderaFor =
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ stacklock2nix.overlay ];
          };

          hlib = pkgs.haskell.lib.compose;

          reactorElmHome = pkgs.callPackage ./nix/reactor-elm-home.nix { };

          # Copy the prefilled package cache somewhere writable: package
          # builds triggered by the reactor compile write their build
          # artifacts (artifacts.*.dat) into the cache directories.
          setupElmHome = ''
            export ELM_HOME="$TMPDIR/elm-home"
            mkdir -p "$ELM_HOME/0.19.1"
            cp -R ${reactorElmHome}/0.19.1/packages "$ELM_HOME/0.19.1/packages"
            chmod -R u+w "$ELM_HOME"
          '';

          src = pkgs.applyPatches {
            name = "lamdera-compiler-src";
            src = self;
            patches = [
              ./nix/patches/version-no-githash.patch
              ./nix/patches/package-replacements-th-no-git.patch
            ];
            postPatch = ''
              if [ ! -f vendor/elm-format/elm-format.cabal ]; then
                echo "ERROR: the git submodules are missing from the flake source." >&2
                echo "Fetch this flake with submodules enabled, e.g.:" >&2
                echo "  nix build 'git+file:///path/to/checkout?ref=<branch>&submodules=1#lamdera'" >&2
                echo "or add '?submodules=1' to the flake input URL." >&2
                exit 1
              fi

              substituteInPlace extra/Lamdera/Version.hs \
                --subst-var-by gitRev "${gitRev}"

              # Stage the package-replacement answers (see
              # extra/Lamdera/PackageReplacementsTH.hs) into the submodule
              # directories, refusing to build from a stale snapshot.
              for c in nix/package-replacements/*/*.commit; do
                rel=''${c#nix/package-replacements/}
                rel=''${rel%.commit}
                dir=extra/package-replacements/$rel
                if [ ! -f "$dir/elm.json" ]; then
                  echo "ERROR: $c has no matching submodule content at $dir (missing submodules?)" >&2
                  exit 1
                fi
                commit=$(cat "$c")
                if ! grep -q "^ $commit $dir " extra/package-replacements/lock.txt; then
                  echo "ERROR: nix/package-replacements/ is out of date for $rel:" >&2
                  echo "  recorded commit $commit does not match extra/package-replacements/lock.txt" >&2
                  echo "  Re-run nix/gen-package-replacements-lock.sh and commit the result." >&2
                  exit 1
                fi
                printf '%s\n' "$commit" > "$dir/.nix-commit"
                cp "nix/package-replacements/$rel.files" "$dir/.nix-changed-files"
              done

              for d in extra/package-replacements/*/*/; do
                d=''${d%/}
                rel=''${d#extra/package-replacements/}
                if [ ! -f "nix/package-replacements/$rel.commit" ]; then
                  echo "ERROR: submodule $d has no entry in nix/package-replacements/" >&2
                  echo "  Re-run nix/gen-package-replacements-lock.sh and commit the result." >&2
                  exit 1
                fi
              done
            '';
          };

          stacklock = pkgs.stacklock2nix {
            stackYaml = src + "/stack.yaml";
            stackYamlLock = src + "/stack.yaml.lock";
            # Must match the GHC of the stack.yaml resolver (lts-20.26).
            baseHaskellPkgSet = pkgs.haskell.packages.ghc928;
            cabal2nixArgsOverrides = args: args // {
              # macOS file-watching dependency of fsnotify; its cabal file
              # references the frameworks as plain dependencies.
              hfsevents = ver: {
                inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices;
              };
            };
            additionalHaskellPkgSetOverrides =
              hfinal: hprev:
              let
                vendored =
                  name: path:
                  # jailbreak: the vendored elm-format packages declare bounds
                  # (text >= 2.0, aeson >= 2.1) that stack overrides with
                  # `allow-newer: true` (see the note in cabal.project); the
                  # lts-20.26 versions are what the official build actually
                  # uses, so drop the declared bounds the same way.
                  hlib.doJailbreak (hlib.dontCheck (hfinal.callCabal2nix name (src + "/${path}") { }));
              in
              {
                # Disable every dependency's test suite: we only want the
                # final executable, and evaluating test dependencies creates
                # an infinite recursion through the mutual http-streams <->
                # snap-server test-suite dependency (nixpkgs carries the same
                # dontCheck fix for that pair).
                mkDerivation = args: hprev.mkDerivation (args // { doCheck = false; });

                # The vendored elm-format packages are local extra-deps in
                # stack.yaml; stack.yaml.lock does not cover local paths, so
                # stacklock2nix cannot create them by itself.
                avh4-lib = vendored "avh4-lib" "vendor/elm-format/avh4-lib";
                elm-format-markdown = vendored "elm-format-markdown" "vendor/elm-format/elm-format-markdown";
                elm-format-lib = vendored "elm-format-lib" "vendor/elm-format/elm-format-lib";
                # Only referenced by elm-format's (unbuilt) test suite, but
                # the generated expression still wants the dependency to
                # exist.
                elm-format-test-lib = vendored "elm-format-test-lib" "vendor/elm-format/elm-format-test-lib";
                elm-format = vendored "elm-format" "vendor/elm-format";

                # The compiler itself (the package is named `elm`, the
                # executable it produces is `lamdera`).
                elm = lib.pipe hprev.elm [
                  (hlib.overrideCabal (drv: {
                    # Build from the FULL patched source, not stacklock2nix's
                    # filtered copy.
                    #
                    # stacklock2nix runs local package sources through nixpkgs'
                    # cleanSource filter, which drops any directory named `dist`.
                    # That silently deletes extra/dist/ - which git tracks and the
                    # build needs, because Lamdera.Live embeds extra/dist/live.js
                    # with a Template Haskell splice. The failure surfaces at module
                    # 218 of 229 as "requireFile:readByteString could not find a
                    # relative path", a long way from the filter that caused it.
                    src = src;
                    preBuild = setupElmHome;
                  }))
                  # The test suite needs a full dev environment (git, network,
                  # local fixtures); do not build or run it here.
                  hlib.dontCheck
                ];
              };
          };

          lamdera = lib.pipe stacklock.pkgSet.elm [
            pkgs.haskell.lib.compose.justStaticExecutables
          ];
        in
        {
          inherit pkgs stacklock lamdera;
        };

      bySystem = forAllSystems lamderaFor;
    in
    {
      packages = forAllSystems (system: {
        lamdera = bySystem.${system}.lamdera;
        default = bySystem.${system}.lamdera;
      });

      apps = forAllSystems (system: {
        lamdera = {
          type = "app";
          program = "${bySystem.${system}.lamdera}/bin/lamdera";
        };
        default = {
          type = "app";
          program = "${bySystem.${system}.lamdera}/bin/lamdera";
        };
      });

      # For debugging in `nix repl`: the whole stacklock2nix result
      # (pkgSet, devShell, ...).
      legacyPackages = forAllSystems (system: {
        lamderaStacklock = bySystem.${system}.stacklock;
      });
    };
}
