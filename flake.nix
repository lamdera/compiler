{
  # Builds the Lamdera compiler from source with nix, via haskell.nix.
  #
  #   nix build '.?submodules=1#lamdera'
  #   ./result/bin/lamdera --version-full
  #
  # or, pinned from another flake (the intended use: downstream NixOS configs pin
  # a compiler git SHA instead of fetching a mutable prebuilt binary):
  #
  #   inputs.lamdera-compiler.url = "git+ssh://git@github.com/lamdera/compiler-internal?ref=<branch>&submodules=1";
  #   ...
  #   inputs.lamdera-compiler.packages.${system}.lamdera
  #
  # From the public repo, by release tag:
  #
  #   nix run 'git+https://github.com/lamdera/compiler?ref=refs/tags/<version>&submodules=1#lamdera'
  #
  # '?submodules=1' is REQUIRED: vendor/elm-format and the
  # extra/package-replacements/* packages are git submodules and part of the build.
  # Use git+https:, not github:. The github: fetcher downloads a tarball, which has
  # no submodules, so the build fails on the missing packages even with
  # '?submodules=1'. The submodule URLs in .gitmodules are https so they fetch
  # without GitHub SSH access.
  #
  # Released builds are in Lamdera's binary cache (cache.lamdera.com/nix, declared in
  # nixConfig below), so a user with it does not build anything: the compiler is a
  # ~16MB download. Without it, the first build compiles most of the Haskell
  # dependencies from source, which takes hours. Lamdera's runtime repo publishes it
  # (scripts/nix-cache-publish.sh).
  #
  # WHY haskell.nix, having previously used stacklock2nix
  # ---------------------------------------------------
  # stack.yaml sets `allow-newer: true` and pins 16 extra-deps, four of them local
  # (the vendored elm-format packages). haskell.nix consumes stack.yaml directly
  # and honours all of that; stacklock2nix does not translate `allow-newer`, so
  # every package whose declared upper bounds GHC 9.8.4's boot libraries exceed had
  # to be jailbroken by hand, one multi-hour build per discovery. snap-core alone
  # wants network <3.2 and network-uri <2.6 against 3.2.8.0 and 2.6.4.2.
  #
  # haskell.nix was rejected the first time round for a reason that no longer
  # applies: it had dropped GHC < 9.6, and 0.19.1 needed 9.2.8. On 0.19.2 the
  # resolver is lts-23.28 (GHC 9.8.4), which it supports directly.
  #
  # Requires IOG's binary cache, or it bootstrap-builds GHC from source:
  #   --option extra-substituters https://cache.iog.io
  #   --option extra-trusted-public-keys hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
  # (also declared in nixConfig below, which interactive callers get prompted for).
  #
  # Three things the stack build takes from its environment must be supplied
  # differently under nix, since the sandbox has no .git and no network. These are
  # unchanged from the previous implementation - they are about the sandbox, not
  # about dependency solving:
  #
  #   1. extra/Lamdera/Version.hs reads the git commit via Template Haskell.
  #      Patched (nix/patches/version-no-githash.patch) to embed the flake rev.
  #   2. extra/Lamdera/PackageReplacementsTH.hs asks git what each
  #      package-replacement submodule changes. Snapshotted in
  #      nix/package-replacements/ (regenerate with
  #      nix/gen-package-replacements-lock.sh).
  #   3. The reactor and repl-worker are compiled by Template Haskell at build
  #      time and need an offline Elm package cache: nix/reactor-elm-home.nix.
  description = "Lamdera compiler, built from source";

  nixConfig = {
    extra-substituters = [
      "https://cache.lamdera.com/nix"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "lamdera-1:w+65NzPpR0TaRfDlHVzCZceZKyvxJZrKvbASMo7DMPA="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
      haskellNix,
    }:
    let
      lib = nixpkgs.lib;

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
            inherit (haskellNix) config;
            overlays = [ haskellNix.overlay ];
          };

          # Keep in step with elm.cabal's `version:`. See the note on the
          # elmVersion argument in nix/reactor-elm-home.nix.
          elmVersion = "0.19.3";

          reactorElmHome = pkgs.callPackage ./nix/reactor-elm-home.nix { inherit elmVersion; };

          # Copy the prefilled package cache somewhere writable: the build-time Elm
          # compiles write artifacts into the cache directories.
          setupElmHome = ''
            export ELM_HOME="$TMPDIR/elm-home"
            mkdir -p "$ELM_HOME/${elmVersion}"
            cp -R ${reactorElmHome}/${elmVersion}/packages "$ELM_HOME/${elmVersion}/packages"
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

          project = pkgs.haskell-nix.stackProject' {
            inherit src;
            # stack.yaml pins the resolver (lts-23.28 = GHC 9.8.4); haskell.nix
            # reads it, along with extra-deps, the local vendored packages and
            # allow-newer, so none of that is restated here.
            modules = [
              {
                # Build from the FULL patched source. haskell.nix filters a
                # package's source down to what its cabal file declares, which
                # drops directories no hs-source-dir mentions - reactor/ and
                # extra/dist/ among them. Both are read by Template Haskell at
                # build time (the reactor frontend, and live.js), so without this
                # the build fails late with "requireDir: could not find a relative
                # path, seeking at: reactor" - which names the symptom, not the
                # source filter that caused it.
                #
                # stacklock2nix needed the same thing for a different reason
                # (nixpkgs' cleanSource strips any directory called `dist`).
                packages.elm.src = src;

                # The test suite needs a full dev environment (git, network, local
                # fixtures); do not build or run it here.
                packages.elm.doCheck = false;

                packages.elm.components.exes.lamdera = {
                  # esbuild is invoked BY the build, not just at runtime: the
                  # Lamdera.Repl splice shells out to it while compiling the repl
                  # worker, and Ext.Common aborts with "please install the
                  # following required binary: esbuild".
                  build-tools = [ pkgs.esbuild ];

                  # Serialize GHC's upsweep for THIS package. Two Template
                  # Haskell splices invoke the Elm compiler during the build
                  # (Develop.StaticFiles for the reactor, Lamdera.Repl for the
                  # repl worker). Run in parallel they race two ways: on the
                  # shared ELM_HOME package lock, and on -dynamic-too output,
                  # where a splice links against a .dyn_o that has not been
                  # written yet:
                  #
                  #   cannot find object file '.../Develop/StaticFiles_TH.dyn_o'
                  #   while linking an interpreted expression
                  #
                  # The 0.19.2 branch reaches the same conclusion for the stack
                  # build ("Build the lamdera package at -j1; deps stay -j4"),
                  # and nixpkgs' own elm derivation disables parallel building
                  # with the note that "with parallelism most of the time breaks
                  # compilation". Dependencies still build in parallel; this is
                  # scoped to the one package whose TH does the racing.
                  ghcOptions = [ "-j1" ];

                  # ELM_HOME must exist before anything in the build reaches for it.
                  preBuild = setupElmHome;
                };

                # HsOpenSSL's FFI wrappers do not compile clean against OpenSSL 3
                # with a modern C compiler, and the warnings are escalated to
                # errors. nixpkgs works around the same thing in
                # configuration-common.nix (HsOpenSSL issues 88/93/95) with
                # -Wno-error=incompatible-pointer-types; on GHC 9.8.4 we also hit
                # -Wdiscarded-qualifiers, from wrappers returning
                # OBJ_nid2sn/OBJ_nid2ln as char* where OpenSSL 3 declares them
                # const char*.
                packages.HsOpenSSL.ghcOptions = [
                  "-optc=-Wno-error=incompatible-pointer-types"
                  "-optc=-Wno-error=discarded-qualifiers"
                ];
              }
            ];
          };

          flake = project.flake { };

          lamdera = flake.packages."elm:exe:lamdera";
        in
        {
          inherit pkgs project lamdera;
        };

      bySystem = forAllSystems lamderaFor;
    in
    {
      packages = forAllSystems (system: {
        inherit (bySystem.${system}) lamdera;
        default = bySystem.${system}.lamdera;
      });

      devShells = forAllSystems (system: {
        default = bySystem.${system}.project.shellFor {
          tools = { cabal = "latest"; };
          buildInputs = [ bySystem.${system}.pkgs.esbuild ];
        };
      });
    };
}
