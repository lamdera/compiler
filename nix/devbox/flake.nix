{
  description = "Stack and GHC for Devbox";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
  let
  in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          system = system;
        };

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in 
      {
        packages.stack = stack-wrapped;
        packages.ghc = pkgs.haskell.compiler.ghc928;
        packages.cc = pkgs.stdenv.cc;
        packages.hls = pkgs.haskell.packages.ghc928.haskell-language-server;
        packages.apple_sdk_CoreServices = pkgs.darwin.apple_sdk.frameworks.CoreServices;
        packages.apple_sdk_CoreFoundation = pkgs.darwin.apple_sdk.frameworks.CoreFoundation;
        packages.apple_sdk_Cocoa = pkgs.darwin.apple_sdk.frameworks.Cocoa;
      });
}
