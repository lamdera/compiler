
## Supported architectures

Distributions for the following architectures are currently supported:

| Build                                 | Linking | Supported build hosts                   |
| ------------------------------------- | ------- | --------------------------------------- |
| `lamdera-[verison]-macos-x86_64`      | Dynamic | `macos-x86_64`, `macos-arm64` (Rosetta) |
| `lamdera-[verison]-macos-arm64`       | Dynamic | `macos-arm64` + llvm                    |
| `lamdera-[verison]-linux-x86_64-musl` | Static  | `linux-x86_64`/`macos-x86_64` (Docker)  |
| `lamdera-[verison]-linux-arm64-musl`  | Static  | `linux-arm64`/`macos-arm64` (Docker)    |
| `lamdera-[verison]-win-x86_64`        | Dynamic | `win-x86_64`                            |


## Building an Lamdera compiler binary

### Pre-requisites

- [ghcup](https://www.haskell.org/ghcup/) (scripts will attempt to install GHC 9.0.2 + Cabal 3.6.2.0)
- LLVM v13+ (MacOS arm64 only, suggest [homebrew](https://brew.sh/) `brew install llvm@13`)

How to install these varies depending on your build host. [GHCup](https://www.haskell.org/ghcup/) is a convenient option for trying out multiple versions.

Once you have these dependencies, running the relevant `build-<os>-<aarch>.sh` should result in an Lamdera binary.


## Macos cross-compiling

Lamdera should compile on both `x86_64` (Intel) and `arm64` (M-series) chipset macs.

If you have an `x86_64` mac, you can only build `x86_64` binaries.

If you have an `arm64` mac and [Rosetta 2](https://support.apple.com/en-gb/HT211861), you can also cross-compile the `x86_64` binary, however it will require the prerequisite toolchain in the respective CPU flavour, i.e.

- llvm-arm64 + GHC-arm64 + cabal-arm64  => lamdera-macos-arm64
- GHC-x86_64 + cabal-x86_64  => lamdera-macos-x86-64

If you use `ghcup`, you can force install the x86 tools to replace the arm64 ones like follows:

```
ghcup install ghc 9.0.2 --force -p x86_64-apple-darwin --set
ghcup install cabal 3.6.2.0 --force -p x86_64-apple-darwin --set
ghcup set ghc 9.0.2
```

It seems ghcup doesn't currently support multi-arch installs. Check the arch of your currently installed binaries as follows:

```
file ~/.ghcup/bin/cabal
file ~/.ghcup/ghc/9.0.2/lib/ghc-9.0.2/bin/ghc
```


## Linux builds with Docker

Docker provides us with the convenience of being able to run encapsulated Linux builds on both Linux and MacOS.

The Docker philosophy of immutable layers is great for reproducibility, but a pain for debugging. To get back some of the benefits of tool-level caching, break up expensive operations whenever possible.

I.e. where `cabal install` run directly will resume based on prior progress (i.e. expensive package compilation), to get similar behaviour via Docker we need to:

```bash
COPY lamdera.cabal ./               # only add lamdera.cabal
RUN cabal build --only-dependencies # single layer for building deps based on lamdera.cabal only
COPY ...                            # add all remaining project files afterward
RUN cabal build                     # run the actual elm build
```

Without this, even changing the readme would mean Docker decides all the packages need recompiling from scratch.
