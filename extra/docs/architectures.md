
A summary of the Elm architectures/compilation/installation/usage matrix state of affairs.

- [Terms](#terms)
- [Context](#context)
  - [Problem aspect 1: `npm/elm` installer](#problem-aspect-1-npmelm-installer)
  - [Problem aspect 2: Rise in arm64 architecture usage](#problem-aspect-2-rise-in-arm64-architecture-usage)
- [Current support by architecture, install method and usage](#current-support-by-architecture-install-method-and-usage)
- [Suggestions](#suggestions)
- [🧪 Actions](#-actions)
- [CI limitations](#ci-limitations)
- [Related issues](#related-issues)


## Terms

I've picked the following of the synonyms that get used interchangeably:

- x86_64: x64, amd64
- arm64: aarch64
- macos: macosx, darwin
- win: windows

For reference:
- `npm/elm` is the [Elm npm installer package: https://www.npmjs.com/package/elm](https://www.npmjs.com/package/elm)
- [GHC](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler) is Haskell's defacto compiler.
- [Docker](https://en.wikipedia.org/wiki/Docker_(software)) is an OS-level virtualisation tool, popular with dev teams for bundling an OS+deps+software 'container' independent of the host system.


## Context

### Problem aspect 1: `npm/elm` installer

The `npm/elm` installer package [was not intended for normal installs](https://github.com/elm/compiler/blame/047d5026fe6547c842db65f7196fed3f0b4743ee/installers/npm/README.md#L12), but appears to have regardless become used as a way to specify an Elm dependency especially on projects, packages and tooling using `npm`.

A particular source of problems is with Javascript bundlers, i.e:

- [elm-community/elm-webpack-loader](https://github.com/elm-community/elm-webpack-loader) has a [peer dependency to elm](https://github.com/elm-community/elm-webpack-loader/blob/master/package.json#L36)
- [Parcel Elm](https://parceljs.org/languages/elm/) specifies "The npm package elm needs to be manually installed beforehand."
- [vite-plugin-elm](https://github.com/hmsk/vite-plugin-elm) plugin [depends on `elm-esm`](https://github.com/hmsk/vite-plugin-elm/blob/main/package.json#L51), and [`elm-esm` depends on elm](https://github.com/ChristophP/elm-esm/blob/master/package.json#L18).

Because `npm/elm` [explicitly adds support for `os,arch` pairs](https://github.com/elm/compiler/blob/047d5026fe6547c842db65f7196fed3f0b4743ee/installers/npm/download.js#L21-L24), it thus becomes the defacto arbiter of os/arch support source for projects that rely on it as a dependency.

There is currently no standard way to override the Elm binary pulled in by `npm/elm`, i.e. with an `ELM_COMPILER` ENV var or otherwise. Individual packages/tools that consume Elm npm would have to each implement a mechanism to do this.

Some folks have had success by manually replacing the binary in `./node_modules/elm/bin/elm` after an npm install.



### Problem aspect 2: Rise in arm64 architecture usage

ARM based processors are increasingly prevalent in both "desktop" devices and cloud computing.

- Amazon Web Services launched ARM-based EC2 A1 instances in late 2018, offering cost savings + improved performance compared to other instance types.
- [Apple Silicon M-series](https://en.wikipedia.org/wiki/Apple_silicon#M_series) chipsets (Nov 2020 onwards), Apple completely moving away from Intel x86 chips.
  - Has the side-effect that developers with new Macbooks who use docker, which runs as `linux-arm64` on macos-arm64 chips, run into problem 1.
- Cloud CI pipelines are starting to show arm64 support, i.e. [GitHub Actions: Self-hosted runners now support Apple M1 hardware](https://github.blog/changelog/2022-08-09-github-actions-self-hosted-runners-now-support-apple-m1-hardware/)
- [.NET 4.8.1 added arm64 support](https://devblogs.microsoft.com/dotnet/announcing-dotnet-framework-481/) (Aug 2022). Possible we'll start to see more win-arm64 demand in future on Windows 11+.

Given Elm is written in Haskell, the availability of an `*-arm64` binary is dependent on GHC's support for arm64 builds, which has evolved since Nov 2020 and is now feasible on both macos and linux.


Related meta issue: [#2007 - What are use-cases for more binaries? (32-bit and ARM)](https://github.com/elm/compiler/issues/2007)


## Current support by architecture, install method and usage

|              | GHC            | Official Binary | Static build  | Elm npm        | Elm npm in Docker |
| ------------ | -------------- | --------------- | ------------- | -------------- | ----------------- |
| macos-x86_64 | ✅              | ✅               | ❓             | ✅              | ✅                 |
| macos-arm64  | 🟠 <sup>1</sup> | ❌🧪              | ❓             | 🟠 <sup>2</sup> | ❌ <sup>3</sup>    |
| linux-x86_64 | ✅              | ✅               | ✅             | ✅              | ✅                 |
| linux-arm64  | ✅              | ❌🧪              | ❌<sup>4</sup> | ❌              | ❌                 |
| win-x86_64   | ✅              | ✅               |               | ✅              | ❌                 |
| win-arm64    | ❓              |                 |               |                |                   |



1. [GHC has two backends](https://downloads.haskell.org/~ghc/latest/docs/users_guide/codegens.html): NCG (native code generator), and LLVM
   - GHC 8.10.5+ only supports macos-arm64 builds using the new LLVM backend (equivalent performance, significant increase in Haskell compile times)
   - GHC 9.2.1+ apparently supports both the faster NCG and LLVM backends for macos-arm64
     - Need to try stack build with system GHC as latest LTS is only 9.0.2 so uses LLVM

2. A workaround was added in [elm/compiler#2156 - Allow M1 users to continue to install elm via our npm package](https://github.com/elm/compiler/pull/2156) which relies on the [Elm npm installer serving macos-x86_64 binaries to both x86_64 _and_ arm64 platforms](https://github.com/elm/compiler/blob/047d5026fe6547c842db65f7196fed3f0b4743ee/installers/npm/download.js#L21-L22).
   - This workaround has an *implicit dependency* on the user additionally installing [Rosetta 2](https://support.apple.com/en-gb/HT211861), a macos-arm64 based x86_64 emulator.
     - This was an okay-ish assumption initially as developers with early M-series Macbook's simply installed Rosetta to get software covergage when arm64 builds were generally unavailable.
     - But it's now been 2 years since macos-arm64 was released, and we're starting to see users choosing to not install Rosetta because they don't need it.
     - So IMO this workaround has now become a 'hack', thus increasingly causing issues for folks installing Elm or tooling relying on the Elm npm package on macos-arm64.
   - Related issues:
     - [#2234 - Expand npm installer to include ARM64 Linux](https://github.com/elm/compiler/pull/2234)

3. Docker on macos-arm64 will use a linux-arm64 engine, hence why Elm install via npm fails as [there is no `linux_arm64` mapping in the Elm npm package](https://github.com/elm/compiler/blob/047d5026fe6547c842db65f7196fed3f0b4743ee/installers/npm/download.js#L21-L24).
   - Rosetta cannot help the user in this case, as Rosetta is a macos-x86_64-emulator for macos-arm64, not a linux-x86_64-emulator for linux-arm64.
   - Related issues:
     - [#2283 - Provide a 0.19.1 binary for linux_arm64 to be used in containers on Apple Silicon (M1) ](https://github.com/elm/compiler/issues/2283)
     - [#2244 - Cannot compile with ELM inside Docker on Mac M1](https://github.com/elm/compiler/issues/2244)
     - [#2232 - Cross-platform Docker setup fails on M1 mac while setting up elm using using npm install](https://github.com/elm/compiler/issues/2232)

4. GLIBC isn't suitable for linux static builds, but building under Alpine with MUSL (the x86 solution) doesn't yet work for Alpine as no official GHC/Cabal/Stack builds exist for alpine-arm64.


## Suggestions

- We work on an official `macos-arm64` build and fix the hack in Elm npm
  - Stops aarch-mixing confusion, removes implicit dependency on Rosetta
  - The Homebrew binary could be offered as a quick interim fix

- We work on an official `linux-arm64` build and add support to Elm npm
  - Enables `linux-arm64` usage and fixes Docker on `macos-arm64` problems.

- We update the `npm/elm` package to use `optionalDependencies` (@lydell is on this)


## 🧪 Actions

Work underway to get official builds for `*-arm64` platforms:

- `macos-arm64`
  - [x] Working with GHC 9.0.2 via [stack](https://docs.haskellstack.org/en/stable/): [commit](https://github.com/supermario/elm-tooling-compiler/commit/4f93889a14f74c3491f719ae63bf400a204b9081)
    ```
    $ file `which elmx`
    /Users/mario/.local/bin/elmx: Mach-O 64-bit executable arm64
    $ ls -alh `which elm-dev`
    -rwxr-xr-x  1 mario  staff    87M  5 Nov 11:05 /Users/mario/.local/bin/elmx
    ```
    - [ ] The same x86_64 build is 17M so definitely some build size issue to hunt down.
  - [ ] 🚧 Direct cabal based build for 9.0.2
  - [ ] 🚧 Direct cabal based build for 9.2.*
  - [ ] 🚧 Direct cabal based build for 9.4.*
    - [ ] Blocked on https://github.com/haskell/double-conversion/issues/34
  - Notes:
    - Homebrew is offering a macos-arm64 build of Elm, however it is even larger (92M), possibly related to LLVM. See further below for other unofficial binaries.

- `linux-arm64`:
  - [x] Dynamic build working on MacOS arm64 system via docker: [commit](https://github.com/supermario/elm-tooling-compiler/commit/c4b826afa60a1cbb010b6e0ec42104d510f6affd)
  - [ ] 🚧 Static build on Alpine
    - GHC/Cabal/Stack don't have alpine-arm64 builds unfortunately
  - Notes:
    - PR# CarWow appear to have put in some effort in getting a linux-arm64 build going: [commit](https://github.com/carwow/elm-compiler/commit/4708b37e4bd4460362a930ec0eb7ca27ef0037d9)
    - https://dev.to/csaltos/elm-for-linux-arm64-32bc - unclear what GHC version build this results in
    - Vaguely aware some folks have had success with nix-based linux-arm64 builds
    - https://github.com/dmy/elm-raspberry-pi has some armv7 builds, unclear on status of linkage issues


## CI limitations

- Github CI does not yet support `macos-arm64` nor `linux-arm64` runtimes on Github runners, we'd have to look at setting up self-hosted runners.


## Related issues

- Linux has some library upgrade issues with libtinfo5 / ncurses5-compat. These have been resolved in other Haskell projects by building with MUSL instead of GCC. Might be worth looking into that simultaneously. More info TBC.
