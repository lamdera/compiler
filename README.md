# Lamdera Elm Compiler

Elm is [a delightful language for reliable webapps](https://elm-lang.org/).

The Lamdera compiler extends the official Elm compiler with tooling that works for any regular Elm frontend project, as well as specific features for Elm projects on [Lamdera: A delightful platform for full-stack Elm web apps](https://lamdera.com).

The Lamdera **compiler** is a free, open-source and open-contribution [un-fork of the Elm compiler](https://dashboard.lamdera.app/releases/open-source-compiler).

The Lamdera **platform** is a paid service with a free tier to try, and is how we keep our work funded and sustainable.

New to Elm? Check out the [Home Page](http://elm-lang.org/), [Try Online](http://elm-lang.org/try), or [The Official Guide](http://guide.elm-lang.org/).


<br>

## Installation

Recommended: see [Lamdera downloads](https://dashboard.lamdera.app/docs/download) for binary and nix installations.

`npx lamdera` lets you try it out quickly if you have NodeJS installed, simply replace any `elm` command with `npx lamdera`.

To uninstall, simply delete the `lamdera` binary.

## Usage

The Lamdera compiler is an [un-fork](/releases/open-source-compiler) of the Elm compiler, with additional features and extensions.

## Extended & backwards compatible

The Lamdera compiler can be used to compile any existing Elm 0.19 project using the same commands as the official compiler.

The Lamdera compiler CLI has the following differences from the official Elm compiler:

|                         | elm/compiler | lamdera/compiler | Notes |
|-------------------------|--------------|------------------|-------|
| Elm 0.19 language       | ✅           | ✅               |       |
| `repl`                  | ✅           | ✅               |       |
| `init`                  | ✅           | ⚠️               | Initialises an Elm project setup for Lamdera |
| `reactor`               | ✅           | ⚠️               | Renamed to `live`, full stack, live/hot reload, with [interactive UI source maps](/releases/v1-1-0) |
| `make`                  | ✅           | ✅ ➕            | Adds:<br/>`--optimize-legible` non-obfuscated build<br/>`--no-wire` skips [Lamdera Wire](https://dashboard.lamdera.app/docs/wire) gen |
| `install`               | ✅           | ✅ ➕            | Adds `lamdera/*` packages |
| `diff` `bump` `publish` | ✅           | ❌ Deactivated   | All Lamdera projects use Elm packages from the Elm ecosystem, so continue to use `elm` for this. |
| `format`                | ❌           | ✅               | Embeds [elm-format](https://github.com/avh4/elm-format) |

You should be able to use the `lamdera` compiler on any Elm frontend project as-is.

Lamdera projects are just Elm projects with some additional packages, and a few specific files to set things up for type-safe Elm with a Frontend *and* a Backend. You can [read more here](https://dashboard.lamdera.app/docs) if that interests you.

## Forwards compatible

We love the Elm language – Lamdera would be impossible without it, and our entire platform is built on it.

We plan to continue to upgrade the Lamdera compiler and platform in line with future Elm releases, so Lamdera’s goal has been and continues to be one that looks towards staying forwards compatible. You can read about [Lamdera’s proactive forwards-compatibility approach & techniques](https://github.com/lamdera/compiler/blob/lamdera-next/extra/readme.md).

Given Evan has unwaveringly held to the core design ethos of Elm (immutable, inferred, pure), we believe future Elm releases will stay compatible with Lamdera.


## Getting started

```bash
lamdera init
lamdera live
```

See the [Lamdera overview](https://dashboard.lamdera.app/docs/overview) for more.

<br>

## Development

Looking for build instructions or interested in contributing? See [extra/readme.md](extra/readme.md).

## Help

If you are stuck with Elm, ask around on [the Elm slack channel](http://elmlang.herokuapp.com/). Folks are friendly and happy to help with questions!

For Lamdera compiler/platform discussion, see the [Lamdera Discord](https://dashboard.lamdera.app/docs/discuss).

## Support

You can support the development of the Lamdera compiler with [sponsorship](https://github.com/sponsors/supermario), by [upgrading to a paid Lamdera plan](https://dashboard.lamdera.app/docs/pricing), or by setting [feature bounties (chat with us)](https://dashboard.lamdera.app/docs/discuss).
