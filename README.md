# Lamdera Elm Compiler

Elm is [a delightful language for reliable webapps](https://elm-lang.org/).

The Lamdera compiler extends the official Elm compiler with tooling that works for any regular Elm frontend project, as well as specific features for Elm projects on [Lamdera: A delightful platform for full-stack Elm web apps](https://lamdera.com).

The Lamdera **compiler** is free, open-source and open-contribution [un-fork of the Elm compiler](https://dashboard.lamdera.app/releases/open-source-compiler).

The Lamdera **platform** is a paid service with a free tier to try, and is how we keep our work funded and sustainable.

New to Elm? Check out the [Home Page](http://elm-lang.org/), [Try Online](http://elm-lang.org/try), or [The Official Guide](http://guide.elm-lang.org/).


<br>

## Installation

See [Lamdera downloads](https://dashboard.lamdera.app/docs/download) for binary and nix installations (recommended). Or if you have node, `npx lamdera` lets you try it out quickly.

To uninstall, simply delete the `lamdera` binary.

## Usage

All `elm` commands work in Lamdera, except:

- `reactor` which is replaced with `live`
- `publish` / `bump` / `diff` are removed, continue to use `elm` for Elm package publishing

See [differences](https://dashboard.lamdera.app/docs/differences) for more info.


## Getting started

```bash
lamdera init
lamdera live
```

See the [Lamdera overview](https://dashboard.lamdera.app/docs/overview) for more.

<br>

## Development

Interested in contributing? See [extra/readme.md](extra/readme.md).

## Help

If you are stuck with Elm, ask around on [the Elm slack channel](http://elmlang.herokuapp.com/). Folks are friendly and happy to help with questions!

For Lamdera compiler/platform discussion, see the [Lamdera Discord](https://dashboard.lamdera.app/docs/discuss).

## Support

You can support the development of the Lamdera compiler with [sponsorship](https://github.com/sponsors/supermario), by [upgrading to a paid Lamdera plan](https://dashboard.lamdera.app/docs/pricing), or by setting [feature bounties (chat with us)](https://dashboard.lamdera.app/docs/discuss).
