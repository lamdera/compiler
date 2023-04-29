# Lamdera NPM Installer

[Lamdera](https://lamdera.com) is a delightful platform for full-stack web apps built with [Elm](https://elm-lang.org/).

The Lamdera compiler is a super-set of the Elm compiler and a drop-in replacement: you can use `lamdera` on your existing non-Lamdera Elm projects in the same way you use `elm`.

Beware of [Elm IDE gotchas](https://dashboard.lamdera.app/docs/ides-and-tooling), you'll need to set Lamdera as your Elm compiler.


<br/>

## What is this package for?

**Normal installs** ❌

Use the instructions [here](https://dashboard.lamdera.app/docs/download) instead.

Lamdera has no dependency on `npm` to develop locally, so there's no reason to introduce it unless you specifically want to use `npm` for other reasons.

**Tooling author** ✅

If you're a tooling author, use Lamdera as a dependency and npm as a distribution method, then you can use this package to manage the dependency and installation.

```
npm install --save-dev lamdera@latest
```

**Multiple versions** ⚠️

If you're a tooling author, use Lamdera as a dependency and npm as a distribution method, then you can

You could also use this package to use different versions of Lamdera in different projects. I.e. `npm install lamdera@latest` in each project and use the binary at `./node_modules/.bin/lamdera` for compilation. However this is generally not recommended. Lamdera users should always use the latest release, as that's what is used when you do a `lamdera deploy`.


**Continuous integration** ⚠️

This works, but there are usually faster and more reliable options:

1. You can download `lamdera` directly from [the downloads page](https://dashboard.lamdera.app/docs/download). This is all the `npm` installer does, but with extra HTTP requests to `npmjs.com` servers, making it slower and adding more failure points.
2. Many continuous integration pipelines ways to cache files ([example](https://docs.travis-ci.com/user/caching/)) to make builds faster and more reliable.

That said, it will definitely work to use the `npm` installer on CI if you prefer that option.


<br/>

## Install Locally

The following command should download the latest Lamdera binary:

```
npm install lamdera@latest
```

You should be able to run `npx lamdera --version` within your project and see `0.19.1` (the base Elm version), or `--version-full` to see the full Elm + Lamdera release version. Now you can develop with `npx lamdera live`.
