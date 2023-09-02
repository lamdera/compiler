# Publishing

Here's how to update the `npm` installer.


## 1. GitHub Release

Create a [GitHub Release](https://github.com/lamdera/compiler/releases) with the following files:

1. `binary-for-mac-64-bit.gz`
2. `binary-for-mac-arm-64-bit.gz`
3. `binary-for-linux-64-bit.gz`
4. `binary-for-linux-arm-64-bit.gz`
5. `binary-for-windows-64-bit.gz`

Create each of these by running the `lamdera` executable for each platform through `gzip lamdera`.

## 2. Put the binaries in place

Put the above files at:

1. `packages/lamdera-darwin-arm64/lamdera`
2. `packages/lamdera-darwin-x64/lamdera`
3. `packages/lamdera-linux-x64/lamdera`
4. `packages/lamdera-linux-arm64/lamdera`
5. `packages/lamdera-win32-x64/lamdera.exe` (Note the `.exe` file extension!)

(They are ignored by git.)

## 3. Try a beta release

TODO: Update for also publishing all the sub packages!

In `package.json`, bump the version to `"0.19.2-beta"`.

```bash
npm publish
```

Note: if using `@` package prefix, needs `--access public` on creation as npm assumes private by default.


To test that it works, run these commands:

```bash
<<<<<<< HEAD
npm dist-tags ls @lamdera/compiler
npm install @lamdera/compiler@beta --ignore-scripts
=======
npm dist-tags ls lamdera
npm install lamdera@beta --ignore-scripts
>>>>>>> e73263a0 (First pass of npm package setup based on @lydell/elm)
```

The `latest` tag should not be changed, and there should be an additional `beta` tag.

Try this on Windows, Linux, and Mac.


## 4. Publish final release

Remove the `-beta` suffix from the version in `package.json`. Then run:

```bash
npm publish
```


## 5. Tag the `latest-0.19.1` version

Many compiler releases have needed multiple `npm` publications. Maybe something does not work on Windows or some dependency becomes insecure. Normal `npm` problems.

The convention for each Lamdera release is to create a tag the latest one.

```bash
<<<<<<< HEAD
npm dist-tag add @lamdera/0.19.1-3 latest-0.19.1
=======
npm dist-tag add lamdera@0.19.1-3 latest-0.19.1
>>>>>>> e73263a0 (First pass of npm package setup based on @lydell/elm)
```

That way people who want a specific version can point to `latest-0.19.1` or `latest-0.18.0` instead of knowing the particular names of all the various publications.

You can read more about dist-tags [here](https://docs.npmjs.com/cli/dist-tag).
