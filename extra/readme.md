## Lamdera

### Development approach

The `/extra` folder contains all the Lamdera extension code integrated into the Elm compiler.

The integration philosophy is:

- Inject as small a surface area into actual compiler code as feasibly possible
  - You'll see this done with certain tricks using `&` and `$` to, wherever possible, only add additional lines to source, without modifying any existing lines
  - This means merge conflicts are minimized when trying to stay up to date with upstream Elm Compiler
  - Sometimes entire functions are added to the bottom of core files, this is usually when the current file defines a bunch of types that are needed in the extension code, and it's problematic/messier to externalise that code and cause cyclic dependencies

The idea is that searching the project for `import.*Lamdera` and `@LAMDERA` will reveal every single `/builder` and `/compiler` core file that has some Lamdera related modification.

Alternatively the [diffing `elm/compiler` and `lamdera/compiler`](https://github.com/lamdera/compiler/compare/master...lamdera) gives a nice overview of all core file modifications in the "Files Changed" view.

Otherwise:

  - Reference/use Elm compiler source liberally inside `/extra`
  - We don't want to reinvent the wheel on core compiler concepts, and can lean heavily on Haskell's type system to tell us when there are problems due to core type changes upstream in future
  - Future developers might find things that might seem obviously duplicated - if there's no comment and blame history is not helpful, it was likely accidental (didn't know compiler already had that functionality/code) or transient (i.e. features carried up from 0.19 over the years)

If in doubt, please ask!

### Setup

:warning: We'd like to move to `devbox` to make this a one-shot env setup in future; contributions welcome.

In the meantime, here's our setup on MacOS:

```
brew install gcc pkg-config                # some headers like 'linux/random.h' are implicitly required to build Haskell libs
brew install icdiff                        # for nice side-by-side diffs in test output
git submodule init && git submodule update # setup Git submodule deps for this project
stack install hindent                      # used for debugging haskell values
```

### Developing

First, make sure you have deps installed from the Setup section above.

I've found the fastest way to develop is using ghci with a little harness to invoke target code.

1. Put the following into `~/.ghci`

```
:set -fbyte-code
:set -fobject-code
:def rr const $ return $ unlines [":r","Test.target"]
:set prompt "\ESC[34mλ: \ESC[m"
```

Last line is optional, but it's cool! Lambda prompt!

2. Run `stack ghci`

3. Then the dev feedback loop goes as follows:

- Make changes to Haskell code
- Run `:rr` to recompile + typecheck, and re-run `Test.target`
- Fix any issues, then `:rr` again
- If you want to just type-check _without_ running, use `:r`

Easier to change the target definition than constantly adjust the `:def` in `~/.ghci`!

The [target is defined here](https://github.com/lamdera/compiler/blob/lamdera/test/Test.hs#L37), it can be any Haskell expression you wish. See the commented examples for various things manually tested in the past / for debugging user issues.

By default it's `target = Test.all`, so `:rr` will recompile and run all tests.

Press up arrow to get history of prior commands!

#### Debugging in VSCode

https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode

```
stack install haskell-dap ghci-dap haskell-debug-adapter
```



### Tests

:warning: The compiler tests need work, a few of them are not isolated (i.e they reach out into the world and expect things to exist).

The `test` folder contains tests for aspects of the additional functionality. Most are at the acceptance test level, these have been the most useful so far.

The test framework is a vendored copy of the very lightweight [`easytest`](https://github.com/joelburget/easytest) lib, defined entirely in that one file. There are some `test/Test/Helpers.hs` too.

The easiest way to run is from the project root:

```
$ TOKEN="..." LDEBUG=1 LOVR=~/lamdera/overrides stack ghci
λ: Test.all
...
✅  38 tests passed, no failures!
```

`test/Test.hs` is the entrypoint for all tests. Use `Test.all` in ghci to run everything (it is also the `:rr` target by default, see notes above).

Using the test framework in `test/EasyTest`, a vendored version of  back when it had fun emojis! It's only a single file and easy to read.


#### Testing functions directly

Sometimes you might want to test some function directly with different inputs. It's nice already being in GHCI without having to go elsewhere.

```
$ stack ghci
λ: :set -XOverloadedStrings
λ: import Lamdera.Canonical
λ: showDefAnnotation "/Users/mario/dev/projects/lamdera-compiler/test/scenario-interpreter" "src/Test/Basic.elm" "gimmeTheAnnotationMan"
```

#### IDE Integration

Personally every single time I've tried to add Haskell IDE integration over the past ~5 years, it's sucked really badly.

I'm sure the situation is better and with enough effort you could make it work, but Haskell is not Elm, there are so many moving parts and approaches and tools and methods, I just find the above is the least bad option. 🤷🏻‍♂️ YMMV.


### `extra/Lamdera.hs`

This File acts as a rudimentary custom Prelude. There is some duplication of core compiler functions to avoid cyclic import issues, mainly File IO helpers.

It probably seems like a bad name, but it does allow for a search of `import Lamdera` to reveal all the integration points in the compiler in one hit, given everything is scoped under `Lamdera`.


### `Sanity` (or, how to deal with `Map.!: given key is not an element in the map`)

Sometimes the compiler code will throw an error like this:

```
<interactive>: Map.!: given key is not an element in the map
CallStack (from HasCallStack):
  error, called at libraries/containers/containers/src/Data/Map/Internal.hs:627:17 in containers-0.6.2.1:Data.Map.Internal
```

This is pretty unhelpful as we have no idea where in the source code the (!) failed or with what values (Yep, Map.! is unsafe and throws exceptions 🎉)

Run the `./addSanity.sh` script if this happens and retry. You should now get this:

```
<interactive>: Sanity failed!
CallStack (from HasCallStack):
  error, called at /Users/mario/dev/projects/lamdera-compiler/extra/Sanity.hs:13:9 in main:Sanity
  !, called at /Users/mario/dev/projects/lamdera-compiler/compiler/src/Type/Solve.hs:100:42 in main:Type.Solve
```

Now we have the actual usage point where it failed. If we want to debug it, `Sanity.debugFind` and `Sanity.debugFindCtx` help:

```
do  actual <- makeCopy rank pools (env ! name)

-- Adjust to
do  actual <- makeCopy rank pools (env `debugFind` name)
```

Which will now print out the keys and the missing key, if you have `Show` instances for them.

See `Sanity.hs` for other helpers.

Run `./removeSanity.sh` to revert the debugging changes!


### live.js

Used by `lamdera live`, this file needs to be packaged with parcel into `/extra/dist/live.js` and then inlined by the compiler.

To package whenever changes are made to this file:

- Run the esbuild instructions at the top of ./extra/live.js.
- Modify the `extra/Lamdera/Live.hs` file in some way (`x = 1` works well) to ensure it will get recompiled
- Then re-run the main build with `stack install`.

This inlining happens like this:

```
lamderaLive :: BS.ByteString
lamderaLive =
  $(bsToExp =<< runIO (Lamdera.Relative.readByteString "extra/dist/live.js"))
```

The `$(...)` syntax is invoking Template Haskell.

⚠️ Because unchanged files aren't recompiled, you might need to add an `x = 1` to the bottom of the `.hs` file to force a change, and thus the expression to be re-evaluated. If you find you've updated a static file, but the complied binary still has the old one, this is likely the reason why.

In development, using `LDEBUG=1` will cause `~/dev/projects/lamdera-compiler/extra/dist/live.js` to be dynamically included + rebuilt, helpful when working on it with `lamdera live` to see changes right away, see logic in `Lamdera.Live.lamderaLiveSrc`.
