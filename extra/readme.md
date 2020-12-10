## Lamdera

### Development approach

The `/extra` folder contains all the Lamdera extension code integrated into the Elm compiler.

The integration philosophy is:

- Inject as small a surface area into actual compiler code as feasibly possible
  - You'll see this done with certain tricks using `&` and `$` to, wherever possible, only add additional lines to source, without modifying any existing lines
  - This means merge conflicts are minimized when trying to stay up to date with upstream Elm Compiler
  - Sometimes entire functions are added to the bottom of files, usually when the current file defines a bunch of types that are needed and it's messier to have it external

The idea is that searching the project for `import.*Lamdera` and `@LAMDERA` will reveal every single `/builder` and `/compiler` core file that has some Lamdera related modification.

Alternatively the [diff of `master` and `lamdera` branches](https://github.com/lamdera/compiler/compare/master...lamdera) gives a nice overview of all modifications in the "Files Changed" view.

Otherwise:

  - Use Elm compiler source liberally inside `/extra`
  - We don't want to reinvent the wheel on core compiler concepts, and can lean heavily on Haskell's type system to tell us when there are problems due to core type changes upstream in future
  - Future developers might find things might seem obviously duplicated - if there's no comment and blame history is not helpful, it was likely accidental (didn't know compiler already had that functionality/code) or transient (i.e. features carried up from 0.19 over the years)

If in doubt, please ask!


### Tests

The `test` folder contains tests for aspects of the additional functionality. Most are at the acceptance test level, these have been the most useful so far.

The test framework is a very lightweight vendored copy of `EasyTest.hs`, defined entirely in that one file. There are some `test/Test/Helpers.hs` too.

The easiest way to run is from the project root:

```
$ stack ghci
λ: Tests.all
```

### Lamdera.hs

This File acts as a rudimentary custom Prelude. There is some duplication of core compiler functions to avoid cyclic import issues, mainly File IO helpers.


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
  error, called at /Users/mario/dev/projects/elmx/extra/Sanity.hs:13:9 in main:Sanity
  !, called at /Users/mario/dev/projects/elmx/compiler/src/Type/Solve.hs:100:42 in main:Type.Solve
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

```
cd extra
npm i
parcel build live.js --no-source-maps
```

Then re-run the main build with `stack install`.
