Bytes decoders have the following type:

```
decode : Decoder a -> Bytes -> Maybe a
```

The `Maybe a` result makes it painfully difficult to debug as no error context is available.


## Debugging bytes decoders

### Custom elm/bytes fork

The `LOVR` package override system can be used along with our [custom elm/bytes fork](https://github.com/lamdera/elm-bytes) to get more detailed information about failures.

Assuming you have [`lamdera/runtime`](https://github.com/lamdera/runtime) cloned into `~/lamdera`:

```
mkdir -p ~/lamdera/overrides/packages/elm/bytes
git clone git@github.com:lamdera/elm-bytes.git ~/lamdera/overrides/packages/elm/bytes/1.0.8

```

Package overrides need to be built so the compiler can find the package zip locally:

```
# Make sure you're in the project folder of the Lamdera app you're debugging
yes | lamdera reset
# Make sure to edit this script first and uncomment the elm/bytes build section, which is disabled by default
~/lamdera/scripts/makeDevPackages.sh
```

Now running `LOVR=~/lamdera/overrides LDEBUG=1 lamdera ...` will use our custom fork in place of `elm/bytes`.


### Modifying wire to use `debugEncoder` / `debugDecoder` helpers

Now we have a couple of functions that will wrap an encoder/decoder and add some logging.

Look for the already existing `-- debugEncoder` or `-- debugDecoder` comments in the Wire3 source files.

For example, here's the one within `Lamdera.Wire3.Core.decoderAlias`:

```
generated = Def (a (generatedName)) ptvars $
  -- debugDecoder (Data.Name.toElmString aliasName) $
  decoderForType ifaces cname tipe
```

Uncommenting the line will mean all alias decodings will now log the alias name.

Recompile the lamdera compiler after this adding the debugging you desire, and ensure you're using that binary to test.


### Using the debugging

:warning: Debugging does not work in `--optimize` currently as it relies on `Debug.toString`.

By default, `elm/bytes` failures use a JS `throw`, meaning we lose all current progress and just get `Nothing`.

Our modified `elm/bytes` doesn't throw, instead it starts returning the `!DECODEFAILED!` as the decoded value and attempts to keep decoding.

This means we should always get an Elm value result, and can inspect it to see where decoding issues are, as a starting point to debugging.


When the decoder is finished, it will call [`writeLog`](https://github.com/lamdera/elm-bytes/blob/master/src/Elm/Kernel/Bytes.js#L57).

Debug output will go to a `window.*` var in browser, or written to disk for noedjs.

Sometimes it's easier to just edit the custom package with some `console.log` and poke around that way, depending on the nature of the bug. Don't forget to repackage and nuke caches if you do.
