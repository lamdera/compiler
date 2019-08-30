# The Lamdera Elm Compiler

## Changelog

### 0.6.0
- Massive compatibility improvements; all but 3 packages on package.elm-lang.org compiles with our code injections (ignoring the packages that doesn't build with the standard elm 0.19 compiler, such as packages that are still on 0.18). The three unsupported packages are Skinney/elm-deque, 1602/json-schema and json-tools/json-schema. 
- No longer possible to publish Lamdera packages into the Elm package manager; we'll set up our own for Lamdera-specific packages later
- Compilation now runs two passes; first to check that the dev-supplied elm code is correct, and then once more with our automatic code injections. That means that devs should now never see leaking Evergreen internals when they have made a mistake in their code.
- If the compiler does for whatever reason run into a bug, the debug information is much better, so you should be able to work around the issue yourself, and we should be able to reproduce and fix the problem much easier.
- Rename the elm-stuff folder to lamdera-stuff so devs can run both elmc and lamdera side-by-side. The two compilers have slightly different cache file structures, so previously they sometimes collided and complained about corrupt caches.
- Lots of internal docs and error message changes, so the lamdera binary will help you out much more now when you make mistakes, like you're used to the elm compiler doing. Upgrading an app from pure Elm to Lamdera should be as easy as changing `elm make` to `lamdera make` and following the error messages!
- `lamdera init` now works as expected
