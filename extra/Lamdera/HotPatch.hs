{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Lamdera.HotPatch
  ( Patch(..)
  , diff
  , render
  , varNames
  )
  where

{- @LAMDERA hot patches for `lamdera live --memory`.

After a rebuild the page normally loads the whole new bundle (49MB for a 920 module app),
or in freeze mode evaluates it whole and hot reloads into it (Simon Lydell's hot reload:
extra/live.js hotReload, and Elm.hot.reload in the Platform kernel replacement).
With --memory every build also lists what it emitted (Generate.JavaScript.Listing), so
the page can instead ask for a patch from the build it runs to the latest one: just the
statements that changed, and the values that have to be worked out again, run in the
scope of the running page (Generate.JavaScript.patchHook), then Elm.hot.reload with the
programs exported again.

Why that is enough. Elm's output is one scope of `var` definitions, in dependency order.
A function looks up the definitions it uses each time it runs, so redefining one is
seen by every caller. A value (worked out once, when the page loads) holds on to what it
was worked out from, so it is worked out again if anything it uses, directly or not,
changed. Kernel code comes before every definition, so it can only use them when
called. The programs (main) are values too: they are worked out again, and exporting
them again gives the new impl that Elm.hot.reload swaps in, the same as the full hot
reload does from a whole new scope.

It falls back to the full path (Full) when a patch is not safe, or not worth it: kernel
code changed or appeared (Wire3 codecs, a new package), the exports or the injected
code changed, a constructor changed, a port or an effect manager changed or would have
to be worked out again (they register themselves when they are), a cycle of values
would have to be worked out again, or the patch would be most of the page anyway.
-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Elm.ModuleName as ModuleName
import qualified Generate.JavaScript as JS


data Patch
  = Patch [JS.Emitted] -- the statements to run, in order
  | Full String -- why not


{-| What a page running the build `old` needs to run to be the build `new`. -}
diff :: JS.Listing -> JS.Listing -> Patch
diff old new
  | JS._lsExports old /= JS._lsExports new = Full "the exported programs changed"
  | JS._lsFixed old /= JS._lsFixed new = Full "the injected code changed"
  | Just g <- changedKernel = Full ("kernel code changed: " ++ globalName g)
  | Just reason <- unsafe = Full reason
  | 2 * includedSize > totalSize = Full "most of the page changed"
  | otherwise = Patch included
  where
    oldKernels = Map.fromList [ (g, bytes) | JS.Emitted g _ bytes _ <- JS._lsKernels old ]
    changedKernel =
      Maybe.listToMaybe [ g | JS.Emitted g _ bytes _ <- JS._lsKernels new, Map.lookup g oldKernels /= Just bytes ]

    oldStatements = Map.fromList [ (g, bytes) | JS.Emitted g _ bytes _ <- JS._lsStatements old ]
    isNew (JS.Emitted g _ _ _) = not (Map.member g oldStatements)
    isChanged (JS.Emitted g _ bytes _) = Map.lookup g oldStatements /= Just bytes

    -- in output order every dependency comes first, so one pass finds everything that
    -- changed or uses something that did
    affected =
      List.foldl'
        (\acc e@(JS.Emitted g _ _ deps) ->
            if isChanged e || any (`Set.member` acc) (Set.toList deps) then Set.insert g acc else acc
        )
        Set.empty
        (JS._lsStatements new)

    rerun kind =
      case kind of
        JS.EValue -> True
        JS.ECycleValues -> True
        JS.EPort -> True
        JS.EManager -> True
        _ -> False

    included =
      [ e
      | e@(JS.Emitted g kind _ _) <- JS._lsStatements new
      , kind /= JS.ELink
      , isChanged e || (rerun kind && Set.member g affected)
      ]

    unsafe =
      Maybe.listToMaybe $ Maybe.mapMaybe
        (\e@(JS.Emitted g kind _ _) ->
            case kind of
              JS.EConstant | not (isNew e) -> Just ("a constructor changed: " ++ globalName g)
              JS.EPort -> Just ("a port changed: " ++ globalName g)
              JS.EManager -> Just ("an effect manager changed: " ++ globalName g)
              JS.ECycleValues -> Just ("a cycle of values changed: " ++ globalName g)
              _ -> Nothing
        )
        included

    includedSize = sum (map (BS.length . JS._emBytes) included)
    totalSize = sum (map (BS.length . JS._emBytes) (JS._lsStatements new))


globalName :: Opt.Global -> String
globalName (Opt.Global (ModuleName.Canonical _ home) name) =
  Module.toChars home ++ "." ++ N.toChars name


{-| The JS for the page to run (in the scope of its definitions, see patchHook) for the
statements of a patch, or of several in a row. `exports` is the newest build's
toMainExports: the programs are exported again into a scratch scope, and the running
ones hot reloaded from there.

A statement is one or more `var NAME = ...;` at the start of a line (everything inside
is indented). The definitions of the page are assigned again, and a name the page has
never had is first made a global, so that later patches can assign it too.
-}
render :: [JS.Emitted] -> BS.ByteString -> B.Builder
render statements exports =
  if null statements then mempty else
  let
    names = List.nub (concatMap (varNames . JS._emBytes) statements)
    declare name =
      "if (typeof " <> B.byteString name <> " === 'undefined') globalThis." <> B.byteString name <> " = undefined;\n"
  in
  foldMap declare names
  <> foldMap (assignments . JS._emBytes) statements
  <> "(function () {\n\
     \var __lamdera_scope = scope;\n\
     \var __lamdera_new;\n\
     \scope = {};\n\
     \try {\n"
  <> B.byteString exports
  <> "\n__lamdera_new = scope;\n\
     \} finally {\n\
     \scope = __lamdera_scope;\n\
     \}\n\
     \__lamdera_scope['Elm'].hot.reload(__lamdera_new);\n\
     \})();\n"


-- The names a statement defines (its `var` lines).
varNames :: BS.ByteString -> [BS.ByteString]
varNames bytes =
  [ BS8.takeWhile (/= ' ') rest
  | line <- BS8.lines bytes
  , Just rest <- [BS.stripPrefix "var " line]
  ]


-- The statement with every `var NAME = ` at the start of a line made `NAME = `.
assignments :: BS.ByteString -> B.Builder
assignments bytes =
  foldMap
    (\line ->
        case BS.stripPrefix "var " line of
          Just rest -> B.byteString rest <> "\n"
          Nothing -> B.byteString line <> "\n"
    )
    (BS8.lines bytes)
