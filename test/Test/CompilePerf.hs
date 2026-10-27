{-# LANGUAGE OverloadedStrings #-}

module Test.CompilePerf where

{-| Regressions for the compile time work.

The type checker defers alias real types (Type.Type.Deferred) and converts the ones
it never looked inside once per module (Type.Type.AliasCache). Both must give exactly
the types upstream gives. These are the cases that went wrong while getting there.

-}

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>))

import EasyTest
import Lamdera hiding ((&))
import Test.Helpers

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as TN
import qualified Elm.Interface as I
import qualified File
import qualified Lamdera.Compile
import qualified Test.ElmiTypeTable


all = EasyTest.run suite


suite :: Test ()
suite = tests
  [ scope "an alias inside an alias body keeps the argument of each use" $
      -- `Html a` inside `Wrap a` is written with a type variable, so the same written
      -- alias stands for `Html Msg1` in one annotation and `Html Msg2` in the next. An
      -- early AliasCache keyed it by the written arguments and gave get2's interface
      -- `Html Msg2` with the real type `Node Msg1`.
      let
        project = "./test/scenario-compile-perf"

        setup = do
          rmdir project
          Dir.createDirectoryIfMissing True (project </> "src")
          Dir.copyFile "./test/scenario-empty-elm-init/elm.json" (project </> "elm.json")
          writeUtf8 (project </> "src/Wrapped.elm") wrappedModule
          writeUtf8 (project </> "src/Main.elm") mainModule

        cleanup _ =
          rmdir project

        test _ = do
          ioSilenced $ do
            Lamdera.unsetEnv "LOVR"
            Lamdera.unsetEnv "LTEST"
            Lamdera.unsetEnv "LDEBUG"
            Lamdera.Compile.makeDev project ["src/Main.elm"]

          elmiM <- io $ Test.ElmiTypeTable.findFile (project </> "elm-stuff") "Wrapped.elmi"
          elmi <- maybe (crash "Wrapped.elmi was not written") pure elmiM
          ifaceM <- io $ File.readBytes I.dInterface elmi
          iface <- maybe (crash "Wrapped.elmi could not be read back") pure ifaceM

          let htmlUses name =
                case Map.lookup (N.fromChars name) (I._values iface) of
                  Just (Can.Forall _ t) -> htmlAliases t
                  Nothing -> []

          scope "get1 sees Html Msg1 = Node Msg1" $ do
            expect (not (null (htmlUses "get1")))
            expectEqual [] [ u | u@(arg, real) <- htmlUses "get1", arg /= "Msg1" || real /= "Msg1" ]

          scope "get2 sees Html Msg2 = Node Msg2" $ do
            expect (not (null (htmlUses "get2")))
            expectEqual [] [ u | u@(arg, real) <- htmlUses "get2", arg /= "Msg2" || real /= "Msg2" ]
      in
      using setup cleanup test

  , scope "an expanded alias body merged into an older variable keeps its rank" $
      -- Reduced from elm-pages' Test.PagesProgram, which 1.5.0 failed to compile with
      -- "index out of bounds (3,3)". The first let makes `config` a record at the rank
      -- of `run`. In the second let, `Plat.update config` expands Config's deferred body
      -- (Type.Unify.forceDeferred) and merges it into that record. The solver then
      -- introduced the expanded variables at the inner let's rank, which raised the
      -- merged class back to it. `b` is only reachable through that class, so the inner
      -- generalize left the class at the inner rank, but did not generalize it, as its
      -- representative (`config`'s record) is not in the inner pool. Generalizing `run`
      -- then met a variable with a rank above its pool's (Type.Solve.poolToRankTable).
      let
        project = "./test/scenario-compile-perf-rank"

        setup = do
          rmdir project
          Dir.createDirectoryIfMissing True (project </> "src")
          Dir.copyFile "./test/scenario-empty-elm-init/elm.json" (project </> "elm.json")
          writeUtf8 (project </> "src/Plat.elm") rankPlatModule
          writeUtf8 (project </> "src/Main.elm") rankMainModule

        cleanup _ =
          rmdir project

        test _ = do
          io $ do
            Lamdera.unsetEnv "LOVR"
            Lamdera.unsetEnv "LTEST"
            Lamdera.unsetEnv "LDEBUG"
          actual <- catchOutput $ Lamdera.Compile.makeDev project ["src/Main.elm"]
          expectTextDoesNotContain actual "index out of bounds"
          expectTextContains actual "Success! Compiled 2 modules."
      in
      using setup cleanup test
  ]


-- (argument, argument of Node in the real type) of every `Html x` in a type
htmlAliases :: Can.Type -> [(String, String)]
htmlAliases tipe =
  case tipe of
    Can.TAlias _ name [(_, arg)] (Can.Filled real@(Can.TType _ node [nodeArg])) | TN.nameToChars name == "Html" && TN.nameToChars node == "Node" ->
      (typeName arg, typeName nodeArg) : htmlAliases real

    Can.TAlias _ _ args aliasType ->
      concatMap (htmlAliases . snd) args ++
        case aliasType of
          Can.Filled t -> htmlAliases t
          Can.Holey t -> htmlAliases t

    Can.TLambda a b -> htmlAliases a ++ htmlAliases b
    Can.TType _ _ args -> concatMap htmlAliases args
    Can.TRecord fields _ -> concatMap (\(Can.FieldType _ t) -> htmlAliases t) (Map.elems fields)
    Can.TPair a b -> htmlAliases a ++ htmlAliases b
    Can.TTriple a b c -> htmlAliases a ++ htmlAliases b ++ htmlAliases c
    Can.TUnit -> []
    Can.TVar _ -> []


typeName :: Can.Type -> String
typeName tipe =
  case tipe of
    Can.TType _ name _ -> TN.nameToChars name
    _ -> "?"


wrappedModule :: T.Text
wrappedModule =
  T.unlines
    [ "module Wrapped exposing (..)"
    , ""
    , "import Html exposing (Html)"
    , ""
    , "type Msg1 = M1"
    , ""
    , "type Msg2 = M2"
    , ""
    , "type alias Wrap a = { inner : Html a, n : Int }"
    , ""
    , "get1 : Wrap Msg1 -> Wrap Msg1"
    , "get1 w = { w | n = 1 }"
    , ""
    , "get2 : Wrap Msg2 -> Wrap Msg2"
    , "get2 w = { w | n = 2 }"
    ]


mainModule :: T.Text
mainModule =
  T.unlines
    [ "module Main exposing (main)"
    , ""
    , "import Html"
    , "import Wrapped"
    , ""
    , "main = Html.text (String.fromInt (Wrapped.get2 { inner = Html.text \"\", n = 0 }).n)"
    ]


rankPlatModule :: T.Text
rankPlatModule =
  T.unlines
    [ "module Plat exposing (Config, update)"
    , ""
    , "type alias Config a b ="
    , "    { toRoute : String -> a"
    , "    , other : b -> a"
    , "    }"
    , ""
    , "update : Config a b -> Int"
    , "update config ="
    , "    0"
    ]


rankMainModule :: T.Text
rankMainModule =
  T.unlines
    [ "module Main exposing (main)"
    , ""
    , "import Html"
    , "import Plat"
    , ""
    , "run config ="
    , "    let"
    , "        route ="
    , "            config.toRoute \"\""
    , "    in"
    , "    let"
    , "        n ="
    , "            Plat.update config"
    , "    in"
    , "    n"
    , ""
    , "main ="
    , "    Html.text (String.fromInt (run { toRoute = String.length, other = identity }))"
    ]
