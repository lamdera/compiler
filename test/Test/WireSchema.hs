{-# LANGUAGE OverloadedStrings #-}

module Test.WireSchema where

{-| The wire schema (Lamdera.WireSchema): compiles test/scenario-wire-schema in dev and
--optimize modes, each with the direct codecs and the Elm codecs (LAMDERA_WIRE3_DIRECT=0),
and runs check.js, which encodes sample values with the generated codecs and decodes them
with the schema the build wrote and the reference decoder, runtime/js/wireSchema.js in the
platform repo. That decoder is looked for at $LAMDERA_WIRE_SCHEMA_JS, else in
~/dev/projects/lamdera/runtime/js/wireSchema.js.

The platform repo is private, so CI and other checkouts of this public repo don't have
the decoder: without LAMDERA_WIRE_SCHEMA_JS and without the default file, these cases are
skipped (reported as pending). With LAMDERA_WIRE_SCHEMA_JS set, a missing file fails.
-}

import qualified Data.Map as Map
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import System.FilePath ((</>))

import Lamdera
import EasyTest
import Test.Helpers

import Make (Flags(..))
import qualified Make
import qualified Elm.Package as Pkg
import qualified Ext.Common
import qualified Lamdera.Relative
import qualified Lamdera.WireSchema


suite :: Test ()
suite =
  tests
    [ scope "not a Lamdera app: no schema" $
        expectEqual Nothing (Lamdera.WireSchema.fromNodes Pkg.dummyName Map.empty)
    , scope "schema decodes the codecs' bytes, dev" $ requireBinaryOnPath "node" $ runScenario False Nothing
    , scope "schema decodes the codecs' bytes, --optimize" $ requireBinaryOnPath "node" $ runScenario True Nothing
    , scope "schema decodes the codecs' bytes, Elm codecs" $ requireBinaryOnPath "node" $ runScenario False (Just "0")
    ]


data Decoder
  = Found FilePath
  | NotSet FilePath -- LAMDERA_WIRE_SCHEMA_JS unset, and nothing at the default path
  | Missing FilePath -- LAMDERA_WIRE_SCHEMA_JS names a file that isn't there


decoderPath :: IO Decoder
decoderPath = do
  envM <- Env.lookupEnv "LAMDERA_WIRE_SCHEMA_JS"
  home <- Dir.getHomeDirectory
  let path = maybe (home </> "dev/projects/lamdera/runtime/js/wireSchema.js") id envM
  exists <- Dir.doesFileExist path
  pure $
    if exists then Found path
    else maybe (NotSet path) (const (Missing path)) envM


runScenario :: Bool -> Maybe String -> Test ()
runScenario optimize direct = do
  project <- io $ Lamdera.Relative.requireDir "test/scenario-wire-schema"
  decoderM <- io decoderPath
  case decoderM of
    NotSet path -> do
      noteScoped $ "SKIPPED (no runtime/js/wireSchema.js at " ++ path ++ "; set LAMDERA_WIRE_SCHEMA_JS to its path)"
      pending (pure ())

    Missing path ->
      crash $ "LAMDERA_WIRE_SCHEMA_JS is " ++ path ++ ", which doesn't exist"

    Found decoder -> do
      let
        elmStuff = project </> "elm-stuff"
        output = elmStuff </> "app.js"
        schema = Lamdera.WireSchema.schemaPath project

      _ <- ioSilenced $ do
        rmdir elmStuff
        withEnvVars ([ ("ELM_HOME", project </> "elm-home") ] ++ maybe [] (\d -> [("LAMDERA_WIRE3_DIRECT", d)]) direct) $
          Ext.Common.withProjectRoot project $
            Make.run ["src/Main.elm"] $
              Make.Flags
                { _debug = False
                , _optimize = optimize
                , _output = Just (Make.JS "elm-stuff/app.js")
                , _report = Nothing
                , _docs = Nothing
                , _noWire = False
                , _optimizeLegible = False
                , _esm = False
                }

      written <- io $ Dir.doesFileExist schema
      if not written
        then crash ("no schema written to " ++ schema)
        else do
          (exit, out, err) <- io $ Ext.Common.cq_ "node" [project </> "check.js", output, schema, decoder] ""
          io $ rmdir elmStuff
          if exit == Exit.ExitSuccess
            then ok
            else crash ("check.js failed:\n" ++ take 4000 (out <> err))
