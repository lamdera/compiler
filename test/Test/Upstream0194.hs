{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Upstream0194 where

{- @LAMDERA Bugs in Elm 0.19.4-beta that we fix ourselves. One case per entry in
extra/docs/upstream-divergence.md, each a small plain Elm project that shows the bug with
the beta. When a new upstream release comes out, merge it, drop our fix for an entry,
and run these: if the case still passes, upstream's fix covers it and ours can go.
-}

import Lamdera
import EasyTest
import Test.Helpers

import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>), takeDirectory)
import NeatInterpolation (text)

import Make (Flags(..))
import qualified Make
import qualified Ext.Common
import qualified Lamdera.Compile


suite :: Test ()
suite =
  tests
    [ -- 01, also reported upstream by someone else
      scope "recursive top-level functions are in the JS output" $
        withProject "recursive"
          [ ("src/Main.elm", [text|
              module Main exposing (main)

              import Html exposing (text)

              countdown : Int -> List Int
              countdown n =
                  if n <= 0 then
                      []

                  else
                      n :: countdown (n - 1)

              main =
                  text (String.fromInt (List.length (countdown 3)))
            |])
          ] $ \project -> do
            js <- io $ buildJs project "src/Main.elm"
            expectTextContains js "var $author$project$Main$countdown = "
            expectTextContains js "var $elm$core$List$foldl = "

    , -- 02, also reported upstream by someone else
      scope "a port module can expose its ports" $
        compiles "ports"
          [ ("src/Ports.elm", [text|
              port module Ports exposing (toJs)

              port toJs : String -> Cmd msg
            |])
          , ("src/Main.elm", [text|
              module Main exposing (cmd)

              import Ports

              cmd : Cmd msg
              cmd =
                  Ports.toJs "hi"
            |])
          ]

    , -- 03, also reported upstream by someone else (the Json case)
      scope "a type exposed by two imports, one an alias of the other (elm-geometry)" $
        compiles "ambiguous-alias"
          [ ("src/Types.elm", [text|
              module Types exposing (Spline(..))

              type Spline
                  = Spline Int
            |])
          , ("src/Spline.elm", [text|
              module Spline exposing (Spline)

              import Types

              type alias Spline =
                  Types.Spline
            |])
          , ("src/Main.elm", [text|
              module Main exposing (degree)

              import Spline exposing (Spline)
              import Types exposing (Spline)

              degree : Spline -> Int
              degree _ =
                  2
            |])
          ]

    , scope "Json.Decode.Value and Json.Encode.Value both exposed" $
        compiles "ambiguous-json"
          [ ("src/Main.elm", [text|
              module Main exposing (identity)

              import Json.Decode exposing (Value)
              import Json.Encode exposing (Value)

              identity : Value -> Value
              identity v =
                  v
            |])
          ]

    , -- 04, also reported upstream by someone else
      scope "an unannotated record function's .elmi reads back on the next build" $
        withProject "elmi-fields"
          [ ("src/A.elm", [text|
              module A exposing (getX, zero)

              getX r =
                  r.x

              zero : Int
              zero =
                  0
            |])
          , ("src/Main.elm", [text|
              module Main exposing (one)

              import A

              one : Int
              one =
                  A.getX { x = 1 }
            |])
          ] $ \project -> do
            first <- catchOutput $ Lamdera.Compile.makeDev project ["src/Main.elm"]
            expectTextContains first "Success!"
            -- makeDev touches the file, so this build reads A.elmi back
            second <- catchOutput $ Lamdera.Compile.makeDev project ["src/Main.elm"]
            expectTextDoesNotContain second "Corrupt File"
            expectTextContains second "Success!"

    , -- 05, not reported upstream yet
      scope "an unannotated identity function is exported as a -> a" $
        fails "identity-export" "TYPE MISMATCH"
          [ ("src/A.elm", [text|
              module A exposing (ident)

              ident x =
                  x
            |])
          , ("src/Main.elm", [text|
              module Main exposing (oops)

              import A

              oops : Int
              oops =
                  A.ident "not an Int"
            |])
          ]

    , -- 06, not reported upstream yet
      scope "unannotated mutually recursive functions" $
        compiles "mutual-recursion"
          [ ("src/Main.elm", [text|
              module Main exposing (ping)

              ping x n =
                  if n == 0 then
                      x

                  else
                      pong x (n - 1)

              pong x n =
                  if n == 0 then
                      x

                  else
                      ping x (n - 1)
            |])
          ]

    , -- 07, also reported upstream by someone else
      scope "two extensible records, each with a field the other lacks" $
        compiles "ext-ext"
          [ ("src/Main.elm", [text|
              module Main exposing (update)

              setA : { m | a : Int } -> { m | a : Int }
              setA model =
                  { model | a = 1 }

              update flag model =
                  if flag then
                      setA model

                  else
                      { model | b = 1 }
            |])
          ]

    , scope "the same field used as an Int and a String through an extensible record" $
        fails "ext-ext-mismatch" "TYPE MISMATCH"
          [ ("src/Main.elm", [text|
              module Main exposing (f)

              setA : { r | a : Int } -> { r | a : Int }
              setA r =
                  { r | a = 0 }

              g y =
                  let
                      z =
                          setA y
                  in
                  y.b + String.length z.b

              f =
                  g { a = 1, b = 2 }
            |])
          ]

    , -- 08, also reported upstream by someone else
      scope "an alias of a function type in an annotation" $
        compiles "fn-alias"
          [ ("src/Main.elm", [text|
              module Main exposing (both, f, g)

              type alias Mappend a =
                  a -> a -> a

              both : Mappend a -> Mappend b -> Mappend ( a, b )
              both f1 g1 ( a1, b1 ) ( a2, b2 ) =
                  ( f1 a1 a2, g1 b1 b2 )

              type alias Fn a =
                  Int -> a

              f : Fn Int
              f n =
                  n

              g : b -> Fn (Maybe b)
              g b _ =
                  Just b
            |])
          ]

    , -- 09, also reported upstream by someone else
      scope "the hint for two unexpected record fields" $
        fails "unexpected-fields" "Looks like fields b and c are unexpected"
          [ ("src/Main.elm", [text|
              module Main exposing (r)

              r : { a : Int }
              r =
                  { a = 1, b = 2, c = 3 }
            |])
          ]

    , -- nearestSquare, reported upstream by someone else
      scope "deeply nested lets" $
        compiles "nested-lets"
          [ ("src/Main.elm", nestedLets 20) ]

    -- 10 (initState writing past its 5 bytes) shows no difference an Elm build can
    -- check; see its entry in extra/docs/upstream-divergence.md.
    ]


-- f = (let v19 = (let v18 = ... (let v0 = 1 in v0) ... in v18) in v19)
nestedLets :: Int -> Text
nestedLets depth =
  let
    names = [ "v" <> T.pack (show i) | i <- [depth - 1, depth - 2 .. 0] ]
    opens = T.concat [ "(let " <> n <> " = " | n <- names ]
    closes = T.concat [ " in " <> n <> ")" | n <- reverse names ]
  in
  "module Main exposing (f)\n\nf = " <> opens <> "1" <> closes <> "\n"


compiles :: String -> [(FilePath, Text)] -> Test ()
compiles label files =
  withProject label files $ \project -> do
    actual <- catchOutput $ Lamdera.Compile.makeDev project (mainOf files)
    expectTextContains actual "Success!"


fails :: String -> Text -> [(FilePath, Text)] -> Test ()
fails label problem files =
  withProject label files $ \project -> do
    actual <- catchOutput $ Lamdera.Compile.makeDev project (mainOf files)
    expectTextContains actual problem


-- The last file is the one to build; the others are its imports.
mainOf :: [(FilePath, Text)] -> [FilePath]
mainOf files =
  [ fst (last files) ]


buildJs :: FilePath -> FilePath -> IO Text
buildJs project file = do
  let output = "elm-stuff/upstream-test.js"
  absRoot <- Dir.makeAbsolute project
  Ext.Common.withProjectRoot absRoot $
    Make.run [file] $
      Make.Flags
        { _debug = False
        , _optimize = False
        , _output = Just (Make.JS output)
        , _report = Nothing
        , _docs = Nothing
        , _noWire = True
        , _optimizeLegible = False
        , _esm = False
        }
  maybe "" id <$> readUtf8Text (absRoot </> output)


withProject :: String -> [(FilePath, Text)] -> (FilePath -> Test a) -> Test a
withProject label files test =
  let
    project = "./test/scenario-upstream-0194-" ++ label

    setup = do
      rmdir project
      Lamdera.unsetEnv "LOVR"
      Lamdera.unsetEnv "LTEST"
      Lamdera.unsetEnv "LDEBUG"
      Dir.createDirectoryIfMissing True (project </> "src")
      writeUtf8 (project </> "elm.json") elmJson
      mapM_ (\(path, source) -> do
          Dir.createDirectoryIfMissing True (takeDirectory (project </> path))
          writeUtf8 (project </> path) source
        ) files
  in
  using setup (\_ -> rmdir project) (\_ -> test project)


elmJson :: Text
elmJson =
  [text|
    {
        "type": "application",
        "source-directories": [
            "src"
        ],
        "elm-version": "0.19.4",
        "dependencies": {
            "direct": {
                "elm/core": "1.0.5",
                "elm/html": "1.0.0",
                "elm/json": "1.1.3"
            },
            "indirect": {
                "elm/virtual-dom": "1.0.3"
            }
        },
        "test-dependencies": {
            "direct": {},
            "indirect": {}
        }
    }
  |]
