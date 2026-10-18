{-# LANGUAGE OverloadedStrings #-}

module Test.BinaryCounts where

{-| The *8 counts in Bytes.Encode / Bytes.Decode (list8, dict8, set8, array8, string8).

Elm 0.19.3 writes these counts as a single byte. A 256th element used to wrap the count
silently, so the compiler wrote .elmi files it could not read back ("Corrupt File ...
Error when expecting: Type"). Lamdera's generated Wire3 codecs got there: a large app's
w3_decode_BackendModel has 510 type variables. Counts of 255 and up are now written as
255 followed by a u32, and smaller counts keep the exact one-byte format, so existing
caches stay readable. See Bytes.Encode.count8. Float literals (Data.Utf8.encode8)
had the same wrap.

The end-to-end test compiles one module with every shape whose size the format
records - type variables, constructor args, constructors, record fields, names, float
and string literals - at 300, past the old ceiling, and reads back its .elmi and .elmo.

-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>))

import EasyTest
import Lamdera hiding ((&))
import Test.Helpers

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as TN
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Elm.Interface as I
import qualified File
import qualified Lamdera.Compile
import qualified String as S


all = EasyTest.run suite


suite :: Test ()
suite = tests
  [ scope "counts below 255 keep the old one-byte format, so existing caches stay readable" $ do
      expectEqual [3, 7, 8, 9] (encode (E.list8 E.u8 [7, 8, 9]))
      expectEqual (254 : replicate 254 1) (encode (E.list8 E.u8 (replicate 254 1)))
      expectEqual [2, 104, 105] (encode (E.string8 (S.fromChars "hi")))

  , scope "a count of 255 and up is 255 then a u32" $ do
      expectEqual [255, 255, 0, 0, 0] (take 5 (encode (E.list8 E.u8 (replicate 255 1))))
      expectEqual [255, 254, 1, 0, 0] (take 5 (encode (E.list8 E.u8 (replicate 510 1))))

  , scope "list8 round trips at and past the old ceiling" $
      tests $ flip map [0, 1, 254, 255, 256, 510, 70000] $ \n ->
        scope (show n) $ do
          let xs = take n (cycle [0 .. 250])
          result <- io $ D.fromByteString (D.list8 D.u8) (strict (E.list8 E.u8 xs))
          expectEqual (Right xs) result

  , scope "dict8, set8 and string8 round trip past the old ceiling" $ do
      let d = Map.fromList [ (k, k * 2) | k <- [1 .. 600 :: Int] ]
      dResult <- io $ D.fromByteString (D.dict8 D.int D.int) (strict (E.dict8 E.int E.int d))
      expectEqual (Right d) dResult

      let str = S.fromChars (replicate 300 'x')
      sResult <- io $ D.fromByteString D.string8 (strict (E.string8 str))
      expectEqual (Right (S.toChars str)) (S.toChars <$> sResult)

  , scope "an escaped count split across fromFile's 32kb read buffer decodes" $
      -- The count's five bytes land on every offset around the 32768 byte boundary.
      tests $ flip map [32760 .. 32770] $ \padding ->
        scope (show padding) $ do
          let xs = replicate 300 (7 :: Integer)
              bytes = strict (E.list32 E.u8 (replicate (padding - 4) 0) <> E.list8 E.int (map fromIntegral xs))
              decoder = (,) <$> D.list32 D.u8 <*> D.list8 D.int
          result <- io $ withTempFile bytes (D.fromFile decoder)
          expectEqual (Right (map fromIntegral xs)) (snd <$> result)

  , scope "a module with every size-dependent shape at 300 writes an .elmi and .elmo it can read back" $
      let
        project = "./test/scenario-binary-counts"

        setup = do
          rmdir project
          Dir.createDirectoryIfMissing True (project </> "src")
          Dir.copyFile "./test/scenario-empty-elm-init/elm.json" (project </> "elm.json")
          writeUtf8 (project </> "src/Wide.elm") wideModule
          writeUtf8 (project </> "src/Main.elm") mainModule

        cleanup _ =
          rmdir project

        test _ = do
          ioSilenced $ do
            Lamdera.unsetEnv "LOVR"
            Lamdera.unsetEnv "LTEST"
            Lamdera.unsetEnv "LDEBUG"
            Lamdera.Compile.makeDev project ["src/Main.elm"]

          elmiM <- io $ findFile (project </> "elm-stuff") "Wide.elmi"
          elmi <- maybe (crash "Wide.elmi was not written") pure elmiM

          -- File.readBytes is what the build uses for cached modules: Nothing here is
          -- the "Corrupt File" path that failed a large app's deploy.
          ifaceM <- io $ File.readBytes I.dInterface elmi
          iface <- maybe (crash "Wide.elmi could not be read back") pure ifaceM

          let annotationVars name =
                case Map.lookup (N.fromChars name) (I._values iface) of
                  Just (Can.Forall vars _) -> Just (Map.size vars)
                  Nothing -> Nothing

          scope "annotation free vars (dict8)" $
            expectEqual (Just wide) (annotationVars "useMany")

          scope "long value name (string8)" $
            expectEqual (Just 0) (annotationVars longName)

          scope "union vars and constructor args (list8)" $
            case [ u | (n, u) <- Map.toList (I._unions iface), TN.nameToChars n == "Many" ] of
              [u] ->
                let Can.Union vars ctors _ _ = I.extractUnion u
                in expectEqual (wide, [wide]) (length vars, [ length args | Can.Ctor _ _ _ args <- ctors ])
              _ -> crash "union Many missing from Wide.elmi"

          scope "record with 300 fields (dict32, u16 field index)" $
            case [ a | (n, a) <- Map.toList (I._aliases iface), TN.nameToChars n == "WideRecord" ] of
              [a] ->
                case I.extractAlias a of
                  Can.Alias _ (Can.TRecord fields _) -> expectEqual wide (Map.size fields)
                  _ -> crash "WideRecord is not a record"
              _ -> crash "alias WideRecord missing from Wide.elmi"

          scope "300 constructors, plain and with args (list32, ctor index)" $ do
            let ctorCount name =
                  [ length ctors | (n, u) <- Map.toList (I._unions iface), TN.nameToChars n == name
                                 , let Can.Union _ ctors _ _ = I.extractUnion u ]
            expectEqual [wide] (ctorCount "ManyCtors")
            expectEqual [wide] (ctorCount "ManyCtorsWithArgs")

          -- The .elmo holds the optimized code: float literals (Utf8.encode8), string
          -- literals, constructor indexes and the generated Wire3 codecs for all of the above.
          scope "Wide.elmo reads back" $ do
            elmoM <- io $ findFile (project </> "elm-stuff") "Wide.elmo"
            elmo <- maybe (crash "Wide.elmo was not written") pure elmoM
            graphM <- io $ File.readBytes Opt.dLocalGraph elmo
            expect (isJust graphM)

          scope "alias vars (list8)" $
            case [ a | (n, a) <- Map.toList (I._aliases iface), TN.nameToChars n == "ManyAlias" ] of
              [a] ->
                let Can.Alias vars _ = I.extractAlias a
                in expectEqual wide (length vars)
              _ -> crash "alias ManyAlias missing from Wide.elmi"
      in
      using setup cleanup test
  ]


wide :: Int
wide = 300


longName :: String
longName = "long" ++ replicate 296 'x'


wideModule :: T.Text
wideModule =
  let
    vars = [ "a" ++ show i | i <- [1 .. wide] ]
    spaced = List.intercalate " "
  in
  T.pack $ unlines
    [ "module Wide exposing (..)"
    , ""
    , ""
    , "type Many " ++ spaced vars
    , "    = Many " ++ spaced vars
    , ""
    , ""
    , "type alias ManyAlias " ++ spaced vars ++ " ="
    , "    Many " ++ spaced vars
    , ""
    , ""
    , "useMany : ManyAlias " ++ spaced vars ++ " -> ()"
    , "useMany _ ="
    , "    ()"
    , ""
    , ""
    , longName ++ " : Int"
    , longName ++ " ="
    , "    1"
    , ""
    , ""
    , "type alias WideRecord ="
    , "    { " ++ List.intercalate "\n    , " [ "f" ++ show i ++ " : Int" | i <- [1 .. wide] ]
    , "    }"
    , ""
    , ""
    , "wideRecord : WideRecord"
    , "wideRecord ="
    , "    { " ++ List.intercalate "\n    , " [ "f" ++ show i ++ " = " ++ show i | i <- [1 .. wide] ]
    , "    }"
    , ""
    , ""
    , "type ManyCtors"
    , "    = " ++ List.intercalate "\n    | " [ "C" ++ show i | i <- [1 .. wide] ]
    , ""
    , ""
    , "type ManyCtorsWithArgs"
    , "    = " ++ List.intercalate "\n    | " [ "D" ++ show i ++ " Int" | i <- [1 .. wide] ]
    , ""
    , ""
    , "lastCtor : ManyCtorsWithArgs"
    , "lastCtor ="
    , "    D" ++ show wide ++ " 1"
    , ""
    , ""
    , "longFloat : Float"
    , "longFloat ="
    , "    1." ++ replicate 298 '1'
    , ""
    , ""
    , "longString : String"
    , "longString ="
    , "    \"" ++ replicate 70000 's' ++ "\""
    ]


mainModule :: T.Text
mainModule =
  T.pack $ unlines
    [ "module Main exposing (main)"
    , ""
    , "import Html"
    , "import Wide"
    , ""
    , ""
    , "main ="
    , "    Html.text"
    , "        (String.fromInt Wide." ++ longName
    , "            ++ String.fromFloat Wide.longFloat"
    , "            ++ Wide.longString"
    , "            ++ String.fromInt Wide.wideRecord.f" ++ show wide
    , "            ++ Debug.toString Wide.lastCtor"
    , "            ++ Debug.toString Wide.C" ++ show wide
    , "        )"
    ]


encode :: B.Builder -> [Integer]
encode =
  map fromIntegral . BS.unpack . strict


strict :: B.Builder -> BS.ByteString
strict =
  BSL.toStrict . B.toLazyByteString


withTempFile :: BS.ByteString -> (FilePath -> IO a) -> IO a
withTempFile bytes f = do
  tmp <- Dir.getTemporaryDirectory
  let path = tmp </> "lamdera-binary-counts-test.dat"
  BS.writeFile path bytes
  result <- f path
  Dir.removeFile path
  pure result


findFile :: FilePath -> FilePath -> IO (Maybe FilePath)
findFile dir name = do
  entries <- Dir.listDirectory dir
  let here = dir </> name
  exists <- Dir.doesFileExist here
  if exists
    then pure (Just here)
    else firstJust [ dir </> e | e <- entries ]
  where
    firstJust [] = pure Nothing
    firstJust (p : ps) = do
      isDir <- Dir.doesDirectoryExist p
      found <- if isDir then findFile p name else pure Nothing
      maybe (firstJust ps) (pure . Just) found
