{-# LANGUAGE OverloadedStrings #-}

module Test.ElmiTypeTable where

{-| The type table in interfaces (Elm.Interface, TYPE TABLE).

Inferred annotations hold every alias fully expanded, and interfaces used to write
that expansion at every use: an app with a large Model wrote its body thousands of
times per .elmi. Now each distinct Can.Type node is written once, in a table at the
start of the interface, and referred to by index. Decoding must give back exactly
the values the inline encoding did (type checking, JS, Wire3 and type hashes all see
them), so these tests round trip synthetic and real interfaces, and check that a
module repeating a big alias writes its body once.

-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>))

import EasyTest
import Lamdera hiding ((&))
import Test.Helpers

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as TN
import qualified AST.Prim.TypeVar as TV
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Data.Index as Index
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Lamdera.Compile


all = EasyTest.run suite


suite :: Test ()
suite = tests
  [ scope "a synthetic interface round trips" $ do
      result <- io $ decodeBytes I.dInterface (strict (I.eInterface (bigInterface 300)))
      expectEqual (Right (bigInterface 300)) result

  , scope "a big alias used 300 times is written once" $ do
      let iface = bigInterface 300
          size = BS.length (strict (I.eInterface iface))
          inline = inlineTypeBytes iface
      -- The record has 150 fields; inline, every one of the 300 annotations repeats it
      -- twice (argument and result).
      expect (inline > 300 * 2 * 150 * 10)
      expect (size < 6000)
      expectEqual 1 (occurrences "uniqueMarkerField" (strict (I.eInterface iface)))

  , scope "a table past 65536 entries switches to u32 refs and round trips" $ do
      let iface = wideInterface 70000
          bytes = strict (I.eInterface iface)
      result <- io $ decodeBytes I.dInterface bytes
      expectEqual (Right iface) result

  , scope "a Private dependency interface round trips" $ do
      let dep = I.private (bigInterface 20)
      result <- io $ decodeBytes I.dDependencyInterface (strict (I.eDependencyInterface dep))
      expectEqual (Right (dependencyKey dep)) (dependencyKey <$> result)

  , scope "a ref to the entry itself or later is corrupt, not followed" $ do
      -- table of one entry: a lambda whose children are refs 0 and 0 (u16), itself
      let bytes = strict (E.u32 1 <> E.u8 0 <> E.u16 0 <> E.u16 0)
      result <- io $ D.fromByteString (D.u32 >> D.u8 >> D.u16 >> D.u16) bytes
      expect (either (const False) (const True) result)
      tableResult <- io $ decodeBytes I.dInterface (strict (E.u32 1 <> E.u8 0 <> E.u16 0 <> E.u16 0 <> Pkg.eName Pkg.dummyName <> E.u32 0 <> E.u32 0 <> E.u32 0 <> E.u32 0))
      expect (either (const True) (const False) tableResult)

  , scope "interfaces of a compiled project round trip, and a repeated Model is written once" $
      let
        project = "./test/scenario-elmi-type-table"

        setup = do
          rmdir project
          Dir.createDirectoryIfMissing True (project </> "src")
          Dir.copyFile "./test/scenario-empty-elm-init/elm.json" (project </> "elm.json")
          writeUtf8 (project </> "src/Big.elm") bigModule
          writeUtf8 (project </> "src/Main.elm") mainModule

        cleanup _ =
          rmdir project

        test _ = do
          ioSilenced $ do
            Lamdera.unsetEnv "LOVR"
            Lamdera.unsetEnv "LTEST"
            Lamdera.unsetEnv "LDEBUG"
            Lamdera.Compile.makeDev project ["src/Main.elm"]

          elmiM <- io $ findFile (project </> "elm-stuff") "Big.elmi"
          elmi <- maybe (crash "Big.elmi was not written") pure elmiM
          ifaceM <- io $ File.readBytes I.dInterface elmi
          iface <- maybe (crash "Big.elmi could not be read back") pure ifaceM
          bytes <- io $ BS.readFile elmi

          scope "Big.elmi round trips" $ do
            result <- io $ decodeBytes I.dInterface (strict (I.eInterface iface))
            expectEqual (Right iface) result

          scope "re-encoding gives the same bytes" $
            expectEqual bytes (strict (I.eInterface iface))

          scope "every update function has the expanded Model" $ do
            let modelUses = [ () | (n, Can.Forall _ t) <- Map.toList (I._values iface), "update" `List.isPrefixOf` N.toChars n, Can.TLambda _ (Can.TLambda (Can.TAlias _ m _ (Can.Filled (Can.TRecord fs _))) _) <- [t], TN.nameToChars m == "Model", Map.size fs == modelFields + 1 ]
            expectEqual uses (length modelUses)

          scope "the Model body is written once per distinct record, not per use" $ do
            let records = distinctRecordsWith (N.fromChars "uniqueMarkerField") iface
            -- a few forms of the Model record: the alias definition (Inner Holey, source
            -- field order) and what inference gives (Inner Filled, or Inner expanded, with
            -- other field indexes), each in the table once however many of the 200
            -- annotations use it
            expect (Set.size records <= 3)
            expectEqual (Set.size records) (occurrences "uniqueMarkerField" bytes)
            -- inline, the 150 field Model was written at every use: about 9MB, now 9KB
            expect (BS.length bytes * 100 < inlineTypeBytes iface)

          scope "the package interfaces in i.dat round trip" $ do
            idatM <- io $ findFile (project </> "elm-stuff") "i.dat"
            idat <- maybe (crash "i.dat was not written") pure idatM
            depsM <- io $ File.readBytes (D.dict32 ModuleName.dCanonical I.dDependencyInterface) idat
            deps <- maybe (crash "i.dat could not be read back") pure depsM
            expect (Map.size deps > 10)
            result <- io $ decodeBytes (D.dict32 ModuleName.dCanonical I.dDependencyInterface) (strict (E.dict32 ModuleName.eCanonical I.eDependencyInterface deps))
            expectEqual (Right (Map.map dependencyKey deps)) (Map.map dependencyKey <$> result)
      in
      using setup cleanup test
  ]



-- SYNTHETIC INTERFACES


home :: ModuleName.Canonical
home =
  ModuleName.Canonical Pkg.dummyName "Big"


intType :: Can.Type
intType =
  Can.TType ModuleName.basics "Int" []


inner :: Can.Type
inner =
  Can.TAlias home "Inner" [] (Can.Filled (Can.TRecord (fields ["a", "b", "c"]) Nothing))


fields :: [String] -> Map.Map N.Name Can.FieldType
fields names =
  Map.fromList [ (N.fromChars n, Can.FieldType i intType) | (n, i) <- zip names [0 ..] ]


model :: Can.Type
model =
  Can.TAlias home "Model" []
    (Can.Filled (Can.TRecord
      (Map.fromList ((N.fromChars "uniqueMarkerField", Can.FieldType 150 intType) : [ (N.fromChars ("field" ++ show i), Can.FieldType (fromIntegral i) inner) | i <- [0 .. 149 :: Int] ]))
      Nothing))


bigInterface :: Int -> I.Interface
bigInterface n =
  I.Interface
    { I._home = Pkg.dummyName
    , I._values = Map.fromList [ (N.fromChars ("update" ++ show i), Can.Forall Map.empty (Can.TLambda model model)) | i <- [1 .. n] ]
    , I._unions = Map.singleton "Msg" (I.OpenUnion (Can.Union [] [Can.Ctor "Set" (Index.ZeroBased 0) 1 [model], Can.Ctor "Pair" (Index.ZeroBased 1) 1 [Can.TPair model inner]] 2 Can.Normal))
    , I._aliases = Map.fromList [("Model", I.PublicAlias (Can.Alias [] (case model of Can.TAlias _ _ _ (Can.Filled t) -> t; t -> t))), ("Inner", I.PrivateAlias (Can.Alias ["x"] (Can.TTriple (Can.TVar "x") Can.TUnit inner)))]
    , I._binops = Map.empty
    }


-- n distinct types: more than 65536 table entries
wideInterface :: Int -> I.Interface
wideInterface n =
  (bigInterface 1)
    { I._values = Map.fromList [ (N.fromChars ("v" ++ show i), Can.Forall Map.empty (Can.TType home "T" [Can.TVar (TV.varFromChars ("a" ++ show i))])) | i <- [1 .. n] ]
    }


-- DependencyInterface has no Eq
dependencyKey :: I.DependencyInterface -> Either I.Interface (Pkg.Name, [(TN.Name, Can.Union)], [(TN.Name, Can.Alias)])
dependencyKey dep =
  case dep of
    I.Public i -> Left i
    I.Private p u a -> Right (p, Map.toList u, Map.toList a)



-- MEASURING


-- What the inline encoding (eType at every use) wrote for the types of an interface.
inlineTypeBytes :: I.Interface -> Int
inlineTypeBytes =
  sum . map (BS.length . strict . I.eType) . interfaceTypes


interfaceTypes :: I.Interface -> [Can.Type]
interfaceTypes (I.Interface _ values unions aliases binops) =
  [ t | Can.Forall _ t <- Map.elems values ]
  ++ [ t | u <- Map.elems unions, let Can.Union _ ctors _ _ = I.extractUnion u, Can.Ctor _ _ _ ts <- ctors, t <- ts ]
  ++ [ t | a <- Map.elems aliases, let Can.Alias _ t = I.extractAlias a ]
  ++ [ t | I.Binop _ (Can.Forall _ t) _ _ <- Map.elems binops ]


distinctRecordsWith :: N.Name -> I.Interface -> Set.Set Can.Type
distinctRecordsWith field iface =
  Set.fromList [ r | t <- interfaceTypes iface, r@(Can.TRecord fs _) <- subtrees t, Map.member field fs ]


subtrees :: Can.Type -> [Can.Type]
subtrees tipe =
  tipe :
    case tipe of
      Can.TLambda a b -> subtrees a ++ subtrees b
      Can.TVar _ -> []
      Can.TRecord fs _ -> concat [ subtrees t | Can.FieldType _ t <- Map.elems fs ]
      Can.TUnit -> []
      Can.TPair a b -> subtrees a ++ subtrees b
      Can.TTriple a b c -> subtrees a ++ subtrees b ++ subtrees c
      Can.TAlias _ _ xs (Can.Filled t) -> concatMap (subtrees . snd) xs ++ subtrees t
      Can.TAlias _ _ xs (Can.Holey t) -> concatMap (subtrees . snd) xs ++ subtrees t
      Can.TType _ _ ts -> concatMap subtrees ts


occurrences :: BS.ByteString -> BS.ByteString -> Int
occurrences needle haystack =
  let (_, rest) = BS.breakSubstring needle haystack
  in
  if BS.null rest then 0 else 1 + occurrences needle (BS.drop (BS.length needle) rest)


decodeBytes :: D.Decoder a -> BS.ByteString -> IO (Either String a)
decodeBytes decoder bytes =
  either (Left . show) Right <$> D.fromByteString decoder bytes


strict :: B.Builder -> BS.ByteString
strict =
  BSL.toStrict . B.toLazyByteString


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



-- PROJECT


modelFields :: Int
modelFields = 150


uses :: Int
uses = 200


bigModule :: T.Text
bigModule =
  T.pack $ unlines $
    [ "module Big exposing (..)"
    , ""
    , ""
    , "type alias Inner ="
    , "    { a : Int, b : String, c : List Int }"
    , ""
    , ""
    , "type alias Model ="
    , "    { " ++ List.intercalate "\n    , " ("uniqueMarkerField : Int" : [ "field" ++ show i ++ " : Inner" | i <- [1 .. modelFields] ])
    , "    }"
    , ""
    , ""
    , "type Msg"
    , "    = NoOp"
    , "    | Set Model"
    , ""
    , ""
    ]
    ++ concat
      [ [ "update" ++ show i ++ " : Msg -> Model -> ( Model, Cmd Msg )"
        , "update" ++ show i ++ " msg model ="
        , "    case msg of"
        , "        NoOp -> ( model, Cmd.none )"
        , "        Set m -> ( { m | uniqueMarkerField = " ++ show i ++ " }, Cmd.none )"
        , ""
        , ""
        ]
      | i <- [1 .. uses]
      ]


mainModule :: T.Text
mainModule =
  T.pack $ unlines
    [ "module Main exposing (main)"
    , ""
    , "import Big"
    , "import Html"
    , ""
    , ""
    , "main ="
    , "    Html.text (Debug.toString (Big.update" ++ show uses ++ " Big.NoOp))"
    ]
