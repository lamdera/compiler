{-# LANGUAGE OverloadedStrings #-}

module Test.ElmiTypeTable where

{-| The type table in interfaces (Elm.Interface, TYPE TABLE).

Inferred annotations hold every alias fully expanded, and interfaces used to write
that expansion at every use: an app with a large Model wrote its body thousands of
times per .elmi. Now each distinct Can.Type node is written once, in a table at the
start of the interface, and referred to by index.

@LAMDERA Since Elm 0.19.4 the types are I.Type, and an alias in a type carries the body
it was declared with (TAlias ... Aliased) rather than an expansion, but that body is
just as big and just as repeated, so the table works the same way. Decoding must give back exactly
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
import System.Mem.StableName (StableName, makeStableName)
import System.FilePath ((</>))

import EasyTest
import Lamdera hiding ((&))
import Test.Helpers

import qualified Array as A
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeIndex as TI
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as TN
import qualified AST.Prim.TypeVar as TV
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyMap as NEM
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

  , scope "share gives back an equal interface, with each distinct type allocated once" $ do
      let fresh = unsharedInterface 50
          shared = I.share fresh
      expectEqual fresh shared
      expectEqual (strict (I.eInterface fresh)) (strict (I.eInterface shared))
      freshModels <- io $ traverse stableName (modelsOf fresh)
      sharedModels <- io $ traverse stableName (modelsOf shared)
      -- built separately for every annotation in the fresh interface, one value after share
      expectEqual 50 (length (List.nub freshModels))
      expectEqual 1 (length (List.nub sharedModels))
      -- and the argument and result of every update are that same value too
      sharedResults <- io $ traverse stableName (resultModelsOf shared)
      expect (sharedModels == sharedResults)

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
            let modelUses = [ () | (n, I.Forall _ t) <- Map.toList (I._values iface), "update" `List.isPrefixOf` N.toChars n, I.TLambda _ (I.TAlias _ m _ _ (I.Aliased (I.TRecord fs))) _ <- [t], TN.nameToChars m == "Model", F.size fs == modelFields + 1 ]
            expectEqual uses (length modelUses)

          scope "the Model body is written once per distinct record, not per use" $ do
            let records = distinctRecordsWith (N.fromChars "uniqueMarkerField") iface
            -- a few forms of the Model record: the alias definition (Inner Holey, source
            -- field order) and what inference gives (Inner Filled, or Inner expanded, with
            -- other field indexes), each in the table once however many of the 200
            -- annotations use it
            expect (length records <= 3)
            expectEqual (length records) (occurrences "uniqueMarkerField" bytes)
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


intType :: I.Type
intType =
  I.TType ModuleName.basics "Int" M.int A.empty


inner :: I.Type
inner =
  I.TAlias home "Inner" M.record A.empty (I.Aliased (I.TRecord (fields ["a", "b", "c"])))


-- fields in source order
ordered :: [(N.Name, I.Type)] -> F.Fields I.Type
ordered pairs =
  case pairs of
    [] -> error "ordered: no fields"
    p : ps ->
      Maybe.fromJust $ F.fromOrdered Just $
        NEM.fromList (toOrdered 0 p) (zipWith toOrdered [1 ..] ps)
  where
    toOrdered i (n, t) = (n, F.Ordered i t)


fields :: [String] -> F.Fields I.Type
fields names =
  ordered [ (N.fromChars n, intType) | n <- names ]


model :: I.Type
model =
  I.TAlias home "Model" M.record A.empty
    (I.Aliased (I.TRecord (ordered ([ (N.fromChars ("field" ++ show i), inner) | i <- [0 .. 149 :: Int] ] ++ [(N.fromChars "uniqueMarkerField", intType)]))))


bigInterface :: Int -> I.Interface
bigInterface n =
  I.Interface
    { I._home = Pkg.dummyName
    , I._values = Map.fromList [ (N.fromChars ("update" ++ show i), I.Forall A.empty (I.TLambda model model [])) | i <- [1 .. n] ]
    , I._types =
        Map.fromList
          [ ("Msg", I.UnionOpen (I.Union M.enum A.empty (I.Variants (Tag.table (I.Variant "Set" [model]) (I.Variant "Pair" [I.TPair model inner]) []))))
          , ("Model", I.AliasPublic (I.Alias M.record A.empty (case model of I.TAlias _ _ _ _ t -> t; t -> I.Aliased t)))
          , ("Inner", I.AliasPrivate (I.Alias M.record (A.fromList ["x"]) (I.Aliased (I.TTriple (I.TVar (TI.Index 0)) I.TUnit inner))))
          ]
    , I._binops = Map.empty
    }


-- n distinct types: more than 65536 table entries
wideInterface :: Int -> I.Interface
wideInterface n =
  (bigInterface 1)
    { I._values = Map.fromList [ (N.fromChars ("v" ++ show i), I.Forall (A.fromList [TV.varFromChars ("a" ++ show i)]) (I.TType home (TN.nameFromChars ("T" ++ show i)) M.enum (A.fromList [I.TVar (TI.Index 0)]))) | i <- [1 .. n] ]
    }


-- every annotation builds its own Model tree, like inference does: nothing is shared
unsharedInterface :: Int -> I.Interface
unsharedInterface n =
  (bigInterface 1)
    { I._values = Map.fromList [ (N.fromChars ("update" ++ show i), I.Forall A.empty (I.TLambda (freshModel i) (freshModel (i + n)) [])) | i <- [1 .. n] ]
    }


freshModel :: Int -> I.Type
freshModel i =
  I.TAlias home "Model" M.record A.empty
    (I.Aliased (I.TRecord
      (ordered [ (N.fromChars ("field" ++ show f), I.TType ModuleName.basics (if i < 0 then "Never" else "Int") M.int A.empty) | f <- [0 .. 49 :: Int] ])))


modelsOf :: I.Interface -> [I.Type]
modelsOf iface =
  [ m | I.Forall _ (I.TLambda m _ _) <- Map.elems (I._values iface) ]


resultModelsOf :: I.Interface -> [I.Type]
resultModelsOf iface =
  [ m | I.Forall _ (I.TLambda _ m _) <- Map.elems (I._values iface) ]


stableName :: a -> IO (StableName a)
stableName a =
  makeStableName $! a


-- DependencyInterface has no Eq
dependencyKey :: I.DependencyInterface -> Either I.Interface (Pkg.Name, [(TN.Name, Either I.Union I.Alias)])
dependencyKey dep =
  case dep of
    I.Public i -> Left i
    I.Private p ts -> Right (p, Map.toList (Map.map (\t -> case t of { I.PrivateUnion u -> Left u; I.PrivateAlias a -> Right a }) ts))



-- MEASURING


-- What the inline encoding (eType at every use) wrote for the types of an interface.
inlineTypeBytes :: I.Interface -> Int
inlineTypeBytes =
  sum . map (BS.length . strict . I.eType) . interfaceTypes


interfaceTypes :: I.Interface -> [I.Type]
interfaceTypes (I.Interface _ values types binops) =
  [ t | I.Forall _ t <- Map.elems values ]
  ++ concatMap (I.extract unionTypes (\(I.Alias _ _ (I.Aliased t)) -> [t])) (Map.elems types)
  ++ [ t | I.Binop _ (I.Forall _ t) _ _ <- Map.elems binops ]
  where
    unionTypes (I.Union _ _ variants) =
      case variants of
        I.Variants table -> concat (Tag.unpack table (\_ ts -> ts) (\_ (I.Variant _ ts) -> ts))
        I.Enum _ -> []
        I.Vector _ a b cs -> a : b : cs
        I.Box _ a -> [a]
        I.Void _ -> []


distinctRecordsWith :: N.Name -> I.Interface -> [I.Type]
distinctRecordsWith field iface =
  List.nub [ r | t <- interfaceTypes iface, r@(I.TRecord fs) <- subtrees t, Map.member field (NEM.toDict (F.toNEM fs)) ]


subtrees :: I.Type -> [I.Type]
subtrees tipe =
  tipe :
    case tipe of
      I.TLambda a b cs -> concatMap subtrees (a : b : cs)
      I.TVar _ -> []
      I.TRecord fs -> concatMap subtrees (fmap snd (NEM.toList (F.toNEM fs)))
      I.TExtRec fs _ -> concatMap subtrees (fmap snd (NEM.toList (F.toNEM fs)))
      I.TEmpty -> []
      I.TUnit -> []
      I.TPair a b -> subtrees a ++ subtrees b
      I.TTriple a b c -> subtrees a ++ subtrees b ++ subtrees c
      I.TAlias _ _ _ xs (I.Aliased t) -> concatMap subtrees (A.toList xs) ++ subtrees t
      I.TType _ _ _ ts -> concatMap subtrees (A.toList ts)


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
