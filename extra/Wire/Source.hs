{-# LANGUAGE OverloadedStrings #-}

module Wire.Source (generateCodecs, injectEvergreenExposing, isEvergreenCodecName) where

import qualified AST.Canonical as Can
import AST.Module.Name (Canonical(..))
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type

import qualified Data.Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import Data.List.Index (imap)
import qualified Elm.Name as N
import qualified Elm.Package as Pkg



isEvergreenCodecName :: N.Name -> Bool
isEvergreenCodecName name = "evg_encode_" `T.isPrefixOf` n || "evg_decode_" `T.isPrefixOf` n where n = N.toText name

injectEvergreenExposing :: Can.Module -> String -> String
injectEvergreenExposing (Can.Module _ _ _exports _ _ _ _ _) s =
  -- 1. figure out what types are exposed from `can`
  -- 2. inject the encoder/decoders for those types into the exposing statement by finding the first `)\n\n`, and injecting it before that.
  -- - we can assume there to be no `exposing (..)` or trailing spaces/comments after the last `)` since elm-format will move/remove those.
  -- - - this implementation only assumes there to be no spaces/comments after the last `)`, and that there are two free newlines directly after, which is what elm-format would give us.
  let
    startsWithUppercaseCharacter (x:_) | Data.Char.isUpper x = True
    startsWithUppercaseCharacter _ = False

    inject :: [String] -> String -> String
    inject exposedTypes s | ")\n\n" `List.isPrefixOf` s = -- no need to worry about `exposing ()` since there would be no exposed types to inject then.
      leftWrapWith ", " (codecsFor `concatMap` exposedTypes) <> s
    inject exposedTypes (x:xs) = x : inject exposedTypes xs
    inject _ [] = []

    codecsFor s = ["evg_encode_" ++ s, "evg_decode_" ++ s]

    leftWrapWith _ [] = ""
    leftWrapWith delim (x:xs) = delim <> x <> leftWrapWith delim xs


  in
  case _exports of
    Can.ExportEverything _ -> s
    Can.Export mapNameExport ->
      inject (startsWithUppercaseCharacter `filter` (N.toString <$> Map.keys mapNameExport)) s

-- pkgNone is just to make the types line up; it's stripped out right after it's used
pkgNone = Pkg.Name "-lamdera-inject-package-" "-lamdera-inject-module-"

generateCodecs :: Map.Map N.Name N.Name -> Can.Module -> T.Text
generateCodecs revImportDict (Can.Module _moduName _docs _exports _decls _unions _aliases _binops _effects) =
  let -- massive let-expr so we can closure in _moduName
    -- HELPERS
    qualImport :: Canonical -> Canonical
    qualImport c@(Canonical _ n) =
      case revImportDict Map.!? n of
        Just v -> Canonical pkgNone v
        Nothing -> Canonical pkgNone n

    aliasCodecs :: (N.Name, Can.Alias) -> T.Text
    aliasCodecs (name, (Can.Alias names t)) =
      let
        encoder =
          "evg_encode_" <> N.toText name <> leftWrap (codec <$> names) <> " =\n" <>
          "  " <> encoderForType Map.empty t
        decoder =
          "evg_decode_" <> N.toText name <> leftWrap (codec <$> names) <> " =\n" <>
          "  " <> decoderForType Map.empty t
      in
        encoder <> "\n\n" <> decoder

    unionCodecs :: (N.Name, Can.Union) -> T.Text
    unionCodecs (name, (Can.Union _u_vars _u_alts _u_numAlts _u_opts)) =
      let
        encoder =
          "evg_encode_" <> N.toText name <> leftWrap (codec <$> _u_vars) <> " evg_e_thingy =\n" <>
          "  case evg_e_thingy of\n    "
          <> T.intercalate "\n    "
            (case _u_opts of
              _ -> encodeUnion _u_vars _u_alts
              --Can.Normal -> codecUnion _u_vars _u_alts
              --Can.Enum -> error "codec Enum notimpl"
              --Can.Unbox -> error "codec Unbox notimpl"
            )
        decoder =
          "evg_decode_" <> N.toText name <> leftWrap (codec <$> _u_vars) <> " =\n"
          <> "  Lamdera.Evergreen.decodeString |> Lamdera.Evergreen.andThenDecode (\\evg_e_thingy ->\n"
          <> "    case evg_e_thingy of\n      "
          <> T.intercalate "\n      "
            (case _u_opts of
              _ -> decodeUnion _u_vars _u_alts
              --Can.Normal -> codecUnion _u_vars _u_alts
              --Can.Enum -> error "codec Enum notimpl"
              --Can.Unbox -> error "codec Unbox notimpl"
            )
          <> "\n      _ -> Lamdera.Evergreen.failDecode"
          <> "\n  )"
      in
        encoder <> "\n\n" <> decoder


    -- DECODERS

    decodeUnion :: [N.Name] -> [Can.Ctor] -> [T.Text]
    decodeUnion _u_vars _u_alts =
      (\(Can.Ctor name _ _ tipes) ->
          strQuote(N.toText name) <> " -> " <> T.intercalate " |> Lamdera.Evergreen.andMapDecode " ((decodeSucceed $ N.toText name) : ((\(_, t) -> decoderForType Map.empty t) <$> nargs tipes))) <$> _u_alts

    decoderForType :: Map.Map N.Name Int -> Can.Type -> T.Text
    decoderForType varMap t =
      case t of
        (Can.TVar n) -> codec n
        (Can.TUnit) -> unitDec
        (Can.TType moduName name tipes) ->
          p ((case (moduName, name) `Map.lookup` evergreenCoreDecoders of
            Just c -> c
            Nothing ->
              let qualModuName = qualImport moduName
              in p $ (if _moduName == moduName then "" else moduleToText qualModuName <> ".") <> "evg_decode_" <> N.toText name
          ) <> leftWrap (p <$> decoderForType varMap <$> tipes))
        (Can.TRecord nameFieldTypeMap _) -> p $
          let
            (newVarMap, vars) = manyVars varMap (Map.keys nameFieldTypeMap)
          in
            T.intercalate " |> Lamdera.Evergreen.andMapDecode " $
              p <$> (decodeSucceed "(\\" <> leftWrap vars <> " -> "
                <> recLit ((\k -> (N.toText k, getVar newVarMap k)) <$> Map.keys nameFieldTypeMap) <> ")")
                : (decoderForType newVarMap <$> unpackFieldType <$> Map.elems nameFieldTypeMap)
        (Can.TTuple t1 t2 Nothing) -> p $ pairDec (decoderForType varMap t1) (decoderForType varMap t2)
        (Can.TTuple t1 t2 (Just t3)) -> p $ tripleDec (decoderForType varMap t1) (decoderForType varMap t2) (decoderForType varMap t3)
        (Can.TAlias _ _ nameTypePairs aliasType) -> decoderForType varMap (Type.dealias nameTypePairs aliasType)
        (Can.TLambda _ _) -> "Lamdera.Evergreen.failDecode " -- <> strQuote (T.pack $ show x)

    -- D.succeed (\a b c -> { a = a, b = b, c = c })
    --   |> dAndMap decodeInt64
    --   |> dAndMap c_b
    --   |> dAndMap decodeUnit

    -- ENCODERS

    encodeUnion :: [N.Name] -> [Can.Ctor] -> [T.Text]
    encodeUnion _u_vars _u_alts =
      (\(Can.Ctor name _ _ tipes) ->
          N.toText name <> leftWrap (fst <$> nargs tipes) <> " -> " <> sequenceEncWithoutLength ((strEnc (N.toText name)) : ((\(var, t) -> encoderForType Map.empty t <> " " <> var) <$> nargs tipes))) <$> _u_alts

    encoderForType :: Map.Map N.Name Int -> Can.Type -> T.Text
    encoderForType varMap t =
      case t of
        (Can.TVar n) -> codec n
        (Can.TUnit) -> unitEnc
        (Can.TType moduName name tipes) ->
          p ((case (moduName, name) `Map.lookup` evergreenCoreEncoders of
            Just c -> c
            Nothing ->
              let qualModuName = qualImport moduName
              in (if _moduName == moduName then "" else moduleToText qualModuName <> ".") <> "evg_encode_" <> N.toText name
          ) <> leftWrap (p <$> encoderForType varMap <$> tipes))
        (Can.TRecord nameFieldTypeMap _) ->
          let
            (newVarMap, recVar) = newRecVar varMap -- store `Map Varname Int` so we can append numbers to varnames to avoid shadowing
          in
            "(\\" <> recVar <> " -> "
              <> sequenceEnc ((\(var, t) ->
                encoderForType newVarMap (unpackFieldType t) <> " " <> recAccess newVarMap var) <$> Map.toList nameFieldTypeMap) <> ")"
        (Can.TTuple t1 t2 Nothing) -> pairEnc (encoderForType varMap t1) (encoderForType varMap t2)
        (Can.TTuple t1 t2 (Just t3)) -> tripleEnc (encoderForType varMap t1) (encoderForType varMap t2) (encoderForType varMap t3)
        (Can.TAlias _ _ nameTypePairs aliasType) -> encoderForType varMap (Type.dealias nameTypePairs aliasType)
        (Can.TLambda _ _) -> "Lamdera.Evergreen.failEncode " -- <> strQuote (T.pack $ show x)

  in
    T.intercalate "\n\n\n" ((unionCodecs <$> Map.toList _unions) <> (aliasCodecs <$> Map.toList _aliases))

nargs :: [Can.Type] -> [(T.Text, Can.Type)]
nargs xs =
  let
    f count t = ("v" <> T.pack (show count), t)
  in
    imap f xs

moduleToText (Canonical _ modu) = N.toText modu
unpackFieldType (Can.FieldType _ t) = t

leftWrap [] = ""
leftWrap (x:xs) = " " <> x <> leftWrap xs

sequenceEnc things = "Lamdera.Evergreen.encodeSequence [" <> T.intercalate ", " things <> "]"
sequenceEncWithoutLength things = "Lamdera.Evergreen.encodeSequenceWithoutLength [" <> T.intercalate ", " things <> "]"

p s = "(" <> s <> ")"

codec n = "evg_x_c_" <> (N.toText n)

decodeSucceed s = "Lamdera.Evergreen.succeedDecode " <> s
strQuote s = T.pack (show (T.unpack s))
strEnc s = "Lamdera.Evergreen.encodeString " <> strQuote s
unitEnc = "Lamdera.Evergreen.encodeUnit"
pairEnc a b = "Lamdera.Evergreen.encodePair " <> p a <> " " <> p b
tripleEnc a b c = "Lamdera.Evergreen.encodeTriple " <> p a <> " " <> p b <> " " <> p c
unitDec = "Lamdera.Evergreen.decodeUnit"
pairDec a b = "Lamdera.Evergreen.decodePair " <> p a <> " " <> p b
tripleDec a b c = "Lamdera.Evergreen.decodeTriple " <> p a <> " " <> p b <> " " <> p c
recEnc fields = "{ " <> T.intercalate ", " (N.toText <$> fields) <> " }"
recLit kvpairs = "{ " <> T.intercalate ", " ((\(k, v) -> k <> "=" <> v) <$> kvpairs) <> " }"

(-->) = (,)

evergreenCoreEncoders = fst <$> evergreenCoreCodecs
evergreenCoreDecoders = snd <$> evergreenCoreCodecs

evergreenCoreCodecs :: Map.Map (Canonical, N.Name) (T.Text, T.Text)
evergreenCoreCodecs =
  Map.fromList $
  (\((pkg, modu, tipe), res) -> ((Canonical (pkgFromText pkg) modu, tipe), res)) <$>
  -- non elm/core types
  ( [ (("elm/time", "Time", "Posix") --> ("Lamdera.Evergreen.encodeTimePosix", "Lamdera.Evergreen.decodeTimePosix") )
    , (("elm/bytes", "Bytes", "Bytes") --> ("Lamdera.Evergreen.encodeBytes", "Lamdera.Evergreen.decodeBytes") )
    , (("elm/virtual-dom", "VirtualDom", "Node") --> ("(\\_ -> Lamdera.Evergreen.failEncode)", "(\\_ -> Lamdera.Evergreen.failDecode)") )
    , (("elm/virtual-dom", "VirtualDom", "Attribute") --> ("(\\_ -> Lamdera.Evergreen.failEncode)", "(\\_ -> Lamdera.Evergreen.failDecode)") )
    , (("elm/virtual-dom", "VirtualDom", "Handler") --> ("(\\_ -> Lamdera.Evergreen.failEncode)", "(\\_ -> Lamdera.Evergreen.failDecode)") )
    ] <>
    (
      -- elm/core types
      (\(modu, tipe) -> ("elm/core", modu, tipe) --> ("Lamdera.Evergreen.encode" <> N.toText tipe, "Lamdera.Evergreen.decode" <> N.toText tipe)) <$>
      [ ("Array", "Array")
      , ("Char", "Char")
      , ("Basics", "Bool")
      , ("Basics", "Float")
      , ("Basics", "Int")
      , ("Basics", "Never")
      , ("Basics", "Order")
      , ("List", "List")
      , ("Maybe", "Maybe")
      , ("Set", "Set")
      , ("String", "String")
      , ("Dict", "Dict")
      , ("Result", "Result")
      ]
    )
  )

pkgFromText :: T.Text -> Pkg.Name
pkgFromText s =
  case T.splitOn "/" s of
    [p,m] -> Pkg.Name p m
    _ -> error ("fromText" <> T.unpack s)


-- Record helpers

recNameVar varMap = "evg_rec_var" <> T.pack (show (varMap Map.! "evg_rec_var"))
recAccess varMap var = recNameVar varMap <> "." <> N.toText var

-- TODO: manually passing maps around is very verbose and error prone; make a monad wrapping variables and the text output instead

manyVars :: Map.Map N.Name Int -> [N.Name] -> (Map.Map N.Name Int, [T.Text])
manyVars varMap vars =
  let
    fn varMap vars doneVars =
      case vars of
        [] ->
          (varMap, reverse doneVars)
        (v:rest) ->
          let
            (newVarMap, vx) = newGenVar v varMap
          in
            fn newVarMap rest (vx:doneVars)
  in fn varMap vars []

newRecVar :: Map.Map N.Name Int -> (Map.Map N.Name Int, T.Text)
newRecVar =
  newGenVar "evg_rec_var"

getVar :: Map.Map N.Name Int -> N.Name -> T.Text
getVar varMap varName =
  (N.toText varName) <> T.pack (show (varMap Map.! varName))

newGenVar :: N.Name -> Map.Map N.Name Int -> (Map.Map N.Name Int, T.Text)
newGenVar varName varMap =
  let
    (newVarMap, var) =
      case varMap Map.!? varName of
        Just count -> (Map.insert varName (1 + count) varMap, N.toText varName <> T.pack (show (count+1)))
        Nothing -> (Map.insert varName 0 varMap, N.toText varName <> "0")
  in
    (newVarMap, var)
