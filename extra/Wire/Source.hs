{-# LANGUAGE OverloadedStrings #-}

module Wire.Source (generateCodecs, injectEvergreenImport, injectEvergreenExposing, isEvergreenCodecName) where

import qualified AST.Canonical as Can
import AST.Module.Name (Canonical(..))
import qualified AST.Utils.Type as Type

import qualified Data.Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import Data.List.Index (imap)

import qualified Elm.Name as N
import qualified Elm.Package as Pkg


-- FIXME: this requires all files to contain at least one import statement, and it's not very safe.
injectEvergreenImport :: String -> String
injectEvergreenImport s@(_:withoutNewline) | "\n\nimport " `List.isPrefixOf` s = -- hope that there's an empty line between the import statements and whatever comes before it; this should be the case if the code is elm-formatted
  "\nimport Lamdera.Evergreen" <> withoutNewline
injectEvergreenImport s | "\nimport " `List.isPrefixOf` s = -- otherwise we inject anyway, but now the line numbers are off by one in error messages
  "\nimport Lamdera.Evergreen" <> s
injectEvergreenImport (x:xs) = x : injectEvergreenImport xs
injectEvergreenImport [] = []

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


generateCodecs :: Can.Module -> T.Text
generateCodecs (Can.Module _moduName _docs _exports _decls _unions _aliases _binops _effects) =
  let -- massive let-expr so we can closure in _moduName
    -- HELPERS
    aliasCodecs (name, (Can.Alias names t)) =
      let
        encoder =
          "evg_encode_" <> N.toText name <> leftWrap (codec <$> names) <> " =\n" <>
          "  " <> encoderForType t
        decoder =
          "evg_decode_" <> N.toText name <> leftWrap (codec <$> names) <> " =\n" <>
          "  " <> decoderForType t
      in
        encoder <> "\n\n" <> decoder

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
          strQuote(N.toText name) <> " -> " <> T.intercalate " |> Lamdera.Evergreen.andMapDecode " ((decodeSucceed $ N.toText name) : ((\(_, t) -> decoderForType t) <$> nargs tipes))) <$> _u_alts

    decoderForType :: Can.Type -> T.Text
    decoderForType t =
      case t of
        (Can.TVar n) -> codec n
        (Can.TUnit) -> unitDec
        (Can.TType moduName name tipes) ->
          p ((case (moduName, name) `Map.lookup` evergreenCoreDecoders of
            Just c -> c
            Nothing -> p $ (if _moduName == moduName then "" else moduleToText moduName <> ".") <> "evg_decode_" <> N.toText name
          ) <> leftWrap (p <$> decoderForType <$> tipes))
        (Can.TRecord nameFieldTypeMap _) -> p $
            T.intercalate " |> Lamdera.Evergreen.andMapDecode " $
              p <$> (decodeSucceed "(\\" <> leftWrap (N.toText <$> Map.keys nameFieldTypeMap) <> " -> "
                <> recLit ((\k -> (N.toText k, N.toText k)) <$> Map.keys nameFieldTypeMap) <> ")")
                : (decoderForType <$> unpackFieldType <$> Map.elems nameFieldTypeMap)
        (Can.TTuple t1 t2 Nothing) -> p $ pairDec (decoderForType t1) (decoderForType t2)
        (Can.TTuple t1 t2 (Just t3)) -> p $ tripleDec (decoderForType t1) (decoderForType t2) (decoderForType t3)
        (Can.TAlias _ _ nameTypePairs aliasType) -> decoderForType (Type.dealias nameTypePairs aliasType)
        (Can.TLambda _ _) -> "Lamdera.Evergreen.failDecode " -- <> strQuote (T.pack $ show x)

    -- D.succeed (\a b c -> { a = a, b = b, c = c })
    --   |> dAndMap decodeInt64
    --   |> dAndMap c_b
    --   |> dAndMap decodeUnit

    -- ENCODERS

    encodeUnion :: [N.Name] -> [Can.Ctor] -> [T.Text]
    encodeUnion _u_vars _u_alts =
      (\(Can.Ctor name _ _ tipes) ->
          N.toText name <> leftWrap (fst <$> nargs tipes) <> " -> " <> sequenceEncWithoutLength ((strEnc (N.toText name)) : ((\(var, t) -> encoderForType t <> " " <> var) <$> nargs tipes))) <$> _u_alts

    nargs :: [Can.Type] -> [(T.Text, Can.Type)]
    nargs xs =
      let
        f count t = ("v" <> T.pack (show count), t)
      in
        imap f xs

    encoderForType :: Can.Type -> T.Text
    encoderForType t =
      case t of
        (Can.TVar n) -> codec n
        (Can.TUnit) -> unitEnc
        (Can.TType moduName name tipes) ->
          p ((case (moduName, name) `Map.lookup` evergreenCoreEncoders of
            Just c -> c
            Nothing ->
              (if _moduName == moduName then "" else moduleToText moduName <> ".") <> "evg_encode_" <> N.toText name
          ) <> leftWrap (p <$> encoderForType <$> tipes))

        (Can.TRecord nameFieldTypeMap _) -> "(\\" <> recEnc (Map.keys nameFieldTypeMap) <> " -> "
          <> sequenceEnc ((\(var, t) -> encoderForType (unpackFieldType t) <> " " <> N.toText var) <$> Map.toList nameFieldTypeMap) <> ")"
        (Can.TTuple t1 t2 Nothing) -> pairEnc (encoderForType t1) (encoderForType t2)
        (Can.TTuple t1 t2 (Just t3)) -> tripleEnc (encoderForType t1) (encoderForType t2) (encoderForType t3)
        (Can.TAlias _ _ nameTypePairs aliasType) -> encoderForType (Type.dealias nameTypePairs aliasType)
        (Can.TLambda _ _) -> "Lamdera.Evergreen.failEncode " -- <> strQuote (T.pack $ show x)

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
  in
    T.intercalate "\n\n\n" ((unionCodecs <$> Map.toList _unions) <> (aliasCodecs <$> Map.toList _aliases))
