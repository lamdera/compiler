{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wire.Source where

import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N

import qualified AST.Valid as Valid
import qualified AST.Canonical as Can
import qualified Data.ByteString as BS

import AST.Module.Name (Canonical(..))
import Elm.Package (Name(..))
import qualified Data.Map as Map
import Data.Index
import qualified Data.List as List

import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Index as Index
import Data.Functor ((<&>))
import Data.List.Index

import Prelude hiding (sequenceEnc)

a <.> b = a <> "." <> b

generateCodecs :: Can.Module -> Text
generateCodecs (Can.Module _name _docs _exports _decls _unions _aliases _binops _effects) =
  let
    unionEncoders =
      Map.toList _unions
        <&> \(name, (Can.Union _u_vars _u_alts _u_numAlts _u_opts)) ->
              "encode_" <> N.toText name <> leftWrap (codec <$> _u_vars) <> " evg_e_thingy =\n" <>
              "  case evg_e_thingy of\n    "
              <> T.intercalate "\n    "
                (case _u_opts of
                  _ -> encodeUnion _u_vars _u_alts
                  --Can.Normal -> codecUnion _u_vars _u_alts
                  --Can.Enum -> error "codec Enum notimpl"
                  --Can.Unbox -> error "codec Unbox notimpl"
                )
    aliasEncoders =
      Map.toList _aliases
        <&> \(name, (Can.Alias names t)) ->
              "encode_" <> N.toText name <> leftWrap (codec <$> names) <> " =\n" <>
              "  " <> encoderForType t

--    decodeInt64
--        |> D.andThen
--            (\c ->
--                case c of
--                    0 ->
--                        D.succeed Some |> dAndMap c_a
--
--                    1 ->
--                        D.succeed Nada
--
--                    _ ->
--                        D.fail

    unionDecoders =
      Map.toList _unions
        <&> \(name, (Can.Union _u_vars _u_alts _u_numAlts _u_opts))
              -> "decode_" <> N.toText name <> leftWrap (codec <$> _u_vars) <> " =\n"
              <> "  decodeString |> decodeAndThen (\\evg_d_thingy ->\n"
              <> "    case evg_e_thingy of\n      "
              <> T.intercalate "\n      "
                (case _u_opts of
                  _ -> decodeUnion _u_vars _u_alts
                  --Can.Normal -> codecUnion _u_vars _u_alts
                  --Can.Enum -> error "codec Enum notimpl"
                  --Can.Unbox -> error "codec Unbox notimpl"
                )
              <> "\n      _ -> decodeFailed"
              <> "\n  )"

    aliasDecoders =
      Map.toList _aliases
        <&> \(name, (Can.Alias names t)) ->
              "decode_" <> N.toText name <> leftWrap (codec <$> names) <> " =\n" <>
              "  " <> decoderForType t
  in
    T.intercalate "\n\n" (unionEncoders <> unionDecoders <> aliasEncoders <> aliasDecoders)


-- DECODERS

decodeUnion :: [N.Name] -> [Can.Ctor] -> [Text]
decodeUnion _u_vars _u_alts =
  -- Can.Ctor N.Name Index.ZeroBased Int [Can.Type]
  (\(Can.Ctor name idx _ tipes) ->
      strQuote(N.toText name) <> " -> " <> T.intercalate " |> decodeAndMap " ((decodeSucceed $ N.toText name) : ((\(_, t) -> decoderForType t) <$> nargs tipes))) <$> _u_alts

decoderForType :: Can.Type -> Text
decoderForType (Can.TVar n) = codec n
decoderForType (Can.TUnit) = unitDec
decoderForType (Can.TType moduName name tipes) = p $ moduleToText moduName <.> "decode_" <> N.toText name <> leftWrap (p <$> decoderForType <$> tipes)

decoderForType (Can.TRecord nameFieldTypeMap mName) = p $
  T.intercalate " |> decodeAndMap " $
  p <$> (decodeSucceed "(\\" <> leftWrap (N.toText <$> Map.keys nameFieldTypeMap) <> " -> "
    <> recLit ((\(k, v) -> (N.toText k, N.toText k)) <$> Map.toList nameFieldTypeMap) <> ")")
  : (decoderForType <$> unpackFieldType <$> Map.elems nameFieldTypeMap)

decoderForType (Can.TTuple t1 t2 Nothing) = p $pairDec (decoderForType t1) (decoderForType t2)
decoderForType (Can.TTuple t1 t2 (Just t3)) = p $tripleDec (decoderForType t1) (decoderForType t2) (decoderForType t3)
decoderForType (Can.TAlias moduName name nameTypePairs aliasType) = decoderForType (unpackAliasType aliasType) -- TODO: fix
decoderForType x@(Can.TLambda t1 t2) = "failDecode " <> strQuote (T.pack $ show x)

-- D.succeed (\a b c -> { a = a, b = b, c = c })
--   |> dAndMap decodeInt64
--   |> dAndMap c_b
--   |> dAndMap decodeUnit

-- ENCODERS

encodeUnion :: [N.Name] -> [Can.Ctor] -> [Text]
encodeUnion _u_vars _u_alts =
  -- Can.Ctor N.Name Index.ZeroBased Int [Can.Type]
  (\(Can.Ctor name idx _ tipes) ->
      N.toText name <> leftWrap (fst <$> nargs tipes) <> " -> " <> sequenceEnc ((strEnc (N.toText name)) : ((\(var, t) -> encoderForType t <> " " <> var) <$> nargs tipes))) <$> _u_alts

nargs :: [Can.Type] -> [(Text, Can.Type)]
nargs xs =
  let
    f count t = ("v" <> T.pack (show count), t)
  in
    imap f xs



encoderForType :: Can.Type -> Text
encoderForType (Can.TVar n) = codec n
encoderForType (Can.TUnit) = unitEnc
encoderForType (Can.TType moduName name tipes) = p $ moduleToText moduName <.> "encode_" <> N.toText name <> leftWrap (p <$> encoderForType <$> tipes)
encoderForType (Can.TRecord nameFieldTypeMap mName) = "(\\" <> recEnc (Map.keys nameFieldTypeMap) <> " -> "
  <> sequenceEnc ((\(var, t) -> encoderForType (unpackFieldType t) <> " " <> N.toText var) <$> Map.toList nameFieldTypeMap) <> ")"
encoderForType (Can.TTuple t1 t2 Nothing) = pairEnc (encoderForType t1) (encoderForType t2)
encoderForType (Can.TTuple t1 t2 (Just t3)) = tripleEnc (encoderForType t1) (encoderForType t2) (encoderForType t3)
encoderForType (Can.TAlias moduName name nameTypePairs aliasType) = encoderForType (unpackAliasType aliasType) -- TODO: fix
encoderForType x@(Can.TLambda t1 t2) = "failEncode " <> strQuote (T.pack $ show x)

moduleToText (Canonical pkg modu) = N.toText modu
unpackAliasType (Can.Holey t) = t
unpackAliasType (Can.Filled t) = t
unpackFieldType (Can.FieldType _ t) = t

leftWrap [] = ""
leftWrap (x:xs) = " " <> x <> leftWrap xs

sequenceEnc things = "encodeSequence [" <> T.intercalate ", " things <> "]"
p s = "(" <> s <> ")"

codec n = "encode_" <> (N.toText n)
field n = "f_" <> (N.toText n)

decodeSucceed s = "decodeSucceed " <> s
strQuote s = T.pack (show (T.unpack s))
strEnc s = "encodeString " <> strQuote s
unitEnc = "encodeUnit"
pairEnc a b = "encodePair " <> p a <> " " <> p b
tripleEnc a b c = "encodeTriple " <> p a <> " " <> p b <> " " <> p c
strDec s = "decodeString " <> strQuote s
unitDec = "decodeUnit"
pairDec a b = "decodePair " <> p a <> " " <> p b
tripleDec a b c = "decodeTriple " <> p a <> " " <> p b <> " " <> p c
recEnc fields = "{ " <> T.intercalate ", " (N.toText <$> fields) <> " }"
recLit kvpairs = "{ " <> T.intercalate ", " ((\(k, v) -> k <> "=" <> v) <$> kvpairs) <> " }"
