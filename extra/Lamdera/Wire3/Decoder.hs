{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Wire3.Decoder where

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Graph as Graph

import Elm.Package
import qualified AST.Source as Src
import qualified Elm.Interface as I
import qualified Elm.ModuleName as Module
import qualified Elm.Package as Pkg
import qualified AST.Canonical as Can
import AST.Canonical
import qualified Data.Name
import qualified Data.Utf8 as Utf8
import qualified Reporting.Annotation as A
import qualified Reporting.Error as E
import qualified Reporting.Doc as D
import qualified Data.Text as T
import qualified Data.Index as Index


import Lamdera
import StandaloneInstances
import qualified CanSer.CanSer as ToSource

import Lamdera.Wire3.Helpers


callDecoder :: Data.Name.Name -> Type -> Expr
callDecoder name tipe =
  (a (VarForeign mLamdera_Wire name (Forall Map.empty (TAlias mLamdera_Wire "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))


decoderForType :: Map.Map Module.Raw I.Interface -> Module.Canonical -> Type -> Expr
decoderForType ifaces cname tipe =
   if containsUnsupportedTypes tipe then
    failDecode "contains unsupported types"
  else
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])    -> callDecoder "decodeInt" tipe
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" [])  -> callDecoder "decodeFloat" tipe
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" [])   -> callDecoder "decodeBool" tipe
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" [])  -> callDecoder "decodeOrder" tipe
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" [])  -> callDecoder "decodeNever" tipe
    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" [])     -> callDecoder "decodeChar" tipe
    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) -> callDecoder "decodeString" tipe

    TUnit -> callDecoder "decodeUnit" tipe

    TTuple a_ b Nothing ->
        (a (Call
              (a (VarForeign
                    mLamdera_Wire
                    "decodePair"
                    (Forall
                       (Map.fromList [("a", ()), ("b", ())])
                       (TLambda
                          (TAlias
                             mLamdera_Wire
                             "Decoder"
                             [("a", TVar "a")]
                             (Filled
                                (TType
                                   (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                   "Decoder"
                                   [TVar "a"])))
                          (TLambda
                             (TAlias
                                mLamdera_Wire
                                "Decoder"
                                [("a", TVar "b")]
                                (Filled
                                   (TType
                                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [TVar "b"])))
                             (TAlias
                                mLamdera_Wire
                                "Decoder"
                                [("a", TTuple (TVar "a") (TVar "b") Nothing)]
                                (Filled
                                   (TType
                                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [TTuple (TVar "a") (TVar "b") Nothing]))))))))
              [ decoderForType ifaces cname a_
              , decoderForType ifaces cname b
              ]))

    TTuple a_ b (Just c) ->
      (a (Call
             (a (VarForeign
                   mLamdera_Wire
                   "decodeTriple"
                   (Forall
                      (Map.fromList [("a", ()), ("b", ()), ("c", ())])
                      (TLambda
                         (TAlias mLamdera_Wire "Decoder" [("a", TVar "a")]
                            (Filled
                               (TType
                                  (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                  "Decoder"
                                  [TVar "a"])))
                         (TLambda
                            (TAlias mLamdera_Wire "Decoder" [("a", TVar "b")]
                               (Filled
                                  (TType
                                     (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                     "Decoder"
                                     [TVar "b"])))
                            (TLambda
                               (TAlias mLamdera_Wire "Decoder" [("a", TVar "c")]
                                  (Filled
                                     (TType
                                        (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                        "Decoder"
                                        [TVar "c"])))
                               (TAlias mLamdera_Wire "Decoder"
                                  [("a", TTuple (TVar "a") (TVar "b") (Just (TVar "c")))]
                                  (Filled
                                     (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder"
                                        [TTuple (TVar "a") (TVar "b") (Just (TVar "c"))])))))))))
             [ decoderForType ifaces cname a_
             , decoderForType ifaces cname b
             , decoderForType ifaces cname c
             ]))

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [ptype] ->
      (a (Call
        (a (VarForeign mLamdera_Wire "decodeMaybe"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TAlias mLamdera_Wire "Decoder" [("a", TVar "a")]
                       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                    (TAlias
                       mLamdera_Wire
                       "Decoder"
                       [("a", TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [TVar "a"])]
                       (Filled
                          (TType
                             (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                             "Decoder"
                             [TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [TVar "a"]])))))))
        [ decoderForType ifaces cname ptype ]))

    TType (Module.Canonical (Name "elm" "core") "List") "List" [ptype] ->
      (a (Call
        (a (VarForeign mLamdera_Wire "decodeList"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TAlias mLamdera_Wire "Decoder" [("a", TVar "a")]
                       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                    (TAlias
                       mLamdera_Wire
                       "Decoder"
                       [("a", TType (Module.Canonical (Name "elm" "core") "List") "List" [TVar "a"])]
                       (Filled
                          (TType
                             (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                             "Decoder"
                             [TType (Module.Canonical (Name "elm" "core") "List") "List" [TVar "a"]])))))))
        [ decoderForType ifaces cname ptype ]))

    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [ptype] ->
      (a (Call
        (a (VarForeign mLamdera_Wire "decodeSet"
              (Forall
                 (Map.fromList [("comparable", ())])
                 (TLambda
                    (TAlias mLamdera_Wire "Decoder" [("a", TVar "comparable")]
                        (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "comparable"])))
                    (TAlias
                       mLamdera_Wire
                       "Decoder"
                       [("a", TType (Module.Canonical (Name "elm" "core") "Set") "Set" [TVar "comparable"])]
                       (Filled
                          (TType
                             (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                             "Decoder"
                             [TType (Module.Canonical (Name "elm" "core") "Set") "Set" [TVar "comparable"]])))))))
        [ decoderForType ifaces cname ptype ]))

    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [ptype] ->
      (a (Call
        (a (VarForeign mLamdera_Wire "decodeArray"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TAlias mLamdera_Wire "Decoder" [("a", TVar "a")]
                       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                    (TAlias
                       mLamdera_Wire
                       "Decoder"
                       [("a", TType (Module.Canonical (Name "elm" "core") "Array") "Array" [TVar "a"])]
                       (Filled
                          (TType
                             (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                             "Decoder"
                             [TType (Module.Canonical (Name "elm" "core") "Array") "Array" [TVar "a"]])))))))
        [ decoderForType ifaces cname ptype ]))

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a_] ->
        (a (Call
              (a (VarForeign mLamdera_Wire "decodeResult"
                    (Forall
                       (Map.fromList [("err", ()), ("val", ())])
                       (TLambda
                          (TAlias mLamdera_Wire "Decoder" [("a", TVar "err")]
                             (Filled
                                (TType
                                   (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                   "Decoder"
                                   [TVar "err"])))
                          (TLambda (TAlias mLamdera_Wire "Decoder" [("a", TVar "val")]
                                (Filled
                                   (TType
                                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [TVar "val"])))
                             (TAlias mLamdera_Wire "Decoder" [ ( "a"
                                  , TType
                                      (Module.Canonical (Name "elm" "core") "Result")
                                      "Result"
                                      [TVar "err", TVar "val"])
                                ]
                                (Filled
                                   (TType
                                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [ TType
                                          (Module.Canonical (Name "elm" "core") "Result")
                                          "Result"
                                          [TVar "err", TVar "val"]
                                      ]))))))))
              [ decoderForType ifaces cname err
              , decoderForType ifaces cname a_
              ]))

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
        (a (Call
              (a (VarForeign mLamdera_Wire "decodeDict"
                    (Forall
                       (Map.fromList [("comparable", ()), ("value", ())])
                       (TLambda
                          (TAlias mLamdera_Wire "Decoder" [("a", TVar "comparable")]
                             (Filled
                                (TType
                                   (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                   "Decoder"
                                   [TVar "comparable"])))
                          (TLambda
                             (TAlias mLamdera_Wire "Decoder" [("a", TVar "value")]
                                (Filled
                                   (TType
                                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [TVar "value"])))
                             (TAlias mLamdera_Wire "Decoder"
                                [ ( "a"
                                  , TType
                                      (Module.Canonical (Name "elm" "core") "Dict")
                                      "Dict"
                                      [TVar "comparable", TVar "value"])
                                ]
                                (Filled
                                   (TType
                                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [ TType
                                          (Module.Canonical (Name "elm" "core") "Dict")
                                          "Dict"
                                          [TVar "comparable", TVar "value"]
                                      ]))))))))
              [ decoderForType ifaces cname key
              , decoderForType ifaces cname val
              ]))

    TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Bytes" _ ->
      callDecoder "decodeBytes" tipe

    TType (Module.Canonical (Name "elm" "time") "Time") "Posix" params ->
      decodeTime


    -- Frontend only JS reference types
    TType (Module.Canonical (Name "elm" "file") "File") "File" params -> callDecoder "decodeRef" tipe


    TType moduleName typeName params ->
      let
        generatedName = Data.Name.fromChars $ "w3_decode_" ++ Data.Name.toChars typeName

        decoder =
          if cname == moduleName
            -- Referenced type is defined in the current module
            then (a (VarTopLevel moduleName generatedName))
            else (a (VarForeign moduleName generatedName (getForeignSig tipe moduleName generatedName ifaces)))
      in
      if isUnsupportedKernelType tipe
        then failDecode (Data.Name.toChars generatedName <> " isUnsupportedKernelType")
        else
          case params of
            [] -> decoder
            _  -> call decoder $ fmap (decoderForType ifaces cname) params

    TRecord fieldMap maybeExtensible ->
      -- | TRecord (Map.Map Name FieldType) (Maybe Name)
      case maybeExtensible of
        Just extensibleName ->
          lvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars extensibleName

        Nothing ->
          let fields = fieldMap & fieldsToList & List.sortOn (\(name, field) -> name)
          in
          decodeRecord ifaces cname fields

    TAlias moduleName typeName tvars_ aType ->
      let
        generatedName = Data.Name.fromChars $ "w3_decode_" ++ Data.Name.toChars typeName

        decoder =
          if cname == moduleName
            -- Referenced type is defined in the current module
            then (a (VarTopLevel moduleName generatedName))
            -- Referenced type is defined in another module
            else (a (VarForeign moduleName generatedName (getForeignSig tipe moduleName generatedName ifaces)))

        normalDecoder =
          case tvars_ of
            [] -> decoder
            _ ->
               call decoder $ fmap (\(tvarName, tvarType) ->
                  case tvarType of
                     TVar name ->
                        lvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars name
                     _ ->
                        decoderForType ifaces cname tvarType
               ) tvars_
      in
      if isUnsupportedKernelType tipe
      then failDecode (Data.Name.toChars generatedName <> " isUnsupportedKernelType")
      else
         case aType of
            Holey tipe ->
              case tipe of
                TRecord fieldMap extensibleName ->
                  case resolvedRecordFieldMapM fieldMap extensibleName tvars_ of
                    Just resolved ->
                     let extendedRecord = TRecord resolved Nothing & resolveTvar tvars_
                     in decoderForType ifaces cname extendedRecord
                    Nothing -> normalDecoder
                otherTypes -> normalDecoder
            Filled tipe ->
              case tipe of
                TRecord fieldMap extensibleName ->
                    case resolvedRecordFieldMapM fieldMap extensibleName tvars_ of
                    Just resolved ->
                      let extendedRecord = TRecord resolved Nothing & resolveTvar tvars_
                      in decoderForType ifaces cname extendedRecord
                    Nothing -> normalDecoder
                otherTypes -> normalDecoder

    TVar name ->
      lvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars name

    TLambda t1 t2 ->
      failDecode "lambda"


decodeRecord :: Map.Map Module.Raw I.Interface -> Module.Canonical -> [(Data.Name.Name, Type)] -> Expr
decodeRecord ifaces cname fields =
  let
    pvars :: [Pattern]
    pvars =
      (imap (\i (name, field) -> a (PVar $ Data.Name.fromChars $ Data.Name.toChars name ++ "0")) fields)

    newRecFields :: Map.Map Data.Name.Name Expr
    newRecFields =
      fields
        & fmap (\(name, field) ->
            (name, a (VarLocal $ Data.Name.fromChars $ Data.Name.toChars name ++ "0"))
          )
        & Map.fromList
  in
  [succeedDecode (a (Lambda pvars (a (Record newRecFields ))))]
  ++ fmap (\(name, field) ->
                andMapDecode1 (
                  -- debugDecoder (Utf8.fromChars $ "." <> Data.Name.toChars name) $
                    decoderForType ifaces cname field
                )
          ) fields
    & foldlPairs (|>)
