{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Wire.Decoder where

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

import qualified Wire.Source2 as Source2

import Lamdera
import StandaloneInstances
import qualified CanSer.CanSer as ToSource

import Lamdera.Wire.Helpers


decoderForType ifaces cname tipe =
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeInt" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeFloat" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeBool" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeOrder" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeNever" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeChar" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeString" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    TUnit ->
      (a (VarForeign mLamdera_Wire2 "decodeUnit" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    TTuple a_ b Nothing ->
        (a (Call
              (a (VarForeign
                    (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                    "decodePair"
                    (Forall
                       (Map.fromList [("a", ()), ("b", ())])
                       (TLambda
                          (TAlias
                             (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                             "Decoder"
                             [("a", TVar "a")]
                             (Filled
                                (TType
                                   (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                   "Decoder"
                                   [TVar "a"])))
                          (TLambda
                             (TAlias
                                (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                                "Decoder"
                                [("a", TVar "b")]
                                (Filled
                                   (TType
                                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [TVar "b"])))
                             (TAlias
                                (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
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
                   (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                   "decodeTriple"
                   (Forall
                      (Map.fromList [("a", ()), ("b", ()), ("c", ())])
                      (TLambda
                         (TAlias (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2") "Decoder" [("a", TVar "a")]
                            (Filled
                               (TType
                                  (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                  "Decoder"
                                  [TVar "a"])))
                         (TLambda
                            (TAlias (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2") "Decoder" [("a", TVar "b")]
                               (Filled
                                  (TType
                                     (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                     "Decoder"
                                     [TVar "b"])))
                            (TLambda
                               (TAlias (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2") "Decoder" [("a", TVar "c")]
                                  (Filled
                                     (TType
                                        (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                        "Decoder"
                                        [TVar "c"])))
                               (TAlias (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2") "Decoder"
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
        (a (VarForeign mLamdera_Wire2 "decodeMaybe"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TAlias mLamdera_Wire2 "Decoder" [("a", TVar "a")]
                       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                    (TAlias
                       mLamdera_Wire2
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
        (a (VarForeign mLamdera_Wire2 "decodeList"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TAlias mLamdera_Wire2 "Decoder" [("a", TVar "a")]
                       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                    (TAlias
                       mLamdera_Wire2
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
        (a (VarForeign mLamdera_Wire2 "decodeSet"
              (Forall
                 (Map.fromList [("comparable", ())])
                 (TLambda
                    (TAlias mLamdera_Wire2 "Decoder" [("a", TVar "comparable")]
                        (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "comparable"])))
                    (TAlias
                       mLamdera_Wire2
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
        (a (VarForeign mLamdera_Wire2 "decodeArray"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TAlias mLamdera_Wire2 "Decoder" [("a", TVar "a")]
                       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                    (TAlias
                       mLamdera_Wire2
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
              (a (VarForeign mLamdera_Wire2 "decodeResult"
                    (Forall
                       (Map.fromList [("err", ()), ("val", ())])
                       (TLambda
                          (TAlias mLamdera_Wire2 "Decoder" [("a", TVar "err")]
                             (Filled
                                (TType
                                   (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                   "Decoder"
                                   [TVar "err"])))
                          (TLambda (TAlias mLamdera_Wire2 "Decoder" [("a", TVar "val")]
                                (Filled
                                   (TType
                                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [TVar "val"])))
                             (TAlias mLamdera_Wire2 "Decoder" [ ( "a"
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
              (a (VarForeign mLamdera_Wire2 "decodeDict"
                    (Forall
                       (Map.fromList [("comparable", ()), ("value", ())])
                       (TLambda
                          (TAlias mLamdera_Wire2 "Decoder" [("a", TVar "comparable")]
                             (Filled
                                (TType
                                   (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                   "Decoder"
                                   [TVar "comparable"])))
                          (TLambda
                             (TAlias mLamdera_Wire2 "Decoder" [("a", TVar "value")]
                                (Filled
                                   (TType
                                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [TVar "value"])))
                             (TAlias mLamdera_Wire2 "Decoder"
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


    TRecord fieldMap maybeName ->
    -- | TRecord (Map.Map Name FieldType) (Maybe Name)
      let
        fields = fieldsToList fieldMap

        pvars :: [Pattern]
        pvars =
          (imap (\i (name, field) -> a (PVar $ Data.Name.fromChars $ Data.Name.toChars name ++ "0")) fields)
          -- (imap (\i (name, field) -> a (PVar $ Data.Name.fromChars $ "w_f" ++ show i)) fields) -- @TODO improve gen

        newRecFields :: Map.Map Data.Name.Name Expr
        newRecFields =
          fields
            & fmap (\(name, field) ->
                (name, a (VarLocal $ Data.Name.fromChars $ Data.Name.toChars name ++ "0"))
                -- (name, a (VarLocal $ Data.Name.fromChars $ "w_f" ++ show i)) -- @TODO improve gen
              )
            & Map.fromList
      in
      [succeedDecode (a (Lambda pvars (a (Record newRecFields ))))]
      ++ fmap (\(name, field) -> andMapDecode1 (decoderForType ifaces cname field)) fields
        & foldlPairs (|>)

    TAlias moduleName typeName tvars_ aType ->
      let
        generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars typeName

        decoder =
          if cname == moduleName
            -- Referenced type is defined in the current module
            then (a (VarTopLevel moduleName generatedName))
            -- Referenced type is defined in another module
            else
              let
                getTvars (Module.Canonical pkg (moduleRaw)) = foreignTypeTvars moduleRaw typeName ifaces
                tvars = getTvars moduleName
                tvarsForall = tvars & fmap (\tvar -> (tvar, ())) & Map.fromList
                tvarsTypes = tvars & fmap (\tvar -> TVar tvar)

                tvarsSigDecoders = tvarsTypes & fmap (\tvarType ->
                  (TAlias
                    (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                    "Decoder"
                    [("a", tvarType)]
                    (Filled
                        (TType
                           (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                           "Decoder"
                           [tvarType])))
                  )

                -- This is the signature of the end, i.e.
                -- `decoder : (Decoder tvar1) -> Decoder (Type tvar1)`
                --                               ^^^^^^^^^^^^^^^^^^^^
                decoderEndSig =
                  (TAlias
                    (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                    "Decoder"
                    -- @TODO given this isn't the other definiition of
                    -- (TType moduleName typeName tvarsTypes) then it might be we're
                    -- re-introducing the bug here that the tvarsTypes injection fixed?
                    [("a", unwrapAliasesDeep tipe)]
                    (Filled
                        (TType
                           (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                           "Decoder"
                           [ unwrapAliasesDeep tipe ])))


              in
              (a (VarForeign moduleName generatedName
                (Forall tvarsForall $
                  (tvarsSigDecoders ++ [decoderEndSig])
                     & foldrPairs TLambda
                )
              ))
      in
      if isUnsupportedKernelType tipe
        then failDecode
        else
          case tvars_ of
            [] -> decoder
            _ ->
              call decoder $ fmap (\(tvarName, tvarType) ->
                case tvarType of
                  TVar name ->
                    lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars name
                  _ ->
                    decoderForType ifaces cname tvarType
              ) tvars_


    TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Bytes" params ->
      decodeBytes


    TType moduleName typeName params ->
      let
        generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars typeName

        decoder =
          if cname == moduleName
            -- Referenced type is defined in the current module
            then (a (VarTopLevel moduleName generatedName))
            -- Referenced type is defined in another module
            else genForeignDecoder ifaces generatedName moduleName typeName
      in
      if isUnsupportedKernelType tipe
        then failDecode
        else
          case params of
            [] -> decoder
            _  -> call decoder $ fmap (decoderForType ifaces cname) params

    TVar name ->
      lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars name

    TLambda t1 t2 ->
      failDecode

    _ ->
      error $ "decoder not implemented! " ++ show tipe
      -- str $ Utf8.fromChars $ "decoder not implemented! " ++ show tipe


{-

Referenced type is defined in another module. In order to inject the right
canonical type signature, we have to lookup the type definition to get the
specific tvars for this type.

-}
genForeignDecoder ifaces generatedName moduleName typeName =
  let
    getTvars (Module.Canonical pkg (moduleRaw)) = foreignTypeTvars moduleRaw typeName ifaces
    tvars = getTvars moduleName
    tvarsForall = tvars & fmap (\tvar -> (tvar, ())) & Map.fromList
    tvarsTypes = tvars & fmap (\tvar -> TVar tvar)

    -- These are the signatures for all tvars, i..e
    -- `decoder : (Decoder tvar1) -> Decoder (Type tvar1)`
    --            ^^^^^^^^^^^^^^^
    tvarsSigDecoders = tvarsTypes & fmap (\tvarType ->
      (TAlias
        (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
        "Decoder"
        [("a", tvarType)]
        (Filled
            (TType
               (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
               "Decoder"
               [tvarType])))
      )

    -- This is the signature of the end, i.e.
    -- `decoder : (Decoder tvar1) -> Decoder (Type tvar1)`
    --                               ^^^^^^^^^^^^^^^^^^^^
    decoderEndSig =
      (TAlias
        (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
        "Decoder"
        [("a", (TType moduleName typeName tvarsTypes))]
        (Filled
            (TType
               (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
               "Decoder"
               [(TType moduleName typeName tvarsTypes)])))

    -- @TODO this might be helpful if we add explicit type signatures!
    -- decoderEndSig =
    --   (TAlias
    --     (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
    --     "Decoder"
    --     [("a", tipe)]
    --     (Filled
    --         (TType
    --            (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
    --            "Decoder"
    --            [tipe])))

  in
  (a (VarForeign moduleName generatedName
    (Forall tvarsForall $
      (tvarsSigDecoders ++ [decoderEndSig])
         & foldrPairs TLambda
    )
  ))
