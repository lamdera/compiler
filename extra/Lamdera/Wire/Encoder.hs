{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Wire.Encoder where

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


encoderForType ifaces cname tipe =
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeInt" (Forall Map.empty (TLambda tipe tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeFloat" (Forall Map.empty (TLambda tipe tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeBool" (Forall Map.empty (TLambda tipe tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeOrder" (Forall Map.empty (TLambda tipe tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeNever" (Forall Map.empty (TLambda tipe tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeChar" (Forall Map.empty (TLambda tipe tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeString" (Forall Map.empty (TLambda tipe tLamdera_Wire2__Encoder))))

    TUnit ->
      (a (VarForeign mLamdera_Wire2 "encodeUnit" (Forall Map.empty (TLambda TUnit tLamdera_Wire2__Encoder))))

    TTuple a_ b Nothing ->
      (a (VarForeign mLamdera_Wire2 "encodePair"
            (Forall
               (Map.fromList [("a", ()), ("b", ())])
               (TLambda
                  (TLambda (TVar "a") tLamdera_Wire2__Encoder)
                  (TLambda
                     (TLambda (TVar "b") tLamdera_Wire2__Encoder)
                     (TLambda
                        (TTuple (TVar "a") (TVar "b") Nothing)
                        tLamdera_Wire2__Encoder))))))

    TTuple a_ b (Just c) ->
      (a (VarForeign mLamdera_Wire2 "encodeTriple"
            (Forall
               (Map.fromList [("a", ()), ("b", ()), ("c", ())])
               (TLambda
                  (TLambda (TVar "a") tLamdera_Wire2__Encoder)
                  (TLambda
                     (TLambda (TVar "b") tLamdera_Wire2__Encoder)
                     (TLambda
                        (TLambda (TVar "c") tLamdera_Wire2__Encoder)
                        (TLambda
                           (TTuple (TVar "a") (TVar "b") (Just (TVar "c")))
                           tLamdera_Wire2__Encoder)))))))

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [ptype] ->
      (a (VarForeign mLamdera_Wire2 "encodeMaybe"
           (Forall
              (Map.fromList [("a", ())])
              (TLambda
                 (TLambda (TVar "a") tLamdera_Wire2__Encoder)
                 (TLambda
                    (TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [TVar "a"])
                    tLamdera_Wire2__Encoder)))))

    TType (Module.Canonical (Name "elm" "core") "List") "List" [ptype] ->
      (a (VarForeign mLamdera_Wire2 "encodeList"
           (Forall
              (Map.fromList [("a", ())])
              (TLambda
                 (TLambda (TVar "a") tLamdera_Wire2__Encoder)
                 (TLambda
                    (TType (Module.Canonical (Name "elm" "core") "List") "List" [TVar "a"])
                    tLamdera_Wire2__Encoder)))))

    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [ptype] ->
      (a (VarForeign mLamdera_Wire2 "encodeSet"
           (Forall
              (Map.fromList [("value", ())])
              (TLambda
                 (TLambda (TVar "value") tLamdera_Wire2__Encoder)
                 (TLambda
                    (TType (Module.Canonical (Name "elm" "core") "Set") "Set" [TVar "value"])
                    tLamdera_Wire2__Encoder)))))

    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [ptype] ->
      (a (VarForeign mLamdera_Wire2 "encodeArray"
           (Forall
              (Map.fromList [("a", ())])
              (TLambda
                 (TLambda (TVar "a") tLamdera_Wire2__Encoder)
                 (TLambda
                    (TType (Module.Canonical (Name "elm" "core") "Array") "Array" [TVar "a"])
                    tLamdera_Wire2__Encoder)))))

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a_] ->
      (a (VarForeign mLamdera_Wire2 "encodeResult"
            (Forall
               (Map.fromList [("err", ()), ("val", ())])
               (TLambda
                  (TLambda (TVar "err") tLamdera_Wire2__Encoder)
                  (TLambda (TLambda (TVar "val") tLamdera_Wire2__Encoder)
                     (TLambda
                        (TType (Module.Canonical (Name "elm" "core") "Result") "Result" [TVar "err", TVar "val"])
                        tLamdera_Wire2__Encoder))))))

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, value] ->
      (a (VarForeign mLamdera_Wire2 "encodeDict"
            (Forall
               (Map.fromList [("key", ()), ("value", ())])
               (TLambda
                  (TLambda (TVar "key") tLamdera_Wire2__Encoder)
                  (TLambda (TLambda (TVar "value") tLamdera_Wire2__Encoder)
                     (TLambda
                        (TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [TVar "key", TVar "value"])
                        tLamdera_Wire2__Encoder))))))

    TType (Module.Canonical (Name "elm" "core") _) _ _ ->
      str $ Utf8.fromChars $ "encoderForType not implemented! " ++ show tipe
    TType (Module.Canonical (Name "elm" "bytes") _) _ _ ->
      str $ Utf8.fromChars $ "encoderForType not implemented! " ++ show tipe

    TType moduleName typeName params ->
      let
        generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars typeName

        gen =
          if cname == moduleName
            -- Referenced type is defined in the current module
            then (a (VarTopLevel moduleName generatedName))
            else
              -- Referenced type is defined in another module. In order to inject the right
              -- canonical type signature, we have to lookup the type definition to get the
              -- specific tvars for this type.
              let
                -- !y = debugHaskellPass (T.pack $ "🅰️  " ++ ("w2_encode_" ++ Data.Name.toChars typeName)) (getTvars moduleName) ()

                getTvars (Module.Canonical pkg (moduleRaw)) = foreignTypeTvars moduleRaw typeName ifaces
                tvars = getTvars moduleName
                tvarsForall = tvars & fmap (\tvar -> (tvar, ())) & Map.fromList
                tvarsTypes = tvars & fmap (\tvar -> TVar tvar)

                -- These are the signatures for all tvars, i..e
                -- `encoder : (tvar1 -> Encoder) -> Type tvar1 -> Encoder`
                --            ^^^^^^^^^^^^^^^^^^
                tvarsSigEncoders = tvarsTypes & fmap (\tvarType -> TLambda tvarType tLamdera_Wire2__Encoder)

                -- This is the signature of the end, i.e.
                -- `encoder : (tvar1 -> Encoder) -> Type tvar1 -> Encoder`
                --                                  ^^^^^^^^^^^^^^^^^^^^^
                encoderEndSig = TLambda (TType moduleName typeName tvarsTypes) tLamdera_Wire2__Encoder
                -- @TODO this might be helpful if we add explicit type signatures!
                -- encoderEndSig = TLambda tipe tLamdera_Wire2__Encoder
              in
              (a (VarForeign moduleName generatedName
                (Forall tvarsForall $
                   (tvarsSigEncoders ++ [encoderEndSig])
                      & foldrPairs TLambda
                )
              ))
      in
      gen

    TRecord fieldMap maybeName ->
      let
        fields = fieldsToList fieldMap
        fieldEncoders =
          fields
            & fmap (\(name, field) ->
                encodeTypeValue ifaces cname field (a (Access (a (VarLocal "w2_rec_var0")) (a (name))))
              )
      in
      (a (Lambda [(a (PVar "w2_rec_var0"))]
        (encodeSequenceWithoutLength $ list fieldEncoders)
      ))

    TAlias moduleName typeName tvars aType ->
      let
        generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars typeName

        decoder =
          if cname == moduleName
            -- Referenced type is defined in the current module
            then (a (VarTopLevel moduleName generatedName))
            -- Referenced type is defined in another module
            else
              (a (VarForeign moduleName generatedName
                (Forall
                   (Map.fromList [])
                   (TLambda
                      (TType moduleName typeName [])
                      (TAlias
                         (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                         "Encoder"
                         []
                         (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))))))

      in
      decoder

    TVar name ->
      -- Tvars should always have a local encoder in scope
      lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars name

    TLambda t1 t2 ->
      failEncode

    _ ->
      -- error $ "Not yet implemented: " ++ show tipe
      str $ Utf8.fromChars $ "encoderForType not implemented! " ++ show tipe


deepEncoderForType ifaces cname tipe =
  case tipe of
    TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []    -> encoderForType ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []  -> encoderForType ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []   -> encoderForType ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []  -> encoderForType ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []  -> encoderForType ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Char") "Char" []     -> encoderForType ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "String") "String" [] -> encoderForType ifaces cname tipe
    TUnit                                                             -> encoderForType ifaces cname tipe

    TTuple a b Nothing  -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a, deepEncoderForType ifaces cname b ]
    TTuple a b (Just c) -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a, deepEncoderForType ifaces cname b, deepEncoderForType ifaces cname c ]

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [a] -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a ]
    TType (Module.Canonical (Name "elm" "core") "List") "List" [a]   -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a ]
    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [a]     -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a ]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a ]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname err, deepEncoderForType ifaces cname a ]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname key, deepEncoderForType ifaces cname val ]

    -- TType (Module.Canonical (Name "elm" "core") _) _ _ ->
    --   str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe
    --
    -- TType (Module.Canonical (Name "elm" "bytes") _) _ _ ->
    --   str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe

    TType moduleName typeName params ->
      if isUnsupportedKernelType tipe
        then failEncode
        else encoderForType ifaces cname tipe

    TRecord fieldMap maybeName ->
      encoderForType ifaces cname tipe

    TAlias moduleName typeName tvars aType ->
      case tvars of
        [] -> encoderForType ifaces cname tipe
        _ ->
          call (encoderForType ifaces cname tipe) $ fmap (\(tvarName, tvarType) ->
            lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars tvarName
          ) tvars

    TVar name     -> encoderForType ifaces cname tipe
    TLambda t1 t2 -> encoderForType ifaces cname tipe

    _ ->
      str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe


encodeTypeValue ifaces cname tipe value =
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])    -> call (encoderForType ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" [])  -> call (encoderForType ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" [])   -> call (encoderForType ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" [])  -> call (encoderForType ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" [])  -> call (encoderForType ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" [])     -> call (encoderForType ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) -> call (encoderForType ifaces cname tipe) [ value ]
    TUnit                                                               -> call (encoderForType ifaces cname tipe) [ value ]

    TTuple a b Nothing  -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a, deepEncoderForType ifaces cname b, value ]
    TTuple a b (Just c) -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a, deepEncoderForType ifaces cname b, deepEncoderForType ifaces cname c, value ]

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [a] -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a, value ]
    TType (Module.Canonical (Name "elm" "core") "List") "List" [a]   -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a, value ]
    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [a]     -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a, value ]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname a, value ]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname err, deepEncoderForType ifaces cname a, value ]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      call (encoderForType ifaces cname tipe) [ deepEncoderForType ifaces cname key, deepEncoderForType ifaces cname val, value ]

    TType (Module.Canonical (Name "elm" "core") _) _ _ ->
      str $ Utf8.fromChars $ "encodeTypeValue not implemented! " ++ show tipe

    TType (Module.Canonical (Name "elm" "bytes") _) _ _ ->
      str $ Utf8.fromChars $ "encodeTypeValue not implemented! " ++ show tipe

    TType moduleName typeName params ->
      call (encoderForType ifaces cname tipe) $ fmap (deepEncoderForType ifaces cname) params ++ [ value ]

    TAlias moduleName typeName tvars aType ->
      call (encoderForType ifaces cname tipe) $ fmap (\(tvarName, tvarType) -> deepEncoderForType ifaces cname tvarType) tvars ++ [ value ]

    TVar name ->
      -- Tvars should always have a local encoder in scope
      call (lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars name) [value]

    TLambda t1 t2 ->
      call failEncode [ value ]

    _ ->
      -- error $ "Not yet implemented: " ++ show tipe
      str $ Utf8.fromChars $ "encodeTypeValue not implemented! " ++ show tipe
