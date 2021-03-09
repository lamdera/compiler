{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Wire2.Encoder where

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

import Lamdera.Wire2.Helpers


encoderNotImplemented tag tipe =
  error $ tag ++ " not implemented! " ++ show tipe
  -- str $ Utf8.fromChars $ tag ++ " not implemented! " ++ show tipe


encoderForType depth ifaces cname tipe =
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
      -- @OPTIMIZE remove this, should just encodeFail...
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


    TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Bytes" _ ->
      (a (VarForeign mLamdera_Wire2 "encodeBytes" (Forall Map.empty (TLambda tipe tLamdera_Wire2__Encoder))))


    TType (Module.Canonical (Name "elm" "time") "Time") "Posix" _ ->
      (a (Lambda
            [(a (PVar "t"))]
            (a (Call
                  (a (VarForeign
                        (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                        "encodeInt"
                        (Forall
                           (Map.fromList [])
                           (TLambda
                              (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                              (TAlias
                                 (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                                 "Encoder"
                                 []
                                 (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))))))
                  [ (a (Call
                          (a (VarForeign
                                (Module.Canonical (Name "elm" "time") "Time")
                                "posixToMillis"
                                (Forall
                                   (Map.fromList [])
                                   (TLambda
                                      (TType (Module.Canonical (Name "elm" "time") "Time") "Posix" [])
                                      (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])))))
                          [(a (VarLocal "t"))]))
                  ]))))


    -- Frontend only JS reference types
    TType (Module.Canonical (Name "elm" "file") "File") "File" _ ->
      (a (VarForeign mLamdera_Wire2 "encodeRef" (Forall Map.empty (TLambda tipe tLamdera_Wire2__Encoder))))


    TType moduleName typeName params ->
      let
        generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars typeName

        decoder =
          if cname == moduleName
            -- Referenced type is defined in the current module
            then (a (VarTopLevel moduleName generatedName))
            else (a (VarForeign moduleName generatedName (getForeignSig tipe moduleName generatedName ifaces)))

      in
      if isUnsupportedKernelType tipe
        then failEncode
        else decoder


    TRecord fieldMap maybeExtensible ->
      case maybeExtensible of
        Just extensibleName ->
          -- @EXTENSIBLERECORDS not supported yet
          failEncode

        Nothing ->
          let
            fields = fieldsToList fieldMap
            fieldEncoders =
              fields
                & fmap (\(name, field) ->
                    -- debugEncoder_ (Utf8.fromChars $ "." <> Data.Name.toChars name) $
                      encodeTypeValue (depth + 1) ifaces cname field (a (Access (a (VarLocal $ Utf8.fromChars $ "w2_rec_var" ++ show depth)) (a (name))))
                  )
          in
          (a (Lambda [(a (PVar $ Utf8.fromChars $ "w2_rec_var" ++ show depth))]
            (encodeSequenceWithoutLength $ list fieldEncoders)
          ))



    TAlias moduleName typeName tvars_ aType ->
      let
        generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars typeName

        encoder =
          if cname == moduleName
            -- Referenced type is defined in the current module
            then (a (VarTopLevel moduleName generatedName))
            -- Referenced type is defined in another module
            else (a (VarForeign moduleName generatedName (getForeignSig tipe moduleName generatedName ifaces)))
      in
      if isUnsupportedKernelType tipe
        then failEncode
        else encoder


    TVar name ->
      -- Tvars should always have a local encoder in scope
      lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars name

    TLambda t1 t2 ->
      failEncode


deepEncoderForType depth ifaces cname tipe =
  case tipe of
    TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []    -> encoderForType depth ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []  -> encoderForType depth ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []   -> encoderForType depth ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []  -> encoderForType depth ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []  -> encoderForType depth ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "Char") "Char" []     -> encoderForType depth ifaces cname tipe
    TType (Module.Canonical (Name "elm" "core") "String") "String" [] -> encoderForType depth ifaces cname tipe
    TUnit                                                             -> encoderForType depth ifaces cname tipe

    TTuple a b Nothing  -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a, deepEncoderForType depth ifaces cname b ]
    TTuple a b (Just c) -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a, deepEncoderForType depth ifaces cname b, deepEncoderForType depth ifaces cname c ]

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [a] -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a ]
    TType (Module.Canonical (Name "elm" "core") "List") "List" [a]   -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a ]
    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [a]     -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a ]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a ]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname err, deepEncoderForType depth ifaces cname a ]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname key, deepEncoderForType depth ifaces cname val ]

    TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Bytes" _ -> encoderForType depth ifaces cname tipe
    TType (Module.Canonical (Name "elm" "time") "Time") "Posix" _ -> encoderForType depth ifaces cname tipe

    -- Frontend only JS reference types
    TType (Module.Canonical (Name "elm" "file") "File") "File" _ -> encoderForType depth ifaces cname tipe


    TType moduleName typeName params ->
      if isUnsupportedKernelType tipe
        then failEncode
        else
          case params of
            [] -> encoderForType depth ifaces cname tipe
            _ ->
              call (encoderForType depth ifaces cname tipe) $ fmap (\tvarType ->
                case tvarType of
                  TVar name ->
                    lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars name
                  _ ->
                    deepEncoderForType depth ifaces cname tvarType
              ) params


    TRecord fieldMap maybeName ->
      encoderForType depth ifaces cname tipe

    TAlias moduleName typeName tvars aType ->
      if isUnsupportedKernelType tipe
        then failEncode
        else
          case tvars of
            [] -> encoderForType depth ifaces cname tipe
            _ ->
              call (encoderForType depth ifaces cname tipe) $ fmap (\(tvarName, tvarType) ->
                case tvarType of
                  TVar name ->
                    lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars name
                  _ ->
                    deepEncoderForType depth ifaces cname tvarType
              ) tvars

    TVar name     -> encoderForType depth ifaces cname tipe
    TLambda t1 t2 -> encoderForType depth ifaces cname tipe


encodeTypeValue depth ifaces cname tipe value =
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])    -> call (encoderForType depth ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" [])  -> call (encoderForType depth ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" [])   -> call (encoderForType depth ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" [])  -> call (encoderForType depth ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" [])  -> call (encoderForType depth ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" [])     -> call (encoderForType depth ifaces cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) -> call (encoderForType depth ifaces cname tipe) [ value ]
    TUnit                                                               -> call (encoderForType depth ifaces cname tipe) [ value ]

    TTuple a b Nothing  -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a, deepEncoderForType depth ifaces cname b, value ]
    TTuple a b (Just c) -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a, deepEncoderForType depth ifaces cname b, deepEncoderForType depth ifaces cname c, value ]

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [a] -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a, value ]
    TType (Module.Canonical (Name "elm" "core") "List") "List" [a]   -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a, value ]
    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [a]     -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a, value ]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname a, value ]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname err, deepEncoderForType depth ifaces cname a, value ]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      call (encoderForType depth ifaces cname tipe) [ deepEncoderForType depth ifaces cname key, deepEncoderForType depth ifaces cname val, value ]

    TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Bytes" _ -> call (encoderForType depth ifaces cname tipe) [ value ]
    TType (Module.Canonical (Name "elm" "time") "Time") "Posix" _ -> call (encoderForType depth ifaces cname tipe) [ value ]

    -- Frontend only JS reference types
    TType (Module.Canonical (Name "elm" "file") "File") "File" _ -> call (encoderForType depth ifaces cname tipe) [ value ]


    TType moduleName typeName params ->
      if isUnsupportedKernelType tipe
        then call failEncode [ a Unit ]
        else call (encoderForType depth ifaces cname tipe) $ fmap (deepEncoderForType depth ifaces cname) params ++ [ value ]

    TRecord fieldMap maybeExtensible ->
      case maybeExtensible of
        Just extensibleName ->
          -- @EXTENSIBLERECORDS not supported yet
          call failEncode [ a Unit ]

        Nothing ->
          call (encoderForType depth ifaces cname tipe) [ value ]

    TAlias moduleName typeName tvars aType ->
      if isUnsupportedKernelType tipe
        then call failEncode [ a Unit ]
        else call (encoderForType depth ifaces cname tipe) $ fmap (\(tvarName, tvarType) -> deepEncoderForType depth ifaces cname tvarType) tvars ++ [ value ]

    TVar name ->
      -- Tvars should always have a local encoder in scope
      call (lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars name) [value]

    TLambda t1 t2 ->
      call failEncode [ a Unit ]
