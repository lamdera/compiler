{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Lamdera.Wire where

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map
import qualified Data.List as List

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

debugGeneration debugName modul decls generatedName generated canonicalValue = do
  case decls & findDef generatedName of
    Just testDefinition -> do

      debugHaskellPass ("💚 testDefinition " <> show_ (Src.getName modul)) (testDefinition) (pure ())
      debugHaskellPass ("🧡 generated " <> show_ (Src.getName modul)) (generated) (pure ())
      diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
      atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff

      if generated == testDefinition
        then
          debugPassText ("✅ gen " <> debugName <> " " <> show_ (Src.getName modul)) ("okay!") (pure ())
        else do
          debugHaskellPass "🏁 Actual value input" (canonicalValue) (pure ())
          debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert generated) (pure ())
          debugHaskellPass ("🧡 expected implementation AST.Canonical " <> show_ (Src.getName modul)) (testDefinition) (pure ())

          diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
          diff2 <- icdiff (ToSource.convert testDefinition) (ToSource.convert generated)

          atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff
          atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff2
          -- atomicPutStrLn $ "❌❌❌ gen does not match test definition, attempting pretty-print diff:\n <NEUTERED>"

    Nothing ->
      atomicPutStrLn $ "❌❌❌ Error: " ++ show generatedName ++ " implementation not found in " ++ show (Src.getName modul)


addWireGenerations :: Can.Module -> Pkg.Name -> Map.Map Module.Raw I.Interface -> Src.Module -> Either E.Error Can.Module
addWireGenerations canonical pkg ifaces modul =
  if shouldHaveCodecsGenerated pkg then
    case addWireGenerations_ canonical pkg ifaces modul of
      Right canonical_ -> do
        _ <- unsafePerformIO $ do
          writeUtf8 "canprinted_no_interface_impl.txt" $ hindentFormatValue canonical_
          pure (Right canonical_)
        Right canonical_

      Left err ->
        Left $ E.BadLamdera err
  else
    Right canonical


shouldHaveCodecsGenerated :: Pkg.Name -> Bool
shouldHaveCodecsGenerated name =
  case name of
    -- Some elm packages are ignored because of cyclic dependencies.
    -- Those codecs have to be manually defined in `lamdera/codecs`.
    -- All other packages, even if their types are defined in js, have codecs generated for their types.
    -- Then we manually override specific types in `Wire.Source`.

    -- elm deps used by lamdera/codecs
    Name "elm" "bytes" -> False
    Name "elm" "core" -> False

    -- avoid cyclic imports; generated codecs rely on lamdera/codecs:Lamdera.Wire. This is our codec bootstrap module.
    Name "lamdera" "codecs" -> False

    -- Everything else should have codecs generated
    -- _ -> True
    Name "author" "project" -> True -- @TODO REMOVE
    _ -> False -- @TODO REMOVE


-- Can.Module
-- data Module =
--   Module
--     { _name    :: Module.Canonical
--     , _exports :: Exports
--     , _docs    :: Src.Docs
--     , _decls   :: Decls
--     , _unions  :: Map.Map Name Union
--     , _aliases :: Map.Map Name Alias
--     , _binops  :: Map.Map Name Binop
--     , _effects :: Effects
--     }


addWireGenerations_ :: Can.Module -> Pkg.Name -> Map.Map Module.Raw I.Interface -> Src.Module -> Either D.Doc Can.Module
addWireGenerations_ canonical pkg ifaces modul = do
  let
    !result =
      canonical { _decls =
          Can._decls canonical
            & addWireFunctions pkg modul (Can._unions canonical) encoderUnion decoderUnion
            & addWireFunctions pkg modul (Can._aliases canonical) encoderAlias decoderAlias
        }

  unsafePerformIO $ do -- @TODO finally check whether unsafePerformIO is needed at all?

    -- if (Src.getName modul == "WireTypes")
    --   then do
        -- debugHaskellPass ("🧡 expected implementation AST.Canonical " <> show_ (Src.getName modul)) (Can._decls canonical) (pure $ Right canonical)
        -- debugPassText ("💙 expected implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert $ Can._decls canonical) (pure $ Right canonical)

        debugPassText
          ("❤️  oldcodecs " <> show_ (Src.getName modul))
          (Source2.generateCodecs Map.empty canonical)
          (pure $ Right canonical)

        debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert $ Can._decls result) (pure $ Right canonical)
        -- debugHaskellPass ("🧡 aliases " <> show_ (Src.getName modul)) (Can._unions canonical & Map.keys) (pure $ Right canonical)

        pure $ Right result


addWireFunctions
  :: Pkg.Name
  -> Src.Module
  -> Map.Map Data.Name.Name a
  -> (Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> a -> Def)
  -> (Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> a -> Def)
  -> Decls
  -> Decls
addWireFunctions pkg modul types encoder decoder decls_ =
  types
    & Map.foldlWithKey (\decls k v ->
      decls
        & upsertDef (encoder pkg modul decls_ k v)
        & upsertDef (decoder pkg modul decls_ k v)
    )
    decls_


pctorUnion cname name union tagName tagIndex args =
  let
    -- alts = _u_alts union

    -- index :: Index.ZeroBased
    -- index =
    --   alts
    --     & List.find (\(Ctor name index_ numParams paramTypes) -> name == tagName)
    --     & fmap (\(Ctor name index_ numParams paramTypes) -> index_)
    --     & withDefault (error $ "impossible, ctor " ++ show tagName ++ " not found in " ++ show alts)
  in
  (a (PCtor
        { _p_home = cname
        , _p_type = name
        , _p_union = union
        , _p_name = tagName
        , _p_index = tagIndex
        , _p_args = args
        }))


encoderUnion :: Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
encoderUnion pkg modul decls unionName union =
  let
    -- !x = unsafePerformIO $ debugGeneration "encoderUnion" modul decls generatedName generated union

    generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars unionName
    cname = Module.Canonical pkg (Src.getName modul)

    generated =
      Def
        (a (generatedName))
        [ pvar "w2_e_val" ]
        (caseof (lvar "w2_e_val") $
            _u_alts union
              & List.sortOn (\(Ctor name index_ numParams paramTypes) -> name)
              & imap (\i (Ctor tagName tagIndex numParams paramTypes) ->
                  let
                    params =
                      paramTypes & imap (\i paramType ->
                        PatternCtorArg
                          { _index = Index.ZeroBased i
                          , _type = paramType
                          , _arg = pvar (Data.Name.fromChars $ "v" ++ show i)
                          }
                      )

                    paramEncoders =
                      paramTypes & imap (\i paramType ->
                          encodeTypeValue paramType (lvar (Data.Name.fromChars $ "v" ++ show i))
                      )
                  in
                  CaseBranch
                    (pctorUnion cname unionName union tagName tagIndex params)
                    (encodeSequenceWithoutLength $ list $ [ encodeUnsignedInt8 (int i) ] ++ paramEncoders)
              )
            )
  in
  generated


decoderUnion :: Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
decoderUnion pkg modul decls unionName union =
  let
    -- !x = unsafePerformIO $ debugGeneration "decoderUnion" modul decls generatedName generated union

    generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars unionName
    cname = Module.Canonical pkg (Src.getName modul)
    unionType = TType cname unionName []

    generated =
      Def
      -- TypedDef
        (a (generatedName))
        -- (Map.fromList [])
        []
        (decodeUnsignedInt8 |> andThenDecode1
              (lambda1 (pvar "w2v") $
                caseof (lvar "w2v") $
                  _u_alts union
                    & List.sortOn (\(Ctor name index_ numParams paramTypes) -> name)
                    & imap (\i (Ctor tagName tagIndex numParams paramTypes) ->
                        (pint i) –>
                          ([(succeedDecode (vctor cname unionName union tagName tagIndex paramTypes))]
                          ++ fmap (\paramType -> andMapDecode1 ((decoderForType paramType))) paramTypes)
                            & foldlPairs (|>)
                    )
                    & (\l -> l ++ [p_ –> failDecode])
              )
            )
        -- (TAlias
        --   (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
        --   "Decoder"
        --   [("a", unionType)]
        --   (Holey (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
  in
  generated


foldlPairs fn list =
  case list of
    [] -> error "Error: foldlPairs called with no items! Please report this with your code."
    x:[] -> x
    x:xs ->
      foldl (\acc item -> fn acc item ) x xs


encoderAlias :: Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
encoderAlias pkg modul decls aliasName alias =
  namedTodo modul $ Utf8.fromChars $ "w2_encode_" ++ Data.Name.toChars aliasName

decoderAlias :: Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
decoderAlias pkg modul decls aliasName alias =
  namedTodo modul $ Utf8.fromChars $ "w2_decode_" ++ Data.Name.toChars aliasName





{- Equivalent of writing `functionName = Debug.todo "functionName"` in Elm -}
namedTodo :: Src.Module -> Data.Name.Name -> Def
namedTodo modul functionName =
  let functionName_ = Utf8.fromChars . Data.Name.toChars $ functionName
      moduleName = Src.getName modul
  in
   Def
      (a (functionName))
      []
      (a (Call
            (a (VarDebug
                  (Module.Canonical (Name "author" "project") moduleName)
                  "todo"
                  (Forall (Map.fromList [("a", ())]) (TLambda (TType (Module.Canonical (Name "elm" "core") "String") "String" []) (TVar "a")))))
            [(a (Str functionName_))]))


a v =
  A.at (A.Position 0 0) (A.Position 0 10) v


-- data Decls
--   = Declare Def Decls
--   | DeclareRec Def [Def] Decls
--   | SaveTheEnvironment


-- instance Show (Can.Decls) where
--   show decls_ = show $ declsToList decls_


declsToList :: Decls -> [Def]
declsToList d =
  case d of
    Declare def decls ->
      def : (declsToList decls)

    DeclareRec def defs decls ->
      (def : defs) ++ declsToList decls

    SaveTheEnvironment ->
      []


-- addDef def_ decls_ =
--   case decls_ of
--     Declare def decls ->
--       Declare def_ (Declare def decls)
--
--     DeclareRec def defs decls ->
--       Declare def_ (DeclareRec def defs decls)
--
--     SaveTheEnvironment ->
--       Declare def_ SaveTheEnvironment


upsertDef newDef decls_ =
  case decls_ of
    Declare def decls ->
      if sameName def newDef
        then
          Declare newDef decls
        else
          Declare def (upsertDef newDef decls)

    DeclareRec def defs decls ->
      if sameName def newDef
        then
          DeclareRec newDef defs decls
        else
          DeclareRec
            def
            (defs & fmap (\d -> if sameName d newDef then newDef else d))
            (upsertDef newDef decls)

    SaveTheEnvironment ->
      DeclareRec newDef [] SaveTheEnvironment


sameName d1 d2 =
  defName d1 == defName d2
  -- error $ "unexpected sameName comparison: \n" ++ (T.unpack $ hindentFormatValue v1) ++ "\n" ++ (T.unpack $ hindentFormatValue v2)


findDef name decls =
  decls
    & declsToList
    & List.find (defNameIs name)

defNameIs name def =
  name == defName def

defName def =
  case def of
    Def (A.At region name_) _ _ ->
      name_
    TypedDef (A.At region name_) _ _ _ _ ->
      name_


-- @TODO extract this to DSL later...?


encodeSequenceWithoutLength list =
  (a (Call (a (VarForeign mLamdera_Wire2 "encodeSequenceWithoutLength"
              (Forall
                 (Map.fromList [])
                 (TLambda
                    (TType
                       (Module.Canonical (Name "elm" "core") "List")
                       "List"
                       [ tLamdera_Wire2__Encoder
                       ])
                    tLamdera_Wire2__Encoder))))
        [list]))


encodeUnsignedInt8 value =
  (a (Call (a (VarForeign mLamdera_Wire2 "encodeUnsignedInt8"
                (Forall
                   (Map.fromList [])
                   (TLambda
                      (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                      tLamdera_Wire2__Encoder))))
          [value]))


decodeUnsignedInt8 =
  (a (VarForeign mLamdera_Wire2 "decodeUnsignedInt8"
        (Forall
           (Map.fromList [])
           (TAlias
              mLamdera_Wire2
              "Decoder"
              [("a", TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])]
              (Filled
                 (TType
                    (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                    "Decoder"
                    [TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []]))))))


andThenDecode1 lambda =
  (a (Call (a (VarForeign mLamdera_Wire2 "andThenDecode"
              (Forall
                 (Map.fromList [("a", ()), ("b", ())])
                 (TLambda
                    (TLambda
                       (TVar "a")
                       (TAlias
                          mLamdera_Wire2
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))
                    (TLambda
                       (TAlias
                          mLamdera_Wire2
                          "Decoder"
                          [("a", TVar "a")]
                          (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                       (TAlias
                          mLamdera_Wire2
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))))))
        [ lambda
        ]))


andMapDecode1 value =
  (a (Call
        (a (VarForeign
              (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
              "andMapDecode"
              (Forall
                 (Map.fromList [("a", ()), ("b", ())])
                 (TLambda
                    (TAlias
                       (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                       "Decoder"
                       [("a", TVar "a")]
                       (Filled
                          (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                    (TLambda
                       (TAlias
                          (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                          "Decoder"
                          [("a", TLambda (TVar "a") (TVar "b"))]
                          (Filled
                             (TType
                                (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                "Decoder"
                                [TLambda (TVar "a") (TVar "b")])))
                       (TAlias
                          (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled
                             (TType
                                (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                "Decoder"
                                [TVar "b"]))))))))
        [ value
        ]))

succeedDecode value =
  (a (Call (a (VarForeign mLamdera_Wire2 "succeedDecode"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TVar "a")
                    (TAlias
                       mLamdera_Wire2
                       "Decoder"
                       [("a", TVar "a")]
                       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))))))
        [ value
        ]))


failDecode =
  (a (VarForeign mLamdera_Wire2 "failDecode"
        (Forall
           (Map.fromList [("a", ())])
           (TAlias
              mLamdera_Wire2
              "Decoder"
              [("a", TVar "a")]
              (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"]))))))

int value =
  a (Int value)

str value =
  a (Str value)

list values =
  a (List values)

lambda1 pattern expr =
  a (Lambda [ pattern ] expr)

caseof pattern branches =
  a (Case pattern branches)

(–>) pattern expr =
  CaseBranch pattern expr

infixr 0 –>

(|>) expr1 expr2 =
  (a (Binop
        "|>"
        (Module.Canonical (Name "elm" "core") "Basics")
        "apR"
        (Forall (Map.fromList [("a", ()), ("b", ())]) (TLambda (TVar "a") (TLambda (TLambda (TVar "a") (TVar "b")) (TVar "b"))))
        expr1
        expr2
     )
   )

infixr 0 |>

lvar n =
  a (VarLocal n)

vctor cname unionName union tagName index paramTypes =
  let
    unionType = TType cname unionName []

    constructorType =
      paramTypes & foldr (\paramType typeSig -> TLambda paramType typeSig ) unionType
  in
  (a (VarCtor
         (_u_opts union)
         cname
         tagName
         index
         (Forall
            (Map.fromList [])
            constructorType
            )))


-- Patterns

pint i =
  a (PInt i)

pvar n =
  a (PVar n)

p_ =
  a (PAnything)


encoderForType tipe =
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeInt" (Forall (Map.fromList []) (TLambda (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []) tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeFloat" (Forall (Map.fromList []) (TLambda (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []) tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeBool" (Forall (Map.fromList []) (TLambda (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []) tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeOrder" (Forall (Map.fromList []) (TLambda (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []) tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeNever" (Forall (Map.fromList []) (TLambda (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []) tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeChar" (Forall (Map.fromList []) (TLambda (TType (Module.Canonical (Name "elm" "core") "Char") "Char" []) tLamdera_Wire2__Encoder))))

    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeString" (Forall (Map.fromList []) (TLambda (TType (Module.Canonical (Name "elm" "core") "String") "String" []) tLamdera_Wire2__Encoder))))

    TUnit ->
      (a (VarForeign mLamdera_Wire2 "encodeUnit" (Forall (Map.fromList []) (TLambda TUnit tLamdera_Wire2__Encoder))))

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
      -- str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe
      let
        generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars typeName
      in
      (a (VarTopLevel moduleName generatedName))

    _ ->
      -- error $ "Not yet implemented: " ++ show tipe
      str $ Utf8.fromChars $ "encoderForType not implemented! " ++ show tipe


deepEncoderForType tipe =
  case tipe of
    TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []    -> encoderForType tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []  -> encoderForType tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []   -> encoderForType tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []  -> encoderForType tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []  -> encoderForType tipe
    TType (Module.Canonical (Name "elm" "core") "Char") "Char" []     -> encoderForType tipe
    TType (Module.Canonical (Name "elm" "core") "String") "String" [] -> encoderForType tipe
    TUnit                                                             -> encoderForType tipe

    TTuple a b Nothing  -> call (encoderForType tipe) [ deepEncoderForType a, deepEncoderForType b ]
    TTuple a b (Just c) -> call (encoderForType tipe) [ deepEncoderForType a, deepEncoderForType b, deepEncoderForType c ]

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [a] -> call (encoderForType tipe) [ deepEncoderForType a ]
    TType (Module.Canonical (Name "elm" "core") "List") "List" [a]   -> call (encoderForType tipe) [ deepEncoderForType a ]
    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [a]     -> call (encoderForType tipe) [ deepEncoderForType a ]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> call (encoderForType tipe) [ deepEncoderForType a ]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      call (encoderForType tipe) [ deepEncoderForType err, deepEncoderForType a ]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      call (encoderForType tipe) [ deepEncoderForType key, deepEncoderForType val ]

    TType (Module.Canonical (Name "elm" "core") _) _ _ ->
      str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe

    TType (Module.Canonical (Name "elm" "bytes") _) _ _ ->
      str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe

    TType moduleName typeName params ->
      str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe

    _ ->
      str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe


encodeTypeValue tipe value =
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])    -> call (encoderForType tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" [])  -> call (encoderForType tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" [])   -> call (encoderForType tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" [])  -> call (encoderForType tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" [])  -> call (encoderForType tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" [])     -> call (encoderForType tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) -> call (encoderForType tipe) [ value ]
    TUnit                                                               -> call (encoderForType tipe) [ value ]

    TTuple a b Nothing  -> call (encoderForType tipe) [ deepEncoderForType a, deepEncoderForType b, value ]
    TTuple a b (Just c) -> call (encoderForType tipe) [ deepEncoderForType a, deepEncoderForType b, deepEncoderForType c, value ]

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [a] -> call (encoderForType tipe) [ deepEncoderForType a, value ]
    TType (Module.Canonical (Name "elm" "core") "List") "List" [a]   -> call (encoderForType tipe) [ deepEncoderForType a, value ]
    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [a]     -> call (encoderForType tipe) [ deepEncoderForType a, value ]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> call (encoderForType tipe) [ deepEncoderForType a, value ]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      call (encoderForType tipe) [ deepEncoderForType err, deepEncoderForType a, value ]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      call (encoderForType tipe) [ deepEncoderForType key, deepEncoderForType val, value ]

    TType (Module.Canonical (Name "elm" "core") _) _ _ ->
      str $ Utf8.fromChars $ "encodeTypeValue not implemented! " ++ show tipe

    TType (Module.Canonical (Name "elm" "bytes") _) _ _ ->
      str $ Utf8.fromChars $ "encodeTypeValue not implemented! " ++ show tipe

    TType moduleName typeName params ->
      call (encoderForType tipe) $ fmap deepEncoderForType params ++ [ value ]

    _ ->
      -- error $ "Not yet implemented: " ++ show tipe
      str $ Utf8.fromChars $ "encodeTypeValue not implemented! " ++ show tipe



call fn args =
  (a (Call fn args))

decoderForType tipe =
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeInt" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeFloat" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeBool" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeOrder" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeNever" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeChar" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeString" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    TUnit ->
      (a (VarForeign mLamdera_Wire2 "decodeUnit" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

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
              [ decoderForType a_
              , decoderForType b
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
             [ decoderForType a_
             , decoderForType b
             , decoderForType c
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
        [ decoderForType ptype ]))

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
        [ decoderForType ptype ]))

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
        [ decoderForType ptype ]))

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
        [ decoderForType ptype ]))

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
              [ decoderForType err
              , decoderForType a_
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
              [ decoderForType key
              , decoderForType val
              ]))

    TType (Module.Canonical (Name "elm" "core") _) _ _ ->
      str $ Utf8.fromChars $ "decoder not implemented! " ++ show tipe

    TType (Module.Canonical (Name "elm" "bytes") _) _ _ ->
      str $ Utf8.fromChars $ "decoder not implemented! " ++ show tipe

    TType moduleName typeName params ->
      let
        generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars typeName
      in
      if length params == 0
        then
          (a (VarTopLevel moduleName generatedName))
        else
          call (a (VarTopLevel moduleName generatedName)) $ fmap decoderForType params

    _ ->
      -- error $ "Not yet implemented: " ++ show tipe
      str $ Utf8.fromChars $ "decoder not implemented! " ++ show tipe


tLamdera_Wire2__Encoder =
  (TAlias
     mLamdera_Wire2
     "Encoder"
     []
       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))


mLamdera_Wire2 =
  (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
