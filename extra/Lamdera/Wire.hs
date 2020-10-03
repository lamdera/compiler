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
import qualified Elm.ModuleName as ModuleName
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

addWireGenerations :: Can.Module -> Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Can.Module
addWireGenerations canonical pkg ifaces modul =
  if shouldHaveCodecsGenerated pkg then
    case addWireGenerations_ canonical pkg ifaces modul of
      Right canonical_ ->
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
--     { _name    :: ModuleName.Canonical
--     , _exports :: Exports
--     , _docs    :: Src.Docs
--     , _decls   :: Decls
--     , _unions  :: Map.Map Name Union
--     , _aliases :: Map.Map Name Alias
--     , _binops  :: Map.Map Name Binop
--     , _effects :: Effects
--     }


addWireGenerations_ :: Can.Module -> Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either D.Doc Can.Module
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

        -- debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert $ Can._decls result) (pure $ Right canonical)
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
        & addDef (decoder pkg modul decls_ k v)
        & addDef (encoder pkg modul decls_ k v)
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
    !x = unsafePerformIO $ debugGeneration modul decls generatedName generated union

    generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars unionName
    cname = ModuleName.Canonical pkg (Src.getName modul)

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


debugGeneration modul decls generatedName generated canonicalValue = do

  debugHaskellPass "🏁 Actual value input" (canonicalValue) (pure ())
  debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert generated) (pure ())
  -- debugPassText ("💚 actual implementation source " <> show_ (Src.getName modul)) (ToSource.convert generated) (pure ())


  atomicPutStrLn $
    -- "\n----> 🤖 generated function pretty-printed:\n" ++ (T.unpack $ ToSource.convert generated) ++ "\n" ++
    "----> encoder matches test definition: "

  case decls & findDef generatedName of
    Just testDefinition -> do
      if generated == testDefinition
        then
          atomicPutStrLn "✅"
        else do
          debugHaskellPass ("🧡 expected implementation AST.Canonical " <> show_ (Src.getName modul)) (testDefinition) (pure ())
          diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
          diff2 <- icdiff (ToSource.convert testDefinition) (ToSource.convert generated)
          atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff
          atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff2
          -- atomicPutStrLn $ "❌❌❌ gen does not match test definition, attempting pretty-print diff:\n <NEUTERED>"

    Nothing ->
      atomicPutStrLn $ "❌❌❌ Error: " ++ show generatedName ++ " implementation not found in " ++ show (Src.getName modul)


decoderUnion :: Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
decoderUnion pkg modul decls unionName union =
  let
    -- !x = unsafePerformIO $ debugGeneration modul decls generatedName generated union

    generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars unionName
    cname = ModuleName.Canonical pkg (Src.getName modul)

    generated =
      Def
        (a (generatedName))
        []
        (decodeUnsignedInt8 |> andThenDecode1
              (lambda1 (pvar "w2_e_val") $
                caseof (lvar "w2_e_val") $
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
                  (ModuleName.Canonical (Name "author" "project") moduleName)
                  "todo"
                  (Forall (Map.fromList [("a", ())]) (TLambda (TType (ModuleName.Canonical (Name "elm" "core") "String") "String" []) (TVar "a")))))
            [(a (Str functionName_))]))


a v =
  A.at (A.Position 0 0) (A.Position 0 10) v


-- data Decls
--   = Declare Def Decls
--   | DeclareRec Def [Def] Decls
--   | SaveTheEnvironment


instance Show (Can.Decls) where
  show decls_ = show $ declsToList decls_


declsToList :: Decls -> [Def]
declsToList d =
  case d of
    Declare def decls ->
      def : (declsToList decls)

    DeclareRec def defs decls ->
      (def : defs) ++ declsToList decls

    SaveTheEnvironment ->
      []


addDef def_ decls_ =
  case decls_ of
    Declare def decls ->
      Declare def_ (Declare def decls)

    DeclareRec def defs decls ->
      Declare def_ (DeclareRec def defs decls)

    SaveTheEnvironment ->
      Declare def_ SaveTheEnvironment


findDef name decls =
  decls
    & declsToList
    & List.find (defNameIs name)
  -- case decls_ of
  --   Declare def decls ->
  --
  --
  --   DeclareRec def defs decls ->
  --
  --
  --   SaveTheEnvironment ->
  --     Nothing


defNameIs name def =
  case def of
    Def (A.At region name_) _ _ ->
      name == name_
    TypedDef (A.At region name_) _ _ _ _ ->
      name == name_






-- @TODO extract this to DSL later...?


encodeSequenceWithoutLength list =
  (a (Call
        (a (VarForeign
              mLamdera_Wire2
              "encodeSequenceWithoutLength"
              (Forall
                 (Map.fromList [])
                 (TLambda
                    (TType
                       (ModuleName.Canonical (Name "elm" "core") "List")
                       "List"
                       [ tLamdera_Wire2__Encoder
                       ])
                    tLamdera_Wire2__Encoder))))
        [list]))


encodeUnsignedInt8 value =
  (a (Call
          (a (VarForeign
                mLamdera_Wire2
                "encodeUnsignedInt8"
                (Forall
                   (Map.fromList [])
                   (TLambda
                      (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" [])
                      tLamdera_Wire2__Encoder))))
          [value]))


decodeUnsignedInt8 =
  (a (VarForeign
        mLamdera_Wire2
        "decodeUnsignedInt8"
        (Forall
           (Map.fromList [])
           (TAlias
              mLamdera_Wire2
              "Decoder"
              [("a", TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" [])]
              (Filled
                 (TType
                    (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode")
                    "Decoder"
                    [TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" []]))))))


andThenDecode1 lambda =
  (a (Call
        (a (VarForeign
              mLamdera_Wire2
              "andThenDecode"
              (Forall
                 (Map.fromList [("a", ()), ("b", ())])
                 (TLambda
                    (TLambda
                       (TVar "a")
                       (TAlias
                          mLamdera_Wire2
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))
                    (TLambda
                       (TAlias
                          mLamdera_Wire2
                          "Decoder"
                          [("a", TVar "a")]
                          (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                       (TAlias
                          mLamdera_Wire2
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))))))
        [ lambda
        ]))


andMapDecode1 value =
        (a (Call
              (a (VarForeign
                    (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                    "andMapDecode"
                    (Forall
                       (Map.fromList [("a", ()), ("b", ())])
                       (TLambda
                          (TAlias
                             (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                             "Decoder"
                             [("a", TVar "a")]
                             (Filled
                                (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                          (TLambda
                             (TAlias
                                (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                                "Decoder"
                                [("a", TLambda (TVar "a") (TVar "b"))]
                                (Filled
                                   (TType
                                      (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [TLambda (TVar "a") (TVar "b")])))
                             (TAlias
                                (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                                "Decoder"
                                [("a", TVar "b")]
                                (Filled
                                   (TType
                                      (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                      "Decoder"
                                      [TVar "b"]))))))))
              [ value
              ]))

succeedDecode value =
  (a (Call
        (a (VarForeign
              mLamdera_Wire2
              "succeedDecode"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TVar "a")
                    (TAlias
                       mLamdera_Wire2
                       "Decoder"
                       [("a", TVar "a")]
                       (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))))))
        [ value
        ]))


failDecode =
  (a (VarForeign
        mLamdera_Wire2
        "failDecode"
        (Forall
           (Map.fromList [("a", ())])
           (TAlias
              mLamdera_Wire2
              "Decoder"
              [("a", TVar "a")]
              (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"]))))))

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
        (ModuleName.Canonical (Name "elm" "core") "Basics")
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
    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeInt" (Forall (Map.fromList []) (TLambda (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" []) tLamdera_Wire2__Encoder))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Float" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeFloat" (Forall (Map.fromList []) (TLambda (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Float" []) tLamdera_Wire2__Encoder))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Bool" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeBool" (Forall (Map.fromList []) (TLambda (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Bool" []) tLamdera_Wire2__Encoder))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Order" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeOrder" (Forall (Map.fromList []) (TLambda (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Order" []) tLamdera_Wire2__Encoder))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Never" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeNever" (Forall (Map.fromList []) (TLambda (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Never" []) tLamdera_Wire2__Encoder))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Char") "Char" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeChar" (Forall (Map.fromList []) (TLambda (TType (ModuleName.Canonical (Name "elm" "core") "Char") "Char" []) tLamdera_Wire2__Encoder))))

    (TType (ModuleName.Canonical (Name "elm" "core") "String") "String" []) ->
      (a (VarForeign mLamdera_Wire2 "encodeString" (Forall (Map.fromList []) (TLambda (TType (ModuleName.Canonical (Name "elm" "core") "String") "String" []) tLamdera_Wire2__Encoder))))

    TUnit ->
      (a (VarForeign mLamdera_Wire2 "encodeUnit" (Forall (Map.fromList []) (TLambda TUnit tLamdera_Wire2__Encoder))))

       -- maybe
       -- list
    TType (ModuleName.Canonical (Name "elm" "core") "List") "List" [ptype] ->
      (a (VarForeign
           mLamdera_Wire2
           "encodeList"
           (Forall
              (Map.fromList [("a", ())])
              (TLambda
                 (TLambda (TVar "a") tLamdera_Wire2__Encoder)
                 (TLambda
                    (TType (ModuleName.Canonical (Name "elm" "core") "List") "List" [TVar "a"])
                    tLamdera_Wire2__Encoder)))))

       -- array
       -- set
       -- result
       -- dict

    _ ->
      -- error $ "Not yet implemented: " ++ show tipe
      str $ Utf8.fromChars $ "encoder not implemented! " ++ show tipe


encodeTypeValue tipe value =
  case tipe of
    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" []) ->
      call (encoderForType tipe) [value]

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Float" []) ->
      call (encoderForType tipe) [value]

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Bool" []) ->
      call (encoderForType tipe) [value]

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Order" []) ->
      call (encoderForType tipe) [value]

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Never" []) ->
      call (encoderForType tipe) [value]

    (TType (ModuleName.Canonical (Name "elm" "core") "Char") "Char" []) ->
      call (encoderForType tipe) [value]

    (TType (ModuleName.Canonical (Name "elm" "core") "String") "String" []) ->
      call (encoderForType tipe) [value]

    TUnit ->
      call (encoderForType tipe) [value]

    TType (ModuleName.Canonical (Name "elm" "core") "List") "List" [ptype] ->
       call
         (encoderForType tipe)
         [ encoderForType ptype
         , value
         ]



call fn args =
  (a (Call fn args))

decoderForType tipe =
  case tipe of
    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeInt" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Float" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeFloat" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Bool" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeBool" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Order" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeOrder" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Never" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeNever" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (ModuleName.Canonical (Name "elm" "core") "Char") "Char" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeChar" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (ModuleName.Canonical (Name "elm" "core") "String") "String" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeString" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    TUnit ->
      (a (VarForeign mLamdera_Wire2 "decodeUnit" (Forall (Map.fromList []) (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))


-- maybe
-- list

    TType (ModuleName.Canonical (Name "elm" "core") "List") "List" [ptype] ->
      (a (Call
        (a (VarForeign
              mLamdera_Wire2
              "decodeList"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TAlias
                       mLamdera_Wire2
                       "Decoder"
                       [("a", TVar "a")]
                       (Filled
                          (TType
                             (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode")
                             "Decoder"
                             [TVar "a"])))
                    (TAlias
                       mLamdera_Wire2
                       "Decoder"
                       [("a", TType (ModuleName.Canonical (Name "elm" "core") "List") "List" [TVar "a"])]
                       (Filled
                          (TType
                             (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode")
                             "Decoder"
                             [TType (ModuleName.Canonical (Name "elm" "core") "List") "List" [TVar "a"]])))))))
        [ decoderForType ptype ]))


-- array
-- set
-- result
-- dict


    _ ->
      -- error $ "Not yet implemented: " ++ show tipe
      str $ Utf8.fromChars $ "decoder not implemented! " ++ show tipe


tLamdera_Wire2__Encoder =
  (TAlias
     mLamdera_Wire2
     "Encoder"
     []
       (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))


mLamdera_Wire2 =
  (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
