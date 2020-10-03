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
  case addWireGenerations_ canonical pkg ifaces modul of
    Right canonical ->
      Right canonical

    Left err ->
      Left $ E.BadLamdera err


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

        debugHaskellPass ("🧡 expected implementation AST.Canonical " <> show_ (Src.getName modul)) (Can._decls canonical) (pure $ Right canonical)
        debugPassText ("💙 expected implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert $ Can._decls canonical) (pure $ Right canonical)
        -- debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert $ Can._decls result) (pure $ Right canonical)

        -- debugHaskellPass ("🧡 aliases " <> show_ (Src.getName modul)) (Can._unions canonical & Map.keys) (pure $ Right canonical)

        debugPassText
          ("❤️  oldcodecs " <> show_ (Src.getName modul))
          (Source2.generateCodecs Map.empty canonical)
          (pure $ Right canonical)

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
        & addDef (encoder pkg modul decls_ k v)
        & addDef (decoder pkg modul decls_ k v)
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


vctor_Test_Wire__Union1 cname unionName tagName index =
  (a (VarCtor
         Enum -- @TODO this will change depending on the complexity of the union! See Can.CtorOpts
              -- It's also already specified in `Union`... just use that.
         cname
         tagName
         index
         (Forall
            (Map.fromList [])
            (TType cname unionName []))))


encoderUnion :: Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
encoderUnion pkg modul decls unionName union =
  let
    -- !x = unsafePerformIO $
    --   debugGeneration modul decls generatedName generated
      -- debugHaskellPass "union" (union) (pure ())

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
                  CaseBranch
                    (pctorUnion cname unionName union tagName tagIndex [])
                    (encodeSequenceWithoutLength $ list [ encodeUnsignedInt8 (int i) ])
              )
            )
  in
  generated


decoderUnion :: Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
decoderUnion pkg modul decls unionName union =
  let
    !x = unsafePerformIO $ debugGeneration modul decls generatedName generated

    generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars unionName
    cname = ModuleName.Canonical pkg (Src.getName modul)

    generated =
      Def
        (a (generatedName))
        []
        (decodeUnsignedInt8 |> andThenDecode
              (lambda1 (pvar "w2_e_val") $
                caseof (lvar "w2_e_val") $
                  _u_alts union
                    & List.sortOn (\(Ctor name index_ numParams paramTypes) -> name)
                    & imap (\i (Ctor tagName tagIndex numParams paramTypes) ->
                        CaseBranch
                          (pint i)
                          (succeedDecode (vctor_Test_Wire__Union1 cname unionName tagName tagIndex))
                    )
                    & (\l -> l ++ [p_ –> failDecode])
              )
            )
  in
  generated


encoderAlias :: Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
encoderAlias pkg modul decls aliasName alias =
  namedTodo modul $ Utf8.fromChars $ "w2_encode_" ++ Data.Name.toChars aliasName

decoderAlias :: Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
decoderAlias pkg modul decls aliasName alias =
  namedTodo modul $ Utf8.fromChars $ "w2_decode_" ++ Data.Name.toChars aliasName



debugGeneration modul decls generatedName generated = do
  atomicPutStrLn $
    "\n----> 🤖 generated function:\n" ++ (T.unpack $ ToSource.convert generated) ++ "\n" ++
    "----> encoder matches test definition: "

  case decls & findDef generatedName of
    Just testDefinition ->
      if generated == testDefinition
        then
          atomicPutStrLn "✅"
        else do

          debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert generated) (pure ())

          diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
          atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff
          -- atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n <NEUTERED>"

    Nothing ->
      atomicPutStrLn $ "❌❌❌ Error: " ++ show generatedName ++ " implementation not found in " ++ show (Src.getName modul)


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



shouldHaveCodecsGenerated :: Name -> Bool
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
    _ -> True





-- @TODO extract this to DSL later...?


encodeSequenceWithoutLength list =
  (a (Call
        (a (VarForeign
              (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
              "encodeSequenceWithoutLength"
              (Forall
                 (Map.fromList [])
                 (TLambda
                    (TType
                       (ModuleName.Canonical (Name "elm" "core") "List")
                       "List"
                       [ TAlias
                           (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                           "Encoder"
                           []
                           (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" []))
                       ])
                    (TAlias
                       (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                       "Encoder"
                       []
                       (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))))))
        [list]))


encodeUnsignedInt8 value =
  (a (Call
          (a (VarForeign
                (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                "encodeUnsignedInt8"
                (Forall
                   (Map.fromList [])
                   (TLambda
                      (TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" [])
                      (TAlias
                         (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                         "Encoder"
                         []
                         (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))))))
          [value]))


decodeUnsignedInt8 =
  (a (VarForeign
        (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
        "decodeUnsignedInt8"
        (Forall
           (Map.fromList [])
           (TAlias
              (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
              "Decoder"
              [("a", TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" [])]
              (Filled
                 (TType
                    (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode")
                    "Decoder"
                    [TType (ModuleName.Canonical (Name "elm" "core") "Basics") "Int" []]))))))


andThenDecode lambda =
  (a (Call
        (a (VarForeign
              (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
              "andThenDecode"
              (Forall
                 (Map.fromList [("a", ()), ("b", ())])
                 (TLambda
                    (TLambda
                       (TVar "a")
                       (TAlias
                          (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))
                    (TLambda
                       (TAlias
                          (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                          "Decoder"
                          [("a", TVar "a")]
                          (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                       (TAlias
                          (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))))))
        [ lambda
        ]))


succeedDecode value =
  (a (Call
        (a (VarForeign
              (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
              "succeedDecode"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TVar "a")
                    (TAlias
                       (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                       "Decoder"
                       [("a", TVar "a")]
                       (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))))))
        [ value
        ]))


failDecode =
  (a (VarForeign
        (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
        "failDecode"
        (Forall
           (Map.fromList [("a", ())])
           (TAlias
              (ModuleName.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
              "Decoder"
              [("a", TVar "a")]
              (Filled (TType (ModuleName.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"]))))))

int value =
  a (Int value)

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



-- Patterns

pint i =
  a (PInt i)

pvar n =
  a (PVar n)

p_ =
  a (PAnything)
