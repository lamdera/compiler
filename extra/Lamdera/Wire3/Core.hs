{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Lamdera.Wire3.Core where

{-

All the code that does the Wire encoder/decoder code gen and injection

-}

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

-- import qualified Wire.Source2 as Source2

import StandaloneInstances
import qualified CanSer.CanSer as ToSource

import Lamdera
import qualified Lamdera.Project
import Lamdera.Wire3.Helpers
import Lamdera.Wire3.Encoder
import Lamdera.Wire3.Decoder
import Lamdera.Wire3.Graph

import qualified Ext.ElmFormat

runTests isTest_ debugName pkg modul decls generatedName generated canonicalValue wiregen =
  if isTest_
    then
      unsafePerformIO $ do
      let
        testName = Data.Name.fromChars $ "expected_" ++ Data.Name.toChars generatedName

        withName def n_ =
          case def of
            Def (A.At r n) p e -> Def (A.At r n_) p e
            TypedDef (A.At r n) freeVars pts e t -> TypedDef (A.At r n_) freeVars pts e t

        fullTypeRef =
          (T.pack $ Pkg.toChars pkg) <> ":" <> (T.pack $ Data.Name.toChars $ Src.getName modul) <> "." <> (T.pack $ Data.Name.toChars generatedName)

      case decls & findDef testName of
        Just testDefinition -> do

          -- debugHaskellPass ("💚 testDefinition " <> show_ (Src.getName modul)) (testDefinition) (pure ())
          -- debugHaskellPass ("🧡 generated " <> show_ (Src.getName modul)) (generated) (pure ())
          -- diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
          -- atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff

          if generated == testDefinition `withName` generatedName
            then do
              -- atomicPutStrLn $ "✅ gen " <> debugName <> " matches " <> Data.Name.toChars (Src.getName modul) <> "." <> Data.Name.toChars testName
              -- debugPassText ("🧡 expected implementation pretty-printed " <> show_ (Src.getName modul)) (Source2.generateCodecs Map.empty wiregen) (pure ())
              pure ()

            else do
              -- debugHaskellPass ("🏁 Actual value input for " <> (T.pack $ Data.Name.toChars generatedName)) (canonicalValue) (pure ())
              -- let formatted = Ext.ElmFormat.format $ ToSource.convert generated
              -- case formatted of
              --   Right t ->
              --     debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (t) (pure ())
              --   Left err ->
              --     debugPassText ("❌❌❌ actual implementation pretty-printed something went wrong with formatting " <> show_ (Src.getName modul)) (err) (pure ())

              -- debugPassText ("🧡 expected implementation pretty-printed " <> show_ (Src.getName modul)) (Source2.generateCodecs Map.empty wiregen) (pure ())
              -- debugHaskellPass ("🧡 expected implementation AST.Canonical " <> show_ (Src.getName modul)) (testDefinition) (pure ())

              diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
              diff2 <- icdiff (ToSource.convert (testDefinition `withName` generatedName)) (ToSource.convert generated)
              -- diff2 <- do
              --   l <- Ext.ElmFormat.format $ ToSource.convert testDefinition
              --   r <- Ext.ElmFormat.format $ ToSource.convert generated
              --   let withDefault x =
              --         case x of
              --           Left err -> show_ err
              --           Right t  -> t
              --   icdiff (withDefault l) (withDefault r)
              atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff1:\n" ++ diff
              atomicPutStrLn $ "❌❌❌ ASTs do not match, attempting pretty-print diff2:\n" ++ diff2
              -- error "exiting!"
              -- atomicPutStrLn $ "❌❌❌ " ++ Data.Name.toChars (Src.getName modul) ++ "." ++ Data.Name.toChars generatedName ++ " gen does not match test definition."
              pure ()

        Nothing -> do
          -- atomicPutStrLn $ "⚠️  Warning: test not found " ++ show pkg ++ ":" ++ Data.Name.toChars (Src.getName modul) ++ "." ++ Data.Name.toChars testName -- ++ "\n" ++ show (declsToList decls & fmap defName)
          -- debugPassText ("🧡 expected implementation pretty-printed " <> fullTypeRef) (Source2.generateCodecs Map.empty wiregen) (pure ())
          -- error "exiting!"
          pure ()

      else ()

unionAsModule cname name union =
  Can.Module
    { Can._name    = cname
    , Can._exports = Can.ExportEverything A.zero
    , Can._docs    = Src.NoDocs A.zero
    , Can._decls   = Can.SaveTheEnvironment
    , Can._unions  = Map.singleton name union
    , Can._aliases = Map.empty
    , Can._binops  = Map.empty
    , Can._effects = NoEffects
    }

aliasAsModule cname name alias =
  Can.Module
    { Can._name    = cname
    , Can._exports = Can.ExportEverything A.zero
    , Can._docs    = Src.NoDocs A.zero
    , Can._decls   = Can.SaveTheEnvironment
    , Can._unions  = Map.empty
    , Can._aliases = Map.singleton name alias
    , Can._binops  = Map.empty
    , Can._effects = NoEffects
    }

addWireGenerations :: Can.Module -> Pkg.Name -> Map.Map Module.Raw I.Interface -> Src.Module -> Either E.Error Can.Module
addWireGenerations canonical pkg ifaces modul =
  if Lamdera.Wire3.Helpers.shouldHaveCodecsGenerated pkg then
    case addWireGenerations_ canonical pkg ifaces modul of
      Right canonical_ -> do
        Right canonical_

      Left err ->
        Left $ E.BadLamdera "WIRE ERROR" err
  else
    Right canonical


addWireGenerations_ :: Can.Module -> Pkg.Name -> Map.Map Module.Raw I.Interface -> Src.Module -> Either D.Doc Can.Module
addWireGenerations_ canonical pkg ifaces modul =
  let
    !isTest_ = unsafePerformIO Lamdera.isTest

    -- !x = unsafePerformIO $ do
    --       case (pkg, Src.getName modul) of
    --         -- ((Name "Skinney" "elm-deque"), "OldDeque") -> do
    --         ((Name "author" "project"), "Test.Wire_Recursive") -> do
    --           formatHaskellValue "declsBefore" $ declsToSummary $ Can._decls canonical
    --           formatHaskellValue "declsAfter" $ declsToSummary $ extendedDecls
    --
    --         _ ->
    --           pure ()

    -- !x = unsafePerformIO $ do
    --       case (pkg, Src.getName modul) of
    --         ((Name "mdgriffith" "style-elements"), _) -> do
    --         -- ((Name "author" "project"), "Subdir.Subsubdir.SubsubdirType") -> do

    --           newDefs
    --             & fmap ToSource.convert
    --             & mapM (atomicPutStrLn . T.unpack)
    --           -- formatHaskellValue "declsAfter" $ declsToSummary $ extendedDecls
    --           pure ()

    --         _ ->
    --           pure ()

    decls_ = Can._decls canonical

    unionDefs =
      (Can._unions canonical)
        & Map.toList
        & concatMap (\(name, union) ->
            [ (encoderUnion isTest_ ifaces pkg modul decls_ name union)
            , (decoderUnion isTest_ ifaces pkg modul decls_ name union)
            ]
        )

    aliasDefs =
      (Can._aliases canonical)
        & Map.toList
        & concatMap (\(name, alias) ->
            [ (encoderAlias isTest_ ifaces pkg modul decls_ name alias)
            , (decoderAlias isTest_ ifaces pkg modul decls_ name alias)
            ]
        )

    newDefs =
      (unionDefs ++ aliasDefs)

    {- Existing decls, with the injected Wire.Interface placeholders removed -}
    existingDecls =
      foldl (\def decls -> removeDef decls def ) decls_ newDefs

    extendedDecls =
      newDefs
        & Lamdera.Wire3.Graph.stronglyConnCompDefs
        & Lamdera.Wire3.Graph.addGraphDefsToDecls existingDecls

    {- This implementation sorted all decls, however sorting only by lvar is
    not a valid dependency sort for all functions, only for wire functions!
    Left here for reference temporarily in case the new approach also causes
    issues, so we have a record of the things we've tried. -}
    oldDeclsImpl =
      declsToList decls_
        & List.unionBy (\a_ b -> defName a_ == defName b) (unionDefs ++ aliasDefs)
        & Lamdera.Wire3.Graph.stronglyConnCompDefs
        & Lamdera.Wire3.Graph.addGraphDefsToDecls SaveTheEnvironment

    {- For any modules that don't ExportEverything, we add our newDefs to exports -}
    extendedExports =
      case Can._exports canonical of
        ExportEverything region ->
          ExportEverything region

        Export exports ->
          newDefs
            & foldl (\exports_ def ->
                addExport def exports_
              )
              exports
            & Export
  in
  Right $ canonical
    { _decls = extendedDecls
    , _exports = extendedExports
    }


addExport :: Def -> Map.Map Data.Name.Name (A.Located Export) -> Map.Map Data.Name.Name (A.Located Export)
addExport def exports =
  case def of
    Def (A.At region name_) pvars expr ->
      Map.insert name_ (a ExportValue) exports
    TypedDef (A.At region name_) freeVars pvars expr tipe ->
      Map.insert name_ (a ExportValue) exports


encoderUnion :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
encoderUnion isTest_ ifaces pkg modul decls unionName union =
  let
    !x = runTests isTest_ "encoderUnion" pkg modul decls generatedName finalGen union (unionAsModule cname unionName union)

    generatedName = Data.Name.fromChars $ "w3_encode_" ++ Data.Name.toChars unionName
    cname = Module.Canonical pkg (Src.getName modul)
    tvars = _u_vars union
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar )

    ptvarsTyped = tvars & fmap (\tvar ->
        (pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar, TLambda (TVar tvar) tLamdera_Wire_Encoder_Holey)
      )

    numCtors = length $ _u_alts union

    variantIntEncoder n
      | numCtors <= 255        = encodeUnsignedInt8 (int n)
      | numCtors <= 65535      = encodeUnsignedInt16 (int n)
      -- Truly ridiculous but... maybe one day? 🪐
      -- | numCtors <= 4294967295 = encodeUnsignedInt32 (int n)
      | otherwise              = error $ "Unhandled custom type variant size (" ++ show n ++ "), please report this issue for the custom type " ++ Data.Name.toChars unionName

    generatedBody =
        -- debugEncoder_ (Data.Name.toElmString unionName)
        (caseof (lvar "w3v") $
            _u_alts union
              & List.sortOn (\(Ctor name index_ numParams paramTypes) -> name)
              & imap (\i (Ctor tagName tagIndex numParams paramTypes) ->
                  let
                    params =
                      paramTypes & imap (\pi_ paramType ->
                        PatternCtorArg
                          { _index = Index.ZeroBased pi_
                          , _type = paramType
                          , _arg = pvar (Data.Name.fromChars $ "v" ++ show pi_)
                          }
                      )

                    paramEncoders =
                      paramTypes & imap (\pi_ paramType ->
                          encodeTypeValue 0 ifaces cname paramType (lvar (Data.Name.fromChars $ "v" ++ show pi_))
                      )

                    branchHandler =
                      if numParams == 0 then
                        variantIntEncoder i
                      else
                        encodeSequenceWithoutLength $ list $ [ variantIntEncoder i ] ++ paramEncoders
                  in
                  CaseBranch
                    (a (PCtor
                      { _p_home = cname
                      , _p_type = unionName
                      , _p_union = union
                      , _p_name = tagName
                      , _p_index = tagIndex
                      , _p_args = params
                      }))
                    branchHandler
              )
            )

    -- = Def (A.Located Name) [Pattern] Expr
    generated = Def (a (generatedName)) (ptvars ++ [ pvar "w3v" ]) generatedBody

    -- | TypedDef (A.Located Name) FreeVars [(Pattern, Type)] Expr Type
    -- generatedTyped = Def (a (generatedName)) (ptvars ++ [ pvar "w3v" ]) generatedBody

    ttype = (TType cname unionName (tvars & fmap TVar))

    freeVars = tvars & fmap (\tvar -> (tvar, ())) & Map.fromList

    generatedTyped = TypedDef (a (generatedName)) freeVars (ptvarsTyped ++ [(pvar "w3v", ttype)]) generatedBody $
      -- @NOTE: unlike Def, with a TypedDef only the final type is annotated here, the rest of the types
      -- seem to be draw from the Pattern collection.
      (TAlias
            (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire3")
            "Encoder"
            []
            (Holey (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))


    finalGen = generatedTyped
  in
  finalGen


decoderUnion :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
decoderUnion isTest_ ifaces pkg modul decls unionName union =
  let
    !x = runTests isTest_ "decoderUnion" pkg modul decls generatedName generated union (unionAsModule cname unionName union)

    generatedName = Data.Name.fromChars $ "w3_decode_" ++ Data.Name.toChars unionName
    cname = Module.Canonical pkg (Src.getName modul)
    tvars = _u_vars union
    tvarsTypesig = tvars & foldl (\acc name -> Map.insert name () acc ) Map.empty
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar )
    unionType = TType cname unionName (fmap TVar tvars)

    vctor :: Data.Name.Name -> Index.ZeroBased -> [Type] -> Expr
    vctor tagName index paramTypes =
      let
        constructorType = paramTypes & foldr (\paramType typeSig -> TLambda paramType typeSig ) unionType
      in
      (a (VarCtor (_u_opts union) cname tagName index (Forall tvarsTypesig constructorType)))

    numCtors = length $ _u_alts union

    variantIntDecoder
      | numCtors <= 255        = decodeUnsignedInt8
      | numCtors <= 65535      = decodeUnsignedInt16
      -- Truly ridiculous but... maybe one day? 🪐
      -- | numCtors <= 4294967295 = decodeUnsignedInt32
      | otherwise = error $ "Unhandled custom type variant size (" ++ show numCtors ++ "), please report this issue for the custom type " ++ Data.Name.toChars unionName

    generated =
      Def
      -- TypedDef
        (a (generatedName))
        -- Map.empty
        ptvars $
        -- debugDecoder (Data.Name.toElmString unionName)
        (variantIntDecoder |> andThenDecode1
              (lambda1 (pvar "w3v") $
                caseof (lvar "w3v") $
                  _u_alts union
                    & List.sortOn (\(Ctor name index_ numParams paramTypes) -> name)
                    & imap (\i (Ctor tagName tagIndex numParams paramTypes) ->
                        CaseBranch (pint i) $
                          ([(succeedDecode (vctor tagName tagIndex paramTypes))]
                          ++ fmap (\paramType -> andMapDecode1 ((decoderForType ifaces cname paramType))) paramTypes)
                            & foldlPairs (|>)
                    )
                    & (\l -> l ++ [CaseBranch pAny_ $ failDecode (Data.Name.toChars generatedName <> " unexpected union tag index")])
              )
            )
        -- (TAlias
        --   (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire3")
        --   "Decoder"
        --   [("a", unionType)]
        --   (Holey (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
  in
  generated


-- Takes a tvar name and a type, and recursively searches for any extensible record constraints
-- on that tvar name, returning all the constraint field names and types. This can then be used
-- to construct an anonymous record constraint for the type signature.
-- See Wire_Record_Extensible4_DB.elm for an example.
getRecordConstraints :: Data.Name.Name -> Type -> Map.Map Data.Name.Name Type
getRecordConstraints tvarName tipe =
  -- @TODO cover all the possible constraint types!
  -- debugHaskellPass ("🔵 getRecordConstraints ") (tvarName, tipe) $
  case tipe of
    TType cname aliasName tvars ->
      tvars & fmap (getRecordConstraints tvarName) & Map.unions

    TRecord fields ext ->
      case ext of
        Nothing ->
          fields & fmap (\(FieldType _ t) -> getRecordConstraints tvarName t) & Map.unions

        Just extName ->
          if extName == tvarName
            then fields & fmap (\(FieldType _ t) -> t)
            else Map.empty

    TTuple a_ b Nothing  -> [a_, b]    & fmap (getRecordConstraints tvarName) & Map.unions
    TTuple a_ b (Just c) -> [a_, b, c] & fmap (getRecordConstraints tvarName) & Map.unions

    TAlias moduleName typeName tvars aType ->
      let
        aliasedTvarName =
          case List.find (\(t,ti) -> ti == TVar tvarName) tvars of
            Just (tvarNameNew, tvarType) -> tvarNameNew
            Nothing -> tvarName
      in

      case aType of
        Holey t ->
          getRecordConstraints aliasedTvarName t
          -- extractors & fmap (\extractor -> extractor t) & Map.unions

        Filled t ->
          getRecordConstraints aliasedTvarName t
          -- extractors & fmap (\extractor -> extractor t) & Map.unions

    _ ->
      -- no constraint
      Map.empty


renameTvars :: Data.Name.Name -> Data.Name.Name -> Type -> Type
renameTvars oldName newName t =
  case t of
    TVar a ->
      if a == oldName then TVar newName else t
    TLambda t1 t2 -> TLambda (renameTvars oldName newName t1) (renameTvars oldName newName t2)
    TType moduleName typeName params ->
      TType moduleName typeName (fmap (renameTvars oldName newName) params)
    TRecord fieldMap maybeName ->
      fieldMap
        & fmap (\(FieldType index tipe) ->
            FieldType index (renameTvars oldName newName tipe)
          )
        & (\newFieldMap -> TRecord newFieldMap maybeName )
    TUnit -> t
    TTuple a b Nothing  -> TTuple (renameTvars oldName newName a) (renameTvars oldName newName b) Nothing
    TTuple a b (Just c) -> TTuple (renameTvars oldName newName a) (renameTvars oldName newName b) (Just $ renameTvars oldName newName c)

    TAlias moduleName typeName tvars (Holey tipe) ->
      TAlias moduleName typeName (fmap (\(n,v) -> (n, renameTvars oldName newName t)) tvars) (Holey $ renameTvars oldName newName tipe)
    TAlias moduleName typeName tvars (Filled tipe) ->
      TAlias moduleName typeName (fmap (\(n,v) -> (n, renameTvars oldName newName t)) tvars) (Filled $ renameTvars oldName newName tipe)


-- Flatten any TVar renames down through the type tree so that when we extract a constraint,
-- all the TVars are consistent in the top level type signature we extract them for.
normaliseTvarNames :: Map.Map Data.Name.Name Data.Name.Name -> Type -> Type
normaliseTvarNames renames t =
  -- debugHaskellPass "🔵 normaliseTvarNames" (renames, t) $
  case t of
    TVar a ->
      case Map.lookup a renames of
        Just newName -> TVar newName
        Nothing -> t
    TLambda t1 t2 -> TLambda (normaliseTvarNames renames t1) (normaliseTvarNames renames t2)
    TType moduleName typeName params ->
      TType moduleName typeName (fmap (normaliseTvarNames renames) params)
    TRecord fieldMap maybeName ->
      let newMaybeName =
            case maybeName of
              Just name ->
                case Map.lookup name renames of
                  Just newName -> Just newName
                  Nothing -> maybeName
              Nothing -> maybeName
      in
      fieldMap
        & fmap (\(FieldType index tipe) ->
            FieldType index (normaliseTvarNames renames tipe)
          )
        & (\newFieldMap -> TRecord newFieldMap newMaybeName )
    TUnit -> t
    TTuple a b Nothing  -> TTuple (normaliseTvarNames renames a) (normaliseTvarNames renames b) Nothing
    TTuple a b (Just c) -> TTuple (normaliseTvarNames renames a) (normaliseTvarNames renames b) (Just $ normaliseTvarNames renames c)

    TAlias moduleName typeName tvars atype ->
      let
        adjustTvars =
          tvars & fmap (\x@(aliasName,v) ->
            case v of
              TVar originalName ->
                let normalisedName =
                      case Map.lookup originalName renames of
                        Just newName -> newName
                        Nothing -> originalName
                in
                if aliasName /= normalisedName
                  -- we have a tvar rename, just write the tvar for now
                  then (aliasName, TVar normalisedName)
                  -- everything is aligned, carry on
                  else x
              _ -> x
          )

        newRenames =
          adjustTvars & foldl (\acc (aliasName, v) ->
            case v of
              TVar originalName ->
                if aliasName /= originalName
                  then Map.insert aliasName originalName acc
                  else acc
              _ -> acc
          ) Map.empty

        newTvars =
          adjustTvars & fmap (\x@(n,v) ->
            case v of
              TVar originalName -> (originalName, v)
              _ -> x
            )
      in
      case atype of
        Holey tipe  -> TAlias moduleName typeName newTvars (Holey $ normaliseTvarNames newRenames tipe)
        Filled tipe -> TAlias moduleName typeName newTvars (Filled $ normaliseTvarNames newRenames tipe)


-- Sometimes a tvar is a constrained record (aka "extensible record"), and we need to
-- generate a type signature that includes the constraint. This function takes a tvar
-- name and a type, and if the type is a constrained record, it returns a new type
-- that includes the constraint. Otherwise it returns the original TVar.
-- See Wire_Record_Extensible4_DB.elm for an example.
constrainTvar :: Data.Name.Name -> Type -> Type
constrainTvar tvarName tipe =
  -- debugHaskellPassWhen (tvarName == "compatibleA") ("🧡 constrainTvar") (tvarName, tipe) $
  let constraints = getRecordConstraints tvarName tipe
  in
  if Map.size constraints > 0 then
    constraints
      & Map.toList
      & imap (\i (name, t) -> (name, FieldType (fromIntegral i) t))
      & Map.fromList
      & (\fields -> TRecord fields (Just tvarName))
  else
    TVar tvarName


encoderAlias :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
encoderAlias isTest_ ifaces pkg modul decls aliasName alias@(Alias tvars tipe) =
  let
    !x = runTests isTest_ "encoderAlias" pkg modul decls generatedName finalGen alias (aliasAsModule cname aliasName alias)

    generatedName = Data.Name.fromChars $ "w3_encode_" ++ Data.Name.toChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar )

    normalisedTvarsType = normaliseTvarNames Map.empty tipe

    ptvarsTyped = tvars & fmap (\tvar ->
        let constrainedTvar = constrainTvar tvar normalisedTvarsType
        in
        ( pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar
        , TLambda constrainedTvar tLamdera_Wire_Encoder_Holey
        )
      )

    tvarsNameTyped = tvars & fmap (\tvar ->
        (Data.Name.fromChars $ Data.Name.toChars tvar, TVar tvar)
      )

    generatedBody = deepEncoderForType 0 ifaces cname tipe

    -- = Def (A.Located Name) [Pattern] Expr
    generated = Def (a (generatedName)) ptvars $
      -- debugEncoder (Data.Name.toElmString aliasName) $
      generatedBody

    ttype = (TType cname aliasName (tvars & fmap TVar))

    freeVars = tvars & fmap (\tvar -> (tvar, ())) & Map.fromList

    -- | TypedDef (A.Located Name) FreeVars [(Pattern, Type)] Expr Type
    generatedTyped = TypedDef (a (generatedName)) freeVars (ptvarsTyped) generatedBody $
      (TLambda
        (TAlias
            -- (Module.Canonical (Name "author" "project") "Test.Wire_Alias_1_Basic")
            cname
            -- "AliasExternal"
            aliasName
            tvarsNameTyped
            (Holey
              -- (TType
              --     (Module.Canonical (Name "elm" "core") "Maybe")
              --     "Maybe"
              --     [TType (Module.Canonical (Name "author" "project") "Test.External") "ExternalCustomBasic" []])
              tipe
              )
        )
        -- tipe
        (TAlias
            (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire3")
            "Encoder"
            []
            (Holey (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" []))))

    finalGen = generatedTyped
  in
  finalGen


decoderAlias :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
decoderAlias isTest_ ifaces pkg modul decls aliasName alias@(Alias tvars tipe) =
  let
    !x = runTests isTest_ "decoderAlias" pkg modul decls generatedName generated alias (aliasAsModule cname aliasName alias)

    generatedName = Data.Name.fromChars $ "w3_decode_" ++ Data.Name.toChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar )

    generated = Def (a (generatedName)) ptvars $
      -- debugDecoder (Data.Name.toElmString aliasName) $
      decoderForType ifaces cname tipe
  in
  generated
