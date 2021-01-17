{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Lamdera.Wire3.Core where

{-

All the code that does the Wire encoder/decoder code gen and injection

-- @EXTENSIBLERECORDS search for this tag for issues related to skipping
support for extensible records ATM.

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


runTests isTest debugName pkg modul decls generatedName generated canonicalValue wiregen =
  if isTest
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
              -- debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert generated) (pure ())
              -- debugPassText ("🧡 expected implementation pretty-printed " <> show_ (Src.getName modul)) (Source2.generateCodecs Map.empty wiregen) (pure ())
              -- debugHaskellPass ("🧡 expected implementation AST.Canonical " <> show_ (Src.getName modul)) (testDefinition) (pure ())

              diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
              diff2 <- icdiff (ToSource.convert testDefinition) (ToSource.convert generated)
              -- atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff1:\n" ++ diff
              atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff2:\n" ++ diff2
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
        Left $ E.BadLamdera err
  else
    Right canonical


addWireGenerations_ :: Can.Module -> Pkg.Name -> Map.Map Module.Raw I.Interface -> Src.Module -> Either D.Doc Can.Module
addWireGenerations_ canonical pkg ifaces modul =
  let
    !isTest = unsafePerformIO Lamdera.isTest

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
    --         ((Name "elm" "regex"), "Regex") -> do
    --         -- ((Name "author" "project"), "Subdir.Subsubdir.SubsubdirType") -> do
    --
    --           newDefs
    --             & fmap ToSource.convert
    --             & mapM (atomicPutStrLn . T.unpack)
    --           -- formatHaskellValue "declsAfter" $ declsToSummary $ extendedDecls
    --           pure ()
    --
    --         _ ->
    --           pure ()

    decls_ = Can._decls canonical

    unionDefs =
      (Can._unions canonical)
        & Map.toList
        & concatMap (\(name, union) ->
            [ (encoderUnion isTest ifaces pkg modul decls_ name union)
            , (decoderUnion isTest ifaces pkg modul decls_ name union)
            ]
        )

    aliasDefs =
      (Can._aliases canonical)
        & Map.toList
        & concatMap (\(name, alias) ->
            [ (encoderAlias isTest ifaces pkg modul decls_ name alias)
            , (decoderAlias isTest ifaces pkg modul decls_ name alias)
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
        & List.unionBy (\a b -> defName a == defName b) (unionDefs ++ aliasDefs)
        & Lamdera.Wire3.Graph.stronglyConnCompDefs
        & Lamdera.Wire3.Graph.addGraphDefsToDecls SaveTheEnvironment

    {- For any modules that don't ExportEverything, we add our newDefs to exports -}
    extendedExports =
      case Can._exports canonical of
        ExportEverything region ->
          ExportEverything region

        Export exports ->
          newDefs
            & foldl (\exports def ->
                addExport def exports
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
encoderUnion isTest ifaces pkg modul decls unionName union =
  let
    !x = runTests isTest "encoderUnion" pkg modul decls generatedName generated union (unionAsModule cname unionName union)

    generatedName = Data.Name.fromChars $ "w3_encode_" ++ Data.Name.toChars unionName
    cname = Module.Canonical pkg (Src.getName modul)
    tvars = _u_vars union
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar )

    generated =
      Def
        (a (generatedName))
        (ptvars ++ [ pvar "w3v" ]) $
        debugEncoder_ (Data.Name.toElmString unionName)
        (caseof (lvar "w3v") $
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
                          encodeTypeValue 0 ifaces cname paramType (lvar (Data.Name.fromChars $ "v" ++ show i))
                      )
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
                    (encodeSequenceWithoutLength $ list $ [ encodeUnsignedInt8 (int i) ] ++ paramEncoders)
              )
            )
  in
  generated


decoderUnion :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
decoderUnion isTest ifaces pkg modul decls unionName union =
  let
    !x = runTests isTest "decoderUnion" pkg modul decls generatedName generated union (unionAsModule cname unionName union)

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

    generated =
      Def
      -- TypedDef
        (a (generatedName))
        -- Map.empty
        ptvars $
        -- addLetLog (Utf8.fromChars $ "w3_decode_" ++ Data.Name.toChars unionName)
        debugDecoder (Data.Name.toElmString unionName)
        (decodeUnsignedInt8 |> andThenDecode1
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


encoderAlias :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
encoderAlias isTest ifaces pkg modul decls aliasName alias@(Alias tvars tipe) =
  let
    !x = runTests isTest "encoderAlias" pkg modul decls generatedName generated alias (aliasAsModule cname aliasName alias)

    generatedName = Data.Name.fromChars $ "w3_encode_" ++ Data.Name.toChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar )

    generated = Def (a (generatedName)) ptvars $
      debugEncoder (Data.Name.toElmString aliasName) $
      deepEncoderForType 0 ifaces cname tipe
  in
  generated


decoderAlias :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
decoderAlias isTest ifaces pkg modul decls aliasName alias@(Alias tvars tipe) =
  let
    !x = runTests isTest "decoderAlias" pkg modul decls generatedName generated alias (aliasAsModule cname aliasName alias)

    generatedName = Data.Name.fromChars $ "w3_decode_" ++ Data.Name.toChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar )

    generated = Def (a (generatedName)) ptvars $
      -- addLetLog (Utf8.fromChars $ "w3_decode_" ++ Data.Name.toChars aliasName) $
      debugDecoder (Data.Name.toElmString aliasName) $
      decoderForType ifaces cname tipe
  in
  generated
