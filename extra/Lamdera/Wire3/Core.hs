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
        & filter (\(_, Alias _ tipe) -> not (isLambdaType tipe))
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

    cname = Module.Canonical pkg (Src.getName modul)

    {- When a module defines any w3_validate_* functions, the generated wire
    functions must be placed AFTER user definitions so that the generated
    decoders can reference those user validators (a VarTopLevel reference only
    resolves to definitions appearing earlier in the topologically-sorted
    Decls). Otherwise we keep the original behaviour of prepending the generated
    functions, which lets user code reference them. -}
    moduleHasValidators =
      not (null (allValidatorDefs decls_))

    sortedGenerated =
      Lamdera.Wire3.Graph.stronglyConnCompDefs newDefs

    extendedDecls =
      if moduleHasValidators
        then
          spliceDeclsAtEnd existingDecls
            (Lamdera.Wire3.Graph.addGraphDefsToDecls SaveTheEnvironment sortedGenerated)
        else
          Lamdera.Wire3.Graph.addGraphDefsToDecls existingDecls sortedGenerated

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

    {- When generated wire functions are appended after user code (the
    moduleHasValidators path), the user's own top-level code must not also
    reference a generated w3_encode_*/w3_decode_* function in this module: the
    reference would resolve to a definition that now appears later, which the
    type checker can't see (it would otherwise crash with an internal Map.!).
    Detect that and report a clear error. When there are no validators we keep
    the original prepend behaviour, where such references are fine. -}
    wireRefCheck =
      if moduleHasValidators
        then checkNoUserWireRefs newDefs existingDecls
        else Right ()
  in
  case checkValidators cname (Can._unions canonical) (Can._aliases canonical) decls_ of
    Left err ->
      Left err

    Right () ->
      case wireRefCheck of
        Left err ->
          Left err

        Right () ->
          Right $ canonical
            { _decls = extendedDecls
            , _exports = extendedExports
            }


{-

WIRE VALIDATION CHECKS

For every top-level `w3_validate_<TypeName>` function defined in the module,
verify that <TypeName> is a custom type (not a type alias, and actually defined
here) and that the function's signature is exactly:

    w3_validate_<TypeName> : <TypeName> <tvars> -> Result String ()

where <tvars> are the type's declared type variables, in order. Any violation
produces a compile error.

-}
checkValidators :: Module.Canonical -> Map.Map Data.Name.Name Union -> Map.Map Data.Name.Name Alias -> Decls -> Either D.Doc ()
checkValidators cname unions aliases decls =
  allValidatorDefs decls
    & mapM_ (\def ->
        let typeName = validatorTypeName (defName def)
        in
        case Map.lookup typeName unions of
          Just union ->
            checkValidatorSignature cname typeName union def
          Nothing ->
            case Map.lookup typeName aliases of
              Just _  -> Left (validatorAliasError typeName)
              Nothing -> Left (validatorMissingTypeError typeName)
      )


checkValidatorSignature :: Module.Canonical -> Data.Name.Name -> Union -> Def -> Either D.Doc ()
checkValidatorSignature cname typeName union def =
  case def of
    Def _ _ _ ->
      Left (validatorMissingAnnotationError typeName union)

    TypedDef _ _ args _ resultType ->
      let
        actualType = foldr (\(_, t) acc -> TLambda t acc) resultType args
        expectedType = expectedValidatorType cname typeName union
      in
      if actualType == expectedType
        then Right ()
        else Left (validatorBadSignatureError typeName union)


expectedValidatorType :: Module.Canonical -> Data.Name.Name -> Union -> Type
expectedValidatorType cname typeName union =
  TLambda
    (TType cname typeName (fmap TVar (_u_vars union)))
    (TType (Module.Canonical (Name "elm" "core") "Result") "Result"
        [ TType (Module.Canonical (Name "elm" "core") "String") "String" []
        , TUnit
        ])


validatorSignatureString :: Data.Name.Name -> Union -> String
validatorSignatureString typeName union =
  let
    typeWithVars =
      case _u_vars union of
        [] -> Data.Name.toChars typeName
        vars -> Data.Name.toChars typeName ++ " " ++ unwords (fmap Data.Name.toChars vars)
  in
  "w3_validate_" ++ Data.Name.toChars typeName ++ " : " ++ typeWithVars ++ " -> Result String ()"


-- NOTE: The leading D.fromChars line in each error is a contiguous (un-wrapped)
-- marker so tests can reliably match on it; D.reflow may insert line breaks.

validatorMissingTypeError :: Data.Name.Name -> D.Doc
validatorMissingTypeError typeName =
  D.stack
    [ D.fromChars $ "w3_validate_" ++ Data.Name.toChars typeName ++ ": no matching custom type"
    , D.reflow $
        "I found a wire validation function `w3_validate_" ++ Data.Name.toChars typeName
        ++ "`, but there is no custom type named `" ++ Data.Name.toChars typeName
        ++ "` defined in this module."
    , D.reflow $
        "Wire validation functions must be defined in the same module as the custom type they validate. "
        ++ "Define `type " ++ Data.Name.toChars typeName ++ " = ...` here, or remove the validation function."
    ]


validatorAliasError :: Data.Name.Name -> D.Doc
validatorAliasError typeName =
  D.stack
    [ D.fromChars $ "w3_validate_" ++ Data.Name.toChars typeName ++ ": expected a custom type, found a type alias"
    , D.reflow $
        "I found a wire validation function `w3_validate_" ++ Data.Name.toChars typeName
        ++ "`, but `" ++ Data.Name.toChars typeName ++ "` is a type alias, not a custom type."
    , D.reflow
        "Wire validation functions (w3_validate_*) are only supported for custom types (defined with `type`), not for type aliases (`type alias`)."
    ]


validatorMissingAnnotationError :: Data.Name.Name -> Union -> D.Doc
validatorMissingAnnotationError typeName union =
  D.stack
    [ D.fromChars $ "w3_validate_" ++ Data.Name.toChars typeName ++ ": missing type annotation"
    , D.reflow $
        "The wire validation function `w3_validate_" ++ Data.Name.toChars typeName
        ++ "` must have a type annotation. It needs to be annotated exactly like this:"
    , D.fromChars $ "    " ++ validatorSignatureString typeName union
    ]


validatorBadSignatureError :: Data.Name.Name -> Union -> D.Doc
validatorBadSignatureError typeName union =
  D.stack
    [ D.fromChars $ "w3_validate_" ++ Data.Name.toChars typeName ++ ": wrong type signature"
    , D.reflow $
        "The wire validation function `w3_validate_" ++ Data.Name.toChars typeName
        ++ "` has the wrong type signature. It must be exactly:"
    , D.fromChars $ "    " ++ validatorSignatureString typeName union
    ]


{-

When a module defines any w3_validate_* function, the generated wire functions
are emitted *after* the user's definitions (so the generated decoders can call
the validators). That ordering means the user's own top-level code cannot also
reference a generated w3_encode_*/w3_decode_* function in the same module -- such
a reference would point at a definition that now appears later in the Decls,
which the type-inference solver can't resolve (it would crash with an internal
Map.! lookup error). We catch that here and report it as a normal compile error.

`generatedDefs` are the freshly generated wire defs (their names are what we
forbid referencing); `userDecls` is the user's code with the wire stubs removed.

-}
checkNoUserWireRefs :: [Def] -> Decls -> Either D.Doc ()
checkNoUserWireRefs generatedDefs userDecls =
  let
    generatedNames = fmap defName generatedDefs

    conflicts =
      [ (defName userDef, ref)
      | userDef <- declsToList userDecls
      , ref     <- defTopLevelRefs userDef
      , ref `elem` generatedNames
      ]
  in
  case conflicts of
    [] ->
      Right ()

    ((userName, genName) : _) ->
      Left (validatorWireRefError userName genName)


validatorWireRefError :: Data.Name.Name -> Data.Name.Name -> D.Doc
validatorWireRefError userName genName =
  D.stack
    [ D.fromChars $ Data.Name.toChars userName ++ ": cannot reference generated wire functions in a module that defines a validator"
    , D.reflow $
        "`" ++ Data.Name.toChars userName ++ "` references the generated wire function `"
        ++ Data.Name.toChars genName ++ "`, but this module also defines one or more `w3_validate_*` functions."
    , D.reflow
        "When a module defines a wire validator, the generated wire functions (w3_encode_*/w3_decode_*) must be placed after your own definitions so the generated decoders can call your validators. As a result, your top-level code in this module can't also reference those generated functions."
    , D.reflow $
        "To fix this, move `" ++ Data.Name.toChars userName ++ "` (or just its reference to `"
        ++ Data.Name.toChars genName ++ "`) into a different module, or remove the validator(s) from this module."
    ]


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

    baseBody =
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

    {- If the current module defines `w3_validate_<unionName>`, the decoder calls
    it after producing a value. The function's existence and signature are
    verified by checkValidators in addWireGenerations_ before this runs. -}
    finalBody =
      case findValidatorDef decls unionName of
        Just _  -> wrapWithValidator ifaces cname unionName baseBody
        Nothing -> baseBody

    generated =
      Def
      -- TypedDef
        (a (generatedName))
        -- Map.empty
        ptvars
        finalBody
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
