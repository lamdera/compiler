{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Lamdera.Wire3.Core where

{-

All the code that does the Wire encoder/decoder code gen and injection

-}

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import qualified Data.Map as Map
import qualified Data.List as List

import Elm.Package
import qualified AST.Source as Src
import qualified Elm.Interface as I
import qualified Elm.ModuleName as Module
import qualified Elm.Package as Pkg
import qualified AST.Canonical as Can
import AST.Canonical
import qualified AST.Prim.Module as PrimModule
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as TN
import qualified AST.Prim.TypeVar as TV
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
import qualified Lamdera.Lexical as Lexical

import qualified Ext.ElmFormat

{-| Wire test failures recorded for the test harness to assert on.

runTests runs INSIDE the compiler, during a compile the harness drives in-process,
so it cannot fail a test directly - it printed a diff and returned (). EasyTest only
fails on exceptions, so every mismatch was reported to stdout and then ignored: a
green suite meant "nothing threw", not "the codecs match". Both the 233d29d6 era
region-comparison breakage and a missing expectation could sit there indefinitely.

Recording them here lets Test.Wire.wire assert on the result. Shared via a global
IORef because the compile is in-process with the harness, which is also why
runTests can use unsafePerformIO at all.
-}
{-# NOINLINE wireTestFailuresRef #-}
wireTestFailuresRef :: IORef [String]
wireTestFailuresRef = unsafePerformIO (newIORef [])


recordWireTestFailure :: String -> IO ()
recordWireTestFailure failure =
  modifyIORef' wireTestFailuresRef (++ [failure])


resetWireTestFailures :: IO ()
resetWireTestFailures =
  writeIORef wireTestFailuresRef []


readWireTestFailures :: IO [String]
readWireTestFailures =
  readIORef wireTestFailuresRef


runTests :: Bool -> String -> Pkg.Name -> Src.Module -> Decls -> N.Name -> Def -> canonicalValue -> wiregen -> ()
runTests isTest_ debugName pkg modul decls generatedName generated canonicalValue wiregen =
  if isTest_
    then
      unsafePerformIO $ do
      let
        testName = N.fromChars $ "expected_" ++ N.toChars generatedName

        withName def n_ =
          case def of
            Def (A.At r n) p e -> Def (A.At r n_) p e
            TypedDef (A.At r n) freeVars pts e t -> TypedDef (A.At r n_) freeVars pts e t

        -- fullTypeRef =
        --   (T.pack $ Pkg.toChars pkg) <> ":" <> (T.pack $ Data.Name.toChars $ Src.getName modul) <> "." <> (T.pack $ Data.Name.toChars generatedName)

        -- Source regions are not part of what this test is checking: the expectation is
        -- parsed from real source and carries real regions, while everything we generate is
        -- built at A.zero. So a structural (==) reports every single definition as a
        -- mismatch, and the printed diff then shows nothing but the name - which is what
        -- makes it look like a puzzle rather than a bug.
        --
        -- StandaloneInstances used to override Eq for Located with a region-ignoring one
        -- exactly for this. It was commented out on the grounds that Reporting.Annotation
        -- defines it, and Annotation does now derive Eq - but the derived one INCLUDES the
        -- region, so the override was not redundant. Show for Located deliberately prints
        -- no region, so comparing the shown forms is precisely the comparison wanted here,
        -- without changing an Eq the compiler proper relies on.
        matchesExpectation def expectedDef =
          show def == show expectedDef

      case decls & findDef testName of
        Just testDefinition -> do

          -- debugHaskellPass ("💚 testDefinition " <> show_ (Src.getName modul)) (testDefinition) (pure ())
          -- debugHaskellPass ("🧡 generated " <> show_ (Src.getName modul)) (generated) (pure ())
          -- diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
          -- atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff

          if matchesExpectation generated (testDefinition `withName` generatedName)
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
              recordWireTestFailure $
                PrimModule.toChars (Src.getName modul) ++ "." ++ N.toChars generatedName
                  ++ " (" ++ debugName ++ ") does not match " ++ N.toChars testName
              -- error "exiting!"
              -- atomicPutStrLn $ "❌❌❌ " ++ Data.Name.toChars (Src.getName modul) ++ "." ++ Data.Name.toChars generatedName ++ " gen does not match test definition."
              pure ()

        Nothing -> do
          -- No expectation written for this codec. That is the normal case, not an error:
          -- a fixture asserts on the definitions it chooses to cover, and every other codec
          -- the module generates lands here. (Treating this as a failure reports 52 of them
          -- across the current suite.)
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

addWireGenerations :: Can.Module -> Pkg.Name -> Map.Map PrimModule.Name I.Interface -> Src.Module -> Either E.Error Can.Module
addWireGenerations canonical pkg ifaces modul =
  if Lamdera.Wire3.Helpers.shouldHaveCodecsGenerated pkg then
    case addWireGenerations_ canonical pkg ifaces modul of
      Right canonical_ -> do
        Right canonical_

      Left err ->
        Left $ E.BadLamdera "WIRE ERROR" err
  else
    Right canonical


addWireGenerations_ :: Can.Module -> Pkg.Name -> Map.Map PrimModule.Name I.Interface -> Src.Module -> Either D.Doc Can.Module
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

    -- Lamdera's (pre Elm 0.19.3) name order, so the generated definitions come out in the
    -- same order as before (see Lamdera.Lexical).
    unionDefs =
      (Can._unions canonical)
        & Lexical.toList
        & concatMap (\(name, union) ->
            let rowCodecs = unionRowCodecs name union & either (const Nothing) id
            in
            [ (encoderUnion rowCodecs moduleRowConstraints isTest_ ifaces pkg modul decls_ name union)
            , (decoderUnion rowCodecs moduleRowConstraints DecodeValidating isTest_ ifaces pkg modul decls_ name union)
            , (decoderUnion rowCodecs moduleRowConstraints DecodeUnsafe     isTest_ ifaces pkg modul decls_ name union)
            ]
            & (\defs ->
                if recursesIrregularly cname (Can._unions canonical) name union
                  then withPolymorphicRecursion cname decoderComparableParams name union defs
                  else defs
              )
        )

    -- Which decoder type parameters have to be comparable, for the decoder signatures.
    -- Only needed by the decoders withPolymorphicRecursion annotates.
    decoderComparableParams =
      comparableParams ifaces cname (Can._unions canonical) (Can._aliases canonical)

    -- The record constraints the codecs of this module's types put on their parameters,
    -- and those of the types they reference (see RowConstraints)
    moduleRowConstraints =
      rowConstraints ifaces cname (Can._unions canonical) (Can._aliases canonical)

    -- The codecs of the types where a parameter is the row tail of more than one record
    -- (see RowSets), and the compile errors for those that can't have them
    moduleRowSets =
      rowSets ifaces cname (Can._unions canonical) (Can._aliases canonical)

    unionRowCodecs name union =
      rowCodecsFor moduleRowSets cname name (_u_vars union) (_u_alts union & concatMap (\(Ctor _ _ _ types) -> types))

    aliasRowCodecs name (Alias tvars tipe) =
      rowCodecsFor moduleRowSets cname name tvars [tipe]

    rowCodecsCheck =
      ((Can._unions canonical)
        & Lexical.toList
        & fmap (\(name, union) ->
            case unionRowCodecs name union of
              Left problem -> Left (rowCodecsError name problem)
              Right (Just _) | recursesIrregularly cname (Can._unions canonical) name union ->
                -- withPolymorphicRecursion annotates the decoder with a parameter per type
                -- parameter
                Left $ rowCodecsError name $
                  "a type variable is the row tail of more than one record, and the type also refers to itself at other type arguments than its own."
              Right _ -> Right ()
          )
      )
      ++
      ((Can._aliases canonical)
        & Lexical.toList
        & filter (\(_, Alias _ tipe) -> not (isLambdaType tipe))
        & fmap (\(name, alias) ->
            case aliasRowCodecs name alias of
              Left problem -> Left (rowCodecsError name problem)
              Right _ -> Right ()
          )
      )
      & sequence_

    aliasDefs =
      (Can._aliases canonical)
        & Lexical.toList
        & filter (\(_, Alias _ tipe) -> not (isLambdaType tipe))
        & concatMap (\(name, alias) ->
            let rowCodecs = aliasRowCodecs name alias & either (const Nothing) id
            in
            [ (encoderAlias rowCodecs moduleRowConstraints isTest_ ifaces pkg modul decls_ name alias)
            , (decoderAlias rowCodecs moduleRowConstraints DecodeValidating isTest_ ifaces pkg modul decls_ name alias)
            , (decoderAlias rowCodecs moduleRowConstraints DecodeUnsafe     isTest_ ifaces pkg modul decls_ name alias)
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
    -- oldDeclsImpl =
    --   declsToList decls_
    --     & List.unionBy (\a_ b -> defName a_ == defName b) (unionDefs ++ aliasDefs)
    --     & Lamdera.Wire3.Graph.stronglyConnCompDefs
    --     & Lamdera.Wire3.Graph.addGraphDefsToDecls SaveTheEnvironment

    {- For any modules that don't ExportEverything, we add our newDefs to exports -}
    extendedExports =
      case Can._exports canonical of
        ExportEverything region ->
          ExportEverything region

        Export types values binops ->
          newDefs
            & foldl (\exports_ def ->
                addExport def exports_
              )
              values
            & (\newValues -> Export types newValues binops)

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
          case rowCodecsCheck of
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
checkValidators :: Module.Canonical -> Map.Map TN.Name Union -> Map.Map TN.Name Alias -> Decls -> Either D.Doc ()
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


checkValidatorSignature :: Module.Canonical -> TN.Name -> Union -> Def -> Either D.Doc ()
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


expectedValidatorType :: Module.Canonical -> TN.Name -> Union -> Type
expectedValidatorType cname typeName union =
  TLambda
    (TType cname typeName (fmap TVar (_u_vars union)))
    (TType (Module.Canonical (Name "elm" "core") "Result") "Result"
        [ TType (Module.Canonical (Name "elm" "core") "String") "String" []
        , TUnit
        ])


validatorSignatureString :: TN.Name -> Union -> String
validatorSignatureString typeName union =
  let
    typeWithVars =
      case _u_vars union of
        [] -> TN.nameToChars typeName
        vars -> TN.nameToChars typeName ++ " " ++ unwords (fmap TV.varToChars vars)
  in
  "w3_validate_" ++ TN.nameToChars typeName ++ " : " ++ typeWithVars ++ " -> Result String ()"


-- NOTE: The leading D.fromChars line in each error is a contiguous (un-wrapped)
-- marker so tests can reliably match on it; D.reflow may insert line breaks.

validatorMissingTypeError :: TN.Name -> D.Doc
validatorMissingTypeError typeName =
  D.stack
    [ D.fromChars $ "w3_validate_" ++ TN.nameToChars typeName ++ ": no matching custom type"
    , D.reflow $
        "I found a wire validation function `w3_validate_" ++ TN.nameToChars typeName
        ++ "`, but there is no custom type named `" ++ TN.nameToChars typeName
        ++ "` defined in this module."
    , D.reflow $
        "Wire validation functions must be defined in the same module as the custom type they validate. "
        ++ "Define `type " ++ TN.nameToChars typeName ++ " = ...` here, or remove the validation function."
    ]


validatorAliasError :: TN.Name -> D.Doc
validatorAliasError typeName =
  D.stack
    [ D.fromChars $ "w3_validate_" ++ TN.nameToChars typeName ++ ": expected a custom type, found a type alias"
    , D.reflow $
        "I found a wire validation function `w3_validate_" ++ TN.nameToChars typeName
        ++ "`, but `" ++ TN.nameToChars typeName ++ "` is a type alias, not a custom type."
    , D.reflow
        "Wire validation functions (w3_validate_*) are only supported for custom types (defined with `type`), not for type aliases (`type alias`)."
    ]


validatorMissingAnnotationError :: TN.Name -> Union -> D.Doc
validatorMissingAnnotationError typeName union =
  D.stack
    [ D.fromChars $ "w3_validate_" ++ TN.nameToChars typeName ++ ": missing type annotation"
    , D.reflow $
        "The wire validation function `w3_validate_" ++ TN.nameToChars typeName
        ++ "` must have a type annotation. It needs to be annotated exactly like this:"
    , D.fromChars $ "    " ++ validatorSignatureString typeName union
    ]


validatorBadSignatureError :: TN.Name -> Union -> D.Doc
validatorBadSignatureError typeName union =
  D.stack
    [ D.fromChars $ "w3_validate_" ++ TN.nameToChars typeName ++ ": wrong type signature"
    , D.reflow $
        "The wire validation function `w3_validate_" ++ TN.nameToChars typeName
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


validatorWireRefError :: N.Name -> N.Name -> D.Doc
validatorWireRefError userName genName =
  D.stack
    [ D.fromChars $ N.toChars userName ++ ": cannot reference generated wire functions in a module that defines a validator"
    , D.reflow $
        "`" ++ N.toChars userName ++ "` references the generated wire function `"
        ++ N.toChars genName ++ "`, but this module also defines one or more `w3_validate_*` functions."
    , D.reflow
        "When a module defines a wire validator, the generated wire functions (w3_encode_*/w3_decode_*) must be placed after your own definitions so the generated decoders can call your validators. As a result, your top-level code in this module can't also reference those generated functions."
    , D.reflow $
        "To fix this, move `" ++ N.toChars userName ++ "` (or just its reference to `"
        ++ N.toChars genName ++ "`) into a different module, or remove the validator(s) from this module."
    ]


addExport :: Def -> Map.Map N.Name A.Region -> Map.Map N.Name A.Region
addExport def exports =
  case def of
    Def (A.At region name_) pvars expr ->
      Map.insert name_ A.zero exports
    TypedDef (A.At region name_) freeVars pvars expr tipe ->
      Map.insert name_ A.zero exports


encoderUnion :: Maybe RowCodecs -> RowConstraints -> Bool -> Map.Map PrimModule.Name I.Interface -> Pkg.Name -> Src.Module -> Decls -> TN.Name -> Union -> Def
encoderUnion rowCodecs rowConstraints isTest_ ifaces pkg modul decls unionName union =
  let
    !_ = runTests isTest_ "encoderUnion" pkg modul decls generatedName finalGen union (unionAsModule cname unionName union)

    generatedName = N.fromChars $ "w3_encode_" ++ TN.nameToChars unionName
    cname = Module.Canonical pkg (Src.getName modul)
    tvars = _u_vars union
    -- ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar )

    -- A parameter can be constrained to a record by the constructors' arguments, as in
    -- encoderAlias (see RowConstraints)
    ptvarsTyped =
      case rowCodecs of
        -- A codec for each record a parameter is the row tail of (see RowSets)
        Just rc ->
          _rc_params rc & fmap (\(tvar, codecVar, fields) ->
            (pvar $ N.fromChars $ "w3_x_c_" ++ TV.varToChars codecVar, TLambda (constrainTvarBy tvar fields) tLamdera_Wire_Encoder_Holey)
          )
        Nothing ->
          zip tvars (fmap snd (rowConstraints cname unionName) ++ repeat Map.empty) & fmap (\(tvar, constraints) ->
            (pvar $ N.fromChars $ "w3_x_c_" ++ TV.varToChars tvar, TLambda (constrainTvarBy tvar constraints) tLamdera_Wire_Encoder_Holey)
          )

    forCodecs = maybe (extendRowArgs rowConstraints) _rc_select rowCodecs

    numCtors = length $ _u_alts union

    variantIntEncoder n
      | numCtors <= 255        = encodeUnsignedInt8 (int n)
      | numCtors <= 65535      = encodeUnsignedInt16 (int n)
      -- Truly ridiculous but... maybe one day? 🪐
      -- | numCtors <= 4294967295 = encodeUnsignedInt32 (int n)
      | otherwise              = error $ "Unhandled custom type variant size (" ++ show n ++ "), please report this issue for the custom type " ++ TN.nameToChars unionName

    generatedBody =
        -- debugEncoder_ (Data.Name.toElmString unionName)
        (caseof (lvar "w3v") $
            _u_alts union
              -- Wire tags: constructor names in Lamdera\'s (pre Elm 0.19.3) byte order, see Lamdera.Lexical
              & Lexical.sortOn (\(Ctor name index_ numParams paramTypes) -> name)
              & imap (\i (Ctor tagName tagIndex numParams paramTypes) ->
                  let
                    params =
                      paramTypes & imap (\pi_ paramType ->
                        PatternCtorArg
                          { _index = Index.ZeroBased pi_
                          , _type = paramType
                          , _arg = pvar (N.fromChars $ "v" ++ show pi_)
                          }
                      )

                    paramEncoders =
                      paramTypes & imap (\pi_ paramType ->
                          encodeTypeValue 0 ifaces cname (forCodecs paramType) (lvar (N.fromChars $ "v" ++ show pi_))
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
    -- generated = Def (a (generatedName)) (ptvars ++ [ pvar "w3v" ]) generatedBody

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


decoderUnion :: Maybe RowCodecs -> RowConstraints -> DecodeMode -> Bool -> Map.Map PrimModule.Name I.Interface -> Pkg.Name -> Src.Module -> Decls -> TN.Name -> Union -> Def
decoderUnion rowCodecs rowConstraints mode isTest_ ifaces pkg modul decls unionName union =
  let
    !_ = runTests isTest_ "decoderUnion" pkg modul decls generatedName generated union (unionAsModule cname unionName union)

    generatedName = N.fromChars $ decodePrefix mode ++ TN.nameToChars unionName
    cname = Module.Canonical pkg (Src.getName modul)
    tvars = _u_vars union
    tvarsTypesig = tvars & foldl (\acc name -> Map.insert name () acc ) Map.empty
    ptvars =
      case rowCodecs of
        -- A codec for each record a parameter is the row tail of (see RowSets)
        Just rc -> _rc_params rc & fmap (\(_, codecVar, _) -> pvar $ N.fromChars $ "w3_x_c_" ++ TV.varToChars codecVar)
        Nothing -> tvars & fmap (\tvar -> pvar $ N.fromChars $ "w3_x_c_" ++ TV.varToChars tvar )
    unionType = TType cname unionName (fmap TVar tvars)

    -- Only the types decoded change, the constructors keep their own
    forCodecs = maybe (extendRowArgs rowConstraints) _rc_select rowCodecs

    vctor :: N.Name -> Index.ZeroBased -> [Type] -> Expr
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
      | otherwise = error $ "Unhandled custom type variant size (" ++ show numCtors ++ "), please report this issue for the custom type " ++ TN.nameToChars unionName

    baseBody =
        -- debugDecoder (Data.Name.toElmString unionName)
        (variantIntDecoder |> andThenDecode1
              (lambda1 (pvar "w3v") $
                caseof (lvar "w3v") $
                  _u_alts union
                    -- Wire tags: constructor names in Lamdera\'s (pre Elm 0.19.3) byte order, see Lamdera.Lexical
                    & Lexical.sortOn (\(Ctor name index_ numParams paramTypes) -> name)
                    & imap (\i (Ctor tagName tagIndex numParams paramTypes) ->
                        CaseBranch (pint i) $
                          ([(succeedDecode (vctor tagName tagIndex paramTypes))]
                          ++ fmap (\paramType -> andMapDecode1 ((decoderForType mode ifaces cname (forCodecs paramType)))) paramTypes)
                            & foldlPairs (|>)
                    )
                    & (\l -> l ++ [CaseBranch pAny_ $ failDecode (N.toChars generatedName <> " unexpected union tag index")])
              )
            )

    {- Only the validating chain (w3_decode_*) attaches the validator. If the
    current module defines `w3_validate_<unionName>`, the validating decoder
    calls it after producing a value (its existence and signature are verified by
    checkValidators in addWireGenerations_ before this runs). The unsafe chain
    (w3_unsafe_decode_*) never validates. -}
    finalBody =
      case mode of
        DecodeValidating ->
          case findValidatorDef decls unionName of
            Just _  -> wrapWithValidator ifaces cname unionName baseBody
            Nothing -> baseBody
        DecodeUnsafe ->
          baseBody

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
--
-- A custom type's codecs can constrain their parameters too (see RowConstraints), so
-- those are looked up in `rowConstraints`.
getRecordConstraints :: RowConstraints -> TV.Var -> Type -> Map.Map N.Name Type
getRecordConstraints rowConstraints tvarName tipe =
  -- @TODO cover all the possible constraint types!
  -- debugHaskellPass ("🔵 getRecordConstraints ") (tvarName, tipe) $
  case tipe of
    TType cname aliasName tvars ->
      let
        -- `ProcessCheckpointResult race player`'s codecs take the codec of a
        -- `{ player | moves : List Int, status : Int }` for `player`, so where we are the
        -- row tail of the argument applied there, those fields are constraints on us as
        -- well, in terms of the arguments applied here (as for an alias's Holey body below).
        params = rowConstraints cname aliasName
        constraintsOfType =
          zip params tvars
            & List.filter (\(_, argType) -> extendsTvar tvarName argType)
            & fmap (\((_, fields), _) ->
                fields & fmap (substituteTvars (Map.fromList (zip (fmap fst params) tvars)))
              )
            & Map.unions
      in
      Map.union constraintsOfType (tvars & fmap (getRecordConstraints rowConstraints tvarName) & Map.unions)

    TRecord fields ext ->
      case ext of
        Nothing ->
          fields & fmap (\(FieldType _ t) -> getRecordConstraints rowConstraints tvarName t) & Map.unions

        Just extName ->
          if extName == tvarName
            then fields & fmap (\(FieldType _ t) -> t)
            else Map.empty

    TPair a_ b    -> [a_, b]    & fmap (getRecordConstraints rowConstraints tvarName) & Map.unions
    TTriple a_ b c -> [a_, b, c] & fmap (getRecordConstraints rowConstraints tvarName) & Map.unions

    TAlias moduleName typeName tvars aType ->
      let
        -- Our tvar can also be buried INSIDE an argument rather than being one, as in
        -- `BrowserStream (WithWindowSize a)`, where the constraint on `a` lives in the
        -- argument rather than in BrowserStream's own body.
        constraintsInArgs =
          tvars & fmap (\(_, argType) -> getRecordConstraints rowConstraints tvarName argType) & Map.unions

        constraintsInBody =
          case aType of
            Holey t ->
              -- A Holey body is written in the alias's OWN parameter names, so the only
              -- constraints in it that are ours are those on a parameter we are the row
              -- tail of - `BrowserStream (WithWindowSize a)` extends OUR `a` with
              -- BrowserStream's fields, because `a` is what WithWindowSize extends.
              -- Reading the body under our own name instead (as this used to) only lines up
              -- when the alias happens to have named its parameter the same thing we did.
              tvars
                & List.filter (\(paramName, argType) -> extendsTvar tvarName argType)
                & fmap (\(paramName, argType) ->
                    -- ...and the constraint comes out in the alias's names, so it has to be
                    -- re-expressed in terms of the arguments actually applied here.
                    -- Otherwise it mentions a tvar that is not free in the signature we are
                    -- extracting it for, and type inference dies on a Map.! in
                    -- Type.Instantiate, reaching users as "thread blocked indefinitely in an
                    -- MVar operation".
                    getRecordConstraints rowConstraints paramName t
                      & fmap (substituteTvars (Map.fromList tvars))
                  )
                & Map.unions
              -- extractors & fmap (\extractor -> extractor t) & Map.unions

            Filled t ->
              -- A Filled body has already had its arguments substituted into it, so the
              -- names in it are ours as they stand.
              getRecordConstraints rowConstraints tvarName t
              -- extractors & fmap (\extractor -> extractor t) & Map.unions
      in
      Map.union constraintsInBody constraintsInArgs

    _ ->
      -- no constraint
      Map.empty


{-| Is `tvarName` the row tail of this type - i.e. would extending this type with extra
fields extend `tvarName` with them?

This is the question getRecordConstraints has to answer about each argument applied to an
alias, because that is exactly when the alias's own constraints on that parameter are also
constraints on `tvarName`. In `BrowserStream (WithWindowSize a)`, `a` is the row tail of
the argument, so BrowserStream's fields land on `a` too.

-}
extendsTvar :: TV.Var -> Type -> Bool
extendsTvar tvarName t =
  case t of
    TVar a -> a == tvarName
    TRecord _ (Just extName) -> extName == tvarName
    TAlias _ _ tvars (Holey body) ->
      -- The body extends one of the alias's parameters; we are the row tail of the whole
      -- thing when that parameter's argument has us as ITS row tail.
      tvars & any (\(paramName, argType) -> extendsTvar tvarName argType && extendsTvar paramName body)
    TAlias _ _ _ (Filled body) -> extendsTvar tvarName body
    _ -> False


{-| Plain simultaneous substitution of alias parameters by the types applied to them.

Deliberately not resolveTvar: that one declines to substitute when the replacement is
itself still generic, which is correct for the foreign-signature use it was written for,
but wrong here. A constraint being lifted out of an alias body has to be re-expressed in
the referencing type's terms whatever those terms are, or it mentions a name that does not
exist there.

A Holey body is left alone for the same reason extractFreeTvarsInType leaves it alone: the
names in it belong to that alias, so they shadow ours. Only its applied arguments can
mention names from here.

-}
substituteTvars :: Map.Map TV.Var Type -> Type -> Type
substituteTvars subs t =
  case t of
    TVar a ->
      case Map.lookup a subs of
        Just replacement -> replacement
        Nothing -> t

    TLambda t1 t2 -> TLambda (substituteTvars subs t1) (substituteTvars subs t2)

    TType moduleName typeName params ->
      TType moduleName typeName (fmap (substituteTvars subs) params)

    TRecord fieldMap maybeExtensible ->
      let
        -- An extension name can only ever be another tvar name, so a substitution that
        -- isn't a plain rename has nowhere to go here; leave it as it was.
        newMaybeExtensible =
          case maybeExtensible of
            Just extName ->
              case Map.lookup extName subs of
                Just (TVar newName) -> Just newName
                _ -> maybeExtensible
            Nothing -> maybeExtensible
      in
      fieldMap
        & fmap (\(FieldType index tipe) -> FieldType index (substituteTvars subs tipe))
        & (\newFieldMap -> TRecord newFieldMap newMaybeExtensible)

    TUnit -> t

    TPair a_ b -> TPair (substituteTvars subs a_) (substituteTvars subs b)
    TTriple a_ b c -> TTriple (substituteTvars subs a_) (substituteTvars subs b) (substituteTvars subs c)

    TAlias moduleName typeName tvars (Holey tipe) ->
      TAlias moduleName typeName (tvars & fmap (\(n, v) -> (n, substituteTvars subs v))) (Holey tipe)

    TAlias moduleName typeName tvars (Filled tipe) ->
      TAlias moduleName typeName (tvars & fmap (\(n, v) -> (n, substituteTvars subs v))) (Filled $ substituteTvars subs tipe)


renameTvars :: TV.Var -> TV.Var -> Type -> Type
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
    TPair a b -> TPair (renameTvars oldName newName a) (renameTvars oldName newName b)
    TTriple a b c -> TTriple (renameTvars oldName newName a) (renameTvars oldName newName b) (renameTvars oldName newName c)

    TAlias moduleName typeName tvars (Holey tipe) ->
      TAlias moduleName typeName (fmap (\(n,v) -> (n, renameTvars oldName newName t)) tvars) (Holey $ renameTvars oldName newName tipe)
    TAlias moduleName typeName tvars (Filled tipe) ->
      TAlias moduleName typeName (fmap (\(n,v) -> (n, renameTvars oldName newName t)) tvars) (Filled $ renameTvars oldName newName tipe)


-- Flatten any TVar renames down through the type tree so that when we extract a constraint,
-- all the TVars are consistent in the top level type signature we extract them for.
normaliseTvarNames :: Map.Map TV.Var TV.Var -> Type -> Type
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
    TPair a b -> TPair (normaliseTvarNames renames a) (normaliseTvarNames renames b)
    TTriple a b c -> TTriple (normaliseTvarNames renames a) (normaliseTvarNames renames b) (normaliseTvarNames renames c)

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
constrainTvar :: RowConstraints -> TV.Var -> Type -> Type
constrainTvar rowConstraints tvarName tipe =
  -- debugHaskellPassWhen (tvarName == "compatibleA") ("🧡 constrainTvar") (tvarName, tipe) $
  constrainTvarBy tvarName (getRecordConstraints rowConstraints tvarName tipe)


{-| constrainTvar, for constraints already gathered -}
constrainTvarBy :: TV.Var -> Map.Map N.Name Type -> Type
constrainTvarBy tvarName constraints =
  if Map.size constraints > 0 then
    constraints
      -- Field indexes in Lamdera's (pre Elm 0.19.3) name order, see Lamdera.Lexical
      & Lexical.toList
      & imap (\i (name, t) -> (name, FieldType (fromIntegral i) t))
      & Map.fromList
      & (\fields -> TRecord fields (Just tvarName))
  else
    TVar tvarName


{-| The record constraints the codecs of a custom type or alias put on each of its
parameters, by type: the parameter, and the fields its `w3_x_c_<tvar>` codec's record has
to have, written in the type's own parameters. Empty for a parameter that is not
constrained.

    type alias Player a = { a | status : Int, moves : List Int }
    type alias ProcessLine race player = { race : race, player : Player player, step : Int }
    type ProcessCheckpointResult race player
        = PlayerActive (ProcessLine race player)
        | PlayerFinished (ProcessLine race player)

The codec of a type variable that is a row tail is the codec of the whole record it ends
(see Wire_Record_Extensible1_Basic), so ProcessLine's codecs take the codec of a
`{ player | moves : List Int, status : Int }` for `player`, and encoderAlias says so in its
signature. A custom type's codecs pass theirs on, so they need it too: its encoder was
annotated with a plain `player -> Encoder`, a TYPE MISMATCH, and the codecs of any
`ProcessCheckpointResult String { name : String }` were called with the codec of just
`{ name : String }`, where they need the whole `{ moves, name, status }` (see
extendRowArgs). An app hits both. Such a custom type never compiled, and every other
custom type has no constraints, so nothing that compiled changes.

An alias's are the ones encoderAlias already puts in its signature; they are here for
extendRowArgs. The types of this module can constrain each other's parameters, so they are
solved together, to a fixpoint. Foreign ones are read off their encoder's signature in the
interface.
-}
type RowConstraints = Module.Canonical -> TN.Name -> [(TV.Var, Map.Map N.Name Type)]


rowConstraints :: Map.Map PrimModule.Name I.Interface -> Module.Canonical -> Map.Map TN.Name Union -> Map.Map TN.Name Alias -> RowConstraints
rowConstraints ifaces cname unions aliases =
  let
    lookupIn :: Map.Map TN.Name [(TV.Var, Map.Map N.Name Type)] -> RowConstraints
    lookupIn solved moduleName typeName =
      if moduleName == cname
        then Map.findWithDefault [] typeName solved
        else foreignRowConstraints ifaces moduleName typeName

    -- Each type's parameters, and the types its codecs encode. An alias's are the
    -- constraints encoderAlias puts in its signature.
    localTypes :: Map.Map TN.Name ([TV.Var], [Type])
    localTypes =
      Map.union
        (unions & fmap (\union ->
          (_u_vars union, _u_alts union & concatMap (\(Ctor _ _ _ types) -> types))))
        (aliases & fmap (\(Alias tvars tipe) -> (tvars, [tipe])))

    step solved =
      localTypes & fmap (\(tvars, types) ->
          tvars & fmap (\tvar ->
            ( tvar
            , types
                & fmap (getRecordConstraints (lookupIn solved) tvar . normaliseTvarNames Map.empty)
                & Map.unions
            )
          )
        )

    -- Fields only ever get added, and only fields the definitions mention, so this
    -- settles; the bound is only a guard.
    fixpoint :: Int -> Map.Map TN.Name [(TV.Var, Map.Map N.Name Type)] -> Map.Map TN.Name [(TV.Var, Map.Map N.Name Type)]
    fixpoint n solved =
      let next = step solved
      in if next == solved || n <= 0 then next else fixpoint (n - 1) next
  in
  lookupIn (fixpoint (2 * Map.size localTypes + 2) (localTypes & fmap (\(tvars, _) -> tvars & fmap (\tvar -> (tvar, Map.empty)))))


{-| A foreign type's RowConstraints, from its encoder's signature in the interface:
`({ player | moves : List Int, status : Int } -> Encoder) -> ...`. Interfaces built before
custom types' encoders were constrained have none, as before.
-}
foreignRowConstraints :: Map.Map PrimModule.Name I.Interface -> Module.Canonical -> TN.Name -> [(TV.Var, Map.Map N.Name Type)]
foreignRowConstraints ifaces moduleName typeName =
  let
    codecArgs t =
      case t of
        TLambda (TLambda arg _) rest -> arg : codecArgs rest
        _ -> []

    constraintOf arg =
      case arg of
        TVar tvar -> Just (tvar, Map.empty)
        TRecord fields (Just tvar) -> Just (tvar, fields & fmap (\(FieldType _ t) -> t))
        _ -> Nothing
  in
  case foreignTypeSig moduleName (N.fromChars $ "w3_encode_" ++ TN.nameToChars typeName) ifaces of
    Just (Forall _ tipe) -> codecArgs tipe & mapM constraintOf & maybe [] id
    Nothing -> []


{-| `tipe`, with each closed record argument a type constrains (see RowConstraints)
extended with the constraint's fields, so that the codec generated for that argument is the
one the type's codecs take: `ProcessCheckpointResult String { name : String }` becomes
`ProcessCheckpointResult String { moves : List Int, name : String, status : Int }`.

Only for generating codecs: Elm reads the constraint's fields back off the codec, and
infers `{ name : String }` again. Any other argument is left as it is: the codec of a type
variable or of an open record is the `w3_x_c_<tvar>` of its row tail either way.

An alias is called the same way unless the constrained parameter is the row tail of its
own body (`Player a`, `A (B m)`): those are inlined instead (inlineIfRecordOrCall, and the
same in the decoder), which already adds the alias's fields, and stay exactly as they
were. The rest, e.g. `type alias W a = { inner : Player a }`, were called with the codec
of the argument alone, `W { name : String }` with that of `{ name : String }`: the encoder
dropped `moves` and `status`, and the decoder's type was missing them (a TYPE MISMATCH
wherever it is used at `W { name : String }`).
-}
extendRowArgs :: RowConstraints -> Type -> Type
extendRowArgs rowConstraints tipe =
  let
    go = extendRowArgs rowConstraints

    isClosedRecord t =
      case t of
        TRecord _ Nothing -> True
        TAlias _ _ args (Holey body) -> isClosedRecord (substituteRows (Map.fromList args) body)
        TAlias _ _ _ (Filled body) -> isClosedRecord body
        _ -> False

    -- The arguments, each extended with its constraint where `extends` allows
    extendArgs moduleName typeName extends args =
      let
        constraints = rowConstraints moduleName typeName
        subs = Map.fromList (zip (fmap fst constraints) args)

        extendArg arg constraint =
          case constraint of
            Just (param, fields) | not (Map.null fields) && extends param && isClosedRecord arg ->
              fields
                -- Field indexes in Lamdera's (pre Elm 0.19.3) name order, see Lamdera.Lexical
                & Lexical.toList
                & imap (\i (name, t) -> (name, FieldType (fromIntegral i) (substituteRows subs t)))
                & Map.fromList
                & (\extension -> extendRow extension arg)
            _ -> arg
      in
      if all (Map.null . snd) constraints
        then args
        else zipWith extendArg args (fmap Just constraints ++ repeat Nothing)
  in
  case tipe of
    TType moduleName typeName params ->
      TType moduleName typeName (extendArgs moduleName typeName (const True) (fmap go params))

    TLambda t1 t2 -> TLambda (go t1) (go t2)
    TRecord fieldMap ext -> TRecord (fieldMap & fmap (\(FieldType index t) -> FieldType index (go t))) ext
    TUnit -> tipe
    TVar _ -> tipe
    TPair a_ b -> TPair (go a_) (go b)
    TTriple a_ b c -> TTriple (go a_) (go b) (go c)

    TAlias moduleName typeName args (Holey body) ->
      let
        args_ = args & fmap (\(n, t) -> (n, go t))
        extended = extendArgs moduleName typeName (\param -> not (extendsTvar param body)) (fmap snd args_)
      in
      TAlias moduleName typeName (zip (fmap fst args_) extended) (Holey (go body))

    -- Already substituted, so which parameter is the row tail of the body can't be told
    -- any more; left as it was
    TAlias moduleName typeName args (Filled body) ->
      TAlias moduleName typeName (args & fmap (\(n, t) -> (n, go t))) (Filled (go body))


{-| Every record each parameter of a type is the row tail of in its codecs, by type: the
parameter, and the fields of each of those records, in its own parameters, one entry per
set of field names. A parameter used on its own (`List a`) has the record with no fields
of its own, and one that is not used at all has none.

    type alias Player a = { a | status : Int, moves : List Int }
    type T a = A { a | x : Int } | B (Player a)

The codec of a type variable that is a row tail is the codec of the whole record it ends
(see RowConstraints), so one codec for `a` would have to be the codec of a
`{ a | x : Int }` and of a `{ a | moves : List Int, status : Int }` at once. RowConstraints
merged the two into one `{ a | moves, status, x }`, and nothing has both: the encoder was a
TYPE MISMATCH, and inferring the decoder never finished. Elm's unifier has to tell
`{ a | x : Int }` from `{ a | moves : List Int, status : Int }` there, and instead of failing
it trades the row tail for a fresh one and arrives back at the same question, forever
(Type.Unify.unifyRecord, now guarded; plain Elm 0.19.1 hangs on such a decoder written by
hand too). `lamdera make` grew past 46GB on it within a minute.

Such a parameter's codecs now take a codec for each of its records instead, in the order
here, the record with no fields first: `T`'s encoder is

    w3_encode_T :
        ({ a | moves : List Int, status : Int } -> Lamdera.Wire3.Encoder)
        -> ({ a | x : Int } -> Lamdera.Wire3.Encoder)
        -> T a
        -> Lamdera.Wire3.Encoder

and `T { name : String }` is encoded with the codecs of a `{ moves, name, status }` and of
a `{ name, x }` (see selectRows). A parameter with a single record keeps its single codec.
The types of this module are solved together, to a fixpoint, as for RowConstraints, and
foreign ones are read off their encoder's signature in the interface, a parameter's codecs
being next to each other.

Only for types where some parameter, of theirs or of a type they use, has more than one
record (see rowCodecsFor): those never compiled, and every other type's codecs are
generated exactly as before.
-}
type RowSets = Module.Canonical -> TN.Name -> [(TV.Var, [Map.Map N.Name Type])]


rowSets :: Map.Map PrimModule.Name I.Interface -> Module.Canonical -> Map.Map TN.Name Union -> Map.Map TN.Name Alias -> RowSets
rowSets ifaces cname unions aliases =
  let
    lookupIn :: Map.Map TN.Name [(TV.Var, [Map.Map N.Name Type])] -> RowSets
    lookupIn solved moduleName typeName =
      if moduleName == cname
        then Map.findWithDefault [] typeName solved
        else foreignRowSets ifaces moduleName typeName

    localTypes :: Map.Map TN.Name ([TV.Var], [Type])
    localTypes =
      Map.union
        (unions & fmap (\union ->
          (_u_vars union, _u_alts union & concatMap (\(Ctor _ _ _ types) -> types))))
        (aliases
          & Map.filter (\(Alias _ tipe) -> not (isLambdaType tipe))
          & fmap (\(Alias tvars tipe) -> (tvars, [tipe])))

    step solved =
      localTypes & fmap (\(tvars, types) ->
          tvars & fmap (\tvar ->
            (tvar, types & concatMap (rowRecords (lookupIn solved) Map.empty tvar) & distinctRows)
          )
        )

    -- Fields only come from the definitions, so this settles; the bound is only a guard
    -- (a type not settled by then gets a compile error from selectRows, not a hang).
    fixpoint :: Int -> Map.Map TN.Name [(TV.Var, [Map.Map N.Name Type])] -> Map.Map TN.Name [(TV.Var, [Map.Map N.Name Type])]
    fixpoint n solved =
      let next = step solved
      in if next == solved || n <= 0 then next else fixpoint (n - 1) next
  in
  lookupIn (fixpoint (4 * Map.size localTypes + 8) (localTypes & fmap (\(tvars, _) -> tvars & fmap (\tvar -> (tvar, [])))))


{-| A foreign type's RowSets, from its encoder's signature in the interface (see
foreignRowConstraints), a parameter's codecs being next to each other. -}
foreignRowSets :: Map.Map PrimModule.Name I.Interface -> Module.Canonical -> TN.Name -> [(TV.Var, [Map.Map N.Name Type])]
foreignRowSets ifaces moduleName typeName =
  foreignRowConstraints ifaces moduleName typeName
    & List.groupBy (\(tvar1, _) (tvar2, _) -> tvar1 == tvar2)
    & concatMap (\group ->
        case group of
          (tvar, _) : _ -> [(tvar, fmap snd group)]
          [] -> []
      )


{-| The records `tvar` is the row tail of in the codecs of `tipe` (see RowSets), each
with the fields `pending` adds to the record `tipe` itself ends, if it ends in `tvar`.
Follows the codecs Encoder/Decoder generate: nothing inside functions or unsupported types
refers to a codec.
-}
rowRecords :: RowSets -> Map.Map N.Name Type -> TV.Var -> Type -> [Map.Map N.Name Type]
rowRecords sets pending tvar tipe =
  let
    go = rowRecords sets Map.empty tvar

    fieldTypes fields = fields & fmap (\(FieldType _ t) -> t)
  in
  if isUnsupportedKernelType tipe then
    []
  else
  case tipe of
    TVar v -> if v == tvar then [pending] else []

    -- The codec of an open record is its row tail's, whole: its fields are not looked at
    -- here (`type alias Record a = { a | field : a }` has the one record)
    TRecord fields (Just ext) ->
      if ext == tvar then [Map.union pending (fieldTypes fields)] else []

    TRecord fields Nothing ->
      fieldTypes fields & Map.elems & concatMap go

    TType moduleName typeName args ->
      let
        params = sets moduleName typeName
        subs = Map.fromList (zip (fmap fst params) args)
      in
      zip args (fmap snd params ++ repeat [])
        & concatMap (\(arg, argRows) ->
            (if null argRows then [Map.empty] else argRows)
              & concatMap (\fields -> rowRecords sets (fmap (substituteRows subs) fields) tvar arg)
          )

    -- With each alias's arguments substituted in its body, merging rows, so the records
    -- come out whole: `Outer (Middle m)` is the one `{ m | middle, outer }`
    TAlias _ _ args (Holey body) ->
      if isLambdaType body then [] else rowRecords sets pending tvar (substituteRows (Map.fromList args) body)

    TAlias _ _ _ (Filled body) ->
      if isLambdaType body then [] else rowRecords sets pending tvar body

    TPair a_ b -> go a_ ++ go b
    TTriple a_ b c -> go a_ ++ go b ++ go c
    TUnit -> []
    TLambda _ _ -> []


{-| One record per set of field names, in a fixed order, the one with no fields first.
Capped, as a guard: a parameter with more records than that gets a compile error from
selectRows rather than ever more codecs. -}
distinctRows :: [Map.Map N.Name Type] -> [Map.Map N.Name Type]
distinctRows rows =
  rows
    & List.nubBy (\x y -> Map.keys x == Map.keys y)
    -- By the names as text, as Lamdera.Lexical (Name's own Ord isn't that since Elm 0.19.3)
    & List.sortOn (\fields -> fields & Map.keys & fmap N.toChars & List.sort)
    & take 16


{-| The codecs a type's codecs take for one of its parameters: the type variable each one
is named after (`w3_x_c_<tvar>`), and the fields of the record it is for. A parameter with
at most one record has the one codec it always had; one with more has a codec per record
(see RowSets), the one with no fields keeping the parameter's own name and the others
numbered, `w3_x_c_a__1`, `w3_x_c_a__2`, ...
-}
codecParams :: TV.Var -> [Map.Map N.Name Type] -> [(TV.Var, Map.Map N.Name Type)]
codecParams tvar rows =
  case rows of
    [] -> [(tvar, Map.empty)]
    [fields] -> [(tvar, fields)]
    _ ->
      (if any Map.null rows then [(tvar, Map.empty)] else [])
        ++ (rows
              & filter (not . Map.null)
              & zip [1 :: Int ..]
              & fmap (\(i, fields) -> (TV.varFromChars (TV.varToChars tvar ++ "__" ++ show i), fields))
           )


hasManyRows :: [Map.Map N.Name Type] -> Bool
hasManyRows rows =
  length rows >= 2


{-| The codecs of one of this module's types, where some parameter, of its own or of a type
it uses, is the row tail of more than one record (see RowSets): the codecs its codecs take
(the parameter, the type variable the codec is named after, the record's fields), and the
types to generate them from, with each record ending in one of those parameters ending in
the type variable of the codec for it instead, and each argument to such a parameter of
another type given once per record (see selectRows).
-}
data RowCodecs =
  RowCodecs
    { _rc_params :: [(TV.Var, TV.Var, Map.Map N.Name Type)]
    , _rc_select :: Type -> Type
    }


{-| The RowCodecs of a type with these parameters, generating codecs for these types:
Nothing when no parameter involved has more than one record, which is every type that ever
compiled, so their codecs stay exactly as they were; a Left when one of the types can't be
given them.
-}
rowCodecsFor :: RowSets -> Module.Canonical -> TN.Name -> [TV.Var] -> [Type] -> Either String (Maybe RowCodecs)
rowCodecsFor sets cname typeName tvars types =
  let
    ownRows = sets cname typeName
    own = ownRows & filter (hasManyRows . snd) & Map.fromList
    select = selectRows sets own Map.empty
  in
  if Map.null own && not (any (reachesManyRows sets) types)
    then Right Nothing
    else do
      mapM_ select types
      Right $ Just $ RowCodecs
        { _rc_params =
            zip tvars (fmap snd ownRows ++ repeat [])
              & concatMap (\(tvar, rows) -> codecParams tvar rows & fmap (\(codecVar, fields) -> (tvar, codecVar, fields)))
        , _rc_select = \t -> either (\err -> error $ "rowCodecsFor: " ++ err) id (select t)
        }


{-| Does generating codecs for this type call the codecs of a type with a parameter that
has more than one record (see RowSets)? -}
reachesManyRows :: RowSets -> Type -> Bool
reachesManyRows sets tipe =
  let
    go = reachesManyRows sets
    calleeHasMany moduleName typeName = sets moduleName typeName & any (hasManyRows . snd)
  in
  if isUnsupportedKernelType tipe then
    False
  else
  case tipe of
    TType moduleName typeName args -> calleeHasMany moduleName typeName || any go args
    TRecord _ (Just _) -> False
    TRecord fields Nothing -> fields & Map.elems & any (\(FieldType _ t) -> go t)
    TAlias moduleName typeName args (Holey body) -> calleeHasMany moduleName typeName || any (go . snd) args || go body
    TAlias moduleName typeName args (Filled body) -> calleeHasMany moduleName typeName || any (go . snd) args || go body
    TPair a_ b -> go a_ || go b
    TTriple a_ b c -> go a_ || go b || go c
    TVar _ -> False
    TUnit -> False
    TLambda _ _ -> False


{-| `tipe`, for generating codecs from where the parameters in `own` each have more than one
record (see RowSets): a record ending in one of them ends in the type variable of the codec
for that record instead (codecParams), and an argument to a parameter of another type that
has more than one record is given once for each of them, each time as the record that
codec is for:

    T { name : String }  -->  T { moves : List Int, name : String, status : Int } { name : String, x : Int }
    T b                  -->  T b__1 b__2

Not a type Elm would accept, but Encoder and Decoder generate the right codecs from it: a
codec for each argument, `w3_x_c_<tvar>` for each type variable. A closed record argument
to a constrained parameter is extended with the record's fields, as in extendRowArgs, and
the records an argument's row tail ends get the fields of the parameter's record too
(`pending`), so they end in the right codec's type variable.

`own` is empty in the body of an alias (those are in the alias's own parameters).
-}
selectRows :: RowSets -> Map.Map TV.Var [Map.Map N.Name Type] -> Map.Map N.Name Type -> Type -> Either String Type
selectRows sets own pending tipe =
  let
    go = selectRows sets own Map.empty

    codecVar v fields =
      case Map.lookup v own of
        Nothing -> Right v
        Just rows ->
          case codecParams v rows & List.find (\(_, rowFields) -> Map.keys rowFields == Map.keys fields) of
            Just (var, _) -> Right var
            Nothing ->
              Left $ "the type variable `" ++ TV.varToChars v ++ "` is the row tail of a record with "
                ++ fieldNames fields ++ " there, and I could not work out a codec for that record."

    fieldNames fields =
      if Map.null fields
        then "no fields of its own"
        else "the fields " ++ List.intercalate ", " (fields & Map.keys & fmap N.toChars)

    asFieldTypes fields =
      fields
        -- Field indexes in Lamdera's (pre Elm 0.19.3) name order, see Lamdera.Lexical
        & Lexical.toList
        & imap (\i (name, t) -> (name, FieldType (fromIntegral i) t))
        & Map.fromList

    -- The arguments a type with these parameters is called with: for each parameter, an
    -- argument per record (see codecParams), each with its record's fields. `rowTail` says
    -- whether a parameter is the row tail of an alias's own body, which is inlined rather
    -- than called (see extendRowArgs): its argument gets what extends the alias too.
    callArgs :: [(TV.Var, [Map.Map N.Name Type])] -> (TV.Var -> Bool) -> [(TV.Var, Type)] -> Either String [(TV.Var, Type)]
    callArgs params rowTail args =
      let
        subs = Map.fromList (zip (fmap fst params) (fmap snd args))
      in
      zip args (fmap Just params ++ repeat Nothing)
        & mapM (\((name, arg), param) ->
            case param of
              Nothing ->
                go arg & fmap (\arg_ -> [(name, arg_)])

              Just (paramVar, rows) | rowTail paramVar ->
                let fields = rows & Map.unions & fmap (substituteRows subs)
                in selectRows sets own (Map.union pending fields) arg & fmap (\arg_ -> [(name, arg_)])

              Just (paramVar, rows) ->
                codecParams paramVar rows
                  & mapM (\(var, rowFields) ->
                      let
                        fields = rowFields & fmap (substituteRows subs)
                        extended =
                          if not (Map.null fields) && isClosedRecordType arg
                            then extendRow (asFieldTypes fields) arg
                            else arg
                      in
                      selectRows sets own fields extended & fmap (\arg_ -> (if length rows >= 2 then var else name, arg_))
                    )
          )
        & fmap concat
  in
  if isUnsupportedKernelType tipe then
    Right tipe
  else
  case tipe of
    TVar v -> TVar <$> codecVar v pending

    -- Only its row tail's codec is used for an open record, see rowRecords
    TRecord fields (Just ext) -> do
      ext_ <- codecVar ext (Map.union pending (fields & fmap (\(FieldType _ t) -> t)))
      Right (TRecord fields (Just ext_))

    TRecord fields Nothing -> do
      fields_ <- traverse (\(FieldType index t) -> FieldType index <$> go t) fields
      Right (TRecord fields_ Nothing)

    TType moduleName typeName args -> do
      args_ <- callArgs (sets moduleName typeName) (const False) (fmap (\arg -> (TV.varFromChars "_", arg)) args)
      Right (TType moduleName typeName (fmap snd args_))

    TAlias moduleName typeName args (Holey body) ->
      let
        params = sets moduleName typeName
        manyRows = params & filter (hasManyRows . snd) & fmap fst
      in
      if isLambdaType body then
        Right tipe
      else if reachesExtensibleRecord body && not (null manyRows) then
        -- Such an alias is inlined where its row tail is a record (see
        -- inlineIfRecordOrCall), with the one argument of each parameter substituted in
        -- its body, so this inlines it already, with each of its records whole:
        -- `{ r | f : { p | x : Int }, g : { p | y : Int } }` at `{ z : Int }` and
        -- `{ name : String }` is the `{ f : { name, x }, g : { name, y }, z }` it stands for.
        -- One whose row tail is a type variable is a record ending in that variable's codec
        -- either way.
        selectRows sets own pending (substituteRows (Map.fromList args) body)
      else do
        body_ <- selectRows sets Map.empty Map.empty body
        args_ <- callArgs params (\param -> extendsTvar param body) args
        Right (TAlias moduleName typeName args_ (Holey body_))

    TAlias moduleName typeName args (Filled body) -> do
      body_ <- go body
      args_ <- callArgs (sets moduleName typeName) (const False) args
      Right (TAlias moduleName typeName args_ (Filled body_))

    TPair a_ b -> TPair <$> go a_ <*> go b
    TTriple a_ b c -> TTriple <$> go a_ <*> go b <*> go c
    TUnit -> Right tipe
    TLambda _ _ -> Right tipe


{-| Is this a closed record, through its aliases? (As in extendRowArgs.) -}
isClosedRecordType :: Type -> Bool
isClosedRecordType t =
  case t of
    TRecord _ Nothing -> True
    TAlias _ _ args (Holey body) -> isClosedRecordType (substituteRows (Map.fromList args) body)
    TAlias _ _ _ (Filled body) -> isClosedRecordType body
    _ -> False


{-| The compile error for a type whose codecs can't be generated (see rowCodecsFor) -}
rowCodecsError :: TN.Name -> String -> D.Doc
rowCodecsError typeName problem =
  D.stack
    [ D.fromChars $ "Wire3 cannot generate codecs for " ++ TN.nameToChars typeName
    , D.reflow $ "In `" ++ TN.nameToChars typeName ++ "`, " ++ problem
    , D.reflow
        "Giving each record its own type variable avoids this, e.g. `type T a b = A { a | x : Int } | B (Player b)` rather than `type T a = A { a | x : Int } | B (Player a)`. Please also report this at https://github.com/lamdera/compiler/issues with the types involved, so it can be supported."
    ]


encoderAlias :: Maybe RowCodecs -> RowConstraints -> Bool -> Map.Map PrimModule.Name I.Interface -> Pkg.Name -> Src.Module -> Decls -> TN.Name -> Alias -> Def
encoderAlias rowCodecs rowConstraints isTest_ ifaces pkg modul decls aliasName alias@(Alias tvars tipe) =
  let
    !_ = runTests isTest_ "encoderAlias" pkg modul decls generatedName finalGen alias (aliasAsModule cname aliasName alias)

    generatedName = N.fromChars $ "w3_encode_" ++ TN.nameToChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)
    -- ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w3_x_c_" ++ Data.Name.toChars tvar )

    normalisedTvarsType = normaliseTvarNames Map.empty tipe

    ptvarsTyped =
      case rowCodecs of
        -- A codec for each record a parameter is the row tail of (see RowSets)
        Just rc ->
          _rc_params rc & fmap (\(tvar, codecVar, fields) ->
            (pvar $ N.fromChars $ "w3_x_c_" ++ TV.varToChars codecVar, TLambda (constrainTvarBy tvar fields) tLamdera_Wire_Encoder_Holey)
          )
        Nothing ->
          tvars & fmap (\tvar ->
            let constrainedTvar = constrainTvar rowConstraints tvar normalisedTvarsType
            in
            ( pvar $ N.fromChars $ "w3_x_c_" ++ TV.varToChars tvar
            , TLambda constrainedTvar tLamdera_Wire_Encoder_Holey
            )
          )

    tvarsNameTyped = tvars & fmap (\tvar ->
        (tvar, TVar tvar)
      )

    generatedBody = deepEncoderForType 0 ifaces cname (maybe (extendRowArgs rowConstraints) _rc_select rowCodecs tipe)

    -- = Def (A.Located Name) [Pattern] Expr
    -- generated = Def (a (generatedName)) ptvars $
    --   -- debugEncoder (Data.Name.toElmString aliasName) $
    --   generatedBody

    -- ttype = (TType cname aliasName (tvars & fmap TVar))

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


decoderAlias :: Maybe RowCodecs -> RowConstraints -> DecodeMode -> Bool -> Map.Map PrimModule.Name I.Interface -> Pkg.Name -> Src.Module -> Decls -> TN.Name -> Alias -> Def
decoderAlias rowCodecs rowConstraints mode isTest_ ifaces pkg modul decls aliasName alias@(Alias tvars tipe) =
  let
    !_ = runTests isTest_ "decoderAlias" pkg modul decls generatedName generated alias (aliasAsModule cname aliasName alias)

    generatedName = N.fromChars $ decodePrefix mode ++ TN.nameToChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)
    ptvars =
      case rowCodecs of
        -- A codec for each record a parameter is the row tail of (see RowSets)
        Just rc -> _rc_params rc & fmap (\(_, codecVar, _) -> pvar $ N.fromChars $ "w3_x_c_" ++ TV.varToChars codecVar)
        Nothing -> tvars & fmap (\tvar -> pvar $ N.fromChars $ "w3_x_c_" ++ TV.varToChars tvar )

    generated = Def (a (generatedName)) ptvars $
      -- debugDecoder (Data.Name.toElmString aliasName) $
      decoderForType mode ifaces cname (maybe (extendRowArgs rowConstraints) _rc_select rowCodecs tipe)
  in
  generated


{-| Which type parameters of the decoders generated for a module's types have to be
`comparable`, by type and parameter position.

The decoders are annotated, and a parameter whose decoder ends up decoding a Set element
or a Dict key, directly or through another decoder, has to be called comparable in that
annotation. Otherwise the annotation is more general than its body, and the module no
longer typechecks. Encoders never need this, encoding a Set or Dict asks nothing of its
elements, so encoderUnion and encoderAlias get to use the type's own names.

What one type needs depends on the types it references, which in this module includes
itself and its neighbours, so all of the module's types are solved together, to a
fixpoint. Foreign types are read off their decoder's signature in the interface.

-}
type ComparableParams = Module.Canonical -> TN.Name -> [Bool]


comparableParams :: Map.Map PrimModule.Name I.Interface -> Module.Canonical -> Map.Map TN.Name Union -> Map.Map TN.Name Alias -> ComparableParams
comparableParams ifaces cname unions aliases =
  let
    -- The type parameters of each decoder generated here, and the types its body decodes
    localTypes :: Map.Map TN.Name ([TV.Var], [Type])
    localTypes =
      Map.union
        (unions & fmap (\union ->
          (_u_vars union, _u_alts union & concatMap (\(Ctor _ _ _ paramTypes) -> paramTypes))))
        (aliases
          & Map.filter (\(Alias _ tipe) -> not (isLambdaType tipe))
          & fmap (\(Alias tvars tipe) -> (tvars, [tipe])))

    lookupIn :: Map.Map TN.Name [Bool] -> ComparableParams
    lookupIn solved moduleName typeName =
      if moduleName == cname
        then Map.findWithDefault [] typeName solved
        else foreignComparableParams ifaces moduleName typeName

    step solved =
      localTypes & fmap (\(tvars, types) ->
          let needed = types & concatMap (decoderComparableTvars (lookupIn solved))
          in
          tvars & fmap (\tvar -> isComparableTvar tvar || elem tvar needed)
        )

    -- Monotone (a parameter only ever becomes comparable) over finitely many flags, so
    -- this terminates.
    fixpoint solved =
      let next = step solved
      in if next == solved then solved else fixpoint next
  in
  lookupIn (fixpoint (localTypes & fmap (\(tvars, _) -> fmap (const False) tvars)))


{-| The comparable flags of a foreign type's decoder parameters, from its signature in the
interface: `Decoder comparable -> ...`. Interfaces built before decoders were annotated
carry the inferred signature instead, which names its constrained variables the same way.
-}
foreignComparableParams :: Map.Map PrimModule.Name I.Interface -> Module.Canonical -> TN.Name -> [Bool]
foreignComparableParams ifaces moduleName typeName =
  let
    args t =
      case t of
        TLambda arg rest -> arg : args rest
        _ -> []

    decodesComparableTvar arg =
      case arg of
        TAlias _ "Decoder" [(_, TVar tvar)] _ -> isComparableTvar tvar
        TType _ "Decoder" [TVar tvar] -> isComparableTvar tvar
        _ -> False
  in
  case foreignTypeSig moduleName (N.fromChars $ "w3_decode_" ++ TN.nameToChars typeName) ifaces of
    Just (Forall _ tipe) -> args tipe & fmap decodesComparableTvar
    Nothing -> []


isComparableTvar :: TV.Var -> Bool
isComparableTvar (TV.Var _ constraint) =
  case constraint of
    TV.Comparable -> True
    TV.CompAppend -> True
    _ -> False


{-| The type variables whose `w3_x_c_<tvar>` decoders the decoder for `tipe` needs to
produce a comparable value.

This follows decoderForType case for case, because it is answering a question about the
code decoderForType generates, so a change to one needs the same change in the other.
-}
decoderComparableTvars :: ComparableParams -> Type -> [TV.Var]
decoderComparableTvars comparableParams tipe =
  let
    go = decoderComparableTvars comparableParams

    -- Calling the decoder for a type with decoders for its parameters
    applied moduleName typeName params =
      zip (comparableParams moduleName typeName ++ repeat False) params
        & concatMap (\(isComparable, param) ->
            (if isComparable then comparableTvars param else []) ++ go param
          )

    fields fieldMap =
      fieldMap & fieldsToList & concatMap (\(_, field) -> go field)
  in
  if containsUnsupportedTypes tipe then
    []
  else
  case tipe of
    TPair a_ b -> go a_ ++ go b
    TTriple a_ b c -> go a_ ++ go b ++ go c

    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [ptype] ->
      comparableTvars ptype ++ go ptype

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      comparableTvars key ++ go key ++ go val

    -- The rest of decoderForType's built in decoders ask nothing of their parameters
    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [ptype] -> go ptype
    TType (Module.Canonical (Name "elm" "core") "List") "List" [ptype] -> go ptype
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [ptype] -> go ptype
    TType (Module.Canonical (Name "lamdera" "containers") "SeqSet") "SeqSet" [ptype] -> go ptype
    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a_] -> go err ++ go a_
    TType (Module.Canonical (Name "lamdera" "containers") "SeqDict") "SeqDict" [key, val] -> go key ++ go val
    TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Bytes" _ -> []
    TType (Module.Canonical (Name "elm" "time") "Time") "Posix" _ -> []
    TType (Module.Canonical (Name "elm" "file") "File") "File" _ -> []
    TType (Module.Canonical (Name "elm-explorations" "webgl") "WebGL.Texture") "Texture" _ -> []

    TType moduleName typeName params ->
      if isUnsupportedKernelType tipe
        then []
        else applied moduleName typeName params

    TRecord fieldMap maybeExtensible ->
      case maybeExtensible of
        -- The parameter decoder is used for the whole record, as is
        Just _ -> []
        Nothing -> fields fieldMap

    TAlias moduleName typeName tvars_ aType ->
      let
        innerType = case aType of { Holey t -> t; Filled t -> t }

        normalDecoder = applied moduleName typeName (fmap snd tvars_)

        inlinedRecord fieldMap extensibleName =
          case resolvedRecordFieldMapM fieldMap extensibleName tvars_ of
            Just resolved -> go (TRecord resolved Nothing & resolveTvar tvars_)
            Nothing -> normalDecoder
      in
      if isUnsupportedKernelType tipe
      then []
      else if isLambdaType innerType
      then []
      else
        case aType of
          Holey (TRecord fieldMap extensibleName) -> inlinedRecord fieldMap extensibleName
          Holey t ->
            if reachesExtensibleRecord t
              then
                case resolveTvar tvars_ t of
                  TAlias _ _ _ (Filled (TRecord fieldMap Nothing)) -> fields fieldMap
                  _ -> normalDecoder
              else normalDecoder
          Filled (TRecord fieldMap extensibleName) -> inlinedRecord fieldMap extensibleName
          Filled _ -> normalDecoder

    TVar _ -> []
    TLambda _ _ -> []
    TUnit -> []


{-| The type variables that have to be comparable for a value of this type to be. -}
comparableTvars :: Type -> [TV.Var]
comparableTvars tipe =
  case tipe of
    TVar tvar -> [tvar]
    TPair a_ b -> comparableTvars a_ ++ comparableTvars b
    TTriple a_ b c -> comparableTvars a_ ++ comparableTvars b ++ comparableTvars c
    TType (Module.Canonical (Name "elm" "core") "List") "List" [ptype] -> comparableTvars ptype
    TAlias _ _ _ (Filled t) -> comparableTvars t
    TAlias _ _ tvars_ (Holey t) -> comparableTvars (substituteTvars (Map.fromList tvars_) t)
    -- Everything else is either comparable already or never can be, either way there's
    -- nothing a type variable can do about it.
    _ -> []



{-| The name a decoder's signature uses for one of its type's parameters.

The type's own name, unless the parameter has to be comparable (see ComparableParams)
and isn't named that way already, in which case it's renamed to say so:
`type Bag a = Bag (Set a)` decodes with `Decoder comparable_a -> Decoder (Bag comparable_a)`.
-}
decoderSigTvar :: ComparableParams -> Module.Canonical -> TN.Name -> [TV.Var] -> TV.Var -> TV.Var
decoderSigTvar comparableParams cname typeName tvars tvar =
  let
    isComparable =
      zip tvars (comparableParams cname typeName ++ repeat False)
        & any (\(t, flag) -> t == tvar && flag)

    fresh prefix =
      let
        candidates = iterate (++ "_") (prefix ++ TV.varToChars tvar)
        taken = fmap TV.varToChars tvars
      in
      candidates & List.find (\c -> notElem c taken) & maybe tvar TV.varFromChars
  in
  if not isComparable
    then tvar
    else
      case tvar of
        TV.Var _ TV.Any -> fresh "comparable_"
        -- Appendable is the one constraint that doesn't already unify with comparable
        TV.Var _ TV.Appendable -> fresh "compappend_"
        _ -> tvar


{-| Codecs for a custom type that refers to itself at OTHER type arguments than its own
parameters: a non-regular (or "nested") data type, such as elm-fontawesome's

    type Icon hasId = Icon { ..., outer : Maybe (Icon WithId) }

Elm has no polymorphic recursion. Inside its own definition a function is only ever used
at the very type it is being defined at, annotation or not, so the encoder for `Icon hasId`
cannot call itself to encode an `Icon WithId` (a TYPE MISMATCH, `hasId` against `WithId`),
and an unannotated decoder that does so is inferred at `WithId` only, failing wherever
`Icon` is used at another argument (or, for `type Nested a = ... (Nested (List a))`,
failing outright with an INFINITE TYPE). Recursion through another definition is fine
though: an annotated definition used by a DIFFERENT definition of the same recursive group
is instantiated afresh at each use, and an unannotated one is generalised before the
annotated ones are checked.

So for such a type (see recursesIrregularly) each codec:

  - is annotated, if it is not already: the decoders are plain inferred Defs, and only an
    annotation lets the other definitions use them at any argument. The signature is the
    encoders' own shape, `Decoder a -> Decoder (T a)`, with `a` renamed to say comparable
    where the decoder needs it (see ComparableParams);
  - refers to itself through a forwarder instead, `w3_polyrec_<encode|decode|unsafe_decode>_T`,
    an annotated definition that only calls the codec back with the same arguments. The
    forwarder's name deliberately lacks the `w3_encode_`/`w3_decode_` prefixes that
    codecs are recognised by (Lamdera.Wire3.Direct, the reporting filters only look at `w3_`).

The bodies are otherwise exactly what they were, so the bytes are the same. Every other
type's codecs are left exactly as they were.
-}
withPolymorphicRecursion :: Module.Canonical -> ComparableParams -> TN.Name -> Union -> [Def] -> [Def]
withPolymorphicRecursion cname compParams unionName union defs =
  defs
    & fmap (annotateUnionDecoder cname compParams unionName union)
    & concatMap (forwardSelfReferences cname)


{-| Does this custom type reach a reference to itself, at other arguments than its own
parameters? Directly, or through the other types of this module it reaches (custom types
and aliases, with their arguments substituted in): codecs of types from other modules
cannot lead back here.

Functions are not followed: their codecs fail without referring to anything. Each other
custom type is entered once, with the arguments it is first reached at, which is all it
takes for the shapes found in practice and keeps this finite for types that grow on the
way down.
-}
recursesIrregularly :: Module.Canonical -> Map.Map TN.Name Union -> TN.Name -> Union -> Bool
recursesIrregularly cname unions unionName union =
  let
    ownParams = _u_vars union & fmap TVar

    ctorParams u = _u_alts u & concatMap (\(Ctor _ _ _ paramTypes) -> paramTypes)

    walk :: [TN.Name] -> [Type] -> Bool
    walk visited types =
      case types of
        [] -> False
        t : rest ->
          case t of
            TType home name args ->
              if home == cname && name == unionName then
                args /= ownParams || walk visited rest
              else
                case Map.lookup name unions of
                  Just other | home == cname && notElem name visited ->
                    let subs = Map.fromList (zip (_u_vars other) args)
                    in walk (name : visited) (args ++ fmap (substituteTvars subs) (ctorParams other) ++ rest)
                  _ ->
                    walk visited (args ++ rest)

            TRecord fieldMap _ -> walk visited ((fieldMap & Map.elems & fmap (\(FieldType _ ft) -> ft)) ++ rest)

            TAlias _ _ args (Holey body) -> walk visited (substituteTvars (Map.fromList args) body : rest)
            TAlias _ _ _ (Filled body) -> walk visited (body : rest)

            TPair a_ b -> walk visited (a_ : b : rest)
            TTriple a_ b c -> walk visited (a_ : b : c : rest)

            TLambda _ _ -> walk visited rest
            TVar _ -> walk visited rest
            TUnit -> walk visited rest
  in
  walk [] (ctorParams union)


{-| Annotate a custom type's (inferred) decoder, see withPolymorphicRecursion. Encoders and
decoders that are already annotated are left as they are.
-}
annotateUnionDecoder :: Module.Canonical -> ComparableParams -> TN.Name -> Union -> Def -> Def
annotateUnionDecoder cname compParams unionName union def =
  case def of
    Def name ptvars body ->
      let
        tvars = _u_vars union
        sigTvar = decoderSigTvar compParams cname unionName tvars
        ptvarsTyped = zip ptvars tvars & fmap (\(p, tvar) -> (p, tWire_Decoder_Sig (TVar (sigTvar tvar))))
        freeVars = tvars & fmap (\tvar -> (sigTvar tvar, ())) & Map.fromList
      in
      TypedDef name freeVars ptvarsTyped body $
        tWire_Decoder_Sig (TType cname unionName (tvars & fmap (TVar . sigTvar)))

    TypedDef _ _ _ _ _ ->
      def


{-| An annotated codec that refers to itself, split into the codec, now referring to a
forwarder instead, and that forwarder, see withPolymorphicRecursion.
-}
forwardSelfReferences :: Module.Canonical -> Def -> [Def]
forwardSelfReferences cname def =
  case def of
    TypedDef (A.At region name) freeVars typedArgs body resultType | elem name (topLevelRefsInExpr body) ->
      let
        forwarder = N.fromChars $ "w3_polyrec_" ++ drop (length ("w3_" :: String)) (N.toChars name)

        argNames =
          typedArgs & fmap (\(pattern, _) ->
            case pattern of
              A.At _ (PVar n) -> n
              _ -> error $ "forwardSelfReferences: unexpected codec argument pattern in " ++ N.toChars name
          )

        forwarderBody =
          call (a (VarTopLevel cname name)) (argNames & fmap lvar)
      in
      [ TypedDef (A.At region name) freeVars typedArgs (renameTopLevelRef cname name forwarder body) resultType
      , TypedDef (a forwarder) freeVars (zip (argNames & fmap pvar) (fmap snd typedArgs)) forwarderBody resultType
      ]

    _ ->
      [def]
