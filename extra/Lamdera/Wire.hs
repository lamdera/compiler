{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Lamdera.Wire where

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
import Lamdera.Wire.Encoder
import Lamdera.Wire.Decoder


runTests isTest debugName modul decls generatedName generated canonicalValue wire2gen =
  if isTest
    then
      unsafePerformIO $ do
      let
        testName = Data.Name.fromChars $ "expected_" ++ Data.Name.toChars generatedName
        withName (Def (A.At r n) p e) n_ =
          Def (A.At r n_) p e

      case decls & findDef testName of
        Just testDefinition -> do

          -- debugHaskellPass ("💚 testDefinition " <> show_ (Src.getName modul)) (testDefinition) (pure ())
          -- debugHaskellPass ("🧡 generated " <> show_ (Src.getName modul)) (generated) (pure ())
          -- diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
          -- atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff

          if generated == testDefinition `withName` generatedName
            then do
              atomicPutStrLn $ "✅ gen " <> debugName <> " matches " <> Data.Name.toChars (Src.getName modul) <> "." <> Data.Name.toChars testName
              -- debugPassText ("🧡 expected implementation pretty-printed " <> show_ (Src.getName modul)) (Source2.generateCodecs Map.empty wire2gen) (pure ())
            else do
              debugHaskellPass "🏁 Actual value input" (canonicalValue) (pure ())
              -- debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert generated) (pure ())
              debugPassText ("🧡 expected implementation pretty-printed " <> show_ (Src.getName modul)) (Source2.generateCodecs Map.empty wire2gen) (pure ())
              -- debugHaskellPass ("🧡 expected implementation AST.Canonical " <> show_ (Src.getName modul)) (testDefinition) (pure ())

              diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
              diff2 <- icdiff (ToSource.convert testDefinition) (ToSource.convert generated)

              atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff
              atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff2
              -- error "exiting!"
              -- atomicPutStrLn $ "❌❌❌ gen does not match test definition, attempting pretty-print diff:\n <NEUTERED>"

        Nothing -> do
          atomicPutStrLn $ "❌ Error: test not found " ++ Data.Name.toChars (Src.getName modul) ++ "." ++ Data.Name.toChars testName
          debugPassText ("🧡 expected implementation pretty-printed " <> show_ (Src.getName modul)) (Source2.generateCodecs Map.empty wire2gen) (pure ())
          -- error "exiting!"

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
  if shouldHaveCodecsGenerated pkg then
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

    -- !x =
    --   if isTest then
    --     unsafePerformIO $ do
    --
    --       -- debugHaskellPass "decls summary before"
    --       --   (declsToSummary decls_)
    --       --   (pure ())
    --
    --       -- debugPassText
    --       --   ("❤️  oldcodecs " <> show_ (Src.getName modul))
    --       --   (Source2.generateCodecs Map.empty canonical)
    --       --   (pure ())
    --   else ()

    decls_ = Can._decls canonical

    unionDefs =
      (Can._unions canonical)
        & Map.toList
        & concatMap (\(k, v) ->
            [ (encoderUnion isTest ifaces pkg modul decls_ k v)
            , (decoderUnion isTest ifaces pkg modul decls_ k v)
            ]
        )

    aliasDefs =
      (Can._aliases canonical)
        & Map.toList
        & concatMap (\(k, v) ->
            [ (encoderAlias isTest ifaces pkg modul decls_ k v)
            , (decoderAlias isTest ifaces pkg modul decls_ k v)
            ]
        )

    sortedDefs =
      declsToList decls_
        & List.unionBy (\a b -> defName a == defName b) (unionDefs ++ aliasDefs)
        & fmap defToNode
        -- The Decls data structure must be topologically sorted by LocalVar refs,
        -- otherwise type inference will throw Map.! errors and not be able to see sub-functions
        & Graph.stronglyConnComp
        & foldr (\scc decls ->
          case scc of
            Graph.AcyclicSCC def ->
              addDef def decls
            Graph.CyclicSCC defs ->
              addRecDef defs decls
        ) SaveTheEnvironment
  in
  Right $ canonical { _decls = sortedDefs }


defToNode :: Def -> (Def, Data.Name.Name, [Data.Name.Name])
defToNode def =
  ( def, defName def, defGetEdges def )


defGetEdges :: Def -> [Data.Name.Name]
defGetEdges def =
  case def of
    Def (A.At region name_) pvars expr ->
      getLvars expr
    TypedDef (A.At region name_) freeVars pvars expr tipe ->
      getLvars expr


getLvars :: Expr -> [Data.Name.Name]
getLvars (A.At _ expr) =
  case expr of
    VarLocal name -> []
    VarTopLevel cname name -> [name]
    VarKernel module_ name -> []
    VarForeign cname name annotation -> []
    VarCtor ctorOpts cname name index annotation -> []
    VarDebug cname name annotation -> []
    VarOperator name cname name2 annotation -> []
    Chr s -> []
    Str s -> []
    Int i -> []
    Float f -> []
    List exprs -> exprs & concatMap getLvars
    Negate expr -> getLvars expr
    Binop name cname name2 annotation e1 e2 -> [e1, e2] & concatMap getLvars
    Lambda pvars expr -> getLvars expr
    Call expr params -> getLvars expr ++ concatMap getLvars params
    If [(e1, e2)] e3 -> [e1, e2, e3] & concatMap getLvars
    Let def expr -> defGetEdges def ++ getLvars expr
    LetRec defs expr -> concatMap defGetEdges defs ++ getLvars expr
    LetDestruct pat e1 e2 -> [e1, e2] & concatMap getLvars
    Case expr branches -> branches & concatMap (\(CaseBranch pat expr) -> getLvars expr)
    Accessor name -> []
    Access expr aName -> getLvars expr
    Update name expr fieldUpdates -> getLvars expr ++
      (fieldUpdates & Map.toList & concatMap (\(n, (FieldUpdate region expr)) -> getLvars expr))
    Record fields ->
      fields & Map.toList & concatMap (\(n, expr) -> getLvars expr)
    Unit -> []
    Tuple e1 e2 me3 ->
      case me3 of
        Just e3 -> [e1, e2, e3] & concatMap getLvars
        Nothing -> [e1, e2] & concatMap getLvars
    Shader source types -> []


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


encoderUnion :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
encoderUnion isTest ifaces pkg modul decls unionName union =
  let
    !x = runTests isTest "encoderUnion" modul decls generatedName generated union (unionAsModule cname unionName union)

    generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars unionName
    cname = Module.Canonical pkg (Src.getName modul)
    tvars = _u_vars union
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars tvar )

    generated =
      Def
        (a (generatedName))
        (ptvars ++ [ pvar "w2v" ])
        (caseof (lvar "w2v") $
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
                          encodeTypeValue ifaces cname paramType (lvar (Data.Name.fromChars $ "v" ++ show i))
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
    !x = runTests isTest "decoderUnion" modul decls generatedName generated union (unionAsModule cname unionName union)

    generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars unionName
    cname = Module.Canonical pkg (Src.getName modul)
    tvars = _u_vars union
    tvarsTypesig = tvars & foldl (\acc name -> Map.insert name () acc ) Map.empty
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars tvar )
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
        ptvars
        (decodeUnsignedInt8 |> andThenDecode1
              (lambda1 (pvar "w2v") $
                caseof (lvar "w2v") $
                  _u_alts union
                    & List.sortOn (\(Ctor name index_ numParams paramTypes) -> name)
                    & imap (\i (Ctor tagName tagIndex numParams paramTypes) ->
                        (pint i) –>
                          ([(succeedDecode (vctor tagName tagIndex paramTypes))]
                          ++ fmap (\paramType -> andMapDecode1 ((decoderForType ifaces cname paramType))) paramTypes)
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


encoderAlias :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
encoderAlias isTest ifaces pkg modul decls aliasName alias@(Alias tvars tipe) =
  let
    !x = runTests isTest "encoderAlias" modul decls generatedName generated alias (aliasAsModule cname aliasName alias)

    generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars tvar )
    ltvars = tvars & fmap (\tvar -> lvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars tvar )

    generated = Def (a (generatedName)) ptvars $ deepEncoderForType ifaces cname tipe
  in
  generated


decoderAlias :: Bool -> Map.Map Module.Raw I.Interface -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
decoderAlias isTest ifaces pkg modul decls aliasName alias@(Alias tvars tipe) =
  let
    !x = runTests isTest "decoderAlias" modul decls generatedName generated alias (aliasAsModule cname aliasName alias)

    generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)
    ptvars = tvars & fmap (\tvar -> pvar $ Data.Name.fromChars $ "w2_x_c_" ++ Data.Name.toChars tvar )

    generated = Def (a (generatedName)) ptvars $ decoderForType ifaces cname tipe
  in
  generated


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


{- For debugging -}
declsToSummary :: Decls -> [(String, Data.Name.Name)]
declsToSummary d =
  case d of
    Declare def decls ->
      ("Declare", defName def) : (declsToSummary decls)

    DeclareRec def defs decls ->
      let defs_ = fmap (\d -> ("-> DeclareRecSub", defName d)) defs
      in
      (("DeclareRec", defName def) : defs_) ++ declsToSummary decls

    SaveTheEnvironment ->
      []

addDef :: Def -> Decls -> Decls
addDef def_ decls_ =
  case decls_ of
    Declare def decls ->
      Declare def_ (Declare def decls)

    DeclareRec def defs decls ->
      Declare def_ (DeclareRec def defs decls)

    SaveTheEnvironment ->
      Declare def_ SaveTheEnvironment


addRecDef :: [Def] -> Decls -> Decls
addRecDef (def_:defs) decls_ =
  case decls_ of
    Declare def decls ->
      DeclareRec def_ defs (Declare def decls)

    DeclareRec def defs decls ->
      DeclareRec def_ defs (DeclareRec def defs decls)

    SaveTheEnvironment ->
      DeclareRec def_ defs SaveTheEnvironment

sameName :: Def -> Def -> Bool
sameName d1 d2 =
  defName d1 == defName d2

findDef :: Data.Name.Name -> Decls -> Maybe Def
findDef name decls =
  decls
    & declsToList
    & List.find (defNameIs name)

defNameIs :: Data.Name.Name -> Def -> Bool
defNameIs name def =
  name == defName def

defName :: Def -> Data.Name.Name
defName def =
  case def of
    Def (A.At region name_) _ _ ->
      name_
    TypedDef (A.At region name_) _ _ _ _ ->
      name_
