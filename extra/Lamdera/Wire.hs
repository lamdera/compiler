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
              debugPassText ("💚 actual implementation pretty-printed " <> show_ (Src.getName modul)) (ToSource.convert generated) (pure ())
              debugPassText ("🧡 expected implementation pretty-printed " <> show_ (Src.getName modul)) (Source2.generateCodecs Map.empty wire2gen) (pure ())
              debugHaskellPass ("🧡 expected implementation AST.Canonical " <> show_ (Src.getName modul)) (testDefinition) (pure ())

              diff <- icdiff (hindentFormatValue testDefinition) (hindentFormatValue generated)
              diff2 <- icdiff (ToSource.convert testDefinition) (ToSource.convert generated)

              atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff
              atomicPutStrLn $ "❌❌❌ failed, attempting pretty-print diff:\n" ++ diff2
              error "exiting!"
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
    --       debugHaskellPass "decls summary before" (declsToSummary decls_) (pure ())
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
        & fmap (\(k, v) ->
            [ (encoderUnion isTest pkg modul decls_ k v)
            , (decoderUnion isTest pkg modul decls_ k v)
            ]
        )
        & concat

    aliasDefs =
      (Can._aliases canonical)
        & Map.toList
        & fmap (\(k, v) ->
            [ (encoderAlias isTest pkg modul decls_ k v)
            , (decoderAlias isTest pkg modul decls_ k v)
            ]
        )
        & concat

    ordered =
      declsToList decls_
        & List.unionBy (\a b -> defName a == defName b) (unionDefs ++ aliasDefs)
        & fmap defToNode
        & Graph.stronglyConnComp
        & foldr (\scc decls ->
          case scc of
            Graph.AcyclicSCC def ->
              addDef def decls
            Graph.CyclicSCC defs ->
              addRecDef defs decls

        ) SaveTheEnvironment
  in
  Right $ canonical { _decls = ordered }


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


encoderUnion :: Bool -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
encoderUnion isTest pkg modul decls unionName union =
  let
    !x = runTests isTest "encoderUnion" modul decls generatedName generated union (unionAsModule cname unionName union)

    generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars unionName
    cname = Module.Canonical pkg (Src.getName modul)

    generated =
      Def
        (a (generatedName))
        [ pvar "w2v" ]
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
                          encodeTypeValue cname paramType (lvar (Data.Name.fromChars $ "v" ++ show i))
                      )
                  in
                  CaseBranch
                    (pctorUnion cname unionName union tagName tagIndex params)
                    (encodeSequenceWithoutLength $ list $ [ encodeUnsignedInt8 (int i) ] ++ paramEncoders)
              )
            )
  in
  generated


decoderUnion :: Bool -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Union -> Def
decoderUnion isTest pkg modul decls unionName union =
  let
    !x = runTests isTest "decoderUnion" modul decls generatedName generated union (unionAsModule cname unionName union)

    generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars unionName
    cname = Module.Canonical pkg (Src.getName modul)
    unionType = TType cname unionName []

    generated =
      Def
      -- TypedDef
        (a (generatedName))
        -- Map.empty
        []
        (decodeUnsignedInt8 |> andThenDecode1
              (lambda1 (pvar "w2v") $
                caseof (lvar "w2v") $
                  _u_alts union
                    & List.sortOn (\(Ctor name index_ numParams paramTypes) -> name)
                    & imap (\i (Ctor tagName tagIndex numParams paramTypes) ->
                        (pint i) –>
                          ([(succeedDecode (vctor cname unionName union tagName tagIndex paramTypes))]
                          ++ fmap (\paramType -> andMapDecode1 ((decoderForType cname paramType))) paramTypes)
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


encoderAlias :: Bool -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
encoderAlias isTest pkg modul decls aliasName alias =
  let
    !x = runTests isTest "encoderAlias" modul decls generatedName generated alias (aliasAsModule cname aliasName alias)

    generatedName = Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)

    generated_ (Alias tvars tipe) = -- @TODO tvars probs shouldn't be ignored
      Def (a (generatedName)) [] (deepEncoderForType cname tipe)

    generated = generated_ alias
  in
  generated


decoderAlias :: Bool -> Pkg.Name -> Src.Module -> Decls -> Data.Name.Name -> Alias -> Def
decoderAlias isTest pkg modul decls aliasName alias =
  let
    !x = runTests isTest "decoderAlias" modul decls generatedName generated alias (aliasAsModule cname aliasName alias)

    generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars aliasName
    cname = Module.Canonical pkg (Src.getName modul)

    generated_ (Alias tvars tipe) = -- @TODO tvars probs shouldn't be ignored
      Def (a (generatedName)) [] (decoderForType cname tipe)

    generated = generated_ alias
  in
  generated


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


addDef def_ decls_ =
  case decls_ of
    Declare def decls ->
      Declare def_ (Declare def decls)

    DeclareRec def defs decls ->
      Declare def_ (DeclareRec def defs decls)

    SaveTheEnvironment ->
      Declare def_ SaveTheEnvironment


addRecDef (def_:defs) decls_ =
  case decls_ of
    Declare def decls ->
      DeclareRec def_ defs (Declare def decls)

    DeclareRec def defs decls ->
      DeclareRec def_ defs (DeclareRec def defs decls)

    SaveTheEnvironment ->
      DeclareRec def_ defs SaveTheEnvironment


upsertDef newDef decls_ =
  case findDef (defName newDef) decls_ of
    Just _ ->
      replaceDef newDef decls_

    Nothing ->
      -- Inject our definitions at the head, as we know they don't rely on
      -- anything else in the file except themselves, and this allows existing
      -- functions to reference them (potentially..? @TODO)
      DeclareRec newDef [] (decls_)


replaceDef newDef decls_ =
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
      -- @NOTE don't try to optimise to this, it breaks recursive def handling!
      -- Declare newDef SaveTheEnvironment


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
                 Map.empty
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
                   Map.empty
                   (TLambda
                      (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                      tLamdera_Wire2__Encoder))))
          [value]))


decodeUnsignedInt8 =
  (a (VarForeign mLamdera_Wire2 "decodeUnsignedInt8"
        (Forall
           Map.empty
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
            Map.empty
            constructorType
            )))


-- Patterns

pint i =
  a (PInt i)

pvar n =
  a (PVar n)

p_ =
  a (PAnything)


encoderForType cname tipe =
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

    TRecord fieldMap maybeName ->
      let
        fields = fieldsToList fieldMap
        fieldEncoders =
          fields
            & fmap (\(name, field) ->
                encodeTypeValue cname field (a (Access (a (VarLocal "w2_rec_var0")) (a (name))))
              )
      in
      (a (Lambda [(a (PVar "w2_rec_var0"))]
        (encodeSequenceWithoutLength $ list fieldEncoders)
      ))

    TType moduleName typeName params ->
      -- str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe
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

    _ ->
      -- error $ "Not yet implemented: " ++ show tipe
      str $ Utf8.fromChars $ "encoderForType not implemented! " ++ show tipe


deepEncoderForType cname tipe =
  case tipe of
    TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []    -> encoderForType cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []  -> encoderForType cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []   -> encoderForType cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []  -> encoderForType cname tipe
    TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []  -> encoderForType cname tipe
    TType (Module.Canonical (Name "elm" "core") "Char") "Char" []     -> encoderForType cname tipe
    TType (Module.Canonical (Name "elm" "core") "String") "String" [] -> encoderForType cname tipe
    TUnit                                                             -> encoderForType cname tipe

    TTuple a b Nothing  -> call (encoderForType cname tipe) [ deepEncoderForType cname a, deepEncoderForType cname b ]
    TTuple a b (Just c) -> call (encoderForType cname tipe) [ deepEncoderForType cname a, deepEncoderForType cname b, deepEncoderForType cname c ]

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [a] -> call (encoderForType cname tipe) [ deepEncoderForType cname a ]
    TType (Module.Canonical (Name "elm" "core") "List") "List" [a]   -> call (encoderForType cname tipe) [ deepEncoderForType cname a ]
    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [a]     -> call (encoderForType cname tipe) [ deepEncoderForType cname a ]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> call (encoderForType cname tipe) [ deepEncoderForType cname a ]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      call (encoderForType cname tipe) [ deepEncoderForType cname err, deepEncoderForType cname a ]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      call (encoderForType cname tipe) [ deepEncoderForType cname key, deepEncoderForType cname val ]

    TType (Module.Canonical (Name "elm" "core") _) _ _ ->
      str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe

    TType (Module.Canonical (Name "elm" "bytes") _) _ _ ->
      str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe

    TRecord fieldMap maybeName ->
      encoderForType cname tipe

    TType moduleName typeName params ->
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

    _ ->
      str $ Utf8.fromChars $ "deepEncoderForType not implemented! " ++ show tipe


encodeTypeValue cname tipe value =
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])    -> call (encoderForType cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" [])  -> call (encoderForType cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" [])   -> call (encoderForType cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" [])  -> call (encoderForType cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" [])  -> call (encoderForType cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" [])     -> call (encoderForType cname tipe) [ value ]
    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) -> call (encoderForType cname tipe) [ value ]
    TUnit                                                               -> call (encoderForType cname tipe) [ value ]

    TTuple a b Nothing  -> call (encoderForType cname tipe) [ deepEncoderForType cname a, deepEncoderForType cname b, value ]
    TTuple a b (Just c) -> call (encoderForType cname tipe) [ deepEncoderForType cname a, deepEncoderForType cname b, deepEncoderForType cname c, value ]

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [a] -> call (encoderForType cname tipe) [ deepEncoderForType cname a, value ]
    TType (Module.Canonical (Name "elm" "core") "List") "List" [a]   -> call (encoderForType cname tipe) [ deepEncoderForType cname a, value ]
    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [a]     -> call (encoderForType cname tipe) [ deepEncoderForType cname a, value ]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> call (encoderForType cname tipe) [ deepEncoderForType cname a, value ]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      call (encoderForType cname tipe) [ deepEncoderForType cname err, deepEncoderForType cname a, value ]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      call (encoderForType cname tipe) [ deepEncoderForType cname key, deepEncoderForType cname val, value ]

    TType (Module.Canonical (Name "elm" "core") _) _ _ ->
      str $ Utf8.fromChars $ "encodeTypeValue not implemented! " ++ show tipe

    TType (Module.Canonical (Name "elm" "bytes") _) _ _ ->
      str $ Utf8.fromChars $ "encodeTypeValue not implemented! " ++ show tipe

    TType moduleName typeName params ->
      call (encoderForType cname tipe) $ fmap (deepEncoderForType cname) params ++ [ value ]

    _ ->
      -- error $ "Not yet implemented: " ++ show tipe
      str $ Utf8.fromChars $ "encodeTypeValue not implemented! " ++ show tipe



call fn args =
  (a (Call fn args))

decoderForType cname tipe =
  case tipe of
    (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeInt" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Float" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeFloat" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Bool" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeBool" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Order" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeOrder" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Basics") "Never" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeNever" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "Char") "Char" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeChar" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    (TType (Module.Canonical (Name "elm" "core") "String") "String" []) ->
      (a (VarForeign mLamdera_Wire2 "decodeString" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

    TUnit ->
      (a (VarForeign mLamdera_Wire2 "decodeUnit" (Forall Map.empty (TAlias mLamdera_Wire2 "Decoder" [("a", tipe)] (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]))))))

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
              [ decoderForType cname a_
              , decoderForType cname b
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
             [ decoderForType cname a_
             , decoderForType cname b
             , decoderForType cname c
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
        [ decoderForType cname ptype ]))

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
        [ decoderForType cname ptype ]))

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
        [ decoderForType cname ptype ]))

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
        [ decoderForType cname ptype ]))

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
              [ decoderForType cname err
              , decoderForType cname a_
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
              [ decoderForType cname key
              , decoderForType cname val
              ]))

    TType (Module.Canonical (Name "elm" "core") _) _ _ ->
      str $ Utf8.fromChars $ "decoder not implemented! " ++ show tipe

    TType (Module.Canonical (Name "elm" "bytes") _) _ _ ->
      str $ Utf8.fromChars $ "decoder not implemented! " ++ show tipe

    TRecord fieldMap maybeName ->
    -- | TRecord (Map.Map Name FieldType) (Maybe Name)
      let
        fields = fieldsToList fieldMap

        pvars :: [Pattern]
        pvars =
          (imap (\i (name, field) -> a (PVar $ Data.Name.fromChars $ Data.Name.toChars name ++ "0")) fields)
          -- (imap (\i (name, field) -> a (PVar $ Data.Name.fromChars $ "w_f" ++ show i)) fields) -- @TODO improve gen

        newRecFields :: Map.Map Data.Name.Name Expr
        newRecFields =
          fields
            & fmap (\(name, field) ->
                (name, a (VarLocal $ Data.Name.fromChars $ Data.Name.toChars name ++ "0"))
                -- (name, a (VarLocal $ Data.Name.fromChars $ "w_f" ++ show i)) -- @TODO improve gen
              )
            & Map.fromList
      in
      [succeedDecode (a (Lambda pvars (a (Record newRecFields ))))]
      ++ fmap (\(name, field) -> andMapDecode1 (decoderForType cname field)) fields
        & foldlPairs (|>)

    TType moduleName typeName params ->
      let
        generatedName = Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars typeName

        decoder =
          if cname == moduleName
            -- Referenced type is defined in the current module
            then (a (VarTopLevel moduleName generatedName))
            -- Referenced type is defined in another module
            else
              (a (VarForeign moduleName generatedName
                (Forall Map.empty
                  (TAlias
                    (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                    "Decoder"
                    [("a", tipe)]

                    -- @TODO what is the differentiator for using one vs other here?
                    -- (Holey (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"]))))
                    (Filled
                        (TType
                           (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                           "Decoder"
                           [tipe]))))
              ))

      in
      if length params == 0
        then
          decoder
        else
          call decoder $ fmap (decoderForType cname) params

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


foldlPairs fn list =
  case list of
    [] -> error "Error: foldlPairs called with no items! Please report this with your code."
    x:[] -> x
    x:xs ->
      foldl (\acc item -> fn acc item ) x xs
