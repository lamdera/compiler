{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Lamdera.Wire.Helpers where

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map
import Data.Map ((!?))
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


shouldHaveCodecsGenerated :: Elm.Package.Name -> Bool
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
    Name "elm" "time" -> True -- @TODO REMOVE
    Name "author" "project" -> True -- @TODO REMOVE
    _ -> True -- @TODO REMOVE



{- NOTE: Any recursive usage of these types in user-code will get caught in the TypeDiff,
the mapping there has been checked extensively against types in packages that are backed by Kernel.

But we still need to know about them in order to create the right wire encoder/decoder injections
-}
isUnsupportedKernelType tipe =
  case tipe of

  -- Unconstructable
    TType (Module.Canonical (Name "elm" "core") "Basics") "Never" _ -> True

  -- Types backed by JS values
    TType (Module.Canonical (Name "elm" "core") "Task") "Task" _ -> True
    TType (Module.Canonical (Name "elm" "core") "Process") "Id" _ -> True -- alias of Platform.ProcessId
    TType (Module.Canonical (Name "elm" "core") "Platform") "ProcessId" _ -> True
    TType (Module.Canonical (Name "elm" "core") "Platform") "Program" _ -> True
    TType (Module.Canonical (Name "elm" "core") "Platform") "Router" _ -> True
    TType (Module.Canonical (Name "elm" "core") "Platform") "Task" _ -> True
    TType (Module.Canonical (Name "elm" "core") "Platform.Cmd") "Cmd" _ -> True
    TType (Module.Canonical (Name "elm" "core") "Platform.Sub") "Sub" _ -> True

    TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" _ -> True
    TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" _ -> True

    TType (Module.Canonical (Name "elm" "virtualdom") "VirtualDom") "Node" _ -> True
    TType (Module.Canonical (Name "elm" "virtualdom") "VirtualDom") "Attribute" _ -> True
    TType (Module.Canonical (Name "elm" "virtualdom") "VirtualDom") "Handler" _ -> True

    TType (Module.Canonical (Name "elm" "file") "File") "File" _ -> True

    TType (Module.Canonical (Name "elm" "json") "Json.Encode") "Value" _ -> True -- js type
    TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" _ -> True -- js type
    TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Value" _ -> True -- js type

    -- Disable for now, but need to revisit these and whether we want actual proper wire support
    -- , (("elm/browser", "Browser.Navigation") "Key" _ -> True -- This is a JS backed value

    _ -> False



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
addRecDef (def_:defs_) decls_ =
  case decls_ of
    Declare def decls ->
      DeclareRec def_ defs_ (Declare def decls)

    DeclareRec def defs decls ->
      DeclareRec def_ defs_ (DeclareRec def defs decls)

    SaveTheEnvironment ->
      DeclareRec def_ defs_ SaveTheEnvironment


removeDef :: Def -> Decls -> Decls
removeDef def_ decls_ =
  case decls_ of
    Declare def decls ->
      if (sameName def def_) then
        decls
      else
        Declare def (removeDef def_ decls)

    DeclareRec def defs decls ->
      if (sameName def def_) then
        decls
      else
        DeclareRec def (List.deleteBy sameName def_ defs) (removeDef def_ decls)

    SaveTheEnvironment ->
      SaveTheEnvironment


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


foreignTypeTvars :: Module.Raw -> Data.Name.Name -> Map.Map Module.Raw I.Interface -> [Data.Name.Name]
foreignTypeTvars module_ typeName ifaces =
  case ifaces & Map.lookup module_ of
    Just iface ->
      case I._unions iface !? typeName of
        Just union -> unionTvars union
        Nothing ->
          case I._aliases iface !? typeName of
            Just alias -> aliasTvars alias
            Nothing -> []

    Nothing ->
      []


unionTvars union =
  case union of
    I.OpenUnion union_ -> _u_vars union_
    I.ClosedUnion union_ -> _u_vars union_
    I.PrivateUnion union_ -> _u_vars union_


aliasTvars alias =
  case alias of
    I.PublicAlias (Alias tvars tipe) -> tvars
    I.PrivateAlias (Alias tvars tipe) -> tvars


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


decodeBytes =
 (a (VarForeign mLamdera_Wire2 "decodeBytes"
       (Forall
          Map.empty
          (TAlias
             mLamdera_Wire2
             "Decoder"
             [("a", TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Bytes" [])]
             (Filled
                (TType
                   (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                   "Decoder"
                   [TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Bytes" []]))))))


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

failEncode =
   (a (VarForeign
         (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
         "failEncode"
         (Forall
            (Map.fromList [("a", ())])
            (TLambda
               (TVar "a")
               (TAlias
                  (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                  "Encoder"
                  []
                  (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))))))


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


-- Patterns

pint i =
  a (PInt i)

pvar n =
  a (PVar n)

p_ =
  a (PAnything)

call fn args =
  (a (Call fn args))


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

foldrPairs fn list =
  case list of
    [] -> error "Error: foldrPairs called with no items! Please report this with your code."
    x:[] -> x
    x:xs ->
      fn x (foldrPairs fn xs)

unwrapAliasesDeep :: Type -> Type
unwrapAliasesDeep t =
  case t of
    TLambda t1 t2 -> TLambda (unwrapAliasesDeep t1) (unwrapAliasesDeep t2)

    TVar name -> t

    TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [a] -> TType (Module.Canonical (Name "elm" "core") "Maybe") "Maybe" [unwrapAliasesDeep a]
    TType (Module.Canonical (Name "elm" "core") "List") "List" [a]   -> TType (Module.Canonical (Name "elm" "core") "List") "List" [unwrapAliasesDeep a]
    TType (Module.Canonical (Name "elm" "core") "Set") "Set" [a]     -> TType (Module.Canonical (Name "elm" "core") "Set") "Set" [unwrapAliasesDeep a]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> TType (Module.Canonical (Name "elm" "core") "Array") "Array" [unwrapAliasesDeep a]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      TType (Module.Canonical (Name "elm" "core") "Result") "Result" [unwrapAliasesDeep err, unwrapAliasesDeep a]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [unwrapAliasesDeep key, unwrapAliasesDeep val]

    TType moduleName typeName params ->
      -- t -- @TODO wrong to not de-alias params?
      TType moduleName typeName (fmap unwrapAliasesDeep params)

    TRecord fieldMap maybeName ->
      fieldMap
        & fmap (\(FieldType index tipe) ->
            FieldType index (unwrapAliasesDeep tipe)
          )
        -- @EXTENSIBLERECORDS For now we don't support extensible records, so drop the maybeExtensible
        & (\newFieldMap -> TRecord newFieldMap Nothing )

    TUnit -> t

    TTuple a b Nothing  -> TTuple (unwrapAliasesDeep a) (unwrapAliasesDeep b) Nothing
    TTuple a b (Just c) -> TTuple (unwrapAliasesDeep a) (unwrapAliasesDeep b) (Just $ unwrapAliasesDeep c)

    TAlias moduleName typeName tvars (Holey tipe) -> unwrapAliasesDeep tipe
    TAlias moduleName typeName tvars (Filled tipe) -> unwrapAliasesDeep tipe


resolveTvars :: [(Data.Name.Name, Type)] -> Type -> Type
resolveTvars tvarMap t =
  case t of
    TLambda t1 t2 -> TLambda (resolveTvars tvarMap t1) (resolveTvars tvarMap t2)

    TVar a ->
      case List.find (\(t,ti) -> t == a) tvarMap of
        Just (_,ti) -> ti
          -- case ti of
          --   -- If we looked up the Tvar and got another Tvar, we've got a tvar
          --   -- that's not specific higher up, so leave it as a tvar, but with
          --   -- the generalised name that's come down.
          --   TVar b -> TVar b
          --   _ -> ti
        Nothing -> TVar a

    TType modul typename tvars ->
      tvars
        & fmap (resolveTvars tvarMap)
        & TType modul typename

    TRecord fieldMap maybeExtensible ->
      case maybeExtensible of
        Nothing ->
          fieldMap
            & fmap (\(FieldType index tipe) ->
                FieldType index (resolveTvars tvarMap tipe)
              )
            -- @EXTENSIBLERECORDS For now we don't support extensible records, so drop the maybeExtensible
            & (\newFieldMap -> TRecord newFieldMap Nothing )
        Just extensibleName ->
          case resolveTvars tvarMap (TVar extensibleName) of
            TRecord fieldMapExtensible _ ->
              fieldMap
                & Map.union fieldMapExtensible
                & fmap (\(FieldType index tipe) ->
                    FieldType index (resolveTvars tvarMap tipe)
                  )
                -- @EXTENSIBLERECORDS For now we don't support extensible records, so drop the maybeExtensible
                & (\newFieldMap -> TRecord newFieldMap Nothing )

            rt -> error $ "resolveTvars: impossible extensible record with non-record type: " ++ show maybeExtensible ++ "\n\n" ++ show rt


    TUnit -> t

    TTuple a b Nothing  -> TTuple (resolveTvars tvarMap a) (resolveTvars tvarMap b) Nothing
    TTuple a b (Just c) -> TTuple (resolveTvars tvarMap a) (resolveTvars tvarMap b) (Just $ resolveTvars tvarMap c)

    TAlias moduleName typeName tvars (Holey tipe) ->
      let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvars tvarMap t))
      in TAlias moduleName typeName newResolvedTvars (Filled $ resolveTvars newResolvedTvars tipe)

    TAlias moduleName typeName tvars (Filled tipe) ->
      let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvars tvarMap t))
      in TAlias moduleName typeName newResolvedTvars (Filled $ resolveTvars newResolvedTvars tipe)


resolveTvarRenames tvars tvarNames =
  tvarNames
    & fmap (\tvarName ->
      case List.find (\(tvarName_,tvarType) -> tvarName_ == tvarName) tvars of
        Just (_,tvarType) ->
          case tvarType of
            -- If we looked up the Tvar and got another Tvar, we've got a tvar
            -- that's not specific higher up, but has been renamed by the parent
            -- context, so we rename our ForAll clause and thus all the params
            -- that reference back to it
            TVar newName -> newName
            _ -> tvarName
        Nothing -> tvarName
      )


extractTvarsInTvars tvars =
  tvars
    & concatMap (\(tvarName,tvarType) ->
      extractTvarsInTvars_ tvarType
    )

extractTvarsInTvars_ t =
  case t of
    TLambda t1 t2 -> [t1,t2] & concatMap extractTvarsInTvars_
    TVar a -> [a]
    TType modul typename tvars -> tvars & concatMap extractTvarsInTvars_

    TRecord fieldMap maybeExtensible ->
      fieldMap
        & concatMap (\(FieldType index tipe) ->
            extractTvarsInTvars_ tipe
          )

    TUnit -> []

    TTuple a b Nothing  -> [a,b] & concatMap extractTvarsInTvars_
    TTuple a b (Just c) -> [a,b,c] & concatMap extractTvarsInTvars_

    TAlias moduleName typeName tvars (Holey tipe) -> extractTvarsInTvars_ tipe
    TAlias moduleName typeName tvars (Filled tipe) -> extractTvarsInTvars_ tipe
