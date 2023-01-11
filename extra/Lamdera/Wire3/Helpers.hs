{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Lamdera.Wire3.Helpers where

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
import qualified Elm.String as ES

import Lamdera
import StandaloneInstances
import qualified CanSer.CanSer as ToSource


shouldHaveCodecsGenerated :: Elm.Package.Name -> Bool
shouldHaveCodecsGenerated name =
  case name of
    -- Some elm packages are ignored because of cyclic dependencies.
    -- Those codecs have to be manually defined in `lamdera/codecs`.
    -- All other packages, even if their types are defined in js, have codecs generated for their types.
    -- Then we manually override specific types in `Lamdera.Wire.[En|De]coder`.

    -- Elm deps used by lamdera/codecs
    Name "elm" "bytes" -> False
    Name "elm" "core" -> False

    -- Avoid cyclic imports; generated codecs rely on lamdera/codecs:Lamdera.Wire. This is our codec bootstrap module.
    Name "lamdera" "codecs" -> False

    Name "mdgriffith" "style-elements" -> False

    -- Everything else should have codecs generated
    _ -> True


getForeignSig tipe moduleName generatedName ifaces =
  -- debugHaskell (T.pack $ "❎❎❎❎❎ ALIAS ENCODER foreignTypeSig for " ++ (Data.Name.toChars generatedName)) $
    case foreignTypeSig moduleName generatedName ifaces of
      Just (Forall freeVars tipe_) ->
        let
          extractedFreevars = extractTvarsInType tipe_ & fmap (\t -> (t, ())) & Map.fromList
        in
        Forall (Map.union freeVars extractedFreevars) tipe_
      Nothing ->
        -- If a foreign type gen function cannot be find, it must be banned!
        -- So add type-sig for failure encoder or decoder as appropriate.
        if T.isPrefixOf "w3_encode_" (T.pack $ Data.Name.toChars generatedName)
          then
            (Forall
               (Map.fromList [("a", ())])
               (TLambda (TVar "a") tLamdera_Wire_Encoder))

          else if T.isPrefixOf "w3_decode_" (T.pack $ Data.Name.toChars generatedName)
            then
              (Forall
                 (Map.fromList [("a", ())])
                 (TAlias
                    mLamdera_Wire
                    "Decoder"
                    [("a", TVar "a")]
                    (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"]))))
            else
              error $ "impossible getForeignSig on non-wire function: " ++ Data.Name.toChars generatedName



{- NOTE: Any recursive usage of these types in user-code will get caught in the TypeHash first,
the mapping there has been checked extensively against types in packages that are backed by Kernel.

But we still need to know about them in order to create the right wire encoder/decoder injections
-}
isUnsupportedKernelType :: Type -> Bool
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

    TType (Module.Canonical (Name "elm" "json") "Json.Encode") "Value" _ -> True -- js type
    TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" _ -> True -- js type
    TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Value" _ -> True -- js type


    -- JS types we are supporting through JS ref encodings. These serialisations
    -- CANNOT BE DECODED OUTSIDE OF THE JS SCOPE THEY WERE ENCODED IN!
    TType (Module.Canonical (Name "elm" "file") "File") "File" _ -> False


    TAlias moduleName typeName tvars (Holey tipe) -> isUnsupportedKernelType tipe
    TAlias moduleName typeName tvars (Filled tipe) -> isUnsupportedKernelType tipe

    -- Disable for now, but need to revisit these and whether we want actual proper wire support
    -- , (("elm/browser", "Browser.Navigation") "Key" _ -> True -- This is a JS backed value

    _ -> False


containsUnsupportedTypes :: Type -> Bool
containsUnsupportedTypes tipe =
  case tipe of
    TRecord fieldMap maybeExtensible ->
      fieldMap
        & fieldsToList
        & any (\(name, field) ->
          containsUnsupportedTypes field
        )

    TLambda t1 t2 -> True

    _ -> isUnsupportedKernelType tipe


resolvedRecordFieldMap :: (Map.Map Data.Name.Name FieldType) -> (Maybe Data.Name.Name) -> [(Data.Name.Name, Type)] -> (Map.Map Data.Name.Name FieldType)
resolvedRecordFieldMap fieldMap extensibleName tvarMap =
  case resolvedRecordFieldMapM fieldMap extensibleName tvarMap of
    Just extendedFieldMap -> extendedFieldMap
    Nothing -> fieldMap


resolvedRecordFieldMapM :: (Map.Map Data.Name.Name FieldType) -> (Maybe Data.Name.Name) -> [(Data.Name.Name, Type)] -> Maybe (Map.Map Data.Name.Name FieldType)
resolvedRecordFieldMapM fieldMap extensibleName tvarMap =
  case extensibleName of
    Just extensibleName ->
      case List.find (\(n,_) -> n == extensibleName) tvarMap of
        Just (_, extensibleType) ->
          case extensibleType of
            TRecord fieldMapExtended maybeNameExtended ->
              Just $ fieldMap <> fieldMapExtended

            TAlias moduleName typeName tvars (Holey tipe) -> do
              let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
              case resolveTvar newResolvedTvars tipe of
                TRecord fieldMapExtensible _ ->
                  fieldMap
                    & Map.union fieldMapExtensible
                    & fmap (\(FieldType index tipe) ->
                        FieldType index (resolveTvar newResolvedTvars tipe)
                      )
                    & Just
                _ -> Nothing

            TAlias moduleName typeName tvars (Filled tipe) -> do
              let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
              case resolveTvar newResolvedTvars tipe of
                TRecord fieldMapExtensible _ ->
                  fieldMap
                    & Map.union fieldMapExtensible
                    & fmap (\(FieldType index tipe) ->
                        FieldType index (resolveTvar newResolvedTvars tipe)
                      )
                    & Just
                _ -> Nothing

            {-|
              This should be impossible, but can happen with code like this:

              type alias Record a =
                  { a | field : a }

              type alias Blah =
                  Record Int

              And it seems Elm's type checker doesn't pick it up.

            -}
            _ -> Nothing

        -- No tvar found for extensible record with extensible name...
        Nothing -> Nothing

    -- Not an extensible record
    Nothing -> Nothing


resolveFieldMap tipe tvarMap =
  case tipe of
    TRecord fieldMapExtended maybeNameExtended ->
      Just $ fieldMapExtended

    TAlias moduleName typeName tvars (Holey tipe) -> do
      let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
      case resolveTvar newResolvedTvars tipe of
        TRecord fieldMapExtensible _ -> Just fieldMapExtensible
        _ -> Nothing

    TAlias moduleName typeName tvars (Filled tipe) -> do
      let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
      case resolveTvar newResolvedTvars tipe of
        TRecord fieldMapExtensible _ -> Just fieldMapExtensible
        _ -> Nothing

    {-|
      This should be impossible, but can happen with code like this:

      type alias Record a =
          { a | field : a }

      type alias Blah =
          Record Int

      And it seems Elm's type checker doesn't pick it up.

    -}
    _ -> Nothing


extractFieldMap tipe =
  case tipe of
    TRecord fieldMapExtended maybeNameExtended ->
      fieldMapExtended
    TAlias moduleName typeName tvars (Holey tipe) -> do
      extractFieldMap tipe
    TAlias moduleName typeName tvars (Filled tipe) -> do
      extractFieldMap tipe
    _ -> Map.empty

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


foreignTypeSig :: Module.Canonical -> Data.Name.Name -> Map.Map Module.Raw I.Interface -> Maybe Can.Annotation
foreignTypeSig (Module.Canonical pkg (moduleRaw)) defName ifaces =
  case ifaces & Map.lookup moduleRaw of
    Just iface ->
      I._values iface !? defName

    Nothing ->
      Nothing


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
  (a (Call (a (VarForeign mLamdera_Wire "encodeSequenceWithoutLength"
              (Forall
                 Map.empty
                 (TLambda
                    (TType
                       (Module.Canonical (Name "elm" "core") "List")
                       "List"
                       [ tLamdera_Wire_Encoder
                       ])
                    tLamdera_Wire_Encoder))))
        [list]))


encodeUnsignedInt8 value =
  (a (Call (a (VarForeign mLamdera_Wire "encodeUnsignedInt8"
                (Forall
                   Map.empty
                   (TLambda
                      (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                      tLamdera_Wire_Encoder))))
          [value]))


decodeUnsignedInt8 =
  (a (VarForeign mLamdera_Wire "decodeUnsignedInt8"
        (Forall
           Map.empty
           (TAlias
              mLamdera_Wire
              "Decoder"
              [("a", TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])]
              (Filled
                 (TType
                    (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                    "Decoder"
                    [TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []]))))))




decodeTime =
  (a (Binop
    "|>"
    (Module.Canonical (Name "elm" "core") "Basics")
    "apR"
    (Forall
       (Map.fromList [("a", ()), ("b", ())])
       (TLambda (TVar "a") (TLambda (TLambda (TVar "a") (TVar "b")) (TVar "b"))))
    (a (VarForeign
          mLamdera_Wire
          "decodeInt"
          (Forall
             (Map.fromList [])
             (TAlias
                mLamdera_Wire
                "Decoder"
                [("a", TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])]
                (Filled
                   (TType
                      (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                      "Decoder"
                      [TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []]))))))
    (andThenDecode1
           (a (Lambda
                  [(a (PVar "t"))]
                  (a (Call
                        (a (VarForeign
                              mLamdera_Wire
                              "succeedDecode"
                              (Forall
                                 (Map.fromList [("a", ())])
                                 (TLambda
                                    (TVar "a")
                                    (TAlias
                                       mLamdera_Wire
                                       "Decoder"
                                       [("a", TVar "a")]
                                       (Filled
                                          (TType
                                             (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                             "Decoder"
                                             [TVar "a"])))))))
                        [ (a (Call
                                (a (VarForeign
                                      (Module.Canonical (Name "elm" "time") "Time")
                                      "millisToPosix"
                                      (Forall
                                         (Map.fromList [])
                                         (TLambda
                                            (TType
                                               (Module.Canonical (Name "elm" "core") "Basics")
                                               "Int"
                                               [])
                                            (TType
                                               (Module.Canonical (Name "elm" "time") "Time")
                                               "Posix"
                                               [])))))
                                [(a (VarLocal "t"))]))
                        ]))))
          )))



andThenDecode1 lambda =
  (a (Call (a (VarForeign mLamdera_Wire "andThenDecode"
              (Forall
                 (Map.fromList [("a", ()), ("b", ())])
                 (TLambda
                    (TLambda
                       (TVar "a")
                       (TAlias
                          mLamdera_Wire
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))
                    (TLambda
                       (TAlias
                          mLamdera_Wire
                          "Decoder"
                          [("a", TVar "a")]
                          (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                       (TAlias
                          mLamdera_Wire
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))))))
        [ lambda
        ]))


andMapDecode1 value =
  (a (Call
        (a (VarForeign
              mLamdera_Wire
              "andMapDecode"
              (Forall
                 (Map.fromList [("a", ()), ("b", ())])
                 (TLambda
                    (TAlias
                       mLamdera_Wire
                       "Decoder"
                       [("a", TVar "a")]
                       (Filled
                          (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                    (TLambda
                       (TAlias
                          mLamdera_Wire
                          "Decoder"
                          [("a", TLambda (TVar "a") (TVar "b"))]
                          (Filled
                             (TType
                                (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                "Decoder"
                                [TLambda (TVar "a") (TVar "b")])))
                       (TAlias
                          mLamdera_Wire
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
  (a (Call (a (VarForeign mLamdera_Wire "succeedDecode"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TVar "a")
                    (TAlias
                       mLamdera_Wire
                       "Decoder"
                       [("a", TVar "a")]
                       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))))))
        [ value
        ]))


failDecode identifier =
  -- debugDecoder (Utf8.fromChars $ "failDecode:" ++ identifier) $
  (a (VarForeign mLamdera_Wire "failDecode"
        (Forall
           (Map.fromList [("a", ())])
           (TAlias
              mLamdera_Wire
              "Decoder"
              [("a", TVar "a")]
              (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"]))))))


failEncode =
   (a (VarForeign
         mLamdera_Wire
         "failEncode"
         (Forall
            (Map.fromList [("a", ())])
            (TLambda (TVar "a") tLamdera_Wire_Encoder))))


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

pAny_ =
  a (PAnything)

call fn args =
  (a (Call fn args))


tLamdera_Wire_Encoder =
  (TAlias
     mLamdera_Wire
     "Encoder"
     []
     (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))

-- @TODO What's the difference between the Filled and Holey variants? Why does Elm infer
-- this type as Holey only in the context of wire encoder type signatures?
tLamdera_Wire_Encoder_Holey =
  (TAlias
     mLamdera_Wire
     "Encoder"
     []
     (Holey (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))


mLamdera_Wire =
  (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire3")


foldlPairs fn list =
  case list of
    [] -> error "⚠️  Error: foldlPairs called with no items! Please report this with your code."
    x:[] -> x
    x:xs ->
      foldl (\acc item -> fn acc item ) x xs

foldrPairs fn list =
  case list of
    [] -> error "⚠️  Error: foldrPairs called with no items! Please report this with your code."
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
        -- Responsibility of caller to flatten extended records, so unwrapping can drop
        & (\newFieldMap -> TRecord newFieldMap Nothing )

    TUnit -> t

    TTuple a b Nothing  -> TTuple (unwrapAliasesDeep a) (unwrapAliasesDeep b) Nothing
    TTuple a b (Just c) -> TTuple (unwrapAliasesDeep a) (unwrapAliasesDeep b) (Just $ unwrapAliasesDeep c)

    TAlias moduleName typeName tvars (Holey tipe) -> unwrapAliasesDeep tipe
    TAlias moduleName typeName tvars (Filled tipe) -> unwrapAliasesDeep tipe


resolveTvar :: [(Data.Name.Name, Type)] -> Type -> Type
resolveTvar tvarMap t =
  case t of
    TLambda t1 t2 -> TLambda (resolveTvar tvarMap t1) (resolveTvar tvarMap t2)

    TVar a ->
      case List.find (\(t,ti) -> t == a) tvarMap of
        Just (tvarName,tvarType) ->

          case extractTvarsInTvars [(tvarName,tvarType)] of
            [] -> tvarType
            _ ->
              {- The var we looked up itself has tvars. This means this particular function we're dealing with
                 is used in a generic sense relative to other tvars higher up. In our current context, being
                 'resolving tvars in type signatures for VarForeign calls', we thus have no need to specialise.
                 See the Wire_Tvar_Ambiguous.elm test for the example, in particular:

                 CustomTypeDefinition (Test.Wire_Tvar_Ambiguous2.AccessControlled (Maybe a))
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                 Notice the underlined alias takes a type param of `a` – but the given param here itself is `Maybe a`,
                 so the type signature we want to inject for the Test.Wire_Tvar_Ambiguous2.AccessControlled decoder
                 should just be `Decoder (AccessControlled a)` - not `Decoder (AccessControlled (Maybe a))`.

                 TLDR: no need to specialise this one, leave it generic! -}
              t

          -- case ti of
          --   -- If we looked up the Tvar and got another Tvar, we've got a tvar
          --   -- that's not specific higher up, so leave it as a tvar, but with
          --   -- the generalised name that's come down.
          --   TVar b -> TVar b
          --   _ -> ti
        Nothing -> TVar a

    TType modul typename tvars ->
      tvars
        & fmap (resolveTvar tvarMap)
        & TType modul typename

    TRecord fieldMap maybeExtensible ->
      case maybeExtensible of
        Nothing ->
          fieldMap
            & fmap (\(FieldType index tipe) ->
                FieldType index (resolveTvar tvarMap tipe)
              )
            & (\newFieldMap -> TRecord newFieldMap Nothing )
        Just extensibleName -> do
          let resolvedExtension = resolveTvar tvarMap (TVar extensibleName)
          case resolvedExtension of
            TRecord fieldMapExtensible _ ->
              fieldMap
                & Map.union fieldMapExtensible
                & fmap (\(FieldType index tipe) ->
                    FieldType index (resolveTvar tvarMap tipe)
                  )
                -- Now the extensible record has been reified, we can drop the extensible part
                & (\newFieldMap -> TRecord newFieldMap Nothing )

            TAlias moduleName typeName tvars (Holey tipe) -> do
              let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
              case resolveTvar newResolvedTvars tipe of
                TRecord fieldMapExtensible _ ->
                  fieldMap
                    & Map.union fieldMapExtensible
                    & fmap (\(FieldType index tipe) ->
                        FieldType index (resolveTvar newResolvedTvars tipe)
                      )
                    -- Now the extensible record has been reified, we can drop the extensible part
                    & (\newFieldMap -> TRecord newFieldMap Nothing )
                _ -> error $ "bad nesting Holey" ++ show tipe

            TAlias moduleName typeName tvars (Filled tipe) -> do
              let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
              case resolveTvar newResolvedTvars tipe of
                TRecord fieldMapExtensible _ ->
                  fieldMap
                    & Map.union fieldMapExtensible
                    & fmap (\(FieldType index tipe) ->
                        FieldType index (resolveTvar newResolvedTvars tipe)
                      )
                    -- Now the extensible record has been reified, we can drop the extensible part
                    & (\newFieldMap -> TRecord newFieldMap Nothing )
                _ -> error $ "bad nesting Holey" ++ show tipe

            rt ->
              -- @TODO used to have this, but it caused a failure on `TVar a` for which an mcve was elusive...
              -- theory is that a chained tvar case must somehow be possible, perhaps in a compound type.
              -- error $ "resolveTvars: impossible extensible record with non-record type: " ++ show maybeExtensible ++ "\n\n" ++ show rt
              resolveTvar tvarMap rt


    TUnit -> t

    TTuple a b Nothing  -> TTuple (resolveTvar tvarMap a) (resolveTvar tvarMap b) Nothing
    TTuple a b (Just c) -> TTuple (resolveTvar tvarMap a) (resolveTvar tvarMap b) (Just $ resolveTvar tvarMap c)

    TAlias moduleName typeName tvars (Holey tipe) ->
      let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
      in TAlias moduleName typeName newResolvedTvars (Filled $ resolveTvar newResolvedTvars tipe)

    TAlias moduleName typeName tvars (Filled tipe) ->
      let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
      in TAlias moduleName typeName newResolvedTvars (Filled $ resolveTvar newResolvedTvars tipe)


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
      extractTvarsInType tvarType
    )

extractTvarsInType t =
  case t of
    TLambda t1 t2 -> [t1,t2] & concatMap extractTvarsInType
    TVar a -> [a]
    TType modul typename tvars -> tvars & concatMap extractTvarsInType

    TRecord fieldMap maybeExtensible ->
      fieldMap
        & concatMap (\(FieldType index tipe) ->
            extractTvarsInType tipe
          )

    TUnit -> []

    TTuple a b Nothing  -> [a,b] & concatMap extractTvarsInType
    TTuple a b (Just c) -> [a,b,c] & concatMap extractTvarsInType

    TAlias moduleName typeName tvars (Holey tipe) -> extractTvarsInType tipe ++ extractTvarsInTvars tvars
    TAlias moduleName typeName tvars (Filled tipe) -> extractTvarsInType tipe ++ extractTvarsInTvars tvars



{-

Equivalent of making

x = 1

into

x =
  let _ = Debug.log "identifier" ()
  in
  1

Helpful for tracing evaluation of function calls as a rudimentary decoder debugger!

-}
-- addLetLog cname identifier functionBody =
--   (a (Let
--         (Def
--            (a ("_"))
--            []
--            (a (Call
--                  (a (VarDebug
--                        cname
--                        "log"
--                        (Forall
--                           (Map.fromList [("a", ())])
--                           (TLambda (TType (Module.Canonical (Name "elm" "core") "String") "String" []) (TLambda (TVar "a") (TVar "a"))))))
--                  [(a (Str $ identifier)), (a (Unit))])))
--
--          functionBody
--   ))

addLetLog :: ES.String -> Expr -> Expr
addLetLog identifier functionBody =
  (a (Let
        (Def
           (a ("_"))
           []
           (a (Call
                 (a (VarForeign mLamdera_Wire "debug"
                   (Forall
                     (Map.fromList [("a", ())])
                     (TLambda (TType (Module.Canonical (Name "elm" "core") "String") "String" []) (TVar "a")))
                 ))

                 [(a (Str $ identifier))]
              )
           ))
         functionBody
  ))


debugEncoder :: ES.String -> Expr -> Expr
debugEncoder identifier encoder =
  -- encoder
  (a (Call
        (a (VarForeign (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "debugEncoder"
          (Forall
            (Map.fromList [("a", ())])
            (TLambda
               (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
               (TLambda
                  (TLambda (TVar "a") (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" []))
                  (TLambda (TVar "a") (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" []))))
        )))
        [(a (Str $ identifier)), encoder]
     )
  )


debugEncoder_ :: ES.String -> Expr -> Expr
debugEncoder_ identifier encoder =
  (a (Call
        (a (VarForeign (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "debugEncoder_"
          (Forall
            (Map.empty)
            (TLambda
               (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
               (TLambda
                  (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])
                  (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])
               ))
        )))
        [(a (Str $ identifier)), encoder]
     )
  )


debugDecoder :: ES.String -> Expr -> Expr
debugDecoder identifier decoder =
  (a (Call (a (VarForeign (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "debugDecoder"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
                    (TLambda
                      (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])
                      (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])
                    )))))
        [(a (Str $ identifier)), decoder]
        ))


oneOf :: [Expr] -> Expr
oneOf decoders =
  (a (Call (a (VarForeign (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "oneOf"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TType
                       (Module.Canonical (Name "elm" "core") "List")
                       "List"
                       [TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"]])
                    (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])
                 )
              )
              ))
        [a $ List decoders]
        ))


identity :: Expr
identity =
  -- a (Call
      (a (VarForeign
            (Module.Canonical (Name "elm" "core") "Basics")
            "identity"
            (Forall (Map.fromList [("a", ())]) (TLambda (TVar "a") (TVar "a")))
         )
      )
      -- [v]
    -- )
