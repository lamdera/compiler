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

    Name "lamdera" "containers" -> False

    -- Avoid cyclic imports; generated codecs rely on lamdera/codecs:Lamdera.Wire. This is our codec bootstrap module.
    Name "lamdera" "codecs" -> False

    Name "mdgriffith" "style-elements" -> False

    -- Everything else should have codecs generated
    _ -> True


{- Which decoder chain we are generating.

Every custom type and alias gets TWO decoders:

  * w3_decode_<T>        (DecodeValidating) -- applies w3_validate_<T> if present
                          and recurses through the validating chain. Used by the
                          Lamdera runtime for attacker-controlled, backend-inbound
                          data.

  * w3_unsafe_decode_<T> (DecodeUnsafe)     -- never validates and recurses
                          through the unsafe chain. Behaves like w3_decode_<T> did
                          before validation existed. Used for trusted data
                          (persistence, evergreen migrations, etc).

The two chains are otherwise identical; the only difference is the prefix used
when referencing nested user-type decoders (and, for unions, whether the
validator hook is attached). -}
data DecodeMode = DecodeValidating | DecodeUnsafe
  deriving (Eq, Show)


decodePrefix :: DecodeMode -> String
decodePrefix DecodeValidating = "w3_decode_"
decodePrefix DecodeUnsafe     = "w3_unsafe_decode_"


getForeignSig tipe moduleName generatedName ifaces =
  -- debugHaskell (T.pack $ "❎❎❎❎❎ ALIAS ENCODER foreignTypeSig for " ++ (Data.Name.toChars generatedName)) $
    case foreignTypeSig moduleName generatedName ifaces of
      Just (Forall freeVars tipe_) ->
        let
          extractedFreevars = extractTvarsInType tipe_ & fmap (\t -> (t, ())) & Map.fromList
        in
        Forall (Map.union freeVars extractedFreevars) tipe_
      Nothing | Map.notMember (Module._module moduleName) ifaces && not (null (useSiteArgs tipe)) ->
        -- The codec's module is not among this module's imports, so its signature cannot
        -- be looked up: an extensible record alias chain was inlined here (see
        -- reachesExtensibleRecord) and brought in a field type from a module only the
        -- alias's own module imports. The failure signatures below take no codec
        -- arguments, so the call this codec is about to get, with one per type
        -- argument, failed with TOO MANY ARGS (concourse's `Application.Models.Session`,
        -- through `SideBar.Model`'s `WebData (List Concourse.Pipeline)` field).
        --
        -- The codec does exist (its module is compiled, it is just not imported here),
        -- so give it the signature of exactly this use instead: a codec for each
        -- argument, then the type as written here. Only ever reached by a call that
        -- could not typecheck before, so nothing that compiled changes.
        useSiteSig tipe (useSiteArgs tipe) generatedName

      Nothing ->
        -- If a foreign type gen function cannot be find, it must be banned!
        -- So add type-sig for failure encoder or decoder as appropriate.
        if T.isPrefixOf "w3_encode_" (T.pack $ N.toChars generatedName)
          then
            (Forall
               (Map.fromList [("a", ())])
               (TLambda (TVar "a") tLamdera_Wire_Encoder))

          else if T.isPrefixOf "w3_decode_" (T.pack $ N.toChars generatedName)
                  || T.isPrefixOf "w3_unsafe_decode_" (T.pack $ N.toChars generatedName)
            then
              (Forall
                 (Map.fromList [("a", ())])
                 (TAlias
                    mLamdera_Wire
                    "Decoder"
                    [("a", TVar "a")]
                    (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"]))))
            else
              error $ "impossible getForeignSig on non-wire function: " ++ N.toChars generatedName



{-| The type arguments a codec reference for this type is called with: the generated code
passes a codec for each of them before the value (see Encoder/Decoder).
-}
useSiteArgs :: Type -> [Type]
useSiteArgs tipe =
  case tipe of
    TType _ _ params -> params
    TAlias _ _ tvars _ -> fmap snd tvars
    _ -> []


{-| A foreign codec's signature for one use, built from the type at that use, see
getForeignSig: `(arg -> Encoder) -> ... -> tipe -> Encoder`, or
`Decoder arg -> ... -> Decoder tipe`.

Every type variable in it is quantified, record extensions included, so each use gets its
own (Type.Instantiate dies on a Map.! for a variable missing from the quantifier).
-}
useSiteSig :: Type -> [Type] -> N.Name -> Can.Annotation
useSiteSig tipe args generatedName =
  let
    decoderOf t =
      TAlias mLamdera_Wire "Decoder" [("a", t)]
        (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [t]))

    sig =
      if T.isPrefixOf "w3_encode_" (T.pack $ N.toChars generatedName)
        then foldr (\arg rest -> TLambda (TLambda arg tLamdera_Wire_Encoder) rest) (TLambda tipe tLamdera_Wire_Encoder) args
        else foldr (\arg rest -> TLambda (decoderOf arg) rest) (decoderOf tipe) args

    tvarsIn t =
      case t of
        TLambda t1 t2 -> tvarsIn t1 ++ tvarsIn t2
        TVar v -> [v]
        TType _ _ params -> concatMap tvarsIn params
        TRecord fieldMap ext ->
          maybe [] (: []) ext ++ concatMap (\(FieldType _ ft) -> tvarsIn ft) (Map.elems fieldMap)
        TUnit -> []
        TPair a_ b -> tvarsIn a_ ++ tvarsIn b
        TTriple a_ b c -> tvarsIn a_ ++ tvarsIn b ++ tvarsIn c
        TAlias _ _ tvars (Holey body) -> tvarsIn body ++ concatMap (tvarsIn . snd) tvars
        TAlias _ _ tvars (Filled body) -> tvarsIn body ++ concatMap (tvarsIn . snd) tvars
  in
  Forall (tvarsIn sig & fmap (\v -> (v, ())) & Map.fromList) sig


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

    TType (Module.Canonical (Name "lamdera" "containers") "JsArray2") "JsArray2" _ -> True


    -- JS types we are supporting through JS ref encodings. These serialisations
    -- CANNOT BE DECODED OUTSIDE OF THE JS SCOPE THEY WERE ENCODED IN!
    TType (Module.Canonical (Name "elm" "file") "File") "File" _ -> False
    TType (Module.Canonical (Name "elm-explorations" "webgl") "WebGL.Texture") "Texture" _ -> False


    TAlias moduleName typeName tvars (Holey tipe) -> isUnsupportedKernelType tipe
    TAlias moduleName typeName tvars (Filled tipe) -> isUnsupportedKernelType tipe

    -- Disable for now, but need to revisit these and whether we want actual proper wire support
    -- , (("elm/browser", "Browser.Navigation") "Key" _ -> True -- This is a JS backed value

    _ -> False


isLambdaType :: Type -> Bool
isLambdaType tipe =
  case tipe of
    TLambda _ _ -> True
    _ -> False


{-| Does this type reach an EXTENSIBLE record (a TRecord carrying an extension tvar) by
following the alias chain?

The TAlias chain-resolution branches in Encoder/Decoder exist to reify extensible records
reached through an alias chain — see 6447c47e, e.g.

    type alias Color = ColorValue { red : Int, green : Int, blue : Int, alpha : Int }
    type alias ColorValue compatible = { compatible | value : String, color : Compatible }

There the chain MUST be inlined, because the alias's own codec would only see the extension
fields and lose the base ones.

For a plain (non-extensible) chain that inlining is both unnecessary and harmful. The
alias's own codec is already correct, and inlining copies the inner record's field types
into the REFERENCING module — which can then reference codecs from modules it does not
import. foreignTypeSig has no interface for those, so getForeignSig falls back to its
non-parameterized failure signatures (`a -> Encoder` / `Decoder a`) and the reference dies
with TOO MANY ARGS. That is what broke `type alias Frame p = Model p` where Model's record
held a `Form.Field String`, with Form imported by Frame but not by the referencing module.

Only the alias spine is followed: an extensible record sitting inside a record FIELD is
handled by that field's own codegen, and treating it as a reason to inline here would
reintroduce the same over-inlining.
-}
reachesExtensibleRecord :: Type -> Bool
reachesExtensibleRecord tipe =
  case tipe of
    TRecord _ (Just _) -> True
    TAlias _ _ _ (Holey inner) -> reachesExtensibleRecord inner
    TAlias _ _ _ (Filled inner) -> reachesExtensibleRecord inner
    _ -> False


{-| The record at the end of an alias chain that reachesExtensibleRecord, when the chain
is filled all the way down, e.g. for `Concrete = Level2 { field : Bool }` with
`Level2 c = Level1 { c | field2 : Int }` and `Level1 c = { c | field1 : String }`.

One alias layer around the record is the case that always worked, and is matched exactly
as before, so its codecs don't change. Each further layer is resolved with its own type
variables first: before this, a chain of three or more stopped at the first layer and
fell back to the alias codec, which dropped the outer extension's fields and failed to
type check (lamdera/compiler#88).
-}
resolveToRecord :: Type -> Maybe Type
resolveToRecord tipe =
  case tipe of
    TAlias _ _ _ (Filled record@(TRecord _ Nothing)) -> Just record
    TAlias _ _ tvars (Filled inner@(TAlias _ _ _ _)) -> resolveToRecord (resolveTvar tvars inner)
    _ -> Nothing


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


resolvedRecordFieldMap :: (Map.Map N.Name FieldType) -> (Maybe TV.Var) -> [(TV.Var, Type)] -> (Map.Map N.Name FieldType)
resolvedRecordFieldMap fieldMap extensibleName tvarMap =
  case resolvedRecordFieldMapM fieldMap extensibleName tvarMap of
    Just extendedFieldMap -> extendedFieldMap
    Nothing -> fieldMap


resolvedRecordFieldMapM :: (Map.Map N.Name FieldType) -> (Maybe TV.Var) -> [(TV.Var, Type)] -> Maybe (Map.Map N.Name FieldType)
resolvedRecordFieldMapM fieldMap extensibleName tvarMap =
  case extensibleName of
    Just extensibleName ->
      case List.find (\(n,_) -> n == extensibleName) tvarMap of
        Just (_, extensibleType) ->
          case extensibleType of
            TRecord fieldMapExtended maybeNameExtended ->
              Just $ fieldMap <> fieldMapExtended

            TAlias moduleName typeName tvars (Holey tipe) -> do
              -- See expansionTvarMap: without it, nested extensible aliases that
              -- share a parameter name never terminate
              let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar (expansionTvarMap extensibleName tvarMap) t))
              case throughFilledAliases (resolveTvar newResolvedTvars tipe) of
                TRecord fieldMapExtensible _ ->
                  fieldMap
                    & Map.union fieldMapExtensible
                    & fmap (\(FieldType index tipe) ->
                        FieldType index (resolveTvar newResolvedTvars tipe)
                      )
                    & Just
                _ -> closedExtensionFields fieldMap tvarMap extensibleType

            TAlias moduleName typeName tvars (Filled tipe) -> do
              let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar (expansionTvarMap extensibleName tvarMap) t))
              case throughFilledAliases (resolveTvar newResolvedTvars tipe) of
                TRecord fieldMapExtensible _ ->
                  fieldMap
                    & Map.union fieldMapExtensible
                    & fmap (\(FieldType index tipe) ->
                        FieldType index (resolveTvar newResolvedTvars tipe)
                      )
                    & Just
                _ -> closedExtensionFields fieldMap tvarMap extensibleType

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


{-| The substitution to resolve an extension's alias arguments with, while expanding that
extension: `tvarMap` without the extension's own name.

When the row tail of a record being resolved is bound to an alias, e.g. `A`'s `m` in

    type alias A m = { m | a : Int }
    type alias B m = { m | b : Int }
    type alias Model m = A (B { m | x : Bool })

the alias's arguments (`{ m | x : Bool }`) are written in the REFERENCING scope, yet they
are resolved again with `tvarMap`, the alias's own parameters. While the names differ that
second pass finds nothing to substitute. When they coincide, as with `m` above, the
argument's row tail `m` looks up the very binding being expanded, which leads back to this
same expansion: resolveTvar and resolvedRecordFieldMapM recursed until the machine ran out
of memory (concourse's `SideBar.Model`, 22-47GB during `lamdera make`).

Dropping the binding being expanded cuts that cycle and nothing else: any forced use of it
in that second pass was the infinite one, so every type that resolved before resolves to
the same thing now, and the shared name case resolves like the differently named one.
-}
expansionTvarMap :: TV.Var -> [(TV.Var, Type)] -> [(TV.Var, Type)]
expansionTvarMap extensibleName tvarMap =
  tvarMap & filter (\(n, _) -> n /= extensibleName)


{-| A resolved extension's record, looking through the aliases in front of it.

The extension of a record being resolved can be bound to an alias whose body is itself an
alias, e.g. `C` in

    type alias A a = { a | fa : Int }
    type alias B b = { b | fb : Int }
    type alias D d = { d | fd : Int }
    type alias C = D { fe : Int }
    type alias X = A (B C)

Resolving `C` gives `D { fe : Int }`, an alias (Filled, as resolveTvar leaves every alias)
of the record `{ fd : Int, fe : Int }`, not that record itself. Expecting a TRecord there
either crashed the compiler ("bad nesting Holey", resolveTvar) or gave up on inlining the
chain (resolvedRecordFieldMapM), which then called `A`'s codec with `C`'s, the codec of a
record without `A`'s fields: a TYPE MISMATCH in the generated code (massivedecks'
`MassiveDecks.Card.Source.Methods.ExternalMethods`). Neither ever compiled, so looking
through the aliases changes nothing that did.
-}
throughFilledAliases :: Type -> Type
throughFilledAliases tipe =
  case tipe of
    TAlias _ _ _ (Filled inner) -> throughFilledAliases inner
    _ -> tipe


{-| The fields of a record whose row tail is bound to `extensibleType`, when following that
binding down through its aliases ends in a closed record. Nothing when it ends in a bare
type variable, an open row: the alias's own codec is then called with the codec of its
argument, which is row polymorphic, and that typechecks (`A (B m)`).

resolvedRecordFieldMapM's last resort. resolveTvar declines to substitute a binding that
is still generic, which is right for the foreign signatures it was written for, but here
it stops the row tail of an inner alias from being followed whenever an argument mentions
a type variable, e.g. `A1 (B1 (C m))` over

    type alias A1 a = { a | fa : Int }
    type alias B1 b = { b | fb : Int }
    type alias C m = D (E m)

so the chain was not inlined, and `A1`'s codec was called with `B1 (C m)`'s, the codec of
a closed record without `fa`. That can never typecheck (massivedecks'
`MassiveDecks.Card.Source.Methods.ExternalMethods msg`), so this only ever changes what
did not compile.

`extensibleType` is written in the referencing scope, so it is expanded as it stands, with
each alias's parameters substituted in its own body (see substituteRows). `fieldMap` is the
inlined alias's own fields, written in its parameters, `tvarMap`.
-}
closedExtensionFields :: Map.Map N.Name FieldType -> [(TV.Var, Type)] -> Type -> Maybe (Map.Map N.Name FieldType)
closedExtensionFields fieldMap tvarMap extensibleType =
  let
    closedRecord t =
      case t of
        TRecord fields Nothing -> Just fields
        TAlias _ _ _ (Filled body) -> closedRecord body
        TAlias _ _ args (Holey body) -> closedRecord (substituteRows (Map.fromList args) body)
        _ -> Nothing

    ownFields =
      fieldMap & fmap (\(FieldType index tipe) -> FieldType index (substituteRows (Map.fromList tvarMap) tipe))
  in
  closedRecord extensibleType
    & fmap (\extensionFields -> Map.union extensionFields ownFields)


{-| Simultaneous substitution of type variables, row tails included: substituting a record
for the row tail of another merges the two, `{ a | x : Int }` with `a` bound to
`{ r | y : Int }` is `{ r | x : Int, y : Int }`, and an alias bound there is expanded to
its record first. A Holey alias body is in that alias's own parameters, so only its
arguments are substituted.
-}
substituteRows :: Map.Map TV.Var Type -> Type -> Type
substituteRows subs t =
  let
    go = substituteRows subs
  in
  case t of
    TVar v -> Map.findWithDefault t v subs
    TLambda t1 t2 -> TLambda (go t1) (go t2)
    TType home name params -> TType home name (fmap go params)
    TRecord fieldMap ext ->
      let fields = fieldMap & fmap (\(FieldType index tipe) -> FieldType index (go tipe))
      in
      case ext >>= (\e -> Map.lookup e subs) of
        Just bound -> extendRow fields bound
        Nothing -> TRecord fields ext
    TUnit -> t
    TPair a_ b -> TPair (go a_) (go b)
    TTriple a_ b c -> TTriple (go a_) (go b) (go c)
    TAlias home name args (Holey body) -> TAlias home name (args & fmap (\(n, v) -> (n, go v))) (Holey body)
    TAlias home name args (Filled body) -> TAlias home name (args & fmap (\(n, v) -> (n, go v))) (Filled (go body))


{-| `fields` added to the record `bound`, as when `bound` is substituted for the row tail of
`{ r | fields }`: a record's fields are merged in, a type variable becomes the row tail,
and an alias is expanded to its record first.
-}
extendRow :: Map.Map N.Name FieldType -> Type -> Type
extendRow fields bound =
  case bound of
    TRecord moreFields ext -> TRecord (Map.union fields moreFields) ext
    TVar v -> TRecord fields (Just v)
    TAlias _ _ args (Holey body) -> extendRow fields (substituteRows (Map.fromList args) body)
    TAlias _ _ _ (Filled body) -> extendRow fields body
    -- Not a record: Elm does not allow it as a row tail
    _ -> TRecord fields Nothing


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
declsToSummary :: Decls -> [(String, N.Name)]
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
addRecDef list decls_ =
  case list of
    (def_:defs_) ->
      case decls_ of
        Declare def decls ->
          DeclareRec def_ defs_ (Declare def decls)

        DeclareRec def defs decls ->
          DeclareRec def_ defs_ (DeclareRec def defs decls)

        SaveTheEnvironment ->
          DeclareRec def_ defs_ SaveTheEnvironment
    _ -> error $ "addRecDef: impossible list: " ++ show list

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


removeDefByName :: N.Name -> Decls -> Decls
removeDefByName name decls_ =
  case decls_ of
    Declare def decls ->
      if (defName def == name) then
        decls
      else
        Declare def (removeDefByName name decls)

    DeclareRec def defs decls ->
      if (defName def == name) then
        decls
      else
        DeclareRec def (List.filter (\d -> defName d /= name) defs) (removeDefByName name decls)

    SaveTheEnvironment ->
      SaveTheEnvironment


sameName :: Def -> Def -> Bool
sameName d1 d2 =
  defName d1 == defName d2

findDef :: N.Name -> Decls -> Maybe Def
findDef name decls =
  decls
    & declsToList
    & List.find (defNameIs name)

defNameIs :: N.Name -> Def -> Bool
defNameIs name def =
  name == defName def

defName :: Def -> N.Name
defName def =
  case def of
    Def (A.At region name_) _ _ ->
      name_
    TypedDef (A.At region name_) _ _ _ _ ->
      name_


foreignTypeTvars :: PrimModule.Name -> TN.Name -> Map.Map PrimModule.Name I.Interface -> [TV.Var]
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


foreignTypeSig :: Module.Canonical -> N.Name -> Map.Map PrimModule.Name I.Interface -> Maybe Can.Annotation
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
namedTodo :: Src.Module -> N.Name -> Def
namedTodo modul functionName =
  let functionName_ = Utf8.fromChars . N.toChars $ functionName
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
  A.At A.zero v

-- Refer to Lamdera.Wire3.endianness
endianness =
  a (VarForeign mLamdera_Wire "endianness"
        (Forall
           Map.empty
           (TType
              (Module.Canonical (Name "elm" "bytes") "Bytes")
              "Endianness"
              [])))


encodeSequenceWithoutLength list =
  -- @TODO we could optimise all sequences of length 1 to not use the sequence encoding ?
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
  (a (Call (a (VarForeign mBytes_Encode "unsignedInt8"
                (Forall
                   Map.empty
                   (TLambda
                      (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                      tElm_Bytes_Encoder))))
          [value]))


encodeUnsignedInt16 value =
  (a (Call (a (VarForeign mBytes_Encode "unsignedInt16"
                (Forall
                   Map.empty
                   (TLambda
                      ((TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Endianness" []))
                      (TLambda
                          (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                          tElm_Bytes_Encoder)))))
          [endianness, value]))


encodeInt value =
  (a (Call (a (VarForeign mLamdera_Wire "encodeInt"
                (Forall
                   Map.empty
                   (TLambda
                      (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                      tLamdera_Wire_Encoder))))
          [value]))


decodeUnsignedInt8 =
  (a (VarForeign mBytes_Decode "unsignedInt8"
        (Forall
           Map.empty
           (tElm_Bytes_Decoder tInt)
        )
  ))

decodeUnsignedInt16 =
  a (Call (a (VarForeign mBytes_Decode "unsignedInt16"
        (Forall
           Map.empty
           (TLambda
              (TType (Module.Canonical (Name "elm" "bytes") "Bytes") "Endianness" [])
              (tElm_Bytes_Decoder tInt)
           )
        )))
    [endianness]
  )


decodeInt =
  (a (VarForeign mLamdera_Wire "decodeInt"
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


failDecode :: String -> Expr
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


list :: [Expr] -> A.Located Expr_
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

pint i = a (PInt i)
pvar n = a (PVar n)
pAny_  = a (PAnything)

call fn args = (a (Call fn args))

tInt = (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])

tElm_Bytes_Encoder =
  (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])

tElm_Bytes_Decoder v =
  (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [v])

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


{-| The type generated decoders are annotated with: `Bytes.Decode.Decoder tipe`.

Deliberately not `Lamdera.Wire3.Decoder tipe`, though that's the same type. The point of
annotating the decoders is the size of their signatures in the interface, where an alias
is written out with its arguments AND its expanded body, so `Lamdera.Wire3.Decoder T`
carries `T` twice. For a model that's most of the .elmi: on one customer app this alone
made w3_decode_FrontendModelRecord's signature 1.27MB rather than 636KB.

-}
tWire_Decoder_Sig :: Type -> Type
tWire_Decoder_Sig tipe =
  TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [tipe]


mLamdera_Wire = (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire3")
mLamdera_SeqDict = (Module.Canonical (Name "lamdera" "containers") "SeqDict")
mLamdera_SeqSet = (Module.Canonical (Name "lamdera" "containers") "SeqSet")
mLamdera_BiSeqDict = (Module.Canonical (Name "lamdera" "containers") "BiSeqDict")
mLamdera_MultiSeqDict = (Module.Canonical (Name "lamdera" "containers") "MultiSeqDict")
mLamdera_MultiBiSeqDict = (Module.Canonical (Name "lamdera" "containers") "MultiBiSeqDict")
mBytes_Encode = (Module.Canonical (Name "elm" "bytes") "Bytes.Encode")
mBytes_Decode = (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")


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
    TType (Module.Canonical (Name "lamdera" "containers") "SeqSet") "SeqSet" [a] -> TType (Module.Canonical (Name "lamdera" "containers") "SeqSet") "SeqSet" [unwrapAliasesDeep a]
    TType (Module.Canonical (Name "elm" "core") "Array") "Array" [a] -> TType (Module.Canonical (Name "elm" "core") "Array") "Array" [unwrapAliasesDeep a]

    TType (Module.Canonical (Name "elm" "core") "Result") "Result" [err, a] ->
      TType (Module.Canonical (Name "elm" "core") "Result") "Result" [unwrapAliasesDeep err, unwrapAliasesDeep a]

    TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [key, val] ->
      TType (Module.Canonical (Name "elm" "core") "Dict") "Dict" [unwrapAliasesDeep key, unwrapAliasesDeep val]

    TType (Module.Canonical (Name "lamdera" "containers") "SeqDict") "SeqDict" [key, val] ->
      TType (Module.Canonical (Name "lamdera" "containers") "SeqDict") "SeqDict" [unwrapAliasesDeep key, unwrapAliasesDeep val]

    TType (Module.Canonical (Name "lamdera" "containers") "BiSeqDict") "BiSeqDict" [key, val] ->
      TType (Module.Canonical (Name "lamdera" "containers") "BiSeqDict") "BiSeqDict" [unwrapAliasesDeep key, unwrapAliasesDeep val]

    TType (Module.Canonical (Name "lamdera" "containers") "MultiSeqDict") "MultiSeqDict" [key, val] ->
      TType (Module.Canonical (Name "lamdera" "containers") "MultiSeqDict") "MultiSeqDict" [unwrapAliasesDeep key, unwrapAliasesDeep val]

    TType (Module.Canonical (Name "lamdera" "containers") "MultiBiSeqDict") "MultiBiSeqDict" [key, val] ->
      TType (Module.Canonical (Name "lamdera" "containers") "MultiBiSeqDict") "MultiBiSeqDict" [unwrapAliasesDeep key, unwrapAliasesDeep val]

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

    TPair a b -> TPair (unwrapAliasesDeep a) (unwrapAliasesDeep b)
    TTriple a b c -> TTriple (unwrapAliasesDeep a) (unwrapAliasesDeep b) (unwrapAliasesDeep c)

    TAlias moduleName typeName tvars (Holey tipe) -> unwrapAliasesDeep tipe
    TAlias moduleName typeName tvars (Filled tipe) -> unwrapAliasesDeep tipe


resolveTvar :: [(TV.Var, Type)] -> Type -> Type
resolveTvar tvarMap t =
  case t of
    TLambda t1 t2 -> TLambda (resolveTvar tvarMap t1) (resolveTvar tvarMap t2)

    TVar a ->
      case List.find (\(t,ti) -> t == a) tvarMap of
        Just (tvarName,tvarType) ->

          -- extractFreeTvarsInType, not extractTvarsInType: a Holey alias's body mentions
          -- its own BOUND parameters, and counting those made a fully concrete argument
          -- look generic, so we skipped substituting it and left a dangling tvar codec
          -- reference in the generated code. See extractFreeTvarsInType for the detail.
          case extractFreeTvarsInType tvarType of
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
              -- See expansionTvarMap
              let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar (expansionTvarMap extensibleName tvarMap) t))
              case throughFilledAliases (resolveTvar newResolvedTvars tipe) of
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
              let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar (expansionTvarMap extensibleName tvarMap) t))
              case throughFilledAliases (resolveTvar newResolvedTvars tipe) of
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

    TPair a b -> TPair (resolveTvar tvarMap a) (resolveTvar tvarMap b)
    TTriple a b c -> TTriple (resolveTvar tvarMap a) (resolveTvar tvarMap b) (resolveTvar tvarMap c)

    TAlias moduleName typeName tvars (Holey tipe) ->
      let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
      in TAlias moduleName typeName newResolvedTvars (Filled $ resolveTvar newResolvedTvars tipe)

    TAlias moduleName typeName tvars (Filled tipe) ->
      let newResolvedTvars = tvars & fmap (\(n, t) -> (n, resolveTvar tvarMap t))
      in TAlias moduleName typeName newResolvedTvars (Filled $ resolveTvar newResolvedTvars tipe)


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

    TPair a b -> [a,b] & concatMap extractTvarsInType
    TTriple a b c -> [a,b,c] & concatMap extractTvarsInType

    TAlias moduleName typeName tvars (Holey tipe) -> extractTvarsInType tipe ++ extractTvarsInTvars tvars
    TAlias moduleName typeName tvars (Filled tipe) -> extractTvarsInType tipe ++ extractTvarsInTvars tvars


{-| Free type variables of a type, treating a Holey alias body's own parameters as BOUND.

Deliberately different from extractTvarsInType above, which counts them. That one builds
the Forall quantifier for a foreign codec's type signature, where an alias's parameters
really do appear and must be quantified — so it has to descend into the Holey body.

This one answers a different question: "is this applied type still generic?" A Holey alias
carries its own parameter names as TVars in its unsubstituted body, but they are bound by
the alias, so only the applied arguments can contribute free tvars.

Using the wrong one here made resolveTvar treat a fully concrete argument as still generic
and skip substitution, so an alias chain like

    type alias Frame p = Model p
    type alias Model p = { email : Field String, page : p }

inlined at a concrete `Frame PageModel` reference emitted a w3_x_c_p tvar-codec reference
that only exists as a parameter on Frame/Model's own codecs. In a decoder that is an
unbound name, which crashes type inference with a Map.! that surfaces as "thread blocked
indefinitely in an MVar operation"; in an encoder it surfaces as TOO MANY ARGS. A nested
`Field String` was enough to trigger it, since Field's Holey body mentions its bound
`input` tvar.

-}
extractFreeTvarsInType :: Type -> [TV.Var]
extractFreeTvarsInType t =
  case t of
    TLambda t1 t2 -> [t1,t2] & concatMap extractFreeTvarsInType
    TVar a -> [a]
    TType modul typename tvars -> tvars & concatMap extractFreeTvarsInType

    TRecord fieldMap maybeExtensible ->
      fieldMap
        & concatMap (\(FieldType index tipe) ->
            extractFreeTvarsInType tipe
          )

    TUnit -> []

    TPair a b -> [a,b] & concatMap extractFreeTvarsInType
    TTriple a b c -> [a,b,c] & concatMap extractFreeTvarsInType

    -- Holey: body tvars are the alias's own parameters, i.e. bound — only the applied
    -- arguments can be free. Filled: already substituted, so the body is fair game.
    TAlias moduleName typeName tvars (Holey _) ->
      tvars & concatMap (\(_, tvarType) -> extractFreeTvarsInType tvarType)
    TAlias moduleName typeName tvars (Filled tipe) ->
      extractFreeTvarsInType tipe
        ++ (tvars & concatMap (\(_, tvarType) -> extractFreeTvarsInType tvarType))



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


{-

WIRE VALIDATION HELPERS

User code can opt a custom type into post-decode validation by defining a
function `w3_validate_<TypeName> : <TypeName> <tvars> -> Result String ()` in
the same module as the type. The generated `w3_decode_<TypeName>` then calls
it; see Lamdera.Wire3.Decoder.wrapWithValidator and Lamdera.Wire3.Core.

-}

w3ValidatePrefix :: String
w3ValidatePrefix = "w3_validate_"


isValidatorName :: N.Name -> Bool
isValidatorName name =
  List.isPrefixOf w3ValidatePrefix (N.toChars name)


-- "w3_validate_MyType" -> "MyType"
validatorTypeName :: N.Name -> TN.Name
validatorTypeName name =
  TN.nameFromChars $ drop (length w3ValidatePrefix) (N.toChars name)


-- "MyType" -> "w3_validate_MyType"
validatorNameFor :: TN.Name -> N.Name
validatorNameFor typeName =
  N.fromChars $ w3ValidatePrefix ++ TN.nameToChars typeName


allValidatorDefs :: Decls -> [Def]
allValidatorDefs decls =
  declsToList decls & filter (isValidatorName . defName)


findValidatorDef :: Decls -> TN.Name -> Maybe Def
findValidatorDef decls typeName =
  findDef (validatorNameFor typeName) decls


{- Append `tailDecls` after every definition in `decls`, replacing the terminal
SaveTheEnvironment. Used to place generated wire functions *after* user
definitions when a module contains w3_validate_* functions: the generated
decoders reference those user functions via VarTopLevel, and a VarTopLevel only
resolves to definitions appearing earlier in the topologically-sorted Decls. -}
spliceDeclsAtEnd :: Decls -> Decls -> Decls
spliceDeclsAtEnd decls tailDecls =
  case decls of
    Declare def rest ->
      Declare def (spliceDeclsAtEnd rest tailDecls)
    DeclareRec def defs rest ->
      DeclareRec def defs (spliceDeclsAtEnd rest tailDecls)
    SaveTheEnvironment ->
      tailDecls


{- Like addLetLog, but logs a dynamic Expr (e.g. an error string bound in a
pattern) rather than a static identifier. Equivalent to writing:

  let _ = Lamdera.Wire3.debug <logValue>
  in <functionBody>
-}
addLetLogValue :: Expr -> Expr -> Expr
addLetLogValue logValue functionBody =
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
                 [ logValue ]
              )))
         functionBody
  ))


{- A total collector of every VarTopLevel reference in an expression, recursing
through let/case/record/etc.

Unlike getLvars in Lamdera.Wire3.Graph -- which is specialised to generated wire
expressions and intentionally `error`s on shapes that generated code never emits
(e.g. a multi-branch `if`) and ignores `case` scrutinees -- this handles every
Expr_ constructor, so it is safe to run over arbitrary user code. -}
topLevelRefsInExpr :: Expr -> [N.Name]
topLevelRefsInExpr (A.At _ expr) =
  case expr of
    VarLocal _              -> []
    VarTopLevel _ name      -> [name]
    VarKernel _ _           -> []
    VarForeign _ _ _        -> []
    VarCtor _ _ _ _ _       -> []
    VarDebug _ _ _          -> []
    VarOperator _ _ _ _     -> []
    Chr _                   -> []
    Str _                   -> []
    Int _                   -> []
    Float _                 -> []
    List es                 -> concatMap topLevelRefsInExpr es
    Negate e                -> topLevelRefsInExpr e
    Binop _ _ _ _ e1 e2     -> topLevelRefsInExpr e1 ++ topLevelRefsInExpr e2
    Lambda _ e              -> topLevelRefsInExpr e
    Call e es               -> topLevelRefsInExpr e ++ concatMap topLevelRefsInExpr es
    If branches finalElse   ->
      concatMap (\(c, t) -> topLevelRefsInExpr c ++ topLevelRefsInExpr t) branches
        ++ topLevelRefsInExpr finalElse
    Let def e               -> defTopLevelRefs def ++ topLevelRefsInExpr e
    LetRec defs e           -> concatMap defTopLevelRefs defs ++ topLevelRefsInExpr e
    LetDestruct _ e1 e2     -> topLevelRefsInExpr e1 ++ topLevelRefsInExpr e2
    Case scrutinee branches ->
      topLevelRefsInExpr scrutinee
        ++ concatMap (\(CaseBranch _ e) -> topLevelRefsInExpr e) branches
    Accessor _              -> []
    Access e _              -> topLevelRefsInExpr e
    Update _ e fieldUpdates ->
      topLevelRefsInExpr e
        ++ concatMap (\(FieldUpdate _ ue) -> topLevelRefsInExpr ue) (Map.elems fieldUpdates)
    Record fields           -> concatMap topLevelRefsInExpr (Map.elems fields)
    Unit                    -> []
    Pair e1 e2              -> topLevelRefsInExpr e1 ++ topLevelRefsInExpr e2
    Triple e1 e2 e3         ->
      topLevelRefsInExpr e1 ++ topLevelRefsInExpr e2 ++ topLevelRefsInExpr e3
    Shader _ _              -> []


defTopLevelRefs :: Def -> [N.Name]
defTopLevelRefs def =
  case def of
    Def _ _ e          -> topLevelRefsInExpr e
    TypedDef _ _ _ e _ -> topLevelRefsInExpr e


{-| Point every VarTopLevel reference to `from` (in module `home`) at `to` instead,
everywhere in an expression. Covers every Expr_ constructor, like topLevelRefsInExpr.

Used to send a codec's references to itself through a forwarder, see
Lamdera.Wire3.Core.withPolymorphicRecursion.
-}
renameTopLevelRef :: Module.Canonical -> N.Name -> N.Name -> Expr -> Expr
renameTopLevelRef home from to (A.At region expr) =
  let
    go = renameTopLevelRef home from to

    goDef def =
      case def of
        Def n ps e -> Def n ps (go e)
        TypedDef n fv ps e t -> TypedDef n fv ps (go e) t
  in
  A.At region $
  case expr of
    VarTopLevel h name | h == home && name == from -> VarTopLevel h to
    VarLocal _              -> expr
    VarTopLevel _ _         -> expr
    VarKernel _ _           -> expr
    VarForeign _ _ _        -> expr
    VarCtor _ _ _ _ _       -> expr
    VarDebug _ _ _          -> expr
    VarOperator _ _ _ _     -> expr
    Chr _                   -> expr
    Str _                   -> expr
    Int _                   -> expr
    Float _                 -> expr
    List es                 -> List (fmap go es)
    Negate e                -> Negate (go e)
    Binop op h n ann e1 e2  -> Binop op h n ann (go e1) (go e2)
    Lambda ps e             -> Lambda ps (go e)
    Call e es               -> Call (go e) (fmap go es)
    If branches finalElse   -> If (fmap (\(c, t) -> (go c, go t)) branches) (go finalElse)
    Let def e               -> Let (goDef def) (go e)
    LetRec defs e           -> LetRec (fmap goDef defs) (go e)
    LetDestruct p e1 e2     -> LetDestruct p (go e1) (go e2)
    Case scrutinee branches -> Case (go scrutinee) (fmap (\(CaseBranch p e) -> CaseBranch p (go e)) branches)
    Accessor _              -> expr
    Access e field          -> Access (go e) field
    Update n e fieldUpdates -> Update n (go e) (fmap (\(FieldUpdate r ue) -> FieldUpdate r (go ue)) fieldUpdates)
    Record fields           -> Record (fmap go fields)
    Unit                    -> expr
    Pair e1 e2              -> Pair (go e1) (go e2)
    Triple e1 e2 e3         -> Triple (go e1) (go e2) (go e3)
    Shader _ _              -> expr

