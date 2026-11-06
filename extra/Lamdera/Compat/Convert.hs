{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings #-}
module Lamdera.Compat.Convert
  ( -- 0.19.4 to 0.19.3
    Names
  , namesFromInterfaces
  , namesFromModule
  , noNames
  , fromType
  , fromTypeIn
  , fromAnnotation
  , fromUnion
  , fromAlias
  , fromInterface
  , fromInterfaces
  , fromTypeDecls
  , unionsOf
  , aliasesOf
    -- 0.19.3 to 0.19.4
  , Env
  , envFromInterfaces
  , envWithModule
  , toType
  , toAnnotation
  , toTypeWith
  , toDef
  , toDecls
  , toExpr
  , toPattern
  , declsFromGroups
  , Group(..)
  )
  where

{- @LAMDERA Converting between the Elm 0.19.4 AST and the Elm 0.19.3 shapes Lamdera's
own code works with (Lamdera.Compat.Canonical, Lamdera.Compat.Interface).

0.19.4 to 0.19.3 is used to look at types: interfaces, unions, aliases and annotations
become what 0.19.3 had, so TypeHash, Evergreen, Wire3's analysis and the wire schema
see exactly the values they used to.

  - Type variables are names again (I.TVar holds an index into the array of names of the
    annotation, union or alias the type belongs to).
  - Record fields get their source position as the FieldType index, as 0.19.3 gave them
    (0 everywhere when 0.19.4 has no order for them, as for inferred types).
  - An alias in a type becomes a Holey alias: its arguments with the names of the
    alias's parameters, and its declared body over those names. The names come from the
    alias's declaration when it can be found (Names); otherwise they are made up, which
    only renames the parameters inside the body.
  - Custom types get their constructors in tag order, numbered from 0, and the
    0.19.3 CtorOpts: Enum when no constructor has arguments, Unbox for one constructor
    with one argument, Normal otherwise.

0.19.3 to 0.19.4 is used for the definitions Wire3 generates, which go on to be type
checked and compiled like the user's own. They are built as the canonicalizer would
have built them from the same source:

  - Constructors, patterns, record constructors and Bool use the declarations of their
    types (Env): their tags, kinds, masks and annotations come from there, as in
    Canonicalize.Environment.Foreign.
  - A foreign value takes its annotation from the interface it is in when that module is
    imported, and otherwise from the 0.19.3 annotation Wire3 wrote for it.
  - Type variables get indexes in the order Canonicalize.Type registers them.
-}


import qualified Data.List as List
import qualified Data.List.Utils as LU
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyMap as NEM
import GHC.Exts (Int(..), sizeofByteArray#)

import qualified Array as A

import qualified AST.Canonical as New
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeIndex as TI
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Data.Index as Index
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Nitpick.Utils as Nit
import qualified Reporting.Annotation as A

import qualified Lamdera.Compat.Canonical as Can
import qualified Lamdera.Compat.Interface as CI



--------------------------------------------------------------------------------
-- 0.19.4 TO 0.19.3 ------------------------------------------------------------
--------------------------------------------------------------------------------


{-| The parameter names of the aliases that can be found, for the bodies of Holey
aliases. -}
newtype Names =
  Names (ModuleName.Canonical -> T.Name -> Maybe (A.Array T.Var))


noNames :: Names
noNames =
  Names (\_ _ -> Nothing)


namesFromInterfaces :: Map.Map Module.Name I.Interface -> Names
namesFromInterfaces ifaces =
  Names $ \(ModuleName.Canonical pkg home) name ->
    case Map.lookup home ifaces of
      Just (I.Interface ipkg _ types _) | ipkg == pkg ->
        case Map.lookup name types of
          Just decl -> I.extract (const Nothing) (\(I.Alias _ vars _) -> Just vars) decl
          Nothing -> Nothing

      _ ->
        Nothing


{-| The aliases of the module being compiled, before the given names. -}
namesFromModule :: ModuleName.Canonical -> Map.Map T.Name New.TypeDecl -> Names -> Names
namesFromModule home types (Names others) =
  Names $ \h name ->
    if h == home then
      case Map.lookup name types of
        Just (New.Alias (I.Alias _ vars _)) -> Just vars
        _ -> others h name
    else
      others h name


fromAnnotation :: Names -> I.Annotation -> Can.Annotation
fromAnnotation names (I.Forall vars tipe) =
  Can.Forall (Map.fromList (map (\v -> (v, ())) (A.toList vars))) (fromTypeIn names vars tipe)


fromType :: A.Array T.Var -> I.Type -> Can.Type
fromType =
  fromTypeIn noNames


fromTypeIn :: Names -> A.Array T.Var -> I.Type -> Can.Type
fromTypeIn names@(Names lookupNames) vars tipe =
  let
    go = fromTypeIn names vars
  in
  case tipe of
    I.TLambda a b cs ->
      foldr1 Can.TLambda (map go (a : b : cs))

    I.TVar i ->
      Can.TVar (TI.get i vars)

    I.TType h n _ xs ->
      Can.TType h n (map go (A.toList xs))

    I.TAlias h n _ xs (I.Aliased body) ->
      let
        args = A.toList xs
        params =
          case lookupNames h n of
            Just declared | A.size declared == length args -> declared
            _ -> A.fromList (zipWith (\i _ -> T.varFromChars ("x" ++ show (i :: Int))) [0..] args)
      in
      Can.TAlias h n (zip (A.toList params) (map go args)) (Can.Holey (fromTypeIn names params body))

    I.TUnit ->
      Can.TUnit

    I.TPair a b ->
      Can.TPair (go a) (go b)

    I.TTriple a b c ->
      Can.TTriple (go a) (go b) (go c)

    I.TEmpty ->
      Can.TRecord Map.empty Nothing

    I.TRecord fields ->
      Can.TRecord (fromFields go fields) Nothing

    I.TExtRec fields ext ->
      Can.TRecord (fromFields go fields) (Just (TI.get ext vars))


fromFields :: (a -> Can.Type) -> F.Fields a -> Map.Map N.Name Can.FieldType
fromFields go fields@(F.Fields order dict) =
  if hasOrder order (NEM.size dict) then
    let
      ((n, t), rest) = F.flatten fields
    in
    Map.fromList $ zipWith (\i (name, tipe) -> (name, Can.FieldType i (go tipe))) [0..] ((n, t) : rest)
  else
    Map.map (Can.FieldType 0 . go) (NEM.toDict dict)


-- Whether a Fields has a source order (see F.flattenWith)
hasOrder :: F.Order# -> Int -> Bool
hasOrder order num =
  let
    len
      | num <= 0x100   = num
      | num <= 0x10000 = 2 * num
      | otherwise      = 4 * num
  in
  I# (sizeofByteArray# order) == len


fromUnion :: Names -> I.Union -> Can.Union
fromUnion names (I.Union _ vars variants) =
  let
    go = fromTypeIn names vars
    ctors =
      case variants of
        I.Variants table ->
          Tag.unpack table (\_ cs -> cs) $ \tag (I.Variant n ts) ->
            Can.Ctor n (tagIndex tag) (length ts) (map go ts)

        I.Enum table ->
          Tag.unpack table (\_ cs -> cs) $ \tag n ->
            Can.Ctor n (tagIndex tag) 0 []

        I.Vector n a b cs ->
          [ Can.Ctor n Index.first (2 + length cs) (map go (a : b : cs)) ]

        I.Box n a ->
          [ Can.Ctor n Index.first 1 [go a] ]

        I.Void n ->
          [ Can.Ctor n Index.first 0 [] ]

    opts =
      case variants of
        I.Variants _     -> Can.Normal
        I.Enum _         -> Can.Enum
        I.Vector _ _ _ _ -> Can.Normal
        I.Box _ _        -> Can.Unbox
        I.Void _         -> Can.Enum
  in
  Can.Union (A.toList vars) ctors (length ctors) opts


tagIndex :: Tag.Tag -> Index.ZeroBased
tagIndex tag =
  Index.ZeroBased (fromIntegral (Tag.toWord32 tag))


fromAlias :: Names -> I.Alias -> Can.Alias
fromAlias names (I.Alias _ vars (I.Aliased body)) =
  Can.Alias (A.toList vars) (fromTypeIn names vars body)


fromInterface :: Names -> I.Interface -> CI.Interface
fromInterface names (I.Interface home values types binops) =
  CI.Interface
    { CI._home    = home
    , CI._values  = Map.map (fromAnnotation names) values
    , CI._unions  = Map.mapMaybe (toIUnion names) types
    , CI._aliases = Map.mapMaybe (toIAlias names) types
    , CI._binops  = Map.map (\(I.Binop n t a p) -> CI.Binop n (fromAnnotation names t) a p) binops
    }


toIUnion :: Names -> I.TypeDecl -> Maybe CI.Union
toIUnion names decl =
  case decl of
    I.UnionOpen    u -> Just (CI.OpenUnion    (fromUnion names u))
    I.UnionClosed  u -> Just (CI.ClosedUnion  (fromUnion names u))
    I.UnionPrivate u -> Just (CI.PrivateUnion (fromUnion names u))
    I.AliasPublic  _ -> Nothing
    I.AliasPrivate _ -> Nothing


toIAlias :: Names -> I.TypeDecl -> Maybe CI.Alias
toIAlias names decl =
  case decl of
    I.AliasPublic  a -> Just (CI.PublicAlias  (fromAlias names a))
    I.AliasPrivate a -> Just (CI.PrivateAlias (fromAlias names a))
    I.UnionOpen    _ -> Nothing
    I.UnionClosed  _ -> Nothing
    I.UnionPrivate _ -> Nothing


{-| Every interface, with the alias parameter names of all of them. -}
fromInterfaces :: Map.Map Module.Name I.Interface -> Map.Map Module.Name CI.Interface
fromInterfaces ifaces =
  let
    names = namesFromInterfaces ifaces
  in
  Map.map (fromInterface names) ifaces


{-| The unions and aliases declared in a module being compiled. -}
fromTypeDecls :: Names -> Map.Map T.Name New.TypeDecl -> (Map.Map T.Name Can.Union, Map.Map T.Name Can.Alias)
fromTypeDecls names types =
  ( unionsOf names types
  , aliasesOf names types
  )


unionsOf :: Names -> Map.Map T.Name New.TypeDecl -> Map.Map T.Name Can.Union
unionsOf names types =
  Map.mapMaybe (\decl -> case decl of { New.Union u -> Just (fromUnion names u); New.Alias _ -> Nothing }) types


aliasesOf :: Names -> Map.Map T.Name New.TypeDecl -> Map.Map T.Name Can.Alias
aliasesOf names types =
  Map.mapMaybe (\decl -> case decl of { New.Alias a -> Just (fromAlias names a); New.Union _ -> Nothing }) types



--------------------------------------------------------------------------------
-- 0.19.3 TO 0.19.4 ------------------------------------------------------------
--------------------------------------------------------------------------------


{-| What converting generated definitions needs to know: the declarations of the
types they refer to, and the annotations of the foreign values they use. -}
data Env =
  Env
    { _env_types  :: ModuleName.Canonical -> T.Name -> Maybe I.TypeDecl
    , _env_ctors  :: ModuleName.Canonical -> N.Name -> Maybe (T.Name, I.TypeDecl)
    , _env_values :: ModuleName.Canonical -> N.Name -> Maybe I.Annotation
    }


envFromInterfaces :: Map.Map Module.Name I.Interface -> Env
envFromInterfaces ifaces =
  let
    ifaceFor (ModuleName.Canonical pkg home) =
      case Map.lookup home ifaces of
        Just iface@(I.Interface ipkg _ _ _) | ipkg == pkg -> Just iface
        _ -> Nothing

    ctorTables =
      Map.mapWithKey (\_ (I.Interface _ _ types _) -> ctorTable types) ifaces
  in
  Env
    { _env_types = \h n ->
        Map.lookup n . I._types =<< ifaceFor h
    , _env_ctors = \h@(ModuleName.Canonical _ home) n ->
        case ifaceFor h of
          Just _ -> Map.lookup n =<< Map.lookup home ctorTables
          Nothing -> Nothing
    , _env_values = \h n ->
        Map.lookup n . I._values =<< ifaceFor h
    }


{-| The env, with the types declared in the module being compiled. -}
envWithModule :: ModuleName.Canonical -> Map.Map T.Name New.TypeDecl -> Env -> Env
envWithModule home types (Env otherTypes otherCtors otherValues) =
  let
    decls = Map.map toIDecl types
    ctors = ctorTable decls
  in
  Env
    { _env_types = \h n ->
        if h == home then Map.lookup n decls else otherTypes h n
    , _env_ctors = \h n ->
        if h == home then Map.lookup n ctors else otherCtors h n
    , _env_values = otherValues
    }
  where
    toIDecl decl =
      case decl of
        New.Union u -> I.UnionOpen u
        New.Alias a -> I.AliasPublic a


-- constructor name to its type, for the unions (and record aliases) of a module
ctorTable :: Map.Map T.Name I.TypeDecl -> Map.Map N.Name (T.Name, I.TypeDecl)
ctorTable types =
  -- the constructors of custom types, then those of record aliases: in Elm only a record
  -- alias has a constructor, and a custom type's constructor can have the name of an
  -- alias (`type alias Month = Time.Month` next to `type Unit = Month | Year`)
  Map.union
    (Map.foldrWithKey addUnionCtors Map.empty types)
    (Map.foldrWithKey addRecordCtor Map.empty types)
  where
    addUnionCtors name decl dict =
      case unionOf decl of
        Just (I.Union _ _ variants) -> List.foldl' (\d n -> Map.insert n (name, decl) d) dict (variantNames variants)
        Nothing -> dict

    addRecordCtor name decl dict =
      case aliasOf decl of
        Just (I.Alias _ _ (I.Aliased (I.TRecord _))) -> Map.insert (T.nameToName name) (name, decl) dict
        _ -> dict


variantNames :: I.Variants -> [N.Name]
variantNames variants =
  case variants of
    I.Variants table -> Tag.unpack table (\_ ns -> ns) (\_ (I.Variant n _) -> n)
    I.Enum table     -> Tag.unpack table (\_ ns -> ns) (\_ n -> n)
    I.Vector n _ _ _ -> [n]
    I.Box n _        -> [n]
    I.Void n         -> [n]


unionOf :: I.TypeDecl -> Maybe I.Union
unionOf decl =
  I.extract Just (const Nothing) decl


aliasOf :: I.TypeDecl -> Maybe I.Alias
aliasOf decl =
  I.extract (const Nothing) Just decl



-- TYPES


{-| A type and the registry of its type variables, in the order Canonicalize.Type
registers them. -}
type Vars =
  TI.Registry T.Var


toAnnotation :: Env -> Can.Annotation -> I.Annotation
toAnnotation env (Can.Forall _ tipe) =
  let
    (vars, itype) = toTypeWith env TI.empty tipe
  in
  I.Forall (TI.toArray vars) itype


toType :: Env -> Can.Type -> (A.Array T.Var, I.Type)
toType env tipe =
  let
    (vars, itype) = toTypeWith env TI.empty tipe
  in
  (TI.toArray vars, itype)


toTypeWith :: Env -> Vars -> Can.Type -> (Vars, I.Type)
toTypeWith env vars tipe =
  case tipe of
    Can.TLambda arg rest ->
      -- `a -> b -> c` is one TLambda a b [c], as Parse.Type gives it
      case lambdaChain rest of
        (second, more) ->
          let
            (vars1, ia) = toTypeWith env vars arg
            (vars2, ib) = toTypeWith env vars1 second
            (vars3, ics) = mapAccum (toTypeWith env) vars2 more
          in
          (vars3, I.TLambda ia ib ics)

    Can.TVar var ->
      let (i, vars1) = TI.register var vars in
      (vars1, I.TVar i)

    Can.TType h n args ->
      let (vars1, xs) = mapAccum (toTypeWith env) vars args in
      (vars1, I.TType h n (unionMask env h n) (A.fromList xs))

    Can.TAlias h n args aliasType ->
      let
        (vars1, xs) = mapAccum (toTypeWith env) vars (map snd args)
        (mask, aliased) = aliasInfo env h n args aliasType
      in
      (vars1, I.TAlias h n mask (A.fromList xs) aliased)

    Can.TUnit ->
      (vars, I.TUnit)

    Can.TPair a b ->
      let
        (vars1, ia) = toTypeWith env vars a
        (vars2, ib) = toTypeWith env vars1 b
      in
      (vars2, I.TPair ia ib)

    Can.TTriple a b c ->
      let
        (vars1, ia) = toTypeWith env vars a
        (vars2, ib) = toTypeWith env vars1 b
        (vars3, ic) = toTypeWith env vars2 c
      in
      (vars3, I.TTriple ia ib ic)

    Can.TRecord fields maybeExt ->
      case NEM.fromDict fields of
        Nothing ->
          case maybeExt of
            Nothing -> (vars, I.TEmpty)
            Just ext -> let (i, vars1) = TI.register ext vars in (vars1, I.TVar i)

        Just nem ->
          let
            -- fields in key order, as Canonicalize.Type traverses them
            (vars1, typed) = mapAccum (\vs (n, Can.FieldType _ t) -> fmap ((,) n) (toTypeWith env vs t)) vars (NEM.toList nem)
            ifields = toIFields fields (Map.fromList typed)
          in
          case maybeExt of
            Nothing -> (vars1, I.TRecord ifields)
            Just ext -> let (i, vars2) = TI.register ext vars1 in (vars2, I.TExtRec ifields i)


-- the types of `b -> c -> d` as `b` and [c, d]
lambdaChain :: Can.Type -> (Can.Type, [Can.Type])
lambdaChain tipe =
  case tipe of
    Can.TLambda a b ->
      let (x, xs) = lambdaChain b in
      (a, x : xs)

    _ ->
      (tipe, [])


-- the record fields with their source order, when they have one (indexes 0..n-1)
toIFields :: Map.Map N.Name Can.FieldType -> Map.Map N.Name a -> F.Fields a
toIFields fields typed =
  let
    indexes = map (\(Can.FieldType i _) -> fromIntegral i) (Map.elems fields)
    isOrdered = List.sort indexes == [0 .. Map.size fields - 1]
  in
  case NEM.fromDict typed of
    Nothing ->
      error "Lamdera.Compat.Convert.toIFields: no fields"

    Just nem ->
      if isOrdered then
        Maybe.fromJust $
          F.fromOrdered Just $
            NEM.mapWithKey (\n t -> F.Ordered (fieldIndex n) t) nem
      else
        F.fromUnordered nem
  where
    fieldIndex n =
      case Map.lookup n fields of
        Just (Can.FieldType i _) -> fromIntegral i
        Nothing -> 0


mapAccum :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
mapAccum f s xs =
  case xs of
    [] -> (s, [])
    x:rest ->
      let
        (s1, y) = f s x
        (s2, ys) = mapAccum f s1 rest
      in
      (s2, y : ys)


{-| The mask of a custom type: from its declaration when it is known. Only the kernel
types of elm/core have masks other than 0b00 (see M.void), and List. -}
unionMask :: Env -> ModuleName.Canonical -> T.Name -> M.Mask
unionMask env h n =
  case _env_types env h n >>= unionOf of
    Just (I.Union mask _ _) ->
      mask

    Nothing
      | h == ModuleName.list && n == T.list -> M.list
      | otherwise -> M.void h n


{-| The mask and the declared body of an alias: from its declaration when it is known,
and otherwise from the body Wire3 wrote, with the alias's arguments as indexes. -}
aliasInfo :: Env -> ModuleName.Canonical -> T.Name -> [(T.Var, Can.Type)] -> Can.AliasType -> (M.Mask, I.Aliased)
aliasInfo env h n args aliasType =
  case _env_types env h n >>= aliasOf of
    Just (I.Alias mask _ aliased) ->
      (mask, aliased)

    Nothing ->
      let
        body =
          case aliasType of
            Can.Holey t ->
              t

            Can.Filled t ->
              -- the arguments are filled in: put the parameters back where they are
              List.foldl' (\b (param, arg) -> replaceType arg (Can.TVar param) b) t args

        params =
          TI.fromArray (A.fromList (map fst args))

        (_, ibody) =
          toTypeWith env params body
      in
      (maskOf env ibody, I.Aliased ibody)


replaceType :: Can.Type -> Can.Type -> Can.Type -> Can.Type
replaceType from to tipe =
  if tipe == from then
    to
  else
    case tipe of
      Can.TLambda a b -> Can.TLambda (replaceType from to a) (replaceType from to b)
      Can.TVar _ -> tipe
      Can.TType h n xs -> Can.TType h n (map (replaceType from to) xs)
      Can.TAlias h n xs a -> Can.TAlias h n (map (fmap (replaceType from to)) xs) a
      Can.TUnit -> tipe
      Can.TPair a b -> Can.TPair (replaceType from to a) (replaceType from to b)
      Can.TTriple a b c -> Can.TTriple (replaceType from to a) (replaceType from to b) (replaceType from to c)
      Can.TRecord fs ext -> Can.TRecord (Map.map (\(Can.FieldType i t) -> Can.FieldType i (replaceType from to t)) fs) ext


{-| The mask of a type, as Canonicalize.TypeDecl works it out for an alias. -}
maskOf :: Env -> I.Type -> M.Mask
maskOf env tipe =
  case tipe of
    I.TLambda _ _ _ -> M.lambda
    I.TVar _ -> M.var
    I.TType _ _ m xs -> M.args m (maskOf env) xs
    I.TAlias _ _ m xs _ -> M.args m (maskOf env) xs
    I.TUnit -> M.unit
    I.TPair a b -> M.pairOf (maskOf env) a b
    I.TTriple a b c -> M.tripleOf (maskOf env) a b c
    I.TEmpty -> M.empty
    I.TRecord fs -> M.recordOf (maskOf env) fs
    I.TExtRec fs _ -> M.recordOf (maskOf env) fs



-- DECLS


{-| Generated definitions, grouped like Lamdera.Wire3.Graph groups them. -}
data Group
  = Single Can.Def
  | Recursive Can.Def [Can.Def]


declsFromGroups :: Env -> ModuleName.Canonical -> [Group] -> [New.Decl]
declsFromGroups env home groups =
  map toDecl groups
  where
    toDecl group =
      case group of
        Single def -> New.Decl (toDef env home def)
        Recursive d ds -> New.DeclRec (toDef env home d) (map (toDef env home) ds)


toDecls :: Env -> ModuleName.Canonical -> Can.Decls -> [New.Decl]
toDecls env home decls =
  case decls of
    Can.Declare def rest -> New.Decl (toDef env home def) : toDecls env home rest
    Can.DeclareRec d ds rest -> New.DeclRec (toDef env home d) (map (toDef env home) ds) : toDecls env home rest
    Can.SaveTheEnvironment -> []



-- DEFS


toDef :: Env -> ModuleName.Canonical -> Can.Def -> New.Def
toDef env home def =
  case def of
    Can.Def name patterns body ->
      case patterns of
        [] -> New.DValue_ name (toExpr env home body)
        p:ps -> New.DFunc_ name (toPattern env home p) (map (toPattern env home) ps) (toExpr env home body)

    Can.TypedDef name freeVars typedArgs body resultType ->
      let
        tipe = foldr (\(_, t) r -> Can.TLambda t r) resultType typedArgs
        anno@(I.Forall _ itype) = toAnnotation env (Can.Forall freeVars tipe)
        ibody = toExpr env home body
      in
      case typedArgs of
        [] ->
          New.DValue name ibody itype anno

        (p, _) : rest ->
          let
            patterns = p : map fst rest
            (argTypes, result) = unrollArgs (length patterns) itype
            pargs = zipWith3 (\i t pat -> New.PArg (Index.ZeroBased i) t (toPattern env home pat)) [0..] argTypes patterns
          in
          case pargs of
            parg : more -> New.DFunc name parg more ibody result anno
            [] -> error "Lamdera.Compat.Convert.toDef: no arguments"


toFunc :: Env -> ModuleName.Canonical -> Can.Def -> Either New.Value New.Func
toFunc env home def =
  case toDef env home def of
    New.DValue_ n e -> Left (New.Value_ n e)
    New.DValue n e t a -> Left (New.Value n e t a)
    New.DFunc_ n p ps e -> Right (New.Func_ n p ps e)
    New.DFunc n p ps e t a -> Right (New.Func n p ps e t a)


{-| The types of the first `n` arguments of a function type and the type of the rest,
as Canonicalize.Pattern.canTypedArgs unrolls an annotation (through aliases, with their
arguments filled in). -}
unrollArgs :: Int -> I.Type -> ([I.Type], I.Type)
unrollArgs n tipe =
  if n <= 0 then
    ([], tipe)
  else
    case tipe of
      I.TLambda t x xs ->
        let (ts, result) = unrollArgs (n - 1) (rebuild x xs) in
        (t : ts, result)

      I.TAlias _ _ _ xs aliased ->
        unrollArgs n (I.dealias xs aliased)

      _ ->
        ([], tipe)
  where
    rebuild x xs =
      case xs of
        [] -> x
        y:ys -> I.TLambda x y ys



-- EXPRESSIONS


toExpr :: Env -> ModuleName.Canonical -> Can.Expr -> New.Expr
toExpr env home (A.At region expression) =
  let
    go = toExpr env home
    pat = toPattern env home
  in
  A.At region $
  case expression of
    Can.VarLocal n ->
      New.VarLocal n

    Can.VarTopLevel h n ->
      New.VarTopLevel h n

    Can.VarKernel h n ->
      New.VarKernel h n

    Can.VarForeign h n anno ->
      New.VarForeign h n (foreignAnnotation env h n anno)

    Can.VarCtor _ h n _ anno ->
      toCtor env h n anno

    Can.VarDebug h n anno ->
      New.VarDebug h n (foreignAnnotation env h n anno)

    Can.VarOperator op h n anno ->
      New.VarOperator op h n (foreignAnnotation env h n anno)

    Can.Chr c ->
      New.Chr c

    Can.Str s ->
      New.Str s

    Can.Int i ->
      New.Int i

    Can.Float f ->
      New.Float f

    Can.List es ->
      New.List (map go es)

    Can.Negate e ->
      New.Negate (go e)

    Can.Binop op h n anno l r ->
      New.Binop op h n (foreignAnnotation env h n anno) (go l) (go r)

    Can.Lambda patterns body ->
      case patterns of
        p:ps -> New.Lambda (pat p) (map pat ps) (go body)
        [] -> A.toValue (go body)

    Can.Call func args ->
      case args of
        a:as -> New.Call (go func) (go a) (map go as)
        [] -> A.toValue (go func)

    Can.If branches final ->
      New.If (map (\(c, b) -> (go c, go b)) branches) (go final)

    Can.Let def body ->
      case toFunc env home def of
        Left value -> New.LetValue value (go body)
        Right func -> New.LetFunc func (go body)

    Can.LetRec defs body ->
      case Maybe.mapMaybe (either (const Nothing) Just . toFunc env home) defs of
        f:fs -> New.LetRec f fs (go body)
        [] -> error "Lamdera.Compat.Convert.toExpr: a LetRec with no functions"

    Can.LetDestruct p e body ->
      New.LetDestruct (pat p) (go e) (go body)

    Can.Case e branches ->
      case map (\(Can.CaseBranch p b) -> New.Branch (pat p) (go b)) branches of
        b:bs -> New.Case (go e) b bs
        [] -> error "Lamdera.Compat.Convert.toExpr: a case with no branches"

    Can.Accessor field ->
      New.Accessor field

    Can.Access record field ->
      New.Access (go record) field

    Can.Update name record fields ->
      case NEM.fromDict (Map.map (\(Can.FieldUpdate r e) -> New.Update_ r (go e)) fields) of
        Just nem -> New.Update name (go record) (F.fromUnordered nem)
        Nothing -> A.toValue (go record)

    Can.Record fields ->
      case NEM.fromDict (Map.map go fields) of
        Just nem -> New.Record (F.fromUnordered nem)
        Nothing -> New.Empty

    Can.Unit ->
      New.Unit

    Can.Pair a b ->
      New.Pair (go a) (go b)

    Can.Triple a b c ->
      New.Triple (go a) (go b) (go c)

    Can.Shader src types ->
      New.Shader src types


{-| The annotation of a foreign value: the one in its interface when its module is
imported, as the canonicalizer would use, and otherwise the one Wire3 wrote. -}
foreignAnnotation :: Env -> ModuleName.Canonical -> N.Name -> Can.Annotation -> I.Annotation
foreignAnnotation env h n anno =
  case _env_values env h n of
    Just annotation -> annotation
    Nothing -> toAnnotation env anno


{-| A constructor as Canonicalize.Expression.toUnionCtor / toAliasCtor make it. -}
toCtor :: Env -> ModuleName.Canonical -> N.Name -> Can.Annotation -> New.Expr_
toCtor env h n anno =
  case _env_ctors env h n of
    Just (typeName, decl) ->
      case decl of
        I.AliasPublic alias -> recordCtor h n typeName alias
        I.AliasPrivate alias -> recordCtor h n typeName alias
        _ ->
          case unionOf decl of
            Just union -> unionCtor h n typeName union
            Nothing -> fallback

    Nothing ->
      fallback
  where
    fallback =
      -- not found: only Bool and the kernel types of elm/core could be missing, as
      -- generated code refers to the constructors of types its module knows
      if h == ModuleName.basics && (n == N.true || n == N.false) then
        New.Bool (n == N.true)
      else
        error $ "Lamdera.Compat.Convert.toCtor: no declaration for the constructor " ++ N.toChars n


recordCtor :: ModuleName.Canonical -> N.Name -> T.Name -> I.Alias -> New.Expr_
recordCtor h n typeName (I.Alias mask vars aliased@(I.Aliased tipe)) =
  case tipe of
    I.TRecord fields ->
      let
        ((f, t), (fs, ts)) = fmap List.unzip (F.flatten fields)
        result = I.TAlias h typeName mask (TI.imap I.TVar vars) aliased
        anno = I.Forall vars (snoc1 t ts result)
      in
      New.CRecord h n anno f fs

    I.TEmpty ->
      -- an alias of {}: its constructor is the value {} (Canonicalize.TypeDecl.addAlias)
      New.Empty

    _ ->
      error $ "Lamdera.Compat.Convert.recordCtor: not a record alias " ++ N.toChars n


snoc1 :: I.Type -> [I.Type] -> I.Type -> I.Type
snoc1 t ts result =
  LU.snoc1 (I.TLambda t) ts result


unionCtor :: ModuleName.Canonical -> N.Name -> T.Name -> I.Union -> New.Expr_
unionCtor h n typeName (I.Union mask vars variants) =
  let
    result = I.TType h typeName mask (TI.imap I.TVar vars)
  in
  case variants of
    I.Variants table ->
      case findVariant n table of
        Just (tag, I.Variant _ args) ->
          New.CVariant h n (toAnno vars args result) tag (length args)

        Nothing ->
          error $ "Lamdera.Compat.Convert.unionCtor: no variant " ++ N.toChars n

    I.Enum table ->
      case findEnum n table of
        Just tag ->
          if typeName == T.bool && h == ModuleName.basics
            then New.Bool (tag == Tag.zero)
            else New.CEnum h n (I.Forall vars result) tag

        Nothing ->
          error $ "Lamdera.Compat.Convert.unionCtor: no enum " ++ N.toChars n

    I.Vector _ a b cs ->
      New.CVector h n (I.Forall vars (I.TLambda a b (cs ++ [result]))) (2 + length cs)

    I.Box _ arg ->
      New.CBox h n (I.Forall vars (I.TLambda arg result []))

    I.Void _ ->
      New.CVoid h n (I.Forall vars result)


toAnno :: A.Array T.Var -> [I.Type] -> I.Type -> I.Annotation
toAnno vars args result =
  I.Forall vars $
    case args of
      []   -> result
      t:ts -> snoc1 t ts result


findVariant :: N.Name -> Tag.Table I.Variant -> Maybe (Tag.Tag, I.Variant)
findVariant n table =
  Tag.foldlWithTag (\tag v@(I.Variant name _) found -> if name == n then Just (tag, v) else found) Nothing table


findEnum :: N.Name -> Tag.Table N.Name -> Maybe Tag.Tag
findEnum n table =
  Tag.foldlWithTag (\tag name found -> if name == n then Just tag else found) Nothing table



-- PATTERNS


toPattern :: Env -> ModuleName.Canonical -> Can.Pattern -> New.Pattern
toPattern env home (A.At region pattern) =
  let
    go = toPattern env home
  in
  A.At region $
  case pattern of
    Can.PAnything ->
      New.PAnything

    Can.PVar n ->
      New.PVar n

    Can.PRecord fields ->
      case fields of
        [] -> New.PAnything
        f:fs -> New.PRecord (NEM.fromList (f, ()) (map (\x -> (x, ())) fs))

    Can.PAlias p n ->
      New.PAlias (go p) n

    Can.PUnit ->
      New.PUnit

    Can.PPair a b ->
      New.PPair (go a) (go b)

    Can.PTriple a b c ->
      New.PTriple (go a) (go b) (go c)

    Can.PList ps ->
      New.PList (map go ps)

    Can.PCons a b ->
      New.PCons (go a) (go b)

    Can.PBool _ b ->
      case boolType env of
        Just (ptype, tagOf) ->
          let name = if b then N.true else N.false in
          New.PBool b name (tagOf name) (New._p_variants ptype)

        Nothing ->
          error "Lamdera.Compat.Convert.toPattern: Bool is not known"

    Can.PChr c ->
      New.PChr c

    Can.PStr s ->
      New.PStr s

    Can.PInt i ->
      New.PInt i

    Can.PCtor h typeName _ name _ args ->
      case _env_types env h typeName >>= unionOf of
        Just union ->
          ctorPattern env home h typeName name union args

        Nothing ->
          error $ "Lamdera.Compat.Convert.toPattern: no declaration for " ++ T.nameToChars typeName


boolType :: Env -> Maybe (New.PType, N.Name -> Tag.Tag)
boolType env =
  case _env_types env ModuleName.basics T.bool >>= unionOf of
    Just (I.Union mask vars variants@(I.Enum table)) ->
      Just
        ( New.PType ModuleName.basics T.bool mask vars (variantNits variants)
        , \n -> Maybe.fromMaybe Tag.zero (findEnum n table)
        )

    _ ->
      Nothing


ctorPattern :: Env -> ModuleName.Canonical -> ModuleName.Canonical -> T.Name -> N.Name -> I.Union -> [Can.PatternCtorArg] -> New.Pattern_
ctorPattern env home h typeName name (I.Union mask vars variants) args =
  let
    ptype = New.PType h typeName mask vars (variantNits variants)
    toArg i t (Can.PatternCtorArg _ _ p) = New.PArg (Index.ZeroBased i) t (toPattern env home p)
  in
  case variants of
    I.Variants table ->
      case findVariant name table of
        Just (tag, I.Variant _ argTypes) ->
          New.PVariant name ptype tag (zipWith3 toArg [0..] argTypes args)

        Nothing ->
          error $ "Lamdera.Compat.Convert.ctorPattern: no variant " ++ N.toChars name

    I.Enum table ->
      case findEnum name table of
        Just tag ->
          if typeName == T.bool && h == ModuleName.basics
            then New.PBool (name == N.true) name tag (variantNits variants)
            else New.PEnum name ptype tag

        Nothing ->
          error $ "Lamdera.Compat.Convert.ctorPattern: no enum " ++ N.toChars name

    I.Vector _ a b cs ->
      case zipWith3 toArg [0..] (a : b : cs) args of
        x : y : zs -> New.PVector name ptype x y zs
        _ -> error $ "Lamdera.Compat.Convert.ctorPattern: wrong number of arguments for " ++ N.toChars name

    I.Box _ argType ->
      case args of
        [Can.PatternCtorArg _ _ p] -> New.PBox name ptype argType (toPattern env home p)
        _ -> error $ "Lamdera.Compat.Convert.ctorPattern: wrong number of arguments for " ++ N.toChars name

    I.Void _ ->
      New.PVoid name ptype


variantNits :: I.Variants -> Nit.Variants
variantNits variants =
  case variants of
    I.Variants table ->
      Tag.unpack table Nit.Variants_ $ \t (I.Variant n xs) -> Nit.Variant n t (List.length xs)

    I.Enum table ->
      Tag.unpack table Nit.Variants_ $ \t n -> Nit.Variant n t 0

    I.Vector n _ _ cs ->
      Nit.vector (2 + length cs) n

    I.Box n _ ->
      Nit.box n

    I.Void n ->
      Nit.void n


