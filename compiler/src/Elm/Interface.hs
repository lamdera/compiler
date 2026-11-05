{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, TemplateHaskell #-}
module Elm.Interface
  ( Interface(..)
  , TypeDecl(..)
  , Binop(..)
  --
  , Annotation(..)
  , Type(..)
  --
  , Alias(..)
  , Aliased(..)
  --
  , Union(..)
  , Variants(..)
  , Variant(..)
  --
  , dealias
  , deepDealias
  --
  , DependencyInterface(..)
  , PrivateTypeDecl(..)
  , public
  , private
  , privatize
  , extract
  --
  , eInterface, dInterface
  , eDependencyInterface, dDependencyInterface
  , eType, dType
  --
  , share -- @LAMDERA
  )
  where


import Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5)
import qualified Control.Monad.State.Strict as State -- @LAMDERA
import Data.Bits (xor) -- @LAMDERA
import Data.Coerce (coerce)
import qualified Data.IntMap.Strict as IntMap -- @LAMDERA
import qualified Data.List as List -- @LAMDERA
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO) -- @LAMDERA
import System.Mem.StableName (StableName, makeStableName, hashStableName) -- @LAMDERA

import qualified Array as A
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash

import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified AST.Prim.Module as Module -- @LAMDERA
import qualified String as S -- @LAMDERA



-- INTERFACE


data Interface =
  Interface
    { _home   :: Pkg.Name
    , _values :: Map.Map N.Name Annotation
    , _types  :: Map.Map T.Name TypeDecl
    , _binops  :: Map.Map Op.Name Binop
    }
  deriving (Eq)


data TypeDecl
  = UnionOpen    Union
  | UnionClosed  Union
  | UnionPrivate Union
  --
  | AliasPublic  Alias
  | AliasPrivate Alias
  deriving (Eq)


data Binop =
  Binop N.Name Annotation Op.Associativity Op.Precedence
  deriving (Eq)



-- TYPES
--
-- The fields of TRecord carry an index so that they can be resorted into the
-- order they appeared in the source file. THIS ORDER IS NOT GUARANTEED TO BE
-- CORRECT WHEN THE TYPE IS INFERRED, but it is correct in all other cases.
--
-- For example, adding type annotations will guarantee that your Annotation
-- uses the explicitly stated field order. And most importantly, Aliased types
-- preserve the source order to make sure the record constructor arguments are
-- in the right order.


data Annotation =
  Forall (A.Array T.Var) Type
  deriving (Eq)


data Type
  = TLambda Type Type [Type]
  | TVar T.Index
  | TType  ModuleName.Canonical T.Name M.Mask (A.Array Type)
  | TAlias ModuleName.Canonical T.Name M.Mask (A.Array Type) Aliased
  | TUnit
  | TPair Type Type
  | TTriple Type Type Type
  | TEmpty
  | TRecord (F.Fields Type)
  | TExtRec (F.Fields Type) T.Index
  deriving (Eq)



-- ALIAS


data Alias = Alias M.Mask (A.Array T.Var) Aliased
  deriving (Eq)


newtype Aliased = Aliased Type
  deriving (Eq)



-- UNION


data Union =
  Union M.Mask (A.Array T.Var) Variants
  deriving (Eq)


data Variants
  = Variants (Tag.Table Variant)
  | Enum     (Tag.Table N.Name)
  | Vector N.Name Type Type [Type]
  | Box    N.Name Type
  | Void   N.Name
  deriving (Eq)


data Variant = Variant N.Name [Type]
  deriving (Eq)



-- DEALIAS


dealias :: A.Array Type -> Aliased -> Type
dealias args (Aliased aliased) =
    go aliased
  where
    go tipe =
      case tipe of
        TLambda x y zs    -> TLambda (go x) (go y) (map go zs)
        TVar   i          -> T.get i args
        TType  h n m xs   -> TType  h n m (A.map go xs)
        TAlias h n m xs t -> TAlias h n m (A.map go xs) t
        TUnit             -> TUnit
        TPair   a b       -> TPair (go a) (go b)
        TTriple a b c     -> TTriple (go a) (go b) (go c)
        TEmpty            -> TEmpty
        TRecord fs        -> TRecord (F.map go fs)
        TExtRec fs ext    -> extend (F.map go fs) (T.get ext args)

    -- @LAMDERA `tipe` is an argument already substituted (T.get ext args), so its own type
    -- variables are the caller's, not this alias's. 0.19.4-beta looked them up in args again:
    -- for `FieldProperties { a | value : String }` (FieldProperties being `{ a | ... }`) the
    -- TExtRec case found the same record every time and never stopped, so building docs (and
    -- every package depending on such a package) used memory until the machine ran out.
    -- Type.Type.termToType's goExtRec does the same thing correctly.
    extend fields tipe =
      case tipe of
        TEmpty            -> TRecord fields
        TRecord fs        -> TRecord (F.extend fields fs)
        TExtRec fs ext    -> TExtRec (F.extend fields fs) ext
        TVar i            -> TExtRec fields i
        TAlias _ _ _ xs t -> extend fields (dealias xs t)
        TType  _ _ _ _    -> $(Crash.crash 'dealias) "ran into invalid record extension (Union)"
        TLambda _ _ _     -> $(Crash.crash 'dealias) "ran into invalid record extension (Lambda)"
        TUnit             -> $(Crash.crash 'dealias) "ran into invalid record extension (Unit)"
        TPair _ _         -> $(Crash.crash 'dealias) "ran into invalid record extension (Pair)"
        TTriple _ _ _     -> $(Crash.crash 'dealias) "ran into invalid record extension (Triple)"



-- DEEP DEALIAS


deepDealias :: Type -> Type
deepDealias tipe =
  case tipe of
    TLambda a b cs    -> TLambda (deepDealias a) (deepDealias b) (map deepDealias cs)
    TVar _            -> tipe
    TType  h n m xs   -> TType h n m (A.map deepDealias xs)
    TAlias _ _ _ xs t -> deepDealias (dealias xs t)
    TUnit             -> TUnit
    TPair   a b       -> TPair   (deepDealias a) (deepDealias b)
    TTriple a b c     -> TTriple (deepDealias a) (deepDealias b) (deepDealias c)
    TEmpty            -> TEmpty
    TRecord fs        -> TRecord (F.map deepDealias fs)
    TExtRec fs x      -> TExtRec (F.map deepDealias fs) x



-- DEPENDENCY INTERFACE


data DependencyInterface
  = Public Interface
  | Private Pkg.Name (Map.Map T.Name PrivateTypeDecl)


data PrivateTypeDecl
  = PrivateUnion Union
  | PrivateAlias Alias


public :: Interface -> DependencyInterface
public =
  Public


private :: Interface -> DependencyInterface
private (Interface pkg _ types _) =
  Private pkg (Map.map (extract PrivateUnion PrivateAlias) types)


extract :: (Union -> a) -> (Alias -> a) -> TypeDecl -> a
extract okU okA decl =
  case decl of
    UnionOpen    u -> okU u
    UnionClosed  u -> okU u
    UnionPrivate u -> okU u
    AliasPublic  a -> okA a
    AliasPrivate a -> okA a


privatize :: DependencyInterface -> DependencyInterface
privatize di =
  case di of
    Public  i   -> private i
    Private _ _ -> di



--------------------------------------------------------------------------------
-- BINARY ----------------------------------------------------------------------
--------------------------------------------------------------------------------



-- INTERFACE


-- @LAMDERA Every Type in an interface is written once, in a type table at the start of
-- the interface, and referred to by its index everywhere else. See TYPE TABLE below for
-- why and how.


eInterface :: Interface -> E.Builder
eInterface (Interface h vs ts bs) =
  withTypeTable $ \ref ->
    do  vs_ <- traverse (eAnnotation ref) vs
        ts_ <- traverse (eTypeDecl ref) ts
        bs_ <- traverse (eBinop ref) bs
        return $
          Pkg.eName h
          <> E.dict32 N.encode id vs_
          <> E.dict32 T.encode id ts_
          <> E.dict32 Op.encode id bs_


dInterface :: D.Decoder Interface
dInterface =
  do  table <- dTypeTable
      liftM4 Interface
        Pkg.dName
        (D.dict32 N.decode (dAnnotation table))
        (D.dict32 T.decode (dTypeDecl table))
        (D.dict32 Op.decode (dBinop table))



-- TYPE DECL


eTypeDecl :: Ref -> TypeDecl -> Interning E.Builder
eTypeDecl ref union =
  case union of
    UnionOpen    u -> (E.u8# 0#Word8 <>) <$> eUnion ref u
    UnionClosed  u -> (E.u8# 1#Word8 <>) <$> eUnion ref u
    UnionPrivate u -> (E.u8# 2#Word8 <>) <$> eUnion ref u
    AliasPublic  a -> (E.u8# 3#Word8 <>) <$> eAlias ref a
    AliasPrivate a -> (E.u8# 4#Word8 <>) <$> eAlias ref a


dTypeDecl :: TypeTable -> D.Decoder TypeDecl
dTypeDecl table =
  do  n <- D.u8
      case n of
        0 -> UnionOpen    <$> dUnion table
        1 -> UnionClosed  <$> dUnion table
        2 -> UnionPrivate <$> dUnion table
        3 -> AliasPublic  <$> dAlias table
        4 -> AliasPrivate <$> dAlias table
        _ -> D.expecting "TypeDecl"



-- BINOP


eBinop :: Ref -> Binop -> Interning E.Builder
eBinop ref (Binop n t a p) =
  do  t_ <- eAnnotation ref t
      return (N.encode n <> t_ <> eAssociativity a <> ePrecedence p)


dBinop :: TypeTable -> D.Decoder Binop
dBinop table =
  liftM4 Binop N.decode (dAnnotation table) dAssociativity dPrecedence


dPrecedence :: D.Decoder Op.Precedence
dPrecedence =
  coerce D.u8


ePrecedence :: Op.Precedence -> E.Builder
ePrecedence (Op.Precedence n) =
  E.u8 n


dAssociativity :: D.Decoder Op.Associativity
dAssociativity =
  do  n <- D.u8
      case n of
        0 -> return Op.Left
        1 -> return Op.Non
        2 -> return Op.Right
        _ -> D.expecting "Associativity"


eAssociativity :: Op.Associativity -> E.Builder
eAssociativity assoc =
  case assoc of
    Op.Left  -> E.u8# 0#Word8
    Op.Non   -> E.u8# 1#Word8
    Op.Right -> E.u8# 2#Word8



-- DEPENDENCY INTERFACE


eDependencyInterface :: DependencyInterface -> E.Builder
eDependencyInterface iface =
  case iface of
    Public  i   -> E.u8# 0#Word8 <> eInterface i
    -- @LAMDERA a Private interface has its own type table too, after the tag
    Private n t ->
      E.u8# 1#Word8 <>
        withTypeTable (\ref ->
          do  t_ <- traverse (ePrivateTypeDecl ref) t
              return (Pkg.eName n <> E.dict32 T.encode id t_)
        )


dDependencyInterface :: D.Decoder DependencyInterface
dDependencyInterface =
  do  n <- D.u8
      case n of
        0 -> liftM  Public dInterface
        1 ->
          do  table <- dTypeTable
              liftM2 Private Pkg.dName (D.dict32 T.decode (dPrivateTypeDecl table))
        _ -> D.expecting "DependencyInterface"


ePrivateTypeDecl :: Ref -> PrivateTypeDecl -> Interning E.Builder
ePrivateTypeDecl ref union =
  case union of
    PrivateUnion u -> (E.u8# 0#Word8 <>) <$> eUnion ref u
    PrivateAlias a -> (E.u8# 1#Word8 <>) <$> eAlias ref a


dPrivateTypeDecl :: TypeTable -> D.Decoder PrivateTypeDecl
dPrivateTypeDecl table =
  do  n <- D.u8
      case n of
        0 -> PrivateUnion <$> dUnion table
        1 -> PrivateAlias <$> dAlias table
        _ -> D.expecting "PrivateTypeDecl"



-- ALIAS


eAlias :: Ref -> Alias -> Interning E.Builder
eAlias ref (Alias m vs (Aliased t)) =
  do  t_ <- eTypeRef ref t
      return (M.eMask m <> E.array8 T.eVar vs <> t_)


dAlias :: TypeTable -> D.Decoder Alias
dAlias table =
  liftM3 Alias M.dMask (D.array8 T.dVar) (Aliased <$> dTypeRef table)



-- UNION


eUnion :: Ref -> Union -> Interning E.Builder
eUnion ref (Union m xs vs) =
  do  vs_ <- eVariants ref vs
      return (M.eMask m <> E.array8 T.eVar xs <> vs_)


dUnion :: TypeTable -> D.Decoder Union
dUnion table =
  liftM3 Union M.dMask (D.array8 T.dVar) (dVariants table)



-- VARIANTS


eVariants :: Ref -> Variants -> Interning E.Builder
eVariants ref variants =
  case variants of
    Variants vs ->
      do  vs_ <- Tag.traverseWithTag (\_ v -> eVariant ref v) vs
          return (E.u8 0 <> Tag.eTable id vs_)

    Enum es ->
      return (E.u8 1 <> Tag.eTable N.encode es)

    Vector n a b cs ->
      do  a_ <- eTypeRef ref a
          b_ <- eTypeRef ref b
          cs_ <- mapM (eTypeRef ref) cs
          return (E.u8 2 <> N.encode n <> a_ <> b_ <> E.list8 id cs_)

    Box n a ->
      do  a_ <- eTypeRef ref a
          return (E.u8 3 <> N.encode n <> a_)

    Void n ->
      return (E.u8 4 <> N.encode n)


dVariants :: TypeTable -> D.Decoder Variants
dVariants table =
  do  tag <- D.u8
      let dRef = dTypeRef table
      case tag of
        0 -> D.map1 Variants (Tag.dTable (dVariant table))
        1 -> D.map1 Enum     (Tag.dTable N.decode)
        2 -> D.map4 Vector N.decode dRef dRef (D.list8 dRef)
        3 -> D.map2 Box    N.decode dRef
        4 -> D.map1 Void   N.decode
        _ -> D.expecting "Variants"



-- VARIANT


eVariant :: Ref -> Variant -> Interning E.Builder
eVariant ref (Variant n ts) =
  do  ts_ <- mapM (eTypeRef ref) ts
      return (N.encode n <> E.list8 id ts_)


dVariant :: TypeTable -> D.Decoder Variant
dVariant table =
  liftM2 Variant N.decode (D.list8 (dTypeRef table))



-- ANNOTATION


eAnnotation :: Ref -> Annotation -> Interning E.Builder
eAnnotation ref (Forall xs t) =
  do  t_ <- eTypeRef ref t
      return (E.array8 T.eVar xs <> t_)


dAnnotation :: TypeTable -> D.Decoder Annotation
dAnnotation table =
  liftM2 Forall (D.array8 T.dVar) (dTypeRef table)



-- TYPE
--
-- @LAMDERA eType / dType write a whole type inline. Interfaces no longer use them
-- (they use the type table below), but AST.Optimized still does for the message type
-- of Main, so the .elmo format is unchanged.


eType :: Type -> E.Builder
eType tipe =
  case tipe of
    TLambda a b cs    -> E.u8# 0#Word8 <> eType a <> eType b <> E.list8 eType cs
    TVar i            -> E.u8# 1#Word8 <> T.eIndex i
    TType  h n m xs   -> E.u8# 2#Word8 <> ModuleName.eCanonical h <> T.encode n <> M.eMask m <> E.array8 eType xs
    TAlias h n m xs a -> E.u8# 3#Word8 <> ModuleName.eCanonical h <> T.encode n <> M.eMask m <> E.array8 eType xs <> eAliased a
    TUnit             -> E.u8# 4#Word8
    TPair   a b       -> E.u8# 5#Word8 <> eType a <> eType b
    TTriple a b c     -> E.u8# 6#Word8 <> eType a <> eType b <> eType c
    TEmpty            -> E.u8# 7#Word8
    TRecord fs        -> E.u8# 8#Word8 <> F.encode eType fs
    TExtRec fs i      -> E.u8# 9#Word8 <> F.encode eType fs <> T.eIndex i


dType :: D.Decoder Type
dType =
  do  word <- D.u8
      case word of
        0 -> liftM3 TLambda dType dType (D.list8 dType)
        1 -> liftM  TVar T.dIndex
        2 -> liftM4 TType  ModuleName.dCanonical T.decode M.dMask (D.array8 dType)
        3 -> liftM5 TAlias ModuleName.dCanonical T.decode M.dMask (D.array8 dType) dAliased
        4 -> pure   TUnit
        5 -> liftM2 TPair   dType dType
        6 -> liftM3 TTriple dType dType dType
        7 -> pure   TEmpty
        8 -> liftM  TRecord (F.decode dType)
        9 -> liftM2 TExtRec (F.decode dType) T.dIndex
        _ -> D.expecting "Type"



-- ALIASED


eAliased :: Aliased -> E.Builder
eAliased =
  coerce eType


dAliased :: D.Decoder Aliased
dAliased =
  coerce dType





-- TYPE TABLE
--
-- @LAMDERA Every type in an interface is written once. An alias in a type carries the
-- whole body it was declared with (TAlias ... Aliased), so an app with a large Model or
-- page-context record repeats the same subtrees thousands of times per interface: with
-- Elm 0.19.3's interfaces one customer app had 740MB of .elmi, 99.9% of it types, and
-- writing each distinct subtree once brought it to 13%.
--
-- So an interface starts with a table of every distinct Type node in it, each written
-- like eType but with its child types as indexes of earlier entries, and the interface
-- proper follows with an index in place of each type:
--
--   u32 count, then count entries; each type below is a ref
--
--   0 lambda   ref ref list8 ref
--   1 var      T.eIndex
--   2 type     canonical, name, mask, list8 ref
--   3 alias    canonical, name, mask, list8 ref, ref (the aliased body)
--   4 unit
--   5 pair     ref ref
--   6 triple   ref ref ref
--   7 empty
--   8 record   F.encode ref
--   9 extrec   F.encode ref, T.eIndex
--
-- A ref is a u16 when the table has at most 65536 entries and a u32 otherwise. The
-- table is built bottom up (children before parents), so a ref always points at an
-- earlier entry, and the decoder rebuilds each entry from the ones before it. The
-- decoded types are == to what eType / dType gave, so type checking, generated JS,
-- Wire3 and type hashes see exactly the same values; equal subtrees are now also
-- shared in memory.
--
-- Nodes are keyed by their constructor and fields with each child replaced by its
-- index, so finding a node in the table compares small keys, never whole types.


type Ref = Int -> E.Builder


type Interning = State.State TableBuilder


data TableBuilder =
  TableBuilder
    { _tb_ids   :: !(IntMap.IntMap [(Key, Int)]) -- by hashKey, see node
    , _tb_size  :: !Int
    , _tb_nodes :: [Key] -- newest first
    , _tb_seen  :: !(IntMap.IntMap [(StableName Type, Int)]) -- see internBody
    }


data Key
  = KLambda !Int !Int ![Int]
  | KVar !T.Index
  | KType !ModuleName.Canonical !T.Name !M.Mask ![Int]
  | KAlias !ModuleName.Canonical !T.Name !M.Mask ![Int] !Int
  | KUnit
  | KPair !Int !Int
  | KTriple !Int !Int !Int
  | KEmpty
  | KRecord !(F.Fields Int)
  | KExtRec !(F.Fields Int) !T.Index
  deriving (Eq)


{-| Run an encoder that writes refs, and put the type table it collected in front.
The ref encoder depends on the final size of the table, so the encoder gets it as an
argument and must only use it inside the Builders it returns (never to decide what to
intern); it is evaluated after the table is complete.
-}
withTypeTable :: (Ref -> Interning E.Builder) -> E.Builder
withTypeTable encoder =
  let
    (body, TableBuilder _ size nodes _) =
      State.runState (encoder ref) (TableBuilder IntMap.empty 0 [] IntMap.empty)

    ref =
      if size <= 65536
        then \i -> E.u16 (fromIntegral i)
        else \i -> E.u32 (fromIntegral i)
  in
  E.u32 (fromIntegral size)
  <> mconcat (map (eKey ref) (reverse nodes))
  <> body


eTypeRef :: Ref -> Type -> Interning E.Builder
eTypeRef ref tipe =
  ref <$> intern tipe


intern :: Type -> Interning Int
intern tipe =
  case tipe of
    TLambda a b cs ->
      do  ia <- intern a
          ib <- intern b
          ics <- mapM intern cs
          node (KLambda ia ib ics)

    TVar i ->
      node (KVar i)

    TType h n m xs ->
      do  xs_ <- mapM intern (A.toList xs)
          node (KType h n m xs_)

    TAlias h n m xs (Aliased t) ->
      do  xs_ <- mapM intern (A.toList xs)
          it <- internBody t
          node (KAlias h n m xs_ it)

    TUnit ->
      node KUnit

    TPair a b ->
      do  ia <- intern a
          ib <- intern b
          node (KPair ia ib)

    TTriple a b c ->
      do  ia <- intern a
          ib <- intern b
          ic <- intern c
          node (KTriple ia ib ic)

    TEmpty ->
      node KEmpty

    -- @LAMDERA F.withOrder: keyed by the order that is written (see F.withOrder)
    TRecord fs ->
      do  fs_ <- F.traverse intern (F.withOrder fs)
          node (KRecord fs_)

    TExtRec fs i ->
      do  fs_ <- F.traverse intern (F.withOrder fs)
          node (KExtRec fs_ i)


{-| @LAMDERA An alias's body, interned once per value in memory.

The bodies of aliases are where the size is: a Model record in hundreds of annotations.
Every use of an alias carries the body from its declaration, the same Type object, and
decoding an interface shares every repeated subtree, so the same object turns up again
and again. Interning walks every node of a type, so the work was the size of the types
written out in full, not of what is in the table. Interning the same object again can
only give the index it gave the first time (every node in it is in the table by then),
so remember it by identity and skip the walk. The table and its order are exactly what
walking would give.
-}
internBody :: Type -> Interning Int
internBody tipe =
  do  let name = stableNameOf tipe
          hash = hashStableName name
      seen <- State.gets _tb_seen
      case lookup name (IntMap.findWithDefault [] hash seen) of
        Just i ->
          return i

        Nothing ->
          do  i <- intern tipe
              State.modify' $ \tb -> tb { _tb_seen = IntMap.insertWith (++) hash [(name, i)] (_tb_seen tb) }
              return i


-- @LAMDERA makeStableName of an evaluated value is the same every time it is asked for,
-- so this is safe to use from pure code.
{-# NOINLINE stableNameOf #-}
stableNameOf :: Type -> StableName Type
stableNameOf tipe =
  tipe `seq` unsafePerformIO (makeStableName tipe)


-- @LAMDERA The table is a hash table (hashKey, then == within a bucket) rather than a
-- Map: a profile of the 920 module app put most of interface writing in comparing
-- keys for the Map, a module name, package and type name at a time. An entry gets the
-- next index the first time its key is seen.
node :: Key -> Interning Int
node key =
  do  TableBuilder ids size nodes seen <- State.get
      let hash = hashKey key
          bucket = IntMap.findWithDefault [] hash ids
      case lookup key bucket of
        Just i ->
          return i

        Nothing ->
          do  State.put (TableBuilder (IntMap.insert hash ((key, size) : bucket) ids) (size + 1) (key : nodes) seen)
              return size


hashKey :: Key -> Int
hashKey key =
  case key of
    KLambda a b cs     -> List.foldl' mix (mix (mix 1 a) b) cs
    KVar (T.Index i)   -> mix 2 i
    KType h n _ xs     -> List.foldl' mix (mix (mix 3 (hashHome h)) (S.hash (T.nameToString n))) xs
    KAlias h n _ xs t  -> mix (List.foldl' mix (mix (mix 4 (hashHome h)) (S.hash (T.nameToString n))) xs) t
    KUnit              -> 5
    KPair a b          -> mix (mix 6 a) b
    KTriple a b c      -> mix (mix (mix 7 a) b) c
    KEmpty             -> 8
    KRecord fs         -> hashFields 9 fs
    KExtRec fs (T.Index i) -> mix (hashFields 10 fs) i
  where
    mix h x = (h * 1000003) `xor` x
    hashHome (ModuleName.Canonical _ m) = S.hash (Module.toString m)
    hashFields start fs = F.foldrWithKey (\n t h -> mix (mix h (S.hash (N.toString n))) t) start fs


eKey :: Ref -> Key -> E.Builder
eKey ref key =
  case key of
    KLambda a b cs    -> E.u8# 0#Word8 <> ref a <> ref b <> E.list8 ref cs
    KVar i            -> E.u8# 1#Word8 <> T.eIndex i
    KType h n m xs    -> E.u8# 2#Word8 <> ModuleName.eCanonical h <> T.encode n <> M.eMask m <> E.list8 ref xs
    KAlias h n m xs t -> E.u8# 3#Word8 <> ModuleName.eCanonical h <> T.encode n <> M.eMask m <> E.list8 ref xs <> ref t
    KUnit             -> E.u8# 4#Word8
    KPair a b         -> E.u8# 5#Word8 <> ref a <> ref b
    KTriple a b c     -> E.u8# 6#Word8 <> ref a <> ref b <> ref c
    KEmpty            -> E.u8# 7#Word8
    KRecord fs        -> E.u8# 8#Word8 <> F.encode ref fs
    KExtRec fs i      -> E.u8# 9#Word8 <> F.encode ref fs <> T.eIndex i


data TypeTable =
  TypeTable
    { _tt_ref   :: D.Decoder Int
    , _tt_types :: !(IntMap.IntMap Type)
    }


dTypeTable :: D.Decoder TypeTable
dTypeTable =
  do  size <- D.u32
      let dRef = if size <= 65536 then fromIntegral <$> D.u16 else fromIntegral <$> D.u32
          go i types =
            if i == fromIntegral size
              then return (TypeTable dRef types)
              else
                do  t <- dNode (TypeTable dRef types)
                    go (i + 1) (IntMap.insert i t types)
      go 0 IntMap.empty


-- Only entries before this one are in the table yet, so a ref forward (or to itself)
-- is rejected as corrupt rather than followed.
dNode :: TypeTable -> D.Decoder Type
dNode table =
  do  word <- D.u8
      let ref = dTypeRef table
      case word of
        0 -> liftM3 TLambda ref ref (D.list8 ref)
        1 -> liftM  TVar T.dIndex
        2 -> liftM4 TType  ModuleName.dCanonical T.decode M.dMask (A.fromList <$> D.list8 ref)
        3 -> liftM5 TAlias ModuleName.dCanonical T.decode M.dMask (A.fromList <$> D.list8 ref) (Aliased <$> ref)
        4 -> pure   TUnit
        5 -> liftM2 TPair   ref ref
        6 -> liftM3 TTriple ref ref ref
        7 -> pure   TEmpty
        8 -> liftM  TRecord (F.decode ref)
        9 -> liftM2 TExtRec (F.decode ref) T.dIndex
        _ -> D.expecting "Type"


dTypeRef :: TypeTable -> D.Decoder Type
dTypeRef (TypeTable dRef types) =
  do  i <- dRef
      case IntMap.lookup i types of
        Just t  -> return t
        Nothing -> D.expecting "TypeRef"



-- SHARE
--
-- @LAMDERA `lamdera live` keeps interfaces for the whole session (Ext.MemoryCache). An
-- interface fresh from type inference repeats the same alias bodies at every use (see
-- TYPE TABLE above), and holding hundreds of those trees costs gigabytes. `share`
-- rebuilds an interface with each distinct type node allocated once, keyed exactly like
-- the type table: the in-memory equivalent of writing the interface and reading it
-- back, without the bytes. The result is == to the input and holds no reference to it.


share :: Interface -> Interface
share (Interface h vs ts bs) =
  let
    shared@(Interface _ vs_ ts_ bs_) =
      State.evalState
        ( do  !vs1 <- traverse shareAnnotation vs
              !ts1 <- traverse shareTypeDecl ts
              !bs1 <- traverse shareBinop bs
              return (Interface h vs1 ts1 bs1)
        )
        (Sharing IntMap.empty IntMap.empty 0 IntMap.empty)
  in
  vs_ `seq` ts_ `seq` bs_ `seq` shared


type Sharer = State.State Sharing


data Sharing =
  Sharing
    { _sh_ids   :: !(IntMap.IntMap [(Key, Int)]) -- by hashKey
    , _sh_types :: !(IntMap.IntMap Type)
    , _sh_size  :: !Int
    , _sh_seen  :: !(IntMap.IntMap [(StableName Type, (Int, Type))]) -- see shareBody
    }


shareAnnotation :: Annotation -> Sharer Annotation
shareAnnotation (Forall xs t) =
  do  !t_ <- shareType t
      return $! Forall xs t_


shareTypeDecl :: TypeDecl -> Sharer TypeDecl
shareTypeDecl decl =
  case decl of
    UnionOpen    u -> (UnionOpen $!)    <$> shareUnion u
    UnionClosed  u -> (UnionClosed $!)  <$> shareUnion u
    UnionPrivate u -> (UnionPrivate $!) <$> shareUnion u
    AliasPublic  a -> (AliasPublic $!)  <$> shareAlias a
    AliasPrivate a -> (AliasPrivate $!) <$> shareAlias a


shareUnion :: Union -> Sharer Union
shareUnion (Union m xs variants) =
  do  !variants_ <-
        case variants of
          Variants vs ->
            Variants <$> Tag.traverseWithTag (\_ (Variant n ts) -> do { !ts_ <- shareList shareType ts; return $! Variant n ts_ }) vs

          Enum _ ->
            return variants

          Vector n a b cs ->
            do  !a_ <- shareType a
                !b_ <- shareType b
                !cs_ <- shareList shareType cs
                return $! Vector n a_ b_ cs_

          Box n a ->
            do  !a_ <- shareType a
                return $! Box n a_

          Void _ ->
            return variants
      return $! Union m xs variants_


shareAlias :: Alias -> Sharer Alias
shareAlias (Alias m xs (Aliased t)) =
  do  (_, !t_) <- shareBody t
      return $! Alias m xs (Aliased t_)


shareBinop :: Binop -> Sharer Binop
shareBinop (Binop n t a p) =
  do  !t_ <- shareAnnotation t
      return $! Binop n t_ a p


shareList :: (a -> Sharer b) -> [a] -> Sharer [b]
shareList f xs =
  case xs of
    [] ->
      return []

    x:rest ->
      do  !y <- f x
          !ys <- shareList f rest
          return $! (y : ys)


shareType :: Type -> Sharer Type
shareType tipe =
  snd <$> shareNode tipe


{-| The node's index in the sharing table, and the one shared value for it. Children
are shared first, so a new node is built from shared children only.
-}
shareNode :: Type -> Sharer (Int, Type)
shareNode tipe =
  case tipe of
    TLambda a b cs ->
      do  (ia, !ta) <- shareNode a
          (ib, !tb) <- shareNode b
          !cs_ <- shareList shareNode cs
          sharedNode (KLambda ia ib (forceList (map fst cs_))) (TLambda ta tb (forceList (map snd cs_)))

    TVar i ->
      sharedNode (KVar i) tipe

    TType h n m xs ->
      do  !xs_ <- shareList shareNode (A.toList xs)
          sharedNode (KType h n m (forceList (map fst xs_))) (TType h n m (A.fromList (map snd xs_)))

    TAlias h n m xs (Aliased t) ->
      do  !xs_ <- shareList shareNode (A.toList xs)
          (it, !tt) <- shareBody t
          sharedNode (KAlias h n m (forceList (map fst xs_)) it) (TAlias h n m (A.fromList (map snd xs_)) (Aliased tt))

    TUnit ->
      sharedNode KUnit tipe

    TPair a b ->
      do  (ia, !ta) <- shareNode a
          (ib, !tb) <- shareNode b
          sharedNode (KPair ia ib) (TPair ta tb)

    TTriple a b c ->
      do  (ia, !ta) <- shareNode a
          (ib, !tb) <- shareNode b
          (ic, !tc) <- shareNode c
          sharedNode (KTriple ia ib ic) (TTriple ta tb tc)

    TEmpty ->
      sharedNode KEmpty tipe

    TRecord fs ->
      do  !fs_ <- F.traverse shareNode fs
          sharedNode (KRecord (F.map fst fs_)) (TRecord (F.map snd fs_))

    TExtRec fs i ->
      do  !fs_ <- F.traverse shareNode fs
          sharedNode (KExtRec (F.map fst fs_) i) (TExtRec (F.map snd fs_) i)


{-| An alias's body, shared once per value in memory, like internBody for the type
table: the same body object turns up in many annotations, and sharing it again can only
give the same node.
-}
shareBody :: Type -> Sharer (Int, Type)
shareBody tipe =
  do  let name = stableNameOf tipe
          hash = hashStableName name
      seen <- State.gets _sh_seen
      case lookup name (IntMap.findWithDefault [] hash seen) of
        Just done ->
          return done

        Nothing ->
          do  done <- shareNode tipe
              State.modify' $ \sh -> sh { _sh_seen = IntMap.insertWith (++) hash [(name, done)] (_sh_seen sh) }
              return done


sharedNode :: Key -> Type -> Sharer (Int, Type)
sharedNode key fresh =
  do  Sharing ids types size seen <- State.get
      let hash = hashKey key
          bucket = IntMap.findWithDefault [] hash ids
      case lookup key bucket of
        Just i ->
          case IntMap.lookup i types of
            Just t  -> return (i, t)
            Nothing -> $(Crash.crash 'sharedNode) "a shared type index has no type"

        Nothing ->
          do  let !t = fresh
              State.put (Sharing (IntMap.insert hash ((key, size) : bucket) ids) (IntMap.insert size t types) (size + 1) seen)
              return (size, t)


forceList :: [a] -> [a]
forceList xs =
  go xs `seq` xs
  where
    go ys =
      case ys of
        []     -> ()
        y:rest -> y `seq` go rest
