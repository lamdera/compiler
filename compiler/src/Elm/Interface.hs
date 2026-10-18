{-# LANGUAGE ExtendedLiterals, MagicHash, TemplateHaskell #-}
module Elm.Interface
  ( Interface(..)
  , Union(..)
  , Alias(..)
  , Binop(..)
  , fromModule
  , toPublicUnion
  , toPublicAlias
  , DependencyInterface(..)
  , public
  , private
  , privatize
  , extractUnion
  , extractAlias
  --
  , eInterface, dInterface
  , eDependencyInterface, dDependencyInterface
  , eType, dType
  )
  where


import Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5)
import qualified Control.Monad.State.Strict as State -- @LAMDERA
import Data.Coerce (coerce)
import qualified Data.IntMap.Strict as IntMap -- @LAMDERA
import Data.Word (Word16) -- @LAMDERA
import System.IO.Unsafe (unsafePerformIO) -- @LAMDERA
import System.Mem.StableName (StableName, makeStableName, hashStableName) -- @LAMDERA
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Utils as Map

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg



-- INTERFACE


data Interface =
  Interface
    { _home    :: Pkg.Name
    , _values  :: Map.Map N.Name Can.Annotation
    , _unions  :: Map.Map T.Name Union
    , _aliases :: Map.Map T.Name Alias
    , _binops  :: Map.Map Op.Name Binop
    }
  deriving (Eq)


data Union
  = OpenUnion Can.Union
  | ClosedUnion Can.Union
  | PrivateUnion Can.Union
  deriving (Eq)


data Alias
  = PublicAlias Can.Alias
  | PrivateAlias Can.Alias
  deriving (Eq)


data Binop =
  Binop
    { _op_name :: N.Name
    , _op_annotation :: Can.Annotation
    , _op_associativity :: Op.Associativity
    , _op_precedence :: Op.Precedence
    }
  deriving (Eq)



-- FROM MODULE


fromModule :: Pkg.Name -> Can.Module -> Map.Map N.Name Can.Annotation -> Interface
fromModule home (Can.Module _ exports _ _ unions aliases binops _) annotations =
  Interface
    { _home    = home
    , _values  = restrictValues  exports annotations
    , _unions  = restrictUnions  exports unions
    , _aliases = restrictAliases exports aliases
    , _binops  = restrictBinops  exports (Map.map (toOp annotations) binops)
    }


restrictValues :: Can.Exports -> Map.Map N.Name Can.Annotation -> Map.Map N.Name Can.Annotation
restrictValues exports dict =
  case exports of
    Can.ExportEverything _ ->
      dict

    Can.Export _ values _ ->
      Map.intersection dict values


restrictUnions :: Can.Exports -> Map.Map T.Name Can.Union -> Map.Map T.Name Union
restrictUnions exports unions =
  case exports of
    Can.ExportEverything _ ->
      Map.map OpenUnion unions

    Can.Export types _ _ ->
        Map.merge onLeft onRight onBoth types unions
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ u -> PrivateUnion u)
        onBoth = Map.zipWithMatched $ \_ (_, export) union ->
          case export of
            Can.ExportUnionOpen   -> OpenUnion union
            Can.ExportUnionClosed -> ClosedUnion union
            Can.ExportAlias       -> $(Crash.crash 'restrictUnions) "impossible exports discovered"


restrictAliases :: Can.Exports -> Map.Map T.Name Can.Alias -> Map.Map T.Name Alias
restrictAliases exports aliases =
  case exports of
    Can.ExportEverything _ ->
      Map.map PublicAlias aliases

    Can.Export types _ _ ->
        Map.merge onLeft onRight onBoth types aliases
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ a -> PrivateAlias a)
        onBoth = Map.zipWithMatched (\_ _ a -> PublicAlias a)


restrictBinops :: Can.Exports -> Map.Map Op.Name Binop -> Map.Map Op.Name Binop
restrictBinops exports dict =
  case exports of
    Can.ExportEverything _ ->
      dict

    Can.Export _ _ ops ->
      Map.intersection dict ops


toOp :: Map.Map N.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
  Binop name ($(Map.require 'toOp) name types N.toChars) associativity precedence



-- TO PUBLIC


toPublicUnion :: Union -> Maybe Can.Union
toPublicUnion iUnion =
  case iUnion of
    OpenUnion union                       -> Just union
    ClosedUnion (Can.Union vars _ _ opts) -> Just (Can.Union vars [] 0 opts)
    PrivateUnion _                        -> Nothing


toPublicAlias :: Alias -> Maybe Can.Alias
toPublicAlias iAlias =
  case iAlias of
    PublicAlias alias -> Just alias
    PrivateAlias _    -> Nothing



-- DEPENDENCY INTERFACE


data DependencyInterface
  = Public Interface
  | Private
      Pkg.Name
      (Map.Map T.Name Can.Union)
      (Map.Map T.Name Can.Alias)


public :: Interface -> DependencyInterface
public =
  Public


private :: Interface -> DependencyInterface
private (Interface pkg _ unions aliases _) =
  Private pkg (Map.map extractUnion unions) (Map.map extractAlias aliases)


extractUnion :: Union -> Can.Union
extractUnion iUnion =
  case iUnion of
    OpenUnion union -> union
    ClosedUnion union -> union
    PrivateUnion union -> union


extractAlias :: Alias -> Can.Alias
extractAlias iAlias =
  case iAlias of
    PublicAlias alias -> alias
    PrivateAlias alias -> alias


privatize :: DependencyInterface -> DependencyInterface
privatize di =
  case di of
    Public i -> private i
    Private _ _ _ -> di



-- BINARY
--
-- @LAMDERA Every Can.Type in an interface is written once, in a type table at the
-- start of the interface, and referred to by its index everywhere else. See TYPE
-- TABLE below for why and how.


eInterface :: Interface -> E.Builder
eInterface (Interface h vs us as bs) =
  withTypeTable $ \ref ->
    do  vs_ <- traverse (eAnnotation ref) vs
        us_ <- traverse (eUnion ref) us
        as_ <- traverse (eAlias ref) as
        bs_ <- traverse (eBinop ref) bs
        return $
          Pkg.eName h
          <> E.dict32 N.encode id vs_
          <> E.dict32 T.encode id us_
          <> E.dict32 T.encode id as_
          <> E.dict32 Op.encode id bs_


dInterface :: D.Decoder Interface
dInterface =
  do  table <- dTypeTable
      liftM5 Interface
        Pkg.dName
        (D.dict32 N.decode (dAnnotation table))
        (D.dict32 T.decode (dUnion table))
        (D.dict32 T.decode (dAlias table))
        (D.dict32 Op.decode (dBinop table))


eUnion :: Ref -> Union -> Interning E.Builder
eUnion ref union =
  case union of
    OpenUnion    u -> (E.u8# 0#Word8 <>) <$> eUnion_ ref u
    ClosedUnion  u -> (E.u8# 1#Word8 <>) <$> eUnion_ ref u
    PrivateUnion u -> (E.u8# 2#Word8 <>) <$> eUnion_ ref u


dUnion :: TypeTable -> D.Decoder Union
dUnion table =
  do  n <- D.u8
      case n of
        0 -> OpenUnion <$> dUnion_ table
        1 -> ClosedUnion <$> dUnion_ table
        2 -> PrivateUnion <$> dUnion_ table
        _ -> D.expecting "Union"


eAlias :: Ref -> Alias -> Interning E.Builder
eAlias ref alias =
  case alias of
    PublicAlias  a -> (E.u8# 0#Word8 <>) <$> eAlias_ ref a
    PrivateAlias a -> (E.u8# 1#Word8 <>) <$> eAlias_ ref a


dAlias :: TypeTable -> D.Decoder Alias
dAlias table =
  do  n <- D.u8
      case n of
        0 -> PublicAlias  <$> dAlias_ table
        1 -> PrivateAlias <$> dAlias_ table
        _ -> D.expecting "Alias"


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


eDependencyInterface :: DependencyInterface -> E.Builder
eDependencyInterface iface =
  case iface of
    Public  i     -> E.u8# 0#Word8 <> eInterface i
    -- @LAMDERA a Private interface has its own type table too, after the tag
    Private n u a ->
      E.u8# 1#Word8 <>
        withTypeTable (\ref ->
          do  u_ <- traverse (eUnion_ ref) u
              a_ <- traverse (eAlias_ ref) a
              return (Pkg.eName n <> E.dict32 T.encode id u_ <> E.dict32 T.encode id a_)
        )


dDependencyInterface :: D.Decoder DependencyInterface
dDependencyInterface =
  do  n <- D.u8
      case n of
        0 -> liftM  Public dInterface
        1 ->
          do  table <- dTypeTable
              liftM3 Private Pkg.dName (D.dict32 T.decode (dUnion_ table)) (D.dict32 T.decode (dAlias_ table))
        _ -> D.expecting "DependencyInterface"



-- BINARY HELPERS


eAlias_ :: Ref -> Can.Alias -> Interning E.Builder
eAlias_ ref (Can.Alias vs t) =
  do  t_ <- eTypeRef ref t
      return (E.list8 T.eVar vs <> t_)


dAlias_ :: TypeTable -> D.Decoder Can.Alias
dAlias_ table =
  liftM2 Can.Alias (D.list8 T.dVar) (dTypeRef table)


eUnion_ :: Ref -> Can.Union -> Interning E.Builder
eUnion_ ref (Can.Union vs cs n opts) =
  do  cs_ <- mapM (eCtor ref) cs
      return (E.list8 T.eVar vs <> E.list32 id cs_ <> E.int n <> Can.eCtorOpts opts)


dUnion_ :: TypeTable -> D.Decoder Can.Union
dUnion_ table =
  liftM4 Can.Union (D.list8 T.dVar) (D.list32 (dCtor table)) D.int Can.dCtorOpts


eCtor :: Ref -> Can.Ctor -> Interning E.Builder
eCtor ref (Can.Ctor n i a t) =
  do  t_ <- mapM (eTypeRef ref) t
      return (N.encode n <> Index.eZeroBased i <> E.int a <> E.list8 id t_)


dCtor :: TypeTable -> D.Decoder Can.Ctor
dCtor table =
  liftM4 Can.Ctor N.decode Index.dZeroBased D.int (D.list8 (dTypeRef table))


eAnnotation :: Ref -> Can.Annotation -> Interning E.Builder
eAnnotation ref (Can.Forall vs t) =
  do  t_ <- eTypeRef ref t
      return (E.dict8 T.eVar (\() -> mempty) vs <> t_)


dAnnotation :: TypeTable -> D.Decoder Can.Annotation
dAnnotation table =
  liftM2 Can.Forall (D.dict8 T.dVar (pure ())) (dTypeRef table)


-- @LAMDERA eType / dType write a whole type inline. Interfaces no longer use them
-- (they use the type table below), but AST.Optimized still does for the message type
-- of Main, so the .elmo format is unchanged.

eType :: Can.Type -> E.Builder
eType tipe =
  case tipe of
    Can.TLambda a b     -> E.u8# 0#Word8 <> eType a <> eType b
    Can.TVar x          -> E.u8# 1#Word8 <> T.eVar x
    Can.TRecord fs e    -> E.u8# 2#Word8 <> E.dict32 N.encode eFieldType fs <> E.maybe T.eVar e
    Can.TUnit           -> E.u8# 3#Word8
    Can.TPair   a b     -> E.u8# 4#Word8 <> eType a <> eType b
    Can.TTriple a b c   -> E.u8# 5#Word8 <> eType a <> eType b <> eType c
    Can.TAlias h n xs a -> E.u8# 6#Word8 <> ModuleName.eCanonical h <> T.encode n <> E.list8 (\(x,t) -> T.eVar x <> eType t) xs <> eAliasType a
    Can.TType  h n xs   -> E.u8# 7#Word8 <> ModuleName.eCanonical h <> T.encode n <> E.list8 eType xs


dType :: D.Decoder Can.Type
dType =
  do  word <- D.u8
      case word of
        0 -> liftM2 Can.TLambda dType dType
        1 -> liftM  Can.TVar T.dVar
        2 -> liftM2 Can.TRecord (D.dict32 N.decode dFieldType) (D.maybe T.dVar)
        3 -> return Can.TUnit
        4 -> liftM2 Can.TPair dType dType
        5 -> liftM3 Can.TTriple dType dType dType
        6 -> liftM4 Can.TAlias ModuleName.dCanonical T.decode (D.list8 (liftM2 (,) T.dVar dType)) dAliasType
        7 -> liftM3 Can.TType ModuleName.dCanonical T.decode (D.list8 dType)
        _ -> D.expecting "Type"


eAliasType :: Can.AliasType -> E.Builder
eAliasType aliasType =
  case aliasType of
    Can.Holey  tipe -> E.u8 0 <> eType tipe
    Can.Filled tipe -> E.u8 1 <> eType tipe


dAliasType :: D.Decoder Can.AliasType
dAliasType =
  do  n <- D.u8
      case n of
        0 -> liftM Can.Holey dType
        1 -> liftM Can.Filled dType
        _ -> D.expecting "AliasType"


eFieldType :: Can.FieldType -> E.Builder
eFieldType (Can.FieldType i t) =
  E.u16 i <> eType t


dFieldType :: D.Decoder Can.FieldType
dFieldType =
  liftM2 Can.FieldType D.u16 dType



-- TYPE TABLE
--
-- @LAMDERA Type inference expands every alias in the annotations it stores
-- (Type.Type.toCanType writes Can.TAlias with its Filled body), and eType wrote that
-- whole body at every use. An app with a large Model or page-context record repeats
-- the same expanded subtrees thousands of times per interface: in one customer app
-- 99.9% of 740MB of .elmi was types, and writing each distinct subtree once brings
-- it to 13%.
--
-- So an interface starts with a table of every distinct Can.Type node in it, each
-- written like eType but with its child types as indexes of earlier entries, and the
-- interface proper follows with an index in place of each type:
--
--   u32 count, then count entries; each type below is a ref
--
--   0 lambda   ref ref
--   1 var      T.eVar
--   2 record   dict32 (name, u16 field index, ref), maybe T.eVar
--   3 unit
--   4 pair     ref ref
--   5 triple   ref ref ref
--   6 alias    canonical, name, list8 (T.eVar, ref), u8 0 = Holey / 1 = Filled, ref
--   7 type     canonical, name, list8 ref
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
    { _tb_ids   :: !(Map.Map Key Int)
    , _tb_size  :: !Int
    , _tb_nodes :: [Key] -- newest first
    , _tb_seen  :: !(IntMap.IntMap [(StableName Can.Type, Int)]) -- see internBody
    }


data Key
  = KLambda !Int !Int
  | KVar !T.Var
  | KRecord !(Map.Map N.Name (Word16, Int)) !(Maybe T.Var)
  | KUnit
  | KPair !Int !Int
  | KTriple !Int !Int !Int
  | KAlias !ModuleName.Canonical !T.Name ![(T.Var, Int)] !Bool !Int -- True = Filled
  | KType !ModuleName.Canonical !T.Name ![Int]
  deriving (Eq, Ord)


{-| Run an encoder that writes refs, and put the type table it collected in front.
The ref encoder depends on the final size of the table, so the encoder gets it as an
argument and must only use it inside the Builders it returns (never to decide what to
intern); it is evaluated after the table is complete.
-}
withTypeTable :: (Ref -> Interning E.Builder) -> E.Builder
withTypeTable encoder =
  let
    (body, TableBuilder _ size nodes _) =
      State.runState (encoder ref) (TableBuilder Map.empty 0 [] IntMap.empty)

    ref =
      if size <= 65536
        then \i -> E.u16 (fromIntegral i)
        else \i -> E.u32 (fromIntegral i)
  in
  E.u32 (fromIntegral size)
  <> mconcat (map (eKey ref) (reverse nodes))
  <> body


eTypeRef :: Ref -> Can.Type -> Interning E.Builder
eTypeRef ref tipe =
  ref <$> intern tipe


intern :: Can.Type -> Interning Int
intern tipe =
  case tipe of
    Can.TLambda a b ->
      do  ia <- intern a
          ib <- intern b
          node (KLambda ia ib)

    Can.TVar x ->
      node (KVar x)

    Can.TRecord fs e ->
      do  fs_ <- traverse (\(Can.FieldType i t) -> (,) i <$> intern t) fs
          node (KRecord fs_ e)

    Can.TUnit ->
      node KUnit

    Can.TPair a b ->
      do  ia <- intern a
          ib <- intern b
          node (KPair ia ib)

    Can.TTriple a b c ->
      do  ia <- intern a
          ib <- intern b
          ic <- intern c
          node (KTriple ia ib ic)

    Can.TAlias h n xs aliasType ->
      do  xs_ <- mapM (\(x,t) -> (,) x <$> intern t) xs
          case aliasType of
            Can.Holey  t -> node . KAlias h n xs_ False =<< internBody t
            Can.Filled t -> node . KAlias h n xs_ True  =<< internBody t

    Can.TType h n xs ->
      do  xs_ <- mapM intern xs
          node (KType h n xs_)


{-| @LAMDERA An alias's real type, interned once per value in memory.

The real types of aliases are where the size is: a Model record expanded in hundreds of
annotations. Type.Type.toAnnotations shares each one it can between all of a module's
annotations, and decoding an interface shares every repeated subtree, so the same
Can.Type object turns up again and again. Interning walks every node of a type, so the
work was the size of the types written out in full, not of what is in the table.
Interning the same object again can only give the index it gave the first time (every
node in it is in the table by then), so remember it by identity and skip the walk. The
table and its order are exactly what walking would give.
-}
internBody :: Can.Type -> Interning Int
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
stableNameOf :: Can.Type -> StableName Can.Type
stableNameOf tipe =
  tipe `seq` unsafePerformIO (makeStableName tipe)


node :: Key -> Interning Int
node key =
  do  TableBuilder ids size nodes seen <- State.get
      case Map.lookup key ids of
        Just i ->
          return i

        Nothing ->
          do  State.put (TableBuilder (Map.insert key size ids) (size + 1) (key : nodes) seen)
              return size


eKey :: Ref -> Key -> E.Builder
eKey ref key =
  case key of
    KLambda a b         -> E.u8# 0#Word8 <> ref a <> ref b
    KVar x              -> E.u8# 1#Word8 <> T.eVar x
    KRecord fs e        -> E.u8# 2#Word8 <> E.dict32 N.encode (\(i,t) -> E.u16 i <> ref t) fs <> E.maybe T.eVar e
    KUnit               -> E.u8# 3#Word8
    KPair a b           -> E.u8# 4#Word8 <> ref a <> ref b
    KTriple a b c       -> E.u8# 5#Word8 <> ref a <> ref b <> ref c
    KAlias h n xs f t   -> E.u8# 6#Word8 <> ModuleName.eCanonical h <> T.encode n <> E.list8 (\(x,r) -> T.eVar x <> ref r) xs <> E.u8 (if f then 1 else 0) <> ref t
    KType h n xs        -> E.u8# 7#Word8 <> ModuleName.eCanonical h <> T.encode n <> E.list8 ref xs


data TypeTable =
  TypeTable
    { _tt_ref   :: D.Decoder Int
    , _tt_types :: !(IntMap.IntMap Can.Type)
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
dNode :: TypeTable -> D.Decoder Can.Type
dNode table =
  do  word <- D.u8
      let ref = dTypeRef table
      case word of
        0 -> liftM2 Can.TLambda ref ref
        1 -> liftM  Can.TVar T.dVar
        2 -> liftM2 Can.TRecord (D.dict32 N.decode (liftM2 Can.FieldType D.u16 ref)) (D.maybe T.dVar)
        3 -> return Can.TUnit
        4 -> liftM2 Can.TPair ref ref
        5 -> liftM3 Can.TTriple ref ref ref
        6 -> liftM4 Can.TAlias ModuleName.dCanonical T.decode (D.list8 (liftM2 (,) T.dVar ref)) (dAliasTypeRef ref)
        7 -> liftM3 Can.TType ModuleName.dCanonical T.decode (D.list8 ref)
        _ -> D.expecting "Type"


dAliasTypeRef :: D.Decoder Can.Type -> D.Decoder Can.AliasType
dAliasTypeRef ref =
  do  n <- D.u8
      case n of
        0 -> liftM Can.Holey ref
        1 -> liftM Can.Filled ref
        _ -> D.expecting "AliasType"


dTypeRef :: TypeTable -> D.Decoder Can.Type
dTypeRef (TypeTable dRef types) =
  do  i <- dRef
      case IntMap.lookup i types of
        Just t  -> return t
        Nothing -> D.expecting "TypeRef"
