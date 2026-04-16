{-# OPTIONS_GHC -Wall #-}
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
  )
  where


import Control.Monad (liftM, liftM3, liftM4, liftM5)
import Data.Binary
import Data.Binary.Put (putWord32le)
import Data.Binary.Get (getWord32le, lookAhead)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Utils.Binop as Binop
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A



-- INTERFACE


data Interface =
  Interface
    { _home    :: Pkg.Name
    , _values  :: Map.Map Name.Name Can.Annotation
    , _unions  :: Map.Map Name.Name Union
    , _aliases :: Map.Map Name.Name Alias
    , _binops  :: Map.Map Name.Name Binop
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
    { _op_name :: Name.Name
    , _op_annotation :: Can.Annotation
    , _op_associativity :: Binop.Associativity
    , _op_precedence :: Binop.Precedence
    }
  deriving (Eq)



-- FROM MODULE


fromModule :: Pkg.Name -> Can.Module -> Map.Map Name.Name Can.Annotation -> Interface
fromModule home (Can.Module _ exports _ _ unions aliases binops _) annotations =
  Interface
    { _home = home
    , _values = restrict exports annotations
    , _unions = restrictUnions exports unions
    , _aliases = restrictAliases exports aliases
    , _binops = restrict exports (Map.map (toOp annotations) binops)
    }


restrict :: Can.Exports -> Map.Map Name.Name a -> Map.Map Name.Name a
restrict exports dict =
  case exports of
    Can.ExportEverything _ ->
      dict

    Can.Export explicitExports ->
      Map.intersection dict explicitExports


toOp :: Map.Map Name.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
  Binop name (types ! name) associativity precedence


restrictUnions :: Can.Exports -> Map.Map Name.Name Can.Union -> Map.Map Name.Name Union
restrictUnions exports unions =
  case exports of
    Can.ExportEverything _ ->
      Map.map OpenUnion unions

    Can.Export explicitExports ->
        Map.merge onLeft onRight onBoth explicitExports unions
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ union -> PrivateUnion union)
        onBoth = Map.zipWithMatched $ \_ (A.At _ export) union ->
          case export of
            Can.ExportUnionOpen -> OpenUnion union
            Can.ExportUnionClosed -> ClosedUnion union
            _ -> error "impossible exports discovered in restrictUnions"


restrictAliases :: Can.Exports -> Map.Map Name.Name Can.Alias -> Map.Map Name.Name Alias
restrictAliases exports aliases =
  case exports of
    Can.ExportEverything _ ->
      Map.map PublicAlias aliases

    Can.Export explicitExports ->
        Map.merge onLeft onRight onBoth explicitExports aliases
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ a -> PrivateAlias a)
        onBoth = Map.zipWithMatched (\_ _ a -> PublicAlias a)



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
      (Map.Map Name.Name Can.Union)
      (Map.Map Name.Name Can.Alias)


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


instance Binary Interface where
  put iface = do
    putWord8 0x00  -- magic sentinel: new dedup format (old format starts with name length > 0)
    putInterfaceDedup iface

  get = do
    firstByte <- lookAhead getWord8
    if firstByte == 0x00
      then do
        _ <- getWord8  -- consume magic sentinel
        getInterfaceDedup
      else
        -- old format: fall back to standard deserialization
        liftM5 Interface get get get get get


instance Binary Union where
  put union =
    case union of
      OpenUnion    u -> putWord8 0 >> put u
      ClosedUnion  u -> putWord8 1 >> put u
      PrivateUnion u -> putWord8 2 >> put u

  get =
    do  n <- getWord8
        case n of
          0 -> liftM OpenUnion get
          1 -> liftM ClosedUnion get
          2 -> liftM PrivateUnion get
          _ -> fail "binary encoding of Union was corrupted"


instance Binary Alias where
  put iAlias =
    case iAlias of
      PublicAlias  a -> putWord8 0 >> put a
      PrivateAlias a -> putWord8 1 >> put a

  get =
    do  n <- getWord8
        case n of
          0 -> liftM PublicAlias get
          1 -> liftM PrivateAlias get
          _ -> fail "binary encoding of Alias was corrupted"


instance Binary Binop where
  get = do
    n <- get; a <- get; s <- get; p <- get
    return (Binop n a s p)

  put (Binop a b c d) =
    put a >> put b >> put c >> put d


instance Binary DependencyInterface where
  put depIface =
    case depIface of
      Public  a     -> putWord8 0 >> put a
      Private a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do  n <- getWord8
        case n of
          0 -> liftM  Public get
          1 -> liftM3 Private get get get
          _ -> fail "binary encoding of DependencyInterface was corrupted"



-- ============================================================================
-- TYPE DEDUPLICATION
--
-- To avoid serializing the same deeply-nested type trees over and over,
-- we intern all unique Can.Type subtrees into a pool. Each unique subtree
-- gets a Word32 index. Types in the pool reference their children by index.
-- This provides massive deduplication for interfaces where many exported
-- values share the same large type aliases (e.g. FrontendModel, BackendModel).
-- ============================================================================


type TypePool = Map.Map Can.Type Word32
type TypeTable = IntMap.IntMap Can.Type



-- BUILD POOL


data InternState = InternState
  { _pool :: !TypePool
  , _list :: ![Can.Type]
  , _size :: !Word32
  }


emptyIntern :: InternState
emptyIntern = InternState Map.empty [] 0


internDeep :: Can.Type -> InternState -> InternState
internDeep tipe state =
  let state1 = internChildren tipe state
  in internSelf tipe state1


internSelf :: Can.Type -> InternState -> InternState
internSelf tipe state =
  case Map.lookup tipe (_pool state) of
    Just _  -> state
    Nothing ->
      InternState
        (Map.insert tipe (_size state) (_pool state))
        (tipe : _list state)
        (_size state + 1)


internChildren :: Can.Type -> InternState -> InternState
internChildren tipe state =
  case tipe of
    Can.TLambda a b ->
      internDeep b (internDeep a state)

    Can.TVar _ ->
      state

    Can.TType _ _ ts ->
      List.foldl' (flip internDeep) state ts

    Can.TRecord fields _ ->
      Map.foldl' (\s (Can.FieldType _ t) -> internDeep t s) state fields

    Can.TUnit ->
      state

    Can.TTuple a b mc ->
      maybe id internDeep mc (internDeep b (internDeep a state))

    Can.TAlias _ _ args aliasType ->
      let s1 = List.foldl' (\s (_, t) -> internDeep t s) state args
      in case aliasType of
           Can.Holey t  -> internDeep t s1
           Can.Filled t -> internDeep t s1


internAnnotation :: Can.Annotation -> InternState -> InternState
internAnnotation (Can.Forall _ tipe) state =
  internDeep tipe state


buildPool :: Interface -> InternState
buildPool iface =
  let s0 = emptyIntern
      s1 = Map.foldl' (flip internAnnotation) s0 (_values iface)
      s2 = Map.foldl' (\s u -> internCanUnion (extractUnion u) s) s1 (_unions iface)
      s3 = Map.foldl' (\s a -> internCanAlias (extractAlias a) s) s2 (_aliases iface)
      s4 = Map.foldl' (\s (Binop _ ann _ _) -> internAnnotation ann s) s3 (_binops iface)
  in s4


internCanUnion :: Can.Union -> InternState -> InternState
internCanUnion (Can.Union _ ctors _ _) state =
  List.foldl' (\s (Can.Ctor _ _ _ ts) -> List.foldl' (flip internDeep) s ts) state ctors


internCanAlias :: Can.Alias -> InternState -> InternState
internCanAlias (Can.Alias _ tipe) state =
  internDeep tipe state



-- SERIALIZE WITH DEDUP


putInterfaceDedup :: Interface -> Put
putInterfaceDedup iface =
  do  let state = buildPool iface
          pool = _pool state
          table = reverse (_list state)

      putWord32le (_size state)
      mapM_ (putPoolEntry pool) table

      put (_home iface)
      putMapWith (putAnnotationRef pool) (_values iface)
      putMapWith (putUnionRef pool) (_unions iface)
      putMapWith (putAliasRef pool) (_aliases iface)
      putMapWith (putBinopRef pool) (_binops iface)


putPoolEntry :: TypePool -> Can.Type -> Put
putPoolEntry pool tipe =
  case tipe of
    Can.TLambda a b ->
      putWord8 0 >> putRef pool a >> putRef pool b

    Can.TVar name ->
      putWord8 1 >> put name

    Can.TRecord fields ext ->
      do  putWord8 2
          put (Map.size fields)
          mapM_ (\(name, Can.FieldType order t) -> put name >> put order >> putRef pool t) (Map.toAscList fields)
          put ext

    Can.TUnit ->
      putWord8 3

    Can.TTuple a b mc ->
      do  putWord8 4
          putRef pool a
          putRef pool b
          case mc of
            Nothing -> putWord8 0
            Just c  -> putWord8 1 >> putRef pool c

    Can.TAlias home name args aliasType ->
      do  putWord8 5
          put home
          put name
          put (length args)
          mapM_ (\(n, t) -> put n >> putRef pool t) args
          case aliasType of
            Can.Holey t  -> putWord8 0 >> putRef pool t
            Can.Filled t -> putWord8 1 >> putRef pool t

    Can.TType home name ts ->
      let n = length ts + 7 in
      if n <= fromIntegral (maxBound :: Word8)
        then do
          putWord8 (fromIntegral n)
          put home
          put name
          mapM_ (putRef pool) ts
        else do
          putWord8 6
          put home
          put name
          put (length ts)
          mapM_ (putRef pool) ts


putRef :: TypePool -> Can.Type -> Put
putRef pool tipe =
  case Map.lookup tipe pool of
    Just idx -> putWord32le idx
    Nothing  -> error "Elm.Interface: type not in dedup pool"


putAnnotationRef :: TypePool -> Can.Annotation -> Put
putAnnotationRef pool (Can.Forall freeVars tipe) =
  put freeVars >> putRef pool tipe


putUnionRef :: TypePool -> Union -> Put
putUnionRef pool iUnion =
  case iUnion of
    OpenUnion u    -> putWord8 0 >> putCanUnionRef pool u
    ClosedUnion u  -> putWord8 1 >> putCanUnionRef pool u
    PrivateUnion u -> putWord8 2 >> putCanUnionRef pool u


putCanUnionRef :: TypePool -> Can.Union -> Put
putCanUnionRef pool (Can.Union vars ctors numAlts opts) =
  do  put vars
      put (length ctors)
      mapM_ (\(Can.Ctor name idx numArgs ts) -> do put name; put idx; put numArgs; put (length ts); mapM_ (putRef pool) ts) ctors
      put numAlts
      put opts


putAliasRef :: TypePool -> Alias -> Put
putAliasRef pool iAlias =
  case iAlias of
    PublicAlias a  -> putWord8 0 >> putCanAliasRef pool a
    PrivateAlias a -> putWord8 1 >> putCanAliasRef pool a


putCanAliasRef :: TypePool -> Can.Alias -> Put
putCanAliasRef pool (Can.Alias vars tipe) =
  put vars >> putRef pool tipe


putBinopRef :: TypePool -> Binop -> Put
putBinopRef pool (Binop name ann assoc prec) =
  put name >> putAnnotationRef pool ann >> put assoc >> put prec



-- DESERIALIZE WITH DEDUP


getInterfaceDedup :: Get Interface
getInterfaceDedup =
  do  poolSize <- getWord32le
      table <- readPool poolSize
      home <- get
      values <- getMapWith (getAnnotationFromPool table)
      unions <- getMapWith (getUnionFromPool table)
      aliases <- getMapWith (getAliasFromPool table)
      binops <- getMapWith (getBinopFromPool table)
      return (Interface home values unions aliases binops)


readPool :: Word32 -> Get TypeTable
readPool totalSize = go IntMap.empty 0
  where
    go table idx
      | idx >= totalSize = return table
      | otherwise = do
          entry <- getPoolEntry table
          go (IntMap.insert (fromIntegral idx) entry table) (idx + 1)


getPoolEntry :: TypeTable -> Get Can.Type
getPoolEntry table = do
  tag <- getWord8
  case tag of
    0 -> Can.TLambda <$> getRefT table <*> getRefT table

    1 -> Can.TVar <$> get

    2 -> do
      n <- get :: Get Int
      pairs <- sequence $ replicate n $ do
        name <- get
        order <- get
        tipe <- getRefT table
        return (name, Can.FieldType order tipe)
      ext <- get
      return (Can.TRecord (Map.fromDistinctAscList pairs) ext)

    3 -> return Can.TUnit

    4 -> do
      a <- getRefT table
      b <- getRefT table
      tag2 <- getWord8
      mc <- case tag2 of
        0 -> return Nothing
        _ -> Just <$> getRefT table
      return (Can.TTuple a b mc)

    5 -> do
      home <- get
      name <- get
      numArgs <- get :: Get Int
      args <- sequence $ replicate numArgs $ do
        n <- get
        t <- getRefT table
        return (n, t)
      atag <- getWord8
      aliasType <- case atag of
        0 -> Can.Holey <$> getRefT table
        _ -> Can.Filled <$> getRefT table
      return (Can.TAlias home name args aliasType)

    6 -> do
      home <- get
      name <- get
      n <- get :: Get Int
      ts <- sequence $ replicate n (getRefT table)
      return (Can.TType home name ts)

    n -> do
      home <- get
      name <- get
      ts <- sequence $ replicate (fromIntegral (n - 7)) (getRefT table)
      return (Can.TType home name ts)


getRefT :: TypeTable -> Get Can.Type
getRefT table = do
  idx <- getWord32le
  case IntMap.lookup (fromIntegral idx) table of
    Just t  -> return t
    Nothing -> fail "Elm.Interface: invalid type pool index"


getAnnotationFromPool :: TypeTable -> Get Can.Annotation
getAnnotationFromPool table = do
  freeVars <- get
  tipe <- getRefT table
  return (Can.Forall freeVars tipe)


getUnionFromPool :: TypeTable -> Get Union
getUnionFromPool table = do
  tag <- getWord8
  u <- getCanUnionFromPool table
  case tag of
    0 -> return (OpenUnion u)
    1 -> return (ClosedUnion u)
    _ -> return (PrivateUnion u)


getCanUnionFromPool :: TypeTable -> Get Can.Union
getCanUnionFromPool table = do
  vars <- get
  numCtors <- get :: Get Int
  ctors <- sequence $ replicate numCtors $ do
    name <- get
    idx <- get
    numArgs <- get
    numTs <- get :: Get Int
    ts <- sequence $ replicate numTs (getRefT table)
    return (Can.Ctor name idx numArgs ts)
  numAlts <- get
  opts <- get
  return (Can.Union vars ctors numAlts opts)


getAliasFromPool :: TypeTable -> Get Alias
getAliasFromPool table = do
  tag <- getWord8
  a <- getCanAliasFromPool table
  case tag of
    0 -> return (PublicAlias a)
    _ -> return (PrivateAlias a)


getCanAliasFromPool :: TypeTable -> Get Can.Alias
getCanAliasFromPool table = do
  vars <- get
  tipe <- getRefT table
  return (Can.Alias vars tipe)


getBinopFromPool :: TypeTable -> Get Binop
getBinopFromPool table = do
  name <- get
  ann <- getAnnotationFromPool table
  assoc <- get
  prec <- get
  return (Binop name ann assoc prec)



-- HELPERS


putMapWith :: (Binary k) => (v -> Put) -> Map.Map k v -> Put
putMapWith putValue m = do
  put (Map.size m)
  mapM_ (\(k, v) -> put k >> putValue v) (Map.toAscList m)


getMapWith :: (Binary k, Ord k) => Get v -> Get (Map.Map k v)
getMapWith getValue = do
  n <- get :: Get Int
  pairs <- sequence $ replicate n $ do
    k <- get
    v <- getValue
    return (k, v)
  return (Map.fromDistinctAscList pairs)
