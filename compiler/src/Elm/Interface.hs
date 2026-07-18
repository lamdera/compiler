{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
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
  , getDedupTimings
  )
  where


import Control.Monad (liftM, liftM3, liftM4, liftM5)
import Data.Binary
import Data.Binary.Put (putWord32le)
import Data.Binary.Get (getWord32le, lookAhead)
import qualified Data.IORef as IORef
import qualified Data.Time.Clock as Clock
import qualified System.IO.Unsafe as Unsafe
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Utils.Binop as Binop
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Ext.Common as Ext
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
    putWord8 0x00  -- magic sentinel: new dedup format
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
-- TYPE DEDUPLICATION (Shape-based bottom-up interning)
--
-- Each Can.Type subtree gets interned into a "Shape" where children are
-- already Word32 pool IDs. Hashing/comparing a Shape is O(small) regardless
-- of subtree size, so the Map.lookup that gates dedup is never expensive.
-- We thread an InternState through the walk; top-level values produce Put
-- actions that reference children by ID (no re-lookup phase required).
-- ============================================================================


data Shape
  = SLambda !Word32 !Word32
  | SVar !Name.Name
  | SType !ModuleName.Canonical !Name.Name ![Word32]
  | SRecord ![(Name.Name, Word16, Word32)] !(Maybe Name.Name)
  | SUnit
  | STuple !Word32 !Word32 !(Maybe Word32)
  | SAlias !ModuleName.Canonical !Name.Name ![(Name.Name, Word32)] !ShapeAlias
  deriving (Eq, Ord)


data ShapeAlias = SHoley !Word32 | SFilled !Word32
  deriving (Eq, Ord)


type ShapePool = Map.Map Shape Word32
type TypeTable = IntMap.IntMap Can.Type


data InternState = InternState
  { _pool :: !ShapePool
  , _list :: ![Shape]  -- in reverse insertion order
  , _size :: !Word32
  }


emptyIntern :: InternState
emptyIntern = InternState Map.empty [] 0


-- Intern a type bottom-up, returning its pool ID.
internType :: Can.Type -> InternState -> (Word32, InternState)
internType tipe state = case tipe of
  Can.TLambda a b ->
    let (idA, s1) = internType a state
        (idB, s2) = internType b s1
    in registerShape (SLambda idA idB) s2

  Can.TVar n ->
    registerShape (SVar n) state

  Can.TType home name ts ->
    let (ids, s1) = internTypes ts state
    in registerShape (SType home name ids) s1

  Can.TRecord fields ext ->
    let (entries, s1) = internRecordFields (Map.toAscList fields) state
    in registerShape (SRecord entries ext) s1

  Can.TUnit ->
    registerShape SUnit state

  Can.TTuple a b mc ->
    let (idA, s1) = internType a state
        (idB, s2) = internType b s1
    in case mc of
         Nothing ->
           registerShape (STuple idA idB Nothing) s2
         Just c  ->
           let (idC, s3) = internType c s2
           in registerShape (STuple idA idB (Just idC)) s3

  Can.TAlias home name args aliasType ->
    let (argEntries, s1) = internAliasArgs args state
    in case aliasType of
         Can.Holey t ->
           let (idT, s2) = internType t s1
           in registerShape (SAlias home name argEntries (SHoley idT)) s2
         Can.Filled t ->
           let (idT, s2) = internType t s1
           in registerShape (SAlias home name argEntries (SFilled idT)) s2


-- Direct recursion is measurably faster than mapAccumL here because the
-- intermediate (acc, x) tuples mapAccumL builds in a generic shape add GC
-- pressure on the hot pool-building path.

internTypes :: [Can.Type] -> InternState -> ([Word32], InternState)
internTypes ts state =
  case ts of
    [] -> ([], state)
    t : rest ->
      let (i, s1)  = internType t state
          (is, s2) = internTypes rest s1
      in (i : is, s2)


internRecordFields :: [(Name.Name, Can.FieldType)] -> InternState
                   -> ([(Name.Name, Word16, Word32)], InternState)
internRecordFields fs state =
  case fs of
    [] -> ([], state)
    (n, Can.FieldType o t) : rest ->
      let (i, s1)  = internType t state
          (rs, s2) = internRecordFields rest s1
      in ((n, o, i) : rs, s2)


internAliasArgs :: [(Name.Name, Can.Type)] -> InternState
                -> ([(Name.Name, Word32)], InternState)
internAliasArgs args state =
  case args of
    [] -> ([], state)
    (n, t) : rest ->
      let (i, s1)  = internType t state
          (rs, s2) = internAliasArgs rest s1
      in ((n, i) : rs, s2)


registerShape :: Shape -> InternState -> (Word32, InternState)
registerShape shape state =
  case Map.lookup shape (_pool state) of
    Just idx -> (idx, state)
    Nothing ->
      let !idx   = _size state
          !pool' = Map.insert shape idx (_pool state)
          !size' = idx + 1
      in (idx, InternState pool' (shape : _list state) size')



-- INTERN + COLLECT PUT ACTIONS
--
-- Each top-level structure (Annotation, Union, Alias, Binop) is interned
-- and converted to a Put action that uses the resulting Word32 IDs.
-- The Put action is closed over the IDs directly, so serialization needs
-- no second lookup.


internAnnotationP :: Can.Annotation -> InternState -> (Put, InternState)
internAnnotationP (Can.Forall freeVars tipe) state =
  let (idx, s') = internType tipe state
      p = put freeVars >> putWord32le idx
  in (p, s')


internUnionP :: Union -> InternState -> (Put, InternState)
internUnionP iUnion state =
  case iUnion of
    OpenUnion u    -> let (p, s') = internCanUnionP u state in (putWord8 0 >> p, s')
    ClosedUnion u  -> let (p, s') = internCanUnionP u state in (putWord8 1 >> p, s')
    PrivateUnion u -> let (p, s') = internCanUnionP u state in (putWord8 2 >> p, s')


internCanUnionP :: Can.Union -> InternState -> (Put, InternState)
internCanUnionP (Can.Union vars ctors numAlts opts) state =
  let (ctorPuts, state') = internCtorsP ctors state
      p = do put vars
             put (length ctors)
             sequence_ ctorPuts
             put numAlts
             put opts
  in (p, state')


internCtorsP :: [Can.Ctor] -> InternState -> ([Put], InternState)
internCtorsP cs state =
  case cs of
    [] -> ([], state)
    Can.Ctor n idx numArgs ts : rest ->
      let (ids, s1) = internTypes ts state
          (rs, s2)  = internCtorsP rest s1
          p = do put n
                 put idx
                 put numArgs
                 put (length ts)
                 mapM_ putWord32le ids
      in (p : rs, s2)


internAliasP :: Alias -> InternState -> (Put, InternState)
internAliasP iAlias state =
  case iAlias of
    PublicAlias  a -> let (p, s') = internCanAliasP a state in (putWord8 0 >> p, s')
    PrivateAlias a -> let (p, s') = internCanAliasP a state in (putWord8 1 >> p, s')


internCanAliasP :: Can.Alias -> InternState -> (Put, InternState)
internCanAliasP (Can.Alias vars tipe) state =
  let (idx, s') = internType tipe state
      p = put vars >> putWord32le idx
  in (p, s')


internBinopP :: Binop -> InternState -> (Put, InternState)
internBinopP (Binop name ann assoc prec) state =
  let (annP, s') = internAnnotationP ann state
      p = put name >> annP >> put assoc >> put prec
  in (p, s')


-- Intern the values of a Map, preserving keys; returns ordered (key, putAction) list.
internMapP :: (v -> InternState -> (Put, InternState))
           -> Map.Map k v
           -> InternState
           -> ([(k, Put)], InternState)
internMapP f m state0 =
  let go [] s = ([], s)
      go ((k, v) : rest) s =
        let (p, s')   = f v s
            (rs, s'') = go rest s'
        in ((k, p) : rs, s'')
  in go (Map.toAscList m) state0



-- SERIALIZE WITH DEDUP


{-# NOINLINE buildPoolNanos #-}
buildPoolNanos :: IORef.IORef Integer
buildPoolNanos = Unsafe.unsafePerformIO (IORef.newIORef 0)


{-# NOINLINE dedupTimingEnabled #-}
dedupTimingEnabled :: Bool
dedupTimingEnabled = Ext.envFlag "LDEBUG_DEDUP_TIMING"


getDedupTimings :: IO Double
getDedupTimings = do
  b <- IORef.readIORef buildPoolNanos
  return (fromIntegral b / 1e6)


putInterfaceDedup :: Interface -> Put
putInterfaceDedup iface =
  let (valuesPuts,  s1) = internMapP internAnnotationP (_values iface)  emptyIntern
      (unionsPuts,  s2) = internMapP internUnionP      (_unions iface)  s1
      (aliasesPuts, s3) = internMapP internAliasP     (_aliases iface) s2
      (binopsPuts,  s4) = internMapP internBinopP     (_binops iface)  s3
      !state4           = if dedupTimingEnabled then recordPoolTime s4 else s4
      shapes            = reverse (_list state4)
  in
  do  putWord32le (_size state4)
      mapM_ putShape shapes
      put (_home iface)
      putMapPuts valuesPuts
      putMapPuts unionsPuts
      putMapPuts aliasesPuts
      putMapPuts binopsPuts


-- Force pool construction inside a clock and accumulate the duration.
-- Returns the (forced) state unchanged.
recordPoolTime :: InternState -> InternState
recordPoolTime s = Unsafe.unsafePerformIO $ do
  t0 <- Clock.getCurrentTime
  _size s `seq` length (_list s) `seq` return ()
  t1 <- Clock.getCurrentTime
  let dt    = Clock.diffUTCTime t1 t0
      nanos = round (realToFrac dt * 1e9 :: Double) :: Integer
  IORef.atomicModifyIORef' buildPoolNanos (\acc -> (acc + nanos, ()))
  return s


putMapPuts :: Binary k => [(k, Put)] -> Put
putMapPuts kps = do
  put (length kps)
  mapM_ (\(k, p) -> put k >> p) kps


putShape :: Shape -> Put
putShape shape = case shape of
  SLambda a b ->
    putWord8 0 >> putWord32le a >> putWord32le b

  SVar name ->
    putWord8 1 >> put name

  SRecord fields ext ->
    do  putWord8 2
        put (length fields)
        mapM_ (\(n, o, i) -> put n >> put o >> putWord32le i) fields
        put ext

  SUnit ->
    putWord8 3

  STuple a b mc ->
    do  putWord8 4
        putWord32le a
        putWord32le b
        case mc of
          Nothing -> putWord8 0
          Just c  -> putWord8 1 >> putWord32le c

  SAlias home name args aliasType ->
    do  putWord8 5
        put home
        put name
        put (length args)
        mapM_ (\(n, i) -> put n >> putWord32le i) args
        case aliasType of
          SHoley i  -> putWord8 0 >> putWord32le i
          SFilled i -> putWord8 1 >> putWord32le i

  SType home name ts ->
    let n = length ts + 7 in
    if n <= fromIntegral (maxBound :: Word8)
      then do
        putWord8 (fromIntegral n)
        put home
        put name
        mapM_ putWord32le ts
      else do
        putWord8 6
        put home
        put name
        put (length ts)
        mapM_ putWord32le ts



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


getMapWith :: (Binary k, Ord k) => Get v -> Get (Map.Map k v)
getMapWith getValue = do
  n <- get :: Get Int
  pairs <- sequence $ replicate n $ do
    k <- get
    v <- getValue
    return (k, v)
  return (Map.fromList pairs)
