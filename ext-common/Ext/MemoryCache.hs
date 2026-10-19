{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Ext.MemoryCache
  ( Store
  , Backing(..)
  , backing
  , register
  , unregister
  , suspended
  , forStuff
  , reset
  --
  , getDetails
  , setDetails
  , getJsCache
  , setJsCache
  , recordJsBuild
  , jsVersion
  , jsBuildsSince
  --
  , readElmi
  , readElmiFor
  , readElmoFor
  , writeElmo
  , writeElmi
  , keepSame
  , sameOnDisk
  , wroteElmi
  --
  , detailsStillOnDisk
  , loadedDetails
  , wroteDetails
  --
  , flush
  --
  , sourceExistsFor
  , sourceTimeFor
  , sourceReadFor
  , sourceParseFor
  , sourceSizeFor
  , sourcesChanged
  , resetSources
  , reconcileSources
  --
  , Counters(..)
  , takeCounters
  , size
  , externalChanges
  )
  where

{- `lamdera live`: a project's elm-stuff kept as decoded values for the live session.

A Store is registered for one project's stuff directory (elm-stuff/lamdera-*) by a
long-running `lamdera live`. While it is, the builder sites that read or write project
artifacts (Elm.Details.load, Build writeDetails/compile/loadInterface, Generate
loadObject/loadTypesHelp, Ext.Query.Interfaces.cachedHelp) go through it. What it does
there depends on its Backing:

  InMemory (`lamdera live --memory`):
  - writes keep the value (a changed interface is hash-consed first, see I.share) and
    remember the module as dirty; nothing is encoded or written,
  - reads return the kept value, or read the disk artifact once and keep it.

  A module missing from the store has not been compiled in this session, so its disk
  artifacts are still the ones the disk d.dat (the base of the in-memory Details)
  describes. That holds because nothing is written to the project's elm-stuff until
  `flush`, which writes every dirty .elmo, then every dirty .elmi, then d.dat.

  OnDisk (plain `lamdera live`):
  - writes go to elm-stuff exactly as without a store, and the value is kept as well,
    with the stamp (mtime, size, inode) the file has after the write,
  - reads stat the file and return the kept value while the stamp is the same, and
    read (and keep) the file otherwise.

  So elm-stuff stays what every other command expects, and the store only ever holds
  what the files on disk decode to. Another process writing elm-stuff (a `lamdera make`
  in another terminal, a checkout that brings different elm-stuff along, a `rm -rf
  elm-stuff`) changes the stamps: d.dat's is checked at the start of every build
  (detailsStillOnDisk), and when it moved everything kept is dropped and the build
  starts from the disk as a fresh process would; a single artifact whose stamp moved is
  read again. Either way it counts as an external change (externalChanges), and the hot
  patch history is forgotten, so pages get a full reload.

Both backings give the session what the live loop builds on: values that stay the very
same between builds while their module does not change (so the JS of unchanged
definitions is reused, Generate.JavaScript.generateCached, and the type hashes are
skipped, Lamdera.TypeHash.buildCheckHashesFrom), the Details with the package artifacts
resolved, and the listings of recent builds for hot patches (JS BUILDS). Only InMemory
keeps the sources (SOURCES); OnDisk reads them from disk as every build does.

With no store registered (every compile outside `lamdera live`, and `lamdera live` with
LAMDERA_LIVE_SESSION=0) each hook is one IORef read and one lookup in an empty Map, then
the original disk code.

The Details are kept as a Dynamic because Elm.Details imports this module.

This revisits the in-memory compile mode Mario Rogic first wrote for elmx, now in
elm-dev (Ext.FileCache, Ext.FileProxy, Ext.CompileMode, Ext.MemoryCached), which keeps
the encoded bytes of elm-stuff in a hash table under vendored copies of the builder.
Here the decoded values are kept, so a rebuild does no encoding or decoding either, and
the builder is hooked rather than vendored.
-}

import Control.Exception (IOException, catch, finally)
import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.Dynamic (Dynamic)
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FilePath as FP
import System.IO.Unsafe (unsafePerformIO)
import qualified System.PosixCompat.Files as Posix
import qualified Data.Time.Clock.POSIX as POSIX

import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified Elm.Interface as I
import qualified File
import qualified Root as R



-- STORE


data Store =
  Store
    { _dir        :: !FilePath
    , _backing    :: !Backing
    , _modules    :: !(IORef (Map.Map Module.Name Entry))
    , _details    :: !(IORef (Maybe Dynamic))
    , _jsCache    :: !(IORef (Maybe Dynamic)) -- Generate.JavaScript.JsCache
    , _dirty      :: !(IORef Dirty)
    , _counters   :: !(IORef Counters)
    , _sources    :: !(IORef (Map.Map FilePath Source)) -- see SOURCES
    , _sizes      :: !(IORef (Map.Map FilePath (File.Time, Integer))) -- source sizes, by mtime, for Build's scheduler
    , _jsBuilds   :: !(IORef (Int, [(Int, Dynamic)])) -- see JS BUILDS
    , _detailsStamp :: !(IORef (Maybe Stamp)) -- OnDisk: d.dat as the kept Details came from or went to
    , _external   :: !(IORef Int) -- OnDisk: how often elm-stuff was found changed by someone else
    }


{-| Where a store keeps the project's artifacts: only in memory until `flush`
(`lamdera live --memory`), or on disk as always with the decoded values kept as well
(plain `lamdera live`). See the top of this module.
-}
data Backing
  = InMemory
  | OnDisk
  deriving (Eq, Show)


backing :: Store -> Backing
backing =
  _backing


data Entry =
  Entry
    { _iface   :: !(Maybe (Kept I.Interface))
    , _objects :: !(Maybe (Kept Opt.LocalGraph))
    }


-- A kept artifact, with (OnDisk) the stamp of the file it is the value of. InMemory
-- values have no stamp: there the store is where they live.
data Kept a =
  Kept !(Maybe Stamp) a


keptValue :: Kept a -> a
keptValue (Kept _ a) =
  a


-- What says a file is still the one a kept value came from or was written to.
data Stamp =
  Stamp !POSIX.POSIXTime !Integer !Integer -- mtime (to the nanosecond where the file system has it), size, inode
  deriving (Eq, Show)


stampOf :: FilePath -> IO (Maybe Stamp)
stampOf path =
  (do status <- Posix.getFileStatus path
      return $ Just $ Stamp (Posix.modificationTimeHiRes status) (fromIntegral (Posix.fileSize status)) (fromIntegral (Posix.fileID status))
  ) `catch` \(_ :: IOException) -> return Nothing


data Dirty =
  Dirty
    { _dirtyObjects    :: !(Set.Set Module.Name)
    , _dirtyInterfaces :: !(Set.Set Module.Name)
    , _dirtyDetails    :: !Bool
    }


noDirt :: Dirty
noDirt =
  Dirty Set.empty Set.empty False


{-| What the store did since the last takeCounters: modules compiled (objects written),
interfaces and objects served from memory, and artifacts read from disk.
-}
data Counters =
  Counters
    { _compiled      :: !Int
    , _ifaceHits     :: !Int
    , _objectHits    :: !Int
    , _diskReads     :: !Int
    , _sourceFs      :: !Int -- stat or read of a project source on disk
    , _parseHits     :: !Int -- unchanged sources not parsed again
    , _staleReads    :: !Int -- OnDisk: kept artifacts whose file had changed under them
    }
  deriving (Show)


noCounters :: Counters
noCounters =
  Counters 0 0 0 0 0 0 0



-- REGISTRY


data Registry =
  Registry
    { _stores  :: !(Map.Map FilePath Store) -- by canonical stuff dir
    , _aliases :: !(Map.Map FilePath (Maybe Store)) -- stuff dirs as callers spell them
    }


{-# NOINLINE registry #-}
registry :: IORef Registry
registry =
  unsafePerformIO $ newIORef (Registry Map.empty Map.empty)


{-| Keep the artifacts of the project whose stuff directory this is, from now on and for
every build of it in this process. Registering again returns the same store.
-}
register :: Backing -> R.Stuff -> IO Store
register backing_ stuff =
  do  dir <- canonical (R.stuffToFilePath stuff)
      existing <- Map.lookup dir . _stores <$> readIORef registry
      case existing of
        Just store ->
          return store

        Nothing ->
          do  store <- Store dir backing_ <$> newIORef Map.empty <*> newIORef Nothing <*> newIORef Nothing <*> newIORef noDirt <*> newIORef noCounters <*> newIORef Map.empty <*> newIORef Map.empty <*> newIORef (0, []) <*> newIORef Nothing <*> newIORef 0
              insert store
              return store


insert :: Store -> IO ()
insert store =
  atomicModifyIORef' registry $ \(Registry stores _) ->
    (Registry (Map.insert (_dir store) store stores) Map.empty, ())


unregister :: Store -> IO ()
unregister store =
  atomicModifyIORef' registry $ \(Registry stores _) ->
    (Registry (Map.delete (_dir store) stores) Map.empty, ())


{-| Run with the store not registered, as another process building the same project
would (for tests), and register it again after.
-}
suspended :: Store -> IO a -> IO a
suspended store io =
  do  unregister store
      io `finally` insert store


forStuff :: R.Stuff -> IO (Maybe Store)
forStuff stuff =
  do  Registry stores aliases <- readIORef registry
      if Map.null stores
        then return Nothing
        else
          let raw = R.stuffToFilePath stuff in
          case Map.lookup raw aliases of
            Just maybeStore ->
              return maybeStore

            Nothing ->
              do  dir <- canonical raw
                  let maybeStore = Map.lookup dir stores
                  atomicModifyIORef' registry $ \(Registry stores_ aliases_) ->
                    (Registry stores_ (Map.insert raw maybeStore aliases_), ())
                  return maybeStore


canonical :: FilePath -> IO FilePath
canonical path =
  Dir.canonicalizePath path `catch` \(_ :: IOException) -> return path


{-| Forget everything kept, for when the kept Details are dropped: whatever is loaded
next comes from disk again.
-}
reset :: Store -> IO ()
reset store =
  do  writeIORef (_modules store) Map.empty
      writeIORef (_details store) Nothing
      writeIORef (_jsCache store) Nothing
      writeIORef (_dirty store) noDirt
      writeIORef (_detailsStamp store) Nothing
      resetSources store


size :: Store -> IO (Int, Int)
size store =
  do  modules <- readIORef (_modules store)
      let holding f = Map.size (Map.filter f modules)
      return (holding (maybe False (const True) . _iface), holding (maybe False (const True) . _objects))


{-| How many times (OnDisk) elm-stuff was found changed by someone else. -}
externalChanges :: Store -> IO Int
externalChanges store =
  readIORef (_external store)


-- Someone else wrote elm-stuff: count it, and forget the hot patch history, so that
-- every page gets the full reload rather than a patch worked out across the change.
noteExternal :: Store -> IO ()
noteExternal store =
  do  atomicModifyIORef' (_external store) $ \n -> (n + 1, ())
      atomicModifyIORef' (_jsBuilds store) $ \(latest, _) -> ((latest, []), ())



-- DETAILS


getDetails :: Store -> IO (Maybe Dynamic)
getDetails store =
  readIORef (_details store)


{-| Keep the Details a build wrote. InMemory they are written to d.dat by `flush`; OnDisk
the caller has written them, see wroteDetails.
-}
setDetails :: Store -> Dynamic -> IO ()
setDetails store details =
  do  writeIORef (_details store) (Just details)
      if _backing store == InMemory
        then atomicModifyIORef' (_dirty store) $ \d -> (d { _dirtyDetails = True }, ())
        else return ()


{-| OnDisk: whether d.dat is still the one the kept Details were loaded from or written
to. When it is not, someone else built the project (or removed elm-stuff): that counts
as an external change, and the caller drops everything kept and loads from disk.
InMemory there is nothing to check, the kept Details are the truth until `flush`.
-}
detailsStillOnDisk :: Store -> R.Stuff -> IO Bool
detailsStillOnDisk store stuff =
  case _backing store of
    InMemory ->
      return True

    OnDisk ->
      do  recorded <- readIORef (_detailsStamp store)
          case recorded of
            Nothing ->
              return False

            Just stamp ->
              do  now <- stampOf (R.details stuff)
                  if now == Just stamp
                    then return True
                    else noteExternal store >> return False


{-| The Details were just loaded from disk. -}
loadedDetails :: Store -> R.Stuff -> Dynamic -> IO ()
loadedDetails store stuff details =
  do  writeIORef (_details store) (Just details)
      if _backing store == OnDisk
        then writeIORef (_detailsStamp store) =<< stampOf (R.details stuff)
        else return ()


{-| OnDisk: the Details were just written to d.dat (by the caller). -}
wroteDetails :: Store -> R.Stuff -> Dynamic -> IO ()
wroteDetails store stuff details =
  do  writeIORef (_details store) (Just details)
      writeIORef (_detailsStamp store) =<< stampOf (R.details stuff)



-- JS
--
-- The rendered statements of the last JS generation (Generate.JavaScript.generateCached),
-- kept as a Dynamic for the same reason as the Details.


getJsCache :: Store -> IO (Maybe Dynamic)
getJsCache store =
  readIORef (_jsCache store)


setJsCache :: Store -> Dynamic -> IO ()
setJsCache store cache =
  writeIORef (_jsCache store) (Just cache)



-- ARTIFACTS


lookupEntry :: Store -> Module.Name -> IO (Maybe Entry)
lookupEntry store name =
  Map.lookup name <$> readIORef (_modules store)


updateEntry :: Store -> Module.Name -> (Entry -> Entry) -> IO ()
updateEntry store name f =
  atomicModifyIORef' (_modules store) $ \modules ->
    (Map.alter (Just . f . maybe (Entry Nothing Nothing) id) name modules, ())


count :: Store -> (Counters -> Counters) -> IO ()
count store f =
  atomicModifyIORef' (_counters store) $ \c -> (f c, ())


takeCounters :: Store -> IO Counters
takeCounters store =
  atomicModifyIORef' (_counters store) $ \c -> (noCounters, c)


readElmi :: Maybe Store -> R.Stuff -> Module.Name -> IO (Maybe I.Interface)
readElmi maybeStore stuff name =
  case maybeStore of
    Nothing ->
      File.readBytes I.dInterface (R.elmi stuff name)

    Just store ->
      readKept store name _iface (\k e -> e { _iface = Just k }) (\c -> c { _ifaceHits = _ifaceHits c + 1 })
        (R.elmi stuff name) (File.readBytes I.dInterface (R.elmi stuff name))


readElmiFor :: R.Stuff -> Module.Name -> IO (Maybe I.Interface)
readElmiFor stuff name =
  do  maybeStore <- forStuff stuff
      readElmi maybeStore stuff name


readElmoFor :: R.Stuff -> Module.Name -> IO (Maybe Opt.LocalGraph)
readElmoFor stuff name =
  do  maybeStore <- forStuff stuff
      case maybeStore of
        Nothing ->
          File.readBytes Opt.dLocalGraph (R.elmo stuff name)

        Just store ->
          readKept store name _objects (\k e -> e { _objects = Just k }) (\c -> c { _objectHits = _objectHits c + 1 })
            (R.elmo stuff name) (File.readBytes Opt.dLocalGraph (R.elmo stuff name))


-- A kept artifact, or the file read once and kept. OnDisk the kept value is only used
-- while the file has the stamp it had when it was kept; one that moved was written by
-- someone else, and is read again.
readKept :: Store -> Module.Name -> (Entry -> Maybe (Kept a)) -> (Kept a -> Entry -> Entry) -> (Counters -> Counters) -> FilePath -> IO (Maybe a) -> IO (Maybe a)
readKept store name get set hit path fromDisk =
  do  stamp <- if _backing store == OnDisk then stampOf path else return Nothing
      entry <- lookupEntry store name
      case get =<< entry of
        Just (Kept keptStamp a) | keptStamp == stamp ->
          do  count store hit
              return (Just a)

        Just _ ->
          do  noteExternal store
              count store $ \c -> c { _staleReads = _staleReads c + 1 }
              fromDiskKept stamp

        Nothing ->
          fromDiskKept stamp
  where
    fromDiskKept stamp =
      do  count store $ \c -> c { _diskReads = _diskReads c + 1 }
          maybeA <- fromDisk
          forM_ maybeA $ \a ->
            updateEntry store name (set (Kept stamp a))
          return maybeA


writeElmo :: Maybe Store -> File.Writer t -> R.Stuff -> Module.Name -> Opt.LocalGraph -> IO ()
writeElmo maybeStore writer stuff name objects =
  case maybeStore of
    Nothing ->
      File.writeBytes writer (R.elmo stuff name) Opt.eLocalGraph objects

    Just store | _backing store == OnDisk ->
      do  File.writeBytes writer (R.elmo stuff name) Opt.eLocalGraph objects
          stamp <- stampOf (R.elmo stuff name)
          updateEntry store name $ \e -> e { _objects = Just (Kept stamp objects) }
          count store $ \c -> c { _compiled = _compiled c + 1 }

    Just store ->
      do  updateEntry store name $ \e -> e { _objects = Just (Kept Nothing objects) }
          markDirty store $ \d -> d { _dirtyObjects = Set.insert name (_dirtyObjects d) }
          count store $ \c -> c { _compiled = _compiled c + 1 }


{-| Write a changed interface, InMemory (OnDisk Build writes it itself, then wroteElmi).
It is hash-consed first, and the shared value is returned so the rest of the build uses
it too and the fresh tree can be collected.
-}
writeElmi :: Maybe Store -> File.Writer t -> R.Stuff -> Module.Name -> I.Interface -> IO I.Interface
writeElmi maybeStore writer stuff name iface =
  case maybeStore of
    Nothing ->
      do  File.writeBytes writer (R.elmi stuff name) I.eInterface iface
          return iface

    Just store ->
      do  let !shared = if shareInterfaces then I.share iface else iface
          updateEntry store name $ \e -> e { _iface = Just (Kept Nothing shared) }
          markDirty store $ \d -> d { _dirtyInterfaces = Set.insert name (_dirtyInterfaces d) }
          return shared


{-| OnDisk: Build just wrote this changed interface to its .elmi. Keep it, hash-consed,
and return the shared value for the rest of the build. Without a store: the interface.
-}
wroteElmi :: Maybe Store -> R.Stuff -> Module.Name -> I.Interface -> IO I.Interface
wroteElmi maybeStore stuff name iface =
  case maybeStore of
    Just store | _backing store == OnDisk ->
      do  let !shared = if shareInterfaces then I.share iface else iface
          stamp <- stampOf (R.elmi stuff name)
          updateEntry store name $ \e -> e { _iface = Just (Kept stamp shared) }
          return shared

    _ ->
      return iface


{-| OnDisk: Build found the recompiled interface the same as the .elmi file's (by its
bytes). Give back the kept value of that file when there is one, so an interface that
compiled the same stays the very same value (as keepSame does InMemory), or keep this
one. Without a store: the interface.
-}
sameOnDisk :: Maybe Store -> R.Stuff -> Module.Name -> I.Interface -> IO I.Interface
sameOnDisk maybeStore stuff name iface =
  case maybeStore of
    Just store | _backing store == OnDisk ->
      do  stamp <- stampOf (R.elmi stuff name)
          entry <- lookupEntry store name
          case _iface =<< entry of
            Just (Kept keptStamp keptIface) | keptStamp == stamp && Maybe.isJust stamp ->
              return keptIface

            _ ->
              do  let !shared = if shareInterfaces then I.share iface else iface
                  updateEntry store name $ \e -> e { _iface = Just (Kept stamp shared) }
                  return shared

    _ ->
      return iface


{-| LAMDERA_MEMORY_SHARE=0 keeps fresh interfaces as they come out of the compiler, to
measure what hash-consing them saves.
-}
{-# NOINLINE shareInterfaces #-}
shareInterfaces :: Bool
shareInterfaces =
  unsafePerformIO $ (/= Just "0") <$> Env.lookupEnv "LAMDERA_MEMORY_SHARE"


{-| An interface that compiled to the same as before: in memory keep the old value
(already shared, and already in the store) and let the fresh tree go. On disk the
builder's choice is unchanged.
-}
keepSame :: Maybe Store -> I.Interface -> I.Interface -> I.Interface
keepSame maybeStore old fresh =
  case maybeStore of
    Nothing -> fresh
    Just _  -> old


markDirty :: Store -> (Dirty -> Dirty) -> IO ()
markDirty store f =
  atomicModifyIORef' (_dirty store) $ \d -> (f d, ())



-- JS BUILDS
--
-- Each build's Generate.JavaScript.Listing (as a Dynamic), numbered, the last few kept:
-- the page says which build it runs, and Lamdera.HotPatch works out what it needs from
-- the listings since.


keptJsBuilds :: Int
keptJsBuilds =
  10


{-| Keep the listing of a new build, returning its number. -}
recordJsBuild :: Store -> Dynamic -> IO Int
recordJsBuild store listing =
  atomicModifyIORef' (_jsBuilds store) $ \(latest, kept) ->
    let
      next = latest + 1
      -- the spine is forced here: left to `take`, every build would hold on to all the
      -- ones before it until a page asks for a patch
      kept_ = take keptJsBuilds ((next, listing) : kept)
    in
    length kept_ `seq` ((next, kept_), next)


{-| The number of the latest build (0 before the first). -}
jsVersion :: Store -> IO Int
jsVersion store =
  fst <$> readIORef (_jsBuilds store)


{-| The listings of build `from` and of every build after it, oldest first, when `from`
is still kept, with the number of the latest.
-}
jsBuildsSince :: Store -> Int -> IO (Maybe (Int, [Dynamic]))
jsBuildsSince store from =
  do  (latest, kept) <- readIORef (_jsBuilds store)
      let since = takeWhile ((>= from) . fst) kept
      return $
        if any ((== from) . fst) since
          then Just (latest, map snd (reverse since))
          else Nothing



-- FLUSH


{-| Write what was compiled in memory to the project's elm-stuff: every dirty .elmo, then
every dirty .elmi, then d.dat (with the given writer for it). Stopped part way, the old
d.dat makes the next disk build recheck every module that changed, and no .elmi is ever
newer than the .elmo files compiled against it. Returns the number of .elmo and .elmi
files written.
-}
flush :: Store -> File.Writer t -> R.Stuff -> (Dynamic -> IO ()) -> IO (Int, Int)
flush store writer stuff writeDetails =
  do  Dirty objects ifaces details <- atomicModifyIORef' (_dirty store) $ \d -> (noDirt, d)
      modules <- readIORef (_modules store)
      let kept f names = [ (name, keptValue a) | name <- Set.toList names, Just a <- [f =<< Map.lookup name modules] ]
          elmos = kept _objects objects
          elmis = kept _iface ifaces
      forM_ elmos $ \(name, o) -> File.writeBytes writer (R.elmo stuff name) Opt.eLocalGraph o
      forM_ elmis $ \(name, i) -> File.writeBytes writer (R.elmi stuff name) I.eInterface i
      maybeDetails <- readIORef (_details store)
      forM_ maybeDetails $ \d -> if details then writeDetails d else return ()
      return (length elmos, length elmis)



-- SOURCES
--
-- The project's .elm sources, as the builder sees them: whether a path exists, its time,
-- its bytes and its parse. On disk the crawl stats every module in every source
-- directory on every build, and a module recompiled only because an interface it
-- imports changed is read and parsed again. In memory each path is looked at on disk
-- once, and after that only when the file watcher reports it (sourcesChanged) or a
-- reconcile finds it changed (reconcileSources). Times are the files' own mtimes, so
-- the Details flushed to elm-stuff stay valid for a disk build; a file whose bytes
-- change under an unchanged mtime gets its time bumped so the builder still sees it.
--
-- What the watcher cannot be trusted for is handled by forgetting everything
-- (resetSources, so the next build looks at the disk again as a disk build would): a
-- directory event (a rename or a checkout), a path the watcher reports that is gone and
-- is not a file, and an elm.json change (the store is reset). A reconcile, every few
-- seconds in the background and whenever the page is loaded, stats every path looked at,
-- so an edit the watcher misses (an ignored directory, a dropped event) is picked up
-- within seconds. (Matt Griffith's elm-dev rescans every source on each compile
-- instead, updateVfsFromFs; here that is the safety net, not the main path.)
--
-- Only InMemory (OnDisk the sources come from disk, as without a store). Only .elm files outside elm-stuff, and in elm-stuff/lamdera (the extra source
-- directory of the live harness, where every module is looked for and not found), are
-- kept. The live loop reports the files it writes there (Lamdera.CLI.Live.localDevFiles)
-- before every build, as the watcher ignores elm-stuff.
-- LAMDERA_MEMORY_SOURCES=0 turns this off (sources come from disk, as before).


data Source
  = Present !File.Time !(Maybe BS.ByteString) !(Maybe Src.Module)
  | Absent


{-# NOINLINE keepSources #-}
keepSources :: Bool
keepSources =
  unsafePerformIO $ (/= Just "0") <$> Env.lookupEnv "LAMDERA_MEMORY_SOURCES"


kept :: FilePath -> Bool
kept path =
  keepSources
  && FP.takeExtension path == ".elm"
  && (not ("elm-stuff" `elem` dirs) || ["elm-stuff", "lamdera"] `List.isInfixOf` dirs)
  where
    dirs = FP.splitDirectories path


resetSources :: Store -> IO ()
resetSources store =
  do  writeIORef (_sources store) Map.empty
      writeIORef (_sizes store) Map.empty


withSource :: R.Stuff -> FilePath -> IO a -> (Store -> Source -> IO a) -> IO a
withSource stuff path fromDisk inMemory =
  do  maybeStore <- if kept path then forStuff stuff else return Nothing
      case maybeStore of
        Just store | _backing store == InMemory ->
          do  sources <- readIORef (_sources store)
              case Map.lookup path sources of
                Just source ->
                  inMemory store source

                Nothing ->
                  do  source <- lookAtDisk store path
                      atomicModifyIORef' (_sources store) $ \m -> (Map.insertWith (\_ old -> old) path source m, ())
                      inMemory store source

        _ ->
          fromDisk -- no store, or OnDisk: sources come from disk as on every build


lookAtDisk :: Store -> FilePath -> IO Source
lookAtDisk store path =
  do  count store $ \c -> c { _sourceFs = _sourceFs c + 1 }
      exists <- Dir.doesFileExist path
      if exists
        then (\t -> Present t Nothing Nothing) <$> File.getTime path
        else return Absent


sourceExistsFor :: R.Stuff -> FilePath -> IO Bool
sourceExistsFor stuff path =
  withSource stuff path (File.exists path) $ \_ source ->
    return $ case source of
      Present _ _ _ -> True
      Absent        -> False


sourceTimeFor :: R.Stuff -> FilePath -> IO File.Time
sourceTimeFor stuff path =
  withSource stuff path (File.getTime path) $ \_ source ->
    case source of
      Present t _ _ -> return t
      Absent        -> File.getTime path -- fails as it would on disk


{-| The size of a source in bytes, for Build's scheduler (which looks at every cached
module's size on every build): from the kept bytes, or from a size taken at the same mtime.
-}
sourceSizeFor :: R.Stuff -> FilePath -> IO Integer
sourceSizeFor stuff path =
  withSource stuff path (Dir.getFileSize path) $ \store source ->
    case source of
      Present _ (Just bytes) _ ->
        return (fromIntegral (BS.length bytes))

      Present t Nothing _ ->
        do  sizes <- readIORef (_sizes store)
            case Map.lookup path sizes of
              Just (t2, size) | t2 == t ->
                return size

              _ ->
                do  count store $ \c -> c { _sourceFs = _sourceFs c + 1 }
                    size <- Dir.getFileSize path
                    atomicModifyIORef' (_sizes store) $ \m -> (Map.insert path (t, size) m, ())
                    return size

      Absent ->
        Dir.getFileSize path -- fails as it would on disk


sourceReadFor :: R.Stuff -> FilePath -> IO BS.ByteString
sourceReadFor stuff path =
  withSource stuff path (File.readUtf8 path) $ \store source ->
    case source of
      Present _ (Just bytes) _ ->
        return bytes

      Present t Nothing _ ->
        do  count store $ \c -> c { _sourceFs = _sourceFs c + 1 }
            bytes <- File.readUtf8 path
            update store path $ \old ->
              case old of
                Just (Present t2 Nothing m) | t2 == t -> Present t2 (Just bytes) m
                _ -> Maybe.fromMaybe Absent old
            return bytes

      Absent ->
        File.readUtf8 path -- fails as it would on disk


{-| Parse a source, or give back the parse of the same file at the same time. Only
successful parses are kept.
-}
sourceParseFor :: R.Stuff -> FilePath -> File.Time -> IO (Either e Src.Module) -> IO (Either e Src.Module)
sourceParseFor stuff path time parse =
  withSource stuff path parse $ \store source ->
    case source of
      Present t _ (Just modul) | t == time ->
        do  count store $ \c -> c { _parseHits = _parseHits c + 1 }
            return (Right modul)

      _ ->
        do  result <- parse
            case result of
              Right modul ->
                update store path $ \old ->
                  case old of
                    Just (Present t2 bytes _) | t2 == time -> Present t2 bytes (Just modul)
                    _ -> Maybe.fromMaybe Absent old

              Left _ ->
                return ()
            return result


update :: Store -> FilePath -> (Maybe Source -> Source) -> IO ()
update store path f =
  atomicModifyIORef' (_sources store) $ \m -> (Map.insert path (f (Map.lookup path m)) m, ())


{-| What the file watcher reported. A changed .elm file is looked at again; anything that
could mean many files moved at once (a directory, a path that is gone and was not a
file) forgets everything. Returns whether anything kept was affected.
-}
sourcesChanged :: Store -> [FilePath] -> IO Bool
sourcesChanged store _ | _backing store == OnDisk = return False -- nothing kept
sourcesChanged store paths =
  or <$> forM paths (\path ->
    if FP.takeExtension path == ".elm" && kept path then
      refresh store path
    else if "elm-stuff" `elem` FP.splitDirectories path || FP.takeFileName path == "elm.json" || FP.takeExtension path == ".elm" then
      return False
    else
      do  isDir <- Dir.doesDirectoryExist path
          isFile <- Dir.doesFileExist path
          if isDir || (not isFile && null (FP.takeExtension path))
            then resetSources store >> return True
            else return False
  )


refresh :: Store -> FilePath -> IO Bool
refresh store path =
  do  sources <- readIORef (_sources store)
      case Map.lookup path sources of
        Nothing ->
          return False -- never looked at, the next build looks at the disk anyway

        Just old ->
          do  new <- lookAtDisk store path
              case (old, new) of
                (Absent, Absent) ->
                  return False

                (Present oldTime oldBytes parsed, Present newTime _ _) | oldTime == newTime ->
                  -- same mtime: changed only if the bytes are known and differ. Bytes never
                  -- read are read now, to compare against next time.
                  do  bytes <- File.readUtf8 path
                      case oldBytes of
                        Nothing ->
                          do  update store path $ \_ -> Present oldTime (Just bytes) parsed
                              return False

                        Just known | known == bytes ->
                          return False

                        Just _ ->
                          do  update store path $ \_ -> Present (bump oldTime) (Just bytes) Nothing
                              return True

                _ ->
                  do  update store path $ \_ -> new
                      return True


-- Any time other than the one the builder last saw makes it look at the file again. Only
-- used when the file's mtime equals the old one, which is never zero.
bump :: File.Time -> File.Time
bump _ =
  File.zeroTime


{-| Look at every kept path on disk again (a stat each) and refresh the ones that
differ. Cheap enough to run every few seconds, and on demand. Returns the paths that
changed, for the caller to rebuild.
-}
reconcileSources :: Store -> IO [FilePath]
reconcileSources store | _backing store == OnDisk = return [] -- nothing kept
reconcileSources store =
  do  sources <- readIORef (_sources store)
      changed <- forM (Map.toList sources) $ \(path, source) ->
        do  exists <- Dir.doesFileExist path
            differs <-
              case (source, exists) of
                (Absent, False)        -> return False
                (Present t _ _, True)  -> (/= t) <$> File.getTime path
                _                      -> return True
            if differs then refresh store path >>= \r -> return (if r then Just path else Nothing) else return Nothing
      return (Maybe.catMaybes changed)
