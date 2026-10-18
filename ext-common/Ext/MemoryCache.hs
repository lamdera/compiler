{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Ext.MemoryCache
  ( Store
  , register
  , unregister
  , forStuff
  , reset
  --
  , getDetails
  , setDetails
  --
  , readElmi
  , readElmiFor
  , readElmoFor
  , writeElmo
  , writeElmi
  , keepSame
  --
  , flush
  --
  , Counters(..)
  , takeCounters
  , size
  )
  where

{- `lamdera live --memory`: a project's elm-stuff kept as decoded values.

A Store is registered for one project's stuff directory (elm-stuff/lamdera-*). While it
is, the builder sites that read or write project artifacts (Elm.Details.load, Build
writeDetails/compile/loadInterface, Generate loadObject/loadTypesHelp,
Ext.Query.Interfaces.cachedHelp) go through it instead of the disk:

  - writes keep the value (a changed interface is hash-consed first, see I.share) and
    remember the module as dirty; nothing is encoded or written,
  - reads return the kept value, or read the disk artifact once and keep it.

A module missing from the store has not been compiled in this session, so its disk
artifacts are still the ones the disk d.dat (the base of the in-memory Details)
describes. That holds because nothing is written to the project's elm-stuff until
`flush`, which writes every dirty .elmo, then every dirty .elmi, then d.dat.

With no store registered (every compile outside `lamdera live --memory`) each hook is
one IORef read and one lookup in an empty Map, then the original disk code.

The Details are kept as a Dynamic because Elm.Details imports this module.

This is a port of the idea behind elm-dev's in-memory mode (Ext.FileCache and
Ext.MemoryCached, https://github.com/mdgriffith/elm-dev, BSD-3-Clause), which keeps the
encoded bytes of elm-stuff in a hash table under vendored copies of the builder. Here
the decoded values are kept, so a rebuild does no encoding or decoding either, and the
builder is hooked rather than vendored.
-}

import Control.Exception (IOException, catch)
import Control.Monad (forM_)
import Data.Dynamic (Dynamic)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.IO.Unsafe (unsafePerformIO)

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified Elm.Interface as I
import qualified File
import qualified Root as R



-- STORE


data Store =
  Store
    { _dir        :: !FilePath
    , _modules    :: !(IORef (Map.Map Module.Name Entry))
    , _details    :: !(IORef (Maybe Dynamic))
    , _dirty      :: !(IORef Dirty)
    , _counters   :: !(IORef Counters)
    }


data Entry =
  Entry
    { _iface   :: !(Maybe I.Interface)
    , _objects :: !(Maybe Opt.LocalGraph)
    }


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
    }
  deriving (Show)


noCounters :: Counters
noCounters =
  Counters 0 0 0 0



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


{-| Keep the artifacts of the project whose stuff directory this is in memory, from now
on and for every build of it in this process. Registering again returns the same store.
-}
register :: R.Stuff -> IO Store
register stuff =
  do  dir <- canonical (R.stuffToFilePath stuff)
      existing <- Map.lookup dir . _stores <$> readIORef registry
      case existing of
        Just store ->
          return store

        Nothing ->
          do  store <- Store dir <$> newIORef Map.empty <*> newIORef Nothing <*> newIORef noDirt <*> newIORef noCounters
              atomicModifyIORef' registry $ \(Registry stores _) ->
                (Registry (Map.insert dir store stores) Map.empty, ())
              return store


unregister :: Store -> IO ()
unregister store =
  atomicModifyIORef' registry $ \(Registry stores _) ->
    (Registry (Map.delete (_dir store) stores) Map.empty, ())


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
      writeIORef (_dirty store) noDirt


size :: Store -> IO (Int, Int)
size store =
  do  modules <- readIORef (_modules store)
      let holding f = Map.size (Map.filter f modules)
      return (holding (maybe False (const True) . _iface), holding (maybe False (const True) . _objects))



-- DETAILS


getDetails :: Store -> IO (Maybe Dynamic)
getDetails store =
  readIORef (_details store)


setDetails :: Store -> Dynamic -> IO ()
setDetails store details =
  do  writeIORef (_details store) (Just details)
      atomicModifyIORef' (_dirty store) $ \d -> (d { _dirtyDetails = True }, ())



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
      do  entry <- lookupEntry store name
          case _iface =<< entry of
            Just iface ->
              do  count store $ \c -> c { _ifaceHits = _ifaceHits c + 1 }
                  return (Just iface)

            Nothing ->
              do  count store $ \c -> c { _diskReads = _diskReads c + 1 }
                  maybeIface <- File.readBytes I.dInterface (R.elmi stuff name)
                  forM_ maybeIface $ \iface ->
                    updateEntry store name $ \e -> e { _iface = Just iface }
                  return maybeIface


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
          do  entry <- lookupEntry store name
              case _objects =<< entry of
                Just objects ->
                  do  count store $ \c -> c { _objectHits = _objectHits c + 1 }
                      return (Just objects)

                Nothing ->
                  do  count store $ \c -> c { _diskReads = _diskReads c + 1 }
                      maybeObjects <- File.readBytes Opt.dLocalGraph (R.elmo stuff name)
                      forM_ maybeObjects $ \objects ->
                        updateEntry store name $ \e -> e { _objects = Just objects }
                      return maybeObjects


writeElmo :: Maybe Store -> File.Writer t -> R.Stuff -> Module.Name -> Opt.LocalGraph -> IO ()
writeElmo maybeStore writer stuff name objects =
  case maybeStore of
    Nothing ->
      File.writeBytes writer (R.elmo stuff name) Opt.eLocalGraph objects

    Just store ->
      do  updateEntry store name $ \e -> e { _objects = Just objects }
          markDirty store $ \d -> d { _dirtyObjects = Set.insert name (_dirtyObjects d) }
          count store $ \c -> c { _compiled = _compiled c + 1 }


{-| Write a changed interface. In memory it is hash-consed first, and the shared value
is returned so the rest of the build uses it too and the fresh tree can be collected.
-}
writeElmi :: Maybe Store -> File.Writer t -> R.Stuff -> Module.Name -> I.Interface -> IO I.Interface
writeElmi maybeStore writer stuff name iface =
  case maybeStore of
    Nothing ->
      do  File.writeBytes writer (R.elmi stuff name) I.eInterface iface
          return iface

    Just store ->
      do  let !shared = if shareInterfaces then I.share iface else iface
          updateEntry store name $ \e -> e { _iface = Just shared }
          markDirty store $ \d -> d { _dirtyInterfaces = Set.insert name (_dirtyInterfaces d) }
          return shared


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
      let kept f names = [ (name, a) | name <- Set.toList names, Just a <- [f =<< Map.lookup name modules] ]
          elmos = kept _objects objects
          elmis = kept _iface ifaces
      forM_ elmos $ \(name, o) -> File.writeBytes writer (R.elmo stuff name) Opt.eLocalGraph o
      forM_ elmis $ \(name, i) -> File.writeBytes writer (R.elmi stuff name) I.eInterface i
      maybeDetails <- readIORef (_details store)
      forM_ maybeDetails $ \d -> if details then writeDetails d else return ()
      return (length elmos, length elmis)
