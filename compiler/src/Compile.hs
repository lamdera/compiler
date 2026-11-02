{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Compile
  ( Artifacts(..)
  , compile
  )
  where


import qualified Data.Map as Map

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Typed as Tpd
import qualified AST.Optimized as Opt
import qualified AST.Prim.Mode as Mode
import qualified AST.Prim.Module as Module
import qualified Canonicalize.Module as Canonicalize
import qualified Elm.Interface as I
import qualified Nitpick.PatternMatches as PatternMatches
import qualified Optimize.Module as Optimize
import qualified Reporting.Error as E
import qualified Reporting.Render.Type as RT
import qualified Type.Check as Type

import qualified Lamdera.Wire3.Core
import qualified Lamdera.Wire3.Direct
import qualified Lamdera.Wire3.Interfaces
import Lamdera
import qualified Lamdera.UiSourceMap
import qualified Lamdera.Nitpick.DebugLog
import qualified Lamdera.Evergreen.ModifyAST
import qualified Lamdera.Perf
import qualified System.FilePath as FP


-- import StandaloneInstances



-- COMPILE


data Artifacts =
  Artifacts Tpd.Module I.Interface Opt.LocalGraph


{- The original compile function for reference. `maybePath` parameter added by Lamdera. -}
compile :: Maybe FilePath -> Mode.Mode -> Map.Map Module.Name I.Interface -> Src.Module -> IO (Either E.Errors Artifacts)
compile maybePath mode ifaces modul =
  Lamdera.alternativeImplementationWhen Lamdera.isWireEnabled_ (compile_ maybePath mode ifaces modul) $
  run $
  do  can         <- canonicalize mode ifaces modul
      (iface,tpd) <- typeCheck modul can
      ()          <- nitpick tpd
      objects     <- optimize modul iface tpd
      return (Artifacts tpd iface objects)



-- PHASES


canonicalize :: Mode.Mode -> Map.Map Module.Name I.Interface -> Src.Module -> Task Can.Module
canonicalize mode ifaces modul =
  Task $ \ok err ->
    case Canonicalize.canonicalize mode ifaces modul of
      Canonicalize.Result k ->
        k []
          (\_ a -> ok a)
          (\_ x -> err (E.BadNames x))


typeCheck :: Src.Module -> Can.Module -> Task (I.Interface, Tpd.Module)
typeCheck src can =
  Task $ \ok err ->
    do  outcome <- Type.check can
        case outcome of
          Type.Ok (iface,tpd) -> ok (iface, tpd)
          Type.BadType    xs  -> err $ E.BadTypes (RT.fromSourceModule src) xs
          Type.BadNitpick xs  -> err $ E.BadNitpick xs


nitpick :: Tpd.Module -> Task ()
nitpick tpd =
  Task $ \ok err ->
    case PatternMatches.check tpd of
      Right () -> ok ()
      Left  xs -> err $ E.BadPatterns xs


optimize :: Src.Module -> I.Interface -> Tpd.Module -> Task Opt.LocalGraph
optimize modul (I.Interface _ annotations _ _) typed =
  Task $ \ok err ->
    case Optimize.optimize annotations typed of
      Optimize.Result k ->
        k []
          (\_ opt -> ok opt)
          (\_ xs  -> err (E.BadMains (RT.fromSourceModule modul) xs))



-- TASK


newtype Task a =
  Task (forall r. (a -> IO r) -> (E.Errors -> IO r) -> IO r)


run :: Task a -> IO (Either E.Errors a)
run (Task k) =
  k (return . Right) (return . Left)


instance Functor Task where
  {-# INLINE fmap #-}
  fmap func (Task k) =
    Task $ \ok err ->
      k (ok . func) err


instance Applicative Task where
  {-# INLINE pure #-}
  pure a =
    Task $ \ok _ ->
      ok a

  {-# INLINE (<*>) #-}
  (<*>) (Task kFunc) (Task kArg) =
    Task $ \ok err ->
      let
        okF func = kArg (ok . func) err
      in
      kFunc okF err


instance Monad Task where
  {-# INLINE (>>=) #-}
  (>>=) (Task kA) callback =
    Task $ \ok err ->
      let
        okA a =
          case callback a of
            Task kB -> kB ok err
      in
      kA okA err




-- @LAMDERA


compile_ :: Maybe FilePath -> Mode.Mode -> Map.Map Module.Name I.Interface -> Src.Module -> IO (Either E.Errors Artifacts)
compile_ maybePath mode ifaces modul =
  run $
  do  let pkg = Mode.toPackage mode
      -- Inject stub definitions for wire functions, so the canonicalize phase can run
      -- Necessary for user-code which references yet-to-be generated functions
      let modul_ =
            modul
              & Lamdera.Wire3.Interfaces.modifyModul pkg ifaces
      -- @LAMDERA opt-in timing, see Lamdera.Perf
      let perf :: String -> Task a -> Task a
          perf label = perfTask (Module.toChars (Src.getName (Src._header modul)) ++ "\t" ++ label)
      canonical0 <- perf "canonicalize" $ canonicalize mode ifaces modul_

      -- Add Canonical Wire gens, i.e. the `w3_[en|de]code_TYPENAME` functions
      (canonical2, generated) <- perf "wire3_generate" $
        fromEither $
          Lamdera.Wire3.Core.addWireGenerations canonical0 pkg ifaces modul_
            -- Allow migrations to access non-exposed unsafeCoerce in lamdera/core
            & fmap (\(c, g) -> (Lamdera.Evergreen.ModifyAST.update c, g))

      (iface, tpd) <- perf "typecheck" $ typeCheck modul_ canonical2
      ()           <- perf "nitpick" $ nitpick tpd
      ()           <- perf "nitpick_debuglog" $ fromEither $ Lamdera.Nitpick.DebugLog.hasUselessDebugLogs tpd
      let
          tpd3 :: Tpd.Module
          tpd3 =
            if (Lamdera.unsafePerformIO Lamdera.isLiveMode)
              then
                case maybePath of
                  Just path ->
                    Lamdera.UiSourceMap.updateDecls path (Tpd._name tpd) (Tpd._decls tpd)
                         & (\newDecls -> tpd { Tpd._decls = newDecls })
                  Nothing ->
                    tpd
              else tpd

      objects <- perf "optimize" (optimize modul_ iface tpd3)
      -- @LAMDERA carry the Wire3 codec IR to JS generation (Lamdera.Wire3.Direct). Always,
      -- whatever LAMDERA_WIRE3_DIRECT says, so caches are the same with and without it.
      let objects_ = Lamdera.Wire3.Direct.addDirectIR pkg (Tpd._name tpd3) generated objects
      return (Artifacts tpd3 iface objects_)


fromEither :: Either E.Errors a -> Task a
fromEither result =
  Task $ \ok err ->
    case result of
      Right a -> ok a
      Left x -> err x


-- @LAMDERA Lamdera.Perf.record around a phase, decided before the next phase starts
perfTask :: String -> Task a -> Task a
perfTask label task =
  Task $ \ok err ->
    do  result <- Lamdera.Perf.record label (run task)
        case result of
          Right a -> ok a
          Left x -> err x
