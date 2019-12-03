module Elm
  ( debugTrace
  , debug_
  , debug
  , PP.sShow
  , PP.tShow
  , T.Text
  , (<>)
  , mconcat
  , (&)
  , first
  , second
  , List
  , ppElm
  , Data.Witherable.mapMaybe
  , Data.Witherable.catMaybes
  )
  where

-- A prelude-like thing that contains the commonly used things in elm.
-- Names differ, but semantics are similar.

-- import Prelude (Maybe(..), String, IO)

import qualified Debug.Trace as DT
import qualified Wire.PrettyPrint as PP
import qualified Data.Text as T
import Data.Monoid ((<>), mconcat)
import Data.Function ((&))
import Control.Arrow (first, second)
import qualified System.Environment as Env
import Control.Monad.Except (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import qualified Debug.Trace as DT

import CanSer.CanSer (ppElm)
import qualified Data.Witherable

-- import qualified Reporting.Task as Task

type List a = [a]


-- debug :: String -> Task.Task ()
debug str =
  liftIO $ debug_ str


debug_ :: String -> IO ()
debug_ str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> putStrLn $ "DEBUG: " ++ str
    Nothing -> pure ()


debugTrace :: String -> a -> a
debugTrace msg value =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> pure $ DT.trace msg value
      Nothing -> pure value
