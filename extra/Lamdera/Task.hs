module Lamdera.Task where

import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import qualified Control.Monad.Reader as R
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http

import qualified Stuff as PerUserCache
import qualified Reporting as Progress
import qualified Reporting.Exit as Exit
import Reporting.Task


-- @LAMDERA TEMPORARY Used by Lamdera.Http – revisit when we get to it

-- tryEither :: Progress.Reporter -> Task a -> IO (Either Exit.Exit a)
-- tryEither (Progress.Reporter tell ask end) task =
--   do  root <- PerUserCache.getPackageRoot
--       pool <- initPool 4 -- number of threads when compiling
--       httpManager <- Http.newManager Http.tlsManagerSettings
--       let env = Env root pool httpManager tell ask
--       result <- R.runReaderT (runExceptT task) env
--       case result of
--         Left err ->
--           do  end (Just err)
--               pure result
--
--         Right answer ->
--           do  end Nothing
--               pure result
