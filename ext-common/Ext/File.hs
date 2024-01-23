module Ext.File where

import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import qualified Data.Binary as Binary
import qualified System.IO.Temp as Temp
import qualified System.Directory as Dir
import qualified System.FilePath as FP

{-|
Clone of builder/src/File.hs:writeBinary

Uses `System.IO.Temp` to write the file first, and then moves it to the
desired location. This is in an effort to both be strict in the write and
to avoid corrupting the file if the process is killed in the middle of
writing, or to minimise the concurrency contention window with other processes

Note that because Elm itself uses the original potentially non-strict behaviour,
there is no way to 100% guarantee that we won't conflict with its files, hence
why we additionally namespace the caches we use.

So this function replacement itself is more an insurance strategy against
multiple Lamdera processes conflicting.
-}
writeBinaryValueStrict :: (Binary.Binary a) => FilePath -> a -> IO ()
writeBinaryValueStrict path value = do
  let dir = FP.dropFileName path
  Dir.createDirectoryIfMissing True dir
  tmpFile <- Temp.emptyTempFile dir "tmp"
  Binary.encodeFile tmpFile value
  Dir.renameFile tmpFile path
