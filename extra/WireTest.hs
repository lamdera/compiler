{-# LANGUAGE OverloadedStrings #-}
module WireTest where


import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal


-- COMPILE


compile :: IO ()
compile =
  Dir.withCurrentDirectory ("extra") $
    do  reporter <- Terminal.create
        Task.run reporter $
          do  summary <- Project.getRoot
              let jsOutput = Just (Output.Html Nothing tempFileName)
              Project.compile Output.Dev Output.Client jsOutput Nothing summary rootPaths

        result <- BS.readFile tempFileName
        -- seq (BS.length result) (Dir.removeFile tempFileName)
        return ()


tempFileName :: FilePath
tempFileName =
  "wire.html"


rootPaths :: [FilePath]
rootPaths =
  [ "src" </> "AllTypes_Check.elm"
  ]
