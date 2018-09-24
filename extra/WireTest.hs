{-# LANGUAGE OverloadedStrings #-}
module WireTest where


import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal


{-

This is a modified clone of ui/terminal/src/Develop/StaticFiles/Build.hs
specifically to help with the development cycle for Evergreen.

Changes are:
- Generates `Output.Html` instead of `Output.Javascript` (so we can just refresh browser after run)
- Uses `Output.Dev` instead of `Output.Prod` to avoid errors associated with Debug usage in AllTypes_Check


Here's how to use it:

1. Put the following into ~/.ghci

:set -fbyte-code
:set -fobject-code
:set prompt "\ESC[34mλ: \ESC[m"

Last line is optional, but it's cool! Lambda prompt!

2. Run `stack ghci`

3. λ: WireTest.compile
   Success!

4. If you've made Haskell code changes, use `:r` to reload:

   λ: :r
   [169 of 169] Compiling WireTest         ( /Users/mario/dev/projects/elm-compiler/extra/WireTest.hs, /Users/mario/dev/projects/elm-compiler/.stack-work/odir/WireTest.o )
   Ok, 169 modules loaded.

-}


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
