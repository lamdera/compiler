{-# LANGUAGE OverloadedStrings #-}
module Wire.Test where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal

import qualified Check

import System.Process (callCommand)
import System.Environment (setEnv)

{-

This is a modified clone of ui/terminal/src/Develop/StaticFiles/Build.hs
specifically to help with the development cycle for Evergreen.

Changes are:
- Generates `Output.Html` instead of `Output.Javascript` (so we can just refresh browser after run)
- Uses `Output.Dev` instead of `Output.Prod` to avoid errors associated with Debug usage in AllTypes_Check


Here's a suggested development flow to use this:

1. Put the following into ~/.ghci

:set -fbyte-code
:set -fobject-code
:set prompt "\ESC[34mλ: \ESC[m"

Last line is optional, but it's cool! Lambda prompt!

2. Run `stack ghci`

3. Then a feedback loop goes as follows;

  - Make changes to Haskell Wire code
  - Run `:r` to typecheck + recompile & fix any issues
  - Run `WireTest.compile`
    - Executes shell `touch` on `extra/src/AllTypes.elm` to bust Elm compiler's cache
    - Compiles `extra/src/AllTypes_Check.elm`
    - Generates `extra/src/wire.html`
  - Refresh `wire.html` in your browser – you should see big green boxes
    - If something is red, you broke encoders/decoders!

-}


-- COMPILE


compile :: IO ()
compile = do
  -- Bust Elm's caching with this one weird trick!
  -- touch "extra/Wire/src/AllTypes.elm"
  -- touch "extra/Wire/src/Types.elm"
  touch "/Users/mario/dev/projects/lamdera/test/v1/src/Types.elm"
  -- touch "/Users/mario/dev/projects/lamdera/test/v1/src/LamderaFrontendRuntime.elm"

  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "ELM_HOME" "/Users/mario/dev/projects/lamdera/test/v1/elm-home"

  -- =${LOVR} ELM_HOME=$BUILD_DIR/cache/elm-home elmx make src/LamderaBackendRuntime.elm --output="backend-app.js"

  let rootPaths = [ "src" </> "Both.elm" ]

  -- Dir.withCurrentDirectory ("extra/Wire") $
  Dir.withCurrentDirectory ("/Users/mario/dev/projects/lamdera/test/v1") $
    do  reporter <- Terminal.create
        Task.run reporter $
          do  summary <- Project.getRoot
              let jsOutput = Just (Output.Html Nothing tempFileName)
              Project.compile Output.Dev Output.Client jsOutput Nothing summary rootPaths

        _ <- BS.readFile tempFileName
        -- seq (BS.length result) (Dir.removeFile tempFileName)
        return ()


tempFileName :: FilePath
tempFileName =
  "/dev/null"


-- rootPaths :: [FilePath]
-- rootPaths =
--   -- [ "src" </> "AllTypes_Check.elm"
--   [ "src" </> "Backend.elm"
--   ]


touch :: String -> IO ()
touch path = callCommand $ "touch " ++ path



-- CHECK

check = do

  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "ELM_HOME" "/Users/mario/dev/projects/lamdera/test/v1/elm-home"


  Dir.withCurrentDirectory ("/Users/mario/dev/projects/lamdera/test/v1") $
    do
        Check.run () ()