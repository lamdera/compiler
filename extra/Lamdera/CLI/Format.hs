module Lamdera.CLI.Format
  ( run
  ) where

import qualified ElmFormat.Cli

-- | Delegate to elm-format
run :: [String] -> IO ()
run args = ElmFormat.Cli.mainIO args