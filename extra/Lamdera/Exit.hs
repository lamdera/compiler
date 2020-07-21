{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Exit
  ( Exit(..)
  , toReport
  )
  where


import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help

import qualified Elm.Package as Pkg



-- EXITS


data Exit
  = NeedsCompilation
  deriving (Show)



-- TO REPORT


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    NeedsCompilation ->
      Help.report "NEEDS COMPILATION" Nothing
        "Please compile first [TODO:run compilation step here...? How? What parameters?]"
        []
