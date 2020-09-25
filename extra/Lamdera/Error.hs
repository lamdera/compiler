{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Error where

import qualified Reporting.Doc as D
import qualified Reporting.Annotation as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report


report :: D.Doc -> Report.Report
report doc =
  Report.Report "ERROR" R.zero [] $
    doc

report_ :: String -> Report.Report
report_ str =
  Report.Report "ERROR" R.zero [] $
    D.stack [ D.fromChars str ]
