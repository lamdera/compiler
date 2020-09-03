{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Error
  ( Error(..)
  , toReport
  )
  where


import qualified Reporting.Doc as D
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report


-- ERROR


data Error
  = LamderaGenericError D.Doc
  deriving (Show)



-- TO REPORT

toReport :: Error -> Report.Report
toReport err =
  case err of
    LamderaGenericError doc ->
      Report.Report "ERROR" R.zero [] $
        doc
