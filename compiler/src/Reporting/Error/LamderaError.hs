{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.LamderaError
  ( Error(..)
  , toReport
  )
  where


-- import qualified AST.Canonical as Can
-- import qualified Elm.Name as N
import qualified Reporting.Doc as D
-- import qualified Reporting.Error.Canonicalize as E
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code
-- import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report

-- import qualified Reporting.Exit.Help as Help

-- import Data.Text (Text)

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
