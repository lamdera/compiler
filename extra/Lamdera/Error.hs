{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Error where

import qualified Reporting.Doc as D
import qualified Reporting.Annotation as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report
import qualified Stuff
import System.IO.Unsafe (unsafePerformIO)


report :: String -> D.Doc -> Report.Report
report title doc =
  Report.Report title R.zero [] doc


report_ :: String -> Report.Report
report_ str =
  Report.Report "ERROR" R.zero [] $
    D.stack [ D.fromChars str ]


reportFull :: String -> D.Doc -> Report.Report
reportFull title doc =
  Report.Report title R.zero [] doc


corruptCaches :: ([Char] -> Maybe FilePath -> D.Doc -> [D.Doc] -> report) -> report
corruptCaches reportFn =
  let
    elmHome = unsafePerformIO Stuff.getElmHome
  in
  reportFn "CORRUPT CACHE" Nothing
    "It looks like some of the information in caches has been corrupted."
    [ D.reflow $
        "Try deleting your elm-stuff/ and ELM_HOME (" <> elmHome <> ") directories to get unstuck."
    , D.toSimpleNote $
        "This likely means a Lamdera project was built with the Elm compiler originally\
        \ and is causing problems with the elm-stuff/ and ELM_HOME directories. \
        \ Try deleting these and trying again, and ask in Discord if issues persist."
    ]


corruptCachesOffline :: ([Char] -> Maybe FilePath -> D.Doc -> [D.Doc] -> report) -> report
corruptCachesOffline reportFn =
  let
    elmHome = unsafePerformIO Stuff.getElmHome
  in
  reportFn "CORRUPT CACHE" Nothing
    "It looks like some of the information in caches has been corrupted."
    [ D.reflow $
        "Try deleting your elm-stuff/ and ELM_HOME (" <> elmHome <> ") directories to get unstuck, though you will need an internet connection to do so."
    , D.toSimpleNote $
        "This likely means a Lamdera project was built with the Elm compiler originally\
        \ and is causing problems with the elm-stuff/ and ELM_HOME directories. \
        \ Try deleting these and trying again, and ask in Discord if issues persist."
    ]


reportToDoc :: FilePath -> Report.Report -> D.Doc -> D.Doc
reportToDoc relativePath (Report.Report title region _ message) original =
  if title == "WIRE ISSUES" then
    D.vcat
      [ toMessageBarNoPath title
      , ""
      , message
      , ""
      ]
  else
    original


toMessageBarNoPath :: String -> D.Doc
toMessageBarNoPath title =
  let
    usedSpace =
      4 + length title
  in
    D.dullcyan $ D.fromChars $
      "-- " ++ title
      ++ " " ++ replicate (max 1 (80 - usedSpace)) '-'
