module Lamdera.Progress where

import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import System.IO (hFlush, hPutStr, stdout)
import System.Exit (exitFailure)


flushPrintHelp :: D.Doc -> Reporting.Key ()
flushPrintHelp doc =
  Reporting.Key (\_ ->
    do  Help.toStdout doc
        hFlush stdout
  )


report :: D.Doc -> IO ()
report doc = do
  Reporting.report (flushPrintHelp doc) ()


throw :: Help.Report -> IO a
throw rep = throwDoc $ Help.reportToDoc rep


throwDoc :: D.Doc -> IO a
throwDoc doc = do
  Help.toStdout doc
  hFlush stdout
  exitFailure
