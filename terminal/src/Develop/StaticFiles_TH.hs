{-# LANGUAGE OverloadedStrings, TemplateHaskellQuotes #-}
module Develop.StaticFiles_TH
  ( buildReactorFrontEnd
  )
  where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.NonEmptyList as NE
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..))
import Language.Haskell.TH.Syntax (Q, Exp(..), Lit(..), Bytes(Bytes), runIO)
import System.FilePath ((</>))

import qualified BackgroundWriter as BW
import qualified Build
import qualified Elm.Details as Details
import qualified Generate
import qualified Lamdera.Relative
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task



-- BUILD REACTOR ELM


buildReactorFrontEnd :: Q Exp -- BS.ByteString
buildReactorFrontEnd =
  fmap bsToExp $ runIO $
  BW.withScope $ \scope ->
  -- @LAMDERA Resolve an absolute root via Lamdera.Relative rather than chdir'ing the
  -- process-global CWD. This TH runs under GHC's parallel upsweep (-j), where a
  -- withCurrentDirectory here races other TH splices (Lamdera.Repl) and fails
  -- intermittently ("changeWorkingDirectory: does not exist"). Passing absolute paths
  -- keeps Build.getRootInfo's canonicalizePath CWD-independent.
  do  root <- Lamdera.Relative.requireDir "reactor"
      runTaskUnsafe $
        do  details    <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
            artifacts  <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details (fmap (root </>) paths)
            javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.prod root details artifacts
            return (LBS.toStrict (B.toLazyByteString javascript))


paths :: NE.List FilePath
paths =
  NE.List
    ("src" </> "NotFound.elm")
    [ "src" </> "Errors.elm"
    , "src" </> "Index.elm"
    ]


runTaskUnsafe :: Task.Task Exit.Reactor a -> IO a
runTaskUnsafe task =
  do  result <- Task.run task
      case result of
        Right a ->
          return a

        Left exit ->
          do  Exit.toStderr (Exit.reactorToReport exit)
              error
                "\n--------------------------------------------------------\
                \\nError in Develop.StaticFiles.Build.buildReactorFrontEnd\
                \\nCompile with `elm make` directly to figure it out faster\
                \\n--------------------------------------------------------\
                \\n"


bsToExp :: BS.ByteString -> Exp
bsToExp (BS.BS fptr len) =
  ConE 'BS.BS
    `AppE` (ConE 'ForeignPtr `AppE` LitE (BytesPrimL bytes) `AppE` ConE 'FinalPtr)
    `AppE` LitE (IntegerL (fromIntegral len))
  where
    bytes = Bytes fptr 0 (fromIntegral len)


