{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Canonical where

{- Helpers for loading cached interfaces after/outside compilation process -}

import Control.Monad (liftM2)
import qualified Data.Map as Map
import qualified Data.OneOrMore as OneOrMore
import System.FilePath ((</>))
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.Name
import qualified Data.List as List
import qualified Data.ByteString as BS
import Data.Word (Word16)

import qualified AST.Optimized as Opt
import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified BackgroundWriter as BW
import qualified Build
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import qualified Elm.ModuleName as Module
import Reporting.Annotation

import qualified Elm.Package as Pkg
import qualified Compile
import qualified Parse.Module as Parse

import qualified Lamdera.Interfaces
import qualified Lamdera.Progress as Progress
import qualified Reporting.Render.Code as Code
import Lamdera

import Lamdera.Wire3.Helpers (findDef)

-- @TODO
-- Lamdera.Canonical.loadDef "src/Test/Basic.elm" "exampleFunction"


loadSingleCanonical :: FilePath -> IO Can.Module
loadSingleCanonical path = do
  (Compile.Artifacts canonical annotations objects) <- loadSingleArtifacts path
  pure canonical


loadSingleObjects :: FilePath -> IO Opt.LocalGraph
loadSingleObjects path = do
  (Compile.Artifacts canonical annotations objects) <- loadSingleArtifacts path
  pure objects


loadSingleArtifacts :: FilePath -> IO Compile.Artifacts
loadSingleArtifacts path = do
  ifaces <- Lamdera.Interfaces.all [path]
  source <- File.readUtf8 path
  case Parse.fromByteString Parse.Application source of
    Right modul ->
      case Compile.compile Pkg.dummyName ifaces modul of
        Right artifacts ->
          pure artifacts

        Left err -> error $ "error!" ++ show err

    Left err ->
      error "bad syntax"


loadFileSource :: FilePath -> IO (BS.ByteString, Src.Module)
loadFileSource path = do
  project <- getProjectRootFor path

  withCurrentDirectory project $ do
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right modul -> do
        -- hindentPrintValue "module source" modul
        pure $ (source, modul)

      Left err ->
        error "bad syntax"


loadFileSourceValue :: FilePath -> Data.Name.Name -> IO ()
loadFileSourceValue path name = do
  (source, modul) <- loadFileSource path

  let
    imports = modul & Src._imports
    valueLocation =
      modul
        & Src._values
        & List.find (\(At region (Src.Value (At _ name_) params expr typeM)) ->
          name == name_
        )
        & (\v ->
          case v of
            Just (At region (Src.Value (At _ name_) params expr typeM)) -> region
            _ ->
              error $ "Unexpected result in loadFileSourceValue" <> show v
        )

    code = Code.toSource source

  hindentPrintValue "1. module imports" imports
  hindentPrintValue "2. value location" valueLocation

  putStrLn "3. print found definition:\n"

  render code valueLocation Nothing
    & mapM_ (putStrLn . snd)

  {-

    We could also use Elm's style, it works like this

    let doc = Code.render code valueLocation Nothing
    Progress.report doc

    and looks like this:

    61|>suite =
    62|>    test "addOne adds one" <|
    63|>        equals (addOne 123) 124

    Note that extra param is for sepcifying a Region to hilight in Elm's ^^^ way.

  -}


-- Stripped down version of Reporting.Render.Code.render
render :: Code.Source -> Region -> Maybe Region -> [(Word16, String)]
render (Code.Source sourceLines) region@(Region (Position startLine _) (Position endLine _)) maybeSubRegion =
  let
    relevantLines =
      sourceLines
        & drop (fromIntegral (startLine - 1))
        & take (fromIntegral (1 + endLine - startLine))

    width =
      length (show (fst (last relevantLines)))

    smallerRegion =
      maybe region id maybeSubRegion
  in
  relevantLines



showDefOptimized :: FilePath -> FilePath -> Data.Name.Name -> IO ()
showDefOptimized project file name = do
  withCurrentDirectory project $ do

    -- canonical <- Lamdera.Canonical.loadSingleCanonical file

    objects <- Lamdera.Canonical.loadSingleObjects file

        -- objects
        --   & _l_nodes
        --   &

    objects
      & Opt._l_nodes
      -- & Map.lookup name
      & Map.filterWithKey (\k _ ->
          case k of
            Opt.Global (Module.Canonical (Pkg.Name _ _) _) name_ ->
              name_ == name
        )
      & formatHaskellValue ("found")


showDefCanonical :: FilePath -> FilePath -> Data.Name.Name -> IO ()
showDefCanonical project file name = do
  withCurrentDirectory project $ do
    canonical <- Lamdera.Canonical.loadSingleCanonical file
    -- objects <- Lamdera.Canonical.loadSingleObjects file

    -- objects
    --   & Opt._l_nodes
    --   -- & Map.lookup name
    --   -- & Map.filterWithKey (\k _ ->
    --   --     case k of
    --   --       Opt.Global (Module.Canonical (Pkg.Name _ _) _) name_ ->
    --   --         name_ == name
    --   --   )
    --   & formatHaskellValue (T.pack $ file <> Data.Name.toChars name)


    -- formatHaskellValue "canonical Test.Basic" canonical
    --
    case canonical & Can._decls & findDef name of
      Just def -> do
        formatHaskellValue "suite def" def
        -- formatHaskellValue "suite:" $ run def Map.empty

      Nothing ->
        putStrLn "no suite found"

    pure ()


showDefAnnotation :: FilePath -> FilePath -> Data.Name.Name -> IO ()
showDefAnnotation project file name = do
  withCurrentDirectory project $ do
    (Compile.Artifacts canonical annotations objects) <- loadSingleArtifacts file

    -- , _types :: Map.Map Name.Name Can.Annotation

    annotations
      & Map.lookup name
      & formatHaskellValue ("found")

    pure ()
