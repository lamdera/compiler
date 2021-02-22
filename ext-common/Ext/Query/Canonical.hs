{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Ext.Query.Canonical where

{- Helpers for loading cached interfaces after/outside compilation process -}

-- import Control.Monad (liftM2)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.ByteString as BS
import Data.Word (Word16)
import qualified System.Directory as Dir

-- elm/compiler
import qualified Data.Name
import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as Module
import qualified Elm.Package as Pkg
import qualified Parse.Module as Parse
import Reporting.Annotation
import qualified Reporting.Render.Code as Code
import qualified Reporting.Annotation as A
import qualified Compile
import qualified File

-- elmx
import qualified Ext.Query.Interfaces

import Lamdera (hindentPrintValue)
import Ext.Common

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
  ifaces <- Ext.Query.Interfaces.all [path]
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

  Dir.withCurrentDirectory project $ do
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

    value =
      modul
        & Src._values
        & List.find (\(At region (Src.Value (At _ name_) params expr typeM)) ->
          name == name_
        )

    valueLocation =
      value
        & (\v ->
          case v of
            Just (At region (Src.Value (At _ name_) params expr typeM)) -> region
            _ ->
              error $ "Unexpected result in loadFileSourceValue" <> show v
        )

    code = Code.toSource source

  -- hindentPrintValue "1. module imports" imports
  -- hindentPrintValue "2. value location" valueLocation

  putStrLn "3. print found definition:\n"

  hindentPrintValue "4. value" value
  hindentPrintValue "4. docs" (modul & Src._docs)

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
  Dir.withCurrentDirectory project $ do

    -- canonical <- loadSingleCanonical file

    objects <- loadSingleObjects file

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
      & show
      & putStrLn
      -- & formatHaskellValue ("found")


showDefCanonical :: FilePath -> FilePath -> Data.Name.Name -> IO ()
showDefCanonical project file name = do
  Dir.withCurrentDirectory project $ do
    canonical <- loadSingleCanonical file
    -- objects <- loadSingleObjects file

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
        -- formatHaskellValue "suite def" def
        putStrLn $ "suite def" ++ show def
        -- formatHaskellValue "suite:" $ run def Map.empty

      Nothing ->
        putStrLn "no suite found"

    pure ()


findDef :: Data.Name.Name -> Can.Decls -> Maybe Can.Def
findDef name decls =
  decls
    & declsToList
    & List.find (defNameIs name)


declsToList :: Can.Decls -> [Can.Def]
declsToList d =
  case d of
    Can.Declare def decls ->
      def : (declsToList decls)

    Can.DeclareRec def defs decls ->
      (def : defs) ++ declsToList decls

    Can.SaveTheEnvironment ->
      []


defNameIs :: Data.Name.Name -> Can.Def -> Bool
defNameIs name def =
  name == defName def


defName :: Can.Def -> Data.Name.Name
defName def =
  case def of
    Can.Def (A.At region name_) _ _ ->
      name_
    Can.TypedDef (A.At region name_) _ _ _ _ ->
      name_


showDefAnnotation :: FilePath -> FilePath -> Data.Name.Name -> IO ()
showDefAnnotation project file name = do
  Dir.withCurrentDirectory project $ do
    (Compile.Artifacts canonical annotations objects) <- loadSingleArtifacts file

    -- , _types :: Map.Map Name.Name Can.Annotation

    annotations
      & Map.lookup name
      & show
      & putStrLn
      -- & formatHaskellValue ("found")

    pure ()
