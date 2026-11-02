{-# LANGUAGE OverloadedStrings #-}

module Ext.Query.Canonical where

{- Helpers for loading cached interfaces after/outside compilation process -}

-- import Control.Monad (liftM2)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.ByteString as BS
import Data.Word (Word32)
import qualified System.Directory as Dir

-- elm/compiler
import qualified AST.Prim.Name as N
import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as Module
import qualified Elm.Package as Pkg
import qualified Parse.Module as Parse
import qualified AST.Prim.Mode as Mode
import qualified AST.Typed as Tpd
import qualified Elm.Interface as I
import qualified Graph
import Reporting.Annotation
import qualified Reporting.Render.Code as Code
import qualified Reporting.Annotation as A
import qualified Compile
import qualified File

-- elmx
import qualified Ext.Query.Interfaces

import Lamdera (hindentPrintLabelled, last_)
import Ext.Common

-- @TODO
-- Lamdera.Canonical.loadDef "src/Test/Basic.elm" "exampleFunction"


-- @LAMDERA Elm 0.19.4: the type checked module (AST.Typed), which holds the canonical decls
loadSingleCanonical :: FilePath -> IO Tpd.Module
loadSingleCanonical path = do
  (Compile.Artifacts canonical iface objects) <- loadSingleArtifacts path
  pure canonical


loadSingleObjects :: FilePath -> IO Opt.LocalGraph
loadSingleObjects path = do
  (Compile.Artifacts canonical iface objects) <- loadSingleArtifacts path
  pure objects


loadSingleArtifacts :: FilePath -> IO Compile.Artifacts
loadSingleArtifacts path = do
  ifaces <- Ext.Query.Interfaces.all [path]
  source <- File.readUtf8 path
  result <- Parse.fromByteString Mode.Application source
  case result of
    Right modul -> do
      compiled <- Compile.compile Nothing Mode.Application ifaces modul
      case compiled of
        Right artifacts ->
          pure artifacts

        Left err -> error $ "error!" ++ show err

    Left err ->
      error "bad syntax"


loadFileSource :: FilePath -> IO (BS.ByteString, Src.Module)
loadFileSource path = do
  project <- getProjectRootFor path

  Ext.Common.withProjectRoot project $ do
    source <- File.readUtf8 path
    result <- Parse.fromByteString Mode.Application source
    case result of
      Right modul -> do
        -- hindentPrintLabelled "module source" modul
        pure $ (source, modul)

      Left err ->
        error "bad syntax"


loadFileSourceValue :: FilePath -> N.Name -> IO ()
loadFileSourceValue path name = do
  (source, modul) <- loadFileSource path

  let
    -- imports = modul & Src._imports

    -- @LAMDERA Elm 0.19.4: the values are among the module's Decls
    value =
      (case Src._decls modul of
          Src.NoDecls _ -> []
          Src.RevDecls d ds -> reverse (d : ds)
      )
        & concatMap (\d -> case d of { Src.Value_ _ v -> [v]; _ -> [] })
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

  -- hindentPrintLabelled "1. module imports" imports
  -- hindentPrintLabelled "2. value location" valueLocation

  putStrLn "3. print found definition:\n"

  hindentPrintLabelled "4. value" value
  hindentPrintLabelled "4. docs" (modul & Src._docs)

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
render :: Code.Source -> Region -> Maybe Region -> [(Word32, String)]
render (Code.Source sourceLines) region@(Region startCur endCur) maybeSubRegion =
  let
    startLine = A.toRow startCur
    endLine = A.toRow endCur
    relevantLines =
      sourceLines
        & drop (fromIntegral (startLine - 1))
        & take (fromIntegral (1 + endLine - startLine))

    -- width =
    --   length (show (fst (last_ relevantLines (0, ""))))

    -- smallerRegion =
    --   maybe region id maybeSubRegion
  in
  relevantLines



showDefOptimized :: FilePath -> FilePath -> N.Name -> IO (Map.Map Opt.Global Opt.Node)
showDefOptimized project file name = do
  Ext.Common.withProjectRoot project $ do
    -- canonical <- loadSingleCanonical file
    objects <- loadSingleObjects file

    let
      def = objects
        & Opt._l_nodes
        -- & Map.lookup name
        & Map.filterWithKey (\k _ ->
            case k of
              Opt.Global (Module.Canonical (Pkg.Name _ _) _) name_ ->
                name_ == name
          )

    def & show & putStrLn
    pure def


showDefCanonical :: FilePath -> FilePath -> N.Name -> IO ()
showDefCanonical project file name = do
  Ext.Common.withProjectRoot project $ do
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
    case canonical & Tpd._decls & findDef name of
      Just def -> do
        -- formatHaskellValue "suite def" def
        putStrLn $ "suite def" ++ show def
        -- formatHaskellValue "suite:" $ run def Map.empty

      Nothing ->
        putStrLn "no suite found"

    pure ()


findDef :: N.Name -> Can.Decls -> Maybe Can.Def
findDef name decls =
  decls
    & declsToList
    & List.find (defNameIs name)


declsToList :: Can.Decls -> [Can.Def]
declsToList (Can.Decls (Graph.Sorted decls)) =
  -- @LAMDERA Elm 0.19.4 Decls are a sorted list of Decl
  decls & concatMap (\decl ->
    case decl of
      Can.Decl def -> [def]
      Can.DeclRec def defs -> def : defs
  )


defNameIs :: N.Name -> Can.Def -> Bool
defNameIs name def =
  name == defName def


defName :: Can.Def -> N.Name
defName def =
  case def of
    Can.DValue_ (A.At region name_) _ -> name_
    Can.DValue (A.At region name_) _ _ _ -> name_
    Can.DFunc_ (A.At region name_) _ _ _ -> name_
    Can.DFunc (A.At region name_) _ _ _ _ _ -> name_


showDefAnnotation :: FilePath -> FilePath -> N.Name -> IO ()
showDefAnnotation project file name = do
  Ext.Common.withProjectRoot project $ do
    (Compile.Artifacts canonical iface objects) <- loadSingleArtifacts file

    -- , _types :: Map.Map Name.Name Can.Annotation

    I._values iface
      & Map.lookup name
      & show
      & putStrLn
      -- & formatHaskellValue ("found")

    pure ()
