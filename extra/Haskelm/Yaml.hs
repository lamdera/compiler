{-# LANGUAGE OverloadedStrings #-}
module Haskelm.Yaml
  ( generateHaskellYamlFiles
  , generatePkgYamlFiles
  )
  where

import Data.Yaml
import Data.Text
import Data.Aeson.Types as Aeson
import qualified Elm.Package as Pkg
import qualified Reporting.Task as Task
import qualified Data.Map as Map
import qualified Stuff.Paths as Paths
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified File.Crawl as Crawl
import qualified Elm.Name as N
import qualified Elm.Project.Json as ElmJson
import Elm.Project.Licenses (License(License))
import Elm.Project.Json (Project(..), AppInfo(..), PkgInfo(..))
import qualified Elm.Compiler.Module
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty
import qualified System.Directory as Dir
import qualified Elm.Compiler as Compiler
import Control.Monad.Trans (liftIO)
import System.FilePath ((</>), makeRelative, takeDirectory, pathSeparator)
import Control.Monad (filterM)
import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Text as T

type List a = [a]


generateHaskellYamlFiles
  :: FilePath
  -> Project
  -> Crawl.Graph a b
  -> Map.Map Elm.Compiler.Module.Raw Compiler.Artifacts
  -> Task.Task ()
generateHaskellYamlFiles root project graph results = do
  -- write stack.yaml
  stackFileContents <- stackYaml root graph
  liftIO $ encodeFile (root </> "stack.yaml") stackFileContents

  -- write package.yaml files
  case project of
    Project.App info -> liftIO $ do
      let appdir = root -- Paths.haskelmoRoot root
      Dir.createDirectoryIfMissing True appdir
      encodeFile (Paths.haskellAppPackageYaml appdir) (packageYamlFromAppInfo root info)

    Project.Pkg info ->
      -- NOTE: this branch is probably unused; generatePkgYamlFiles is also called through the .elm/ cache mechanism
      generatePkgYamlFiles root results info

generatePkgYamlFiles root results info =
  liftIO $ do
    mapM_ (\(name, hs) -> do
        let path = Paths.haskelmoWithoutStuff root name
        Dir.createDirectoryIfMissing True (takeDirectory path)
        writeFile path (HsPretty.prettyPrint hs)
      ) $ Map.toList $ Compiler._haskelmo <$> results

    -- generate package.yaml
    encodeFile (Paths.haskellPkgPackageYaml root) (packageYamlFromPkgInfo info)


-- HASKELM PROJECT FILE GENERATION

{-
data PkgInfo =
  PkgInfo
    { _pkg_name :: Name
    , _pkg_summary :: Text
    , _pkg_license :: Licenses.License
    , _pkg_version :: Version
    , _pkg_exposed :: Exposed
    , _pkg_deps :: Map Name Con.Constraint
    , _pkg_test_deps :: Map Name Con.Constraint
    , _pkg_elm_version :: Con.Constraint
    }
-}

data HPackYaml =
  HPackYaml
  { name :: Text
  , synopsis :: Text
  , license :: Text
  , version :: Text
  , sourceDirs :: List Text
  , exposedModules :: List Text
  , dependencies :: List Text
  }


instance ToJSON HPackYaml where
  toJSON (HPackYaml
    name
    synopsis
    license
    version
    sourceDirs
    exposedModules
    dependencies)
    = object
      [ "name" .= name
      , "synopsis" .= synopsis
      , "license" .= license
      , "version" .= version
      , "dependencies" .= array (Aeson.String <$> ("lamdera-haskelm" : dependencies))
      -- hard-coded values
      , "default-extensions" .= array (Aeson.String <$>
        [ "ConstraintKinds"
        , "TypeFamilies"
        , "TypeOperators"
        , "OverloadedStrings"
        , "OverloadedLabels"
        , "DataKinds"
        , "NoImplicitPrelude"
        , "DeriveAnyClass"
        , "NoMonomorphismRestriction"
        , "GADTs"
        , "StandaloneDeriving"
        , "FlexibleContexts" -- This prevents issues with SuperRecord when it's used kinda anonymously (i.e. passed around a lot between function calls in a single function body)
        ])
      , "ghc-options" .= Aeson.String "-Wall -Werror"
      , "library" .= object
        [ "exposed-modules" .= array (Aeson.String <$> exposedModules)
        , "source-dirs" .= array (Aeson.String <$> sourceDirs)
        ]
      ]


{-
    "type": "package",
    "name": "elm/url",
    "summary": "Create and parse URLs. Use for HTTP and \"routing\" in single-page apps (SPAs)",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Url",
        "Url.Builder",
        "Url.Parser",
        "Url.Parser.Query"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
    }
-}

packageYamlFromPkgInfo :: PkgInfo -> HPackYaml
packageYamlFromPkgInfo
  info@(PkgInfo
  package
  _summary
  (License _ code)
  _version
  _exposed
  _deps
  _test_deps
  _elm_version) =
  let
    name = Paths.cabalNameOfPackage package
    synopsis = _summary
    license = code
    version = Pkg.versionToText _version
    exposedModules = N.toText <$> ElmJson.getExposed info
    dependencies =
      fmap (\(fqname, constraint) -> Paths.cabalNameOfPackage fqname <> " " <> constraint)
      $ Map.toList
      $ convertConstraints
        <$> _deps
  in
  HPackYaml
    name
    synopsis
    license
    version
    ["src"]
    exposedModules
    dependencies


convertConstraints :: Con.Constraint -> Text
convertConstraints (Con.Range vlower op1 op2 vupper) =
  -- vlower op1 "v" op2 vupper
  -- 1.2.3  <=   v   <  1.3.1
  let
    opToStr (Con.Less) = "<"
    opToStr (Con.LessOrEqual) = "<="
    revOpToStr (Con.Less) = ">"
    revOpToStr (Con.LessOrEqual) = ">="
  in
    revOpToStr op1
      <> " "
      <> Pkg.versionToText vlower
      <> " && "
      <> opToStr op2
      <> " "
      <> Pkg.versionToText vupper





-- GENERATE PACKAGE.YAML

packageYamlFromAppInfo :: FilePath -> Project.AppInfo -> HPackYaml
packageYamlFromAppInfo
  root
  info@(Project.AppInfo
  _elm_version
  _source_dirs
  _deps_direct
  _deps_trans
  _test_direct
  _test_trans
  ) =
  let
    name = "lamdera-elm-main"
    synopsis = "lamdera elm main entrypoint"
    license = "notApplicable"
    version = "0.0.1"
    exposedModules = []
    dependencies =
      fmap (\(fqname, _) -> Paths.cabalNameOfPackage fqname)
      $ Map.toList
      $ (_deps_direct `Map.union` _deps_trans)
  in
  HPackYaml
    name
    synopsis
    license
    version
    (pack <$> (\v -> makeRelative root $ Paths.haskelmoRoot root </> v) <$> _source_dirs)
    exposedModules
    dependencies


-- data AppInfo =
--   AppInfo
--     { _app_elm_version :: Version
--     , _app_source_dirs :: [FilePath]
--     , _app_deps_direct :: Map Name Version
--     , _app_deps_trans :: Map Name Version
--     , _app_test_direct :: Map Name Version
--     , _app_test_trans :: Map Name Version
--     }



-- GENERATE STACK.YAML

stackYaml :: FilePath -> Crawl.Graph a b -> Task.Task StackYaml
stackYaml
  root
  (Crawl.Graph
  _args
  _locals
  _kernels
  _foreigns
  _problems) =
    let
      pkgs =
        _foreigns
        & Map.elems
        & removeDuplicates
        & mapM (\(Pkg.Package name version) ->
          do
            cacheDir <- Task.getPackageCacheDirFor name version
            let absPath = (cacheDir </> "haskelm")
            let relPath = makeRelative root absPath
            pure $ pack relPath
          )
      extDeps =
        let
          getSubdirs :: FilePath -> IO [FilePath]
          getSubdirs d =
            do
              contents <- Dir.listDirectory d
              let contentPaths = (d </>) <$> contents
              subDirs <- filterM (Dir.doesDirectoryExist) contentPaths
              pure subDirs
        in
        do
          cacheDir <- Task.getPackageCacheDir
          sub1 <- liftIO $ getSubdirs cacheDir
          sub2 <- liftIO $ getSubdirs `mapM` sub1
          sub3 <- liftIO $ getSubdirs `mapM` (Prelude.concat sub2)

          let dirs = (</> "haskelm") <$> Prelude.concat sub3
          d2 <-
            mapM (\p -> if "elm/core" `isInfixOf` p then
              do
                let versionString = T.takeWhile (/= pathSeparator) $ T.drop (Prelude.length cacheDir + T.length "/elm/core/") p
                homeDir <- liftIO $ Dir.getHomeDirectory
                let absPath = homeDir </> "lamdera" </> "haskelm" </> "runtime" </> T.unpack versionString
                let relPath = makeRelative root absPath
                pure $ pack relPath
              else
                pure p
            ) (pack <$> dirs)
          -- let appdir = makeRelative root $ Paths.haskelmoRoot root
          pure $ d2
    in do
      p <- pkgs
      e <- extDeps
      pure $ StackYaml p e

removeDuplicates = Prelude.foldr (\x seen -> if x `elem` seen then seen else x : seen) []

-- data Graph kernel problems =
--   Graph
--     { _args :: Args.Args Module.Raw
--     , _locals :: Map.Map Module.Raw Header.Info
--     , _kernels :: Map.Map Module.Raw kernel
--     , _foreigns :: Map.Map Module.Raw Pkg.Package
--     , _problems :: problems
--     }
--     deriving (Show)

-- ( Bitwise
-- , Package
--     { name =
--         Name
--           { author = "elm", project = "core" }
--     , version =
--         Version
--           { major = 1, minor = 0, patch = 0 }
--     }
-- )


data StackYaml =
  StackYaml
  { packages :: List Text
  , extDeps :: List Text
  }


instance ToJSON StackYaml where
  toJSON (StackYaml pkgs extDeps) =
    object
    [ "packages" .= array (Aeson.String <$> ["."])
    , "extra-deps" .=
      array
        ((Aeson.String <$> (extDeps ++ lamderaDeps)) <>
          [object
            [ "git" .= Aeson.String "https://github.com/supermario/hilt.git"
            , "commit" .= Aeson.String "f59eff3a1b4d2d897ddd1ce94c8f7e9a6b4eefea"
            ]
          ]
        )
    , "resolver" .= Aeson.String "lts-11.9"
    , "require-stack-version" .= Aeson.String ">= 1.4.0"
    --, "flags" .= array ()
    --, "extra-package-dbs" .= array ()
    ]

lamderaDeps =
  [ "acid-state-0.14.3"
  , "regexpr-0.5.4"
  , "superrecord-0.5.0.1"
  , "unagi-chan-0.4.1.0"
  , "mtlparse-0.1.4.0"
  ]

