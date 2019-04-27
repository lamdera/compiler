{-# LANGUAGE OverloadedStrings #-}
module Haskelm.Yaml
  ( generateHaskellYamlFiles
  , generatePkgYamlFiles
  )
  where

import Data.Yaml
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import qualified System.Directory as Dir
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
import Elm.Project.Json (Project(..), PkgInfo(..))
import qualified Elm.Compiler.Module
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty
import qualified System.Directory as Dir
import qualified Elm.Compiler as Compiler
import Control.Monad.Trans (liftIO)
import System.FilePath ((</>), makeRelative, takeDirectory)
import Control.Monad (filterM)
import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)

import System.IO.Unsafe (unsafePerformIO)
import qualified Debug.Trace as DT
import Transpile.PrettyPrint

type List a = [a]

encodeYaml :: ToJSON yaml => FilePath -> yaml -> IO ()
encodeYaml path y = do
  writeIfChanged path (Yaml.encode y)

writeIfChanged :: FilePath -> BS.ByteString -> IO ()
writeIfChanged path new = do
  exists <- Dir.doesFileExist path
  if exists then do
      current <- BS.readFile path
      if current == new then
          --putStrLn ("contents equal " <> path)
          pure ()
        else do
          --putStrLn ("not equal " <> path)
          BS.writeFile path new
    else do
      --putStrLn ("not exists " <> path)
      BS.writeFile path new


generateHaskellYamlFiles
  :: FilePath
  -> Project
  -> Crawl.Graph a b
  -> Map.Map Elm.Compiler.Module.Raw Compiler.Artifacts
  -> Task.Task ()
generateHaskellYamlFiles root project graph results = do
  case project of
    Project.App info -> do
      -- write stack.yaml
      stackFileContents <- stackYaml info root graph []
      liftIO $ encodeYaml (root </> "stack.yaml") stackFileContents
      -- harness-stack.yaml
      harnessStackFileContents <- stackYaml info root graph ["../backend"]
      liftIO $ encodeYaml (root </> "harness-stack.yaml") harnessStackFileContents

      -- package.yaml
      let appdir = root -- Paths.haskelmoRoot root
      liftIO $ Dir.createDirectoryIfMissing True appdir
      liftIO $ encodeYaml (Paths.haskellAppPackageYaml appdir) (packageYamlFromAppInfo root info)

    Project.Pkg info ->
      -- NOTE: this branch is probably unused; generatePkgYamlFiles is also called through the .elm/ cache mechanism
      generatePkgYamlFiles root results info

generatePkgYamlFiles
  :: MonadIO m
  => FilePath
  -> Map.Map Elm.Compiler.Module.Raw Compiler.Artifacts
  -> PkgInfo
  -> m ()
generatePkgYamlFiles root results info =
  liftIO $ do
    mapM_ (\(name, hs) -> do
        let path = Paths.haskelmoWithoutStuff root name
        Dir.createDirectoryIfMissing True (takeDirectory path)
        writeIfChanged path (BS8.fromString (HsPretty.prettyPrint hs))
      ) $ Map.toList $ Compiler._haskelmo <$> results

    -- generate package.yaml
    encodeYaml (Paths.haskellPkgPackageYaml root) (packageYamlFromPkgInfo info)


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
      , "dependencies" .= array (Aeson.String <$> dependencies)
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
      , "ghc-options" .= Aeson.String "-Wall -Werror -Wno-unused-imports -Wno-unused-matches -Wno-unused-local-binds -Wno-type-defaults -Wno-name-shadowing -Wno-missing-signatures -Wno-unused-top-binds -fprof-auto -fprof-cafs"
      , "library" .= object
        [ "exposed-modules" .= array (Aeson.String <$> exposedModules)
        , "source-dirs" .= array (Aeson.String <$> sourceDirs)
        ]
      , "verbatim" .= object [ "cabal-version" .= (Aeson.String "2.0")]
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
      haskelm_deps ++
      (fmap (\(fqname, constraint) -> Paths.cabalNameOfPackage fqname <> " " <> constraint)
      $ Map.toList
      $ convertConstraints
        <$> _deps)
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
  ( Project.AppInfo
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
    license = "NONE"
    version = "0.0.1"
    exposedModules = ["Backend","Msg"]
    dependencies =
      haskelm_deps ++
      (fmap (\(fqname, version) -> Paths.cabalNameOfPackage fqname <> " == " <> Pkg.versionToText version)
      $ Map.toList
      $ (_deps_direct `Map.union` _deps_trans))
  in
  --DT.trace (sShow ("direct", _deps_direct, "trans", _deps_trans, "deps", dependencies)) $
  HPackYaml
    name
    synopsis
    license
    version
    (pack <$> (\v -> makeRelative root $ Paths.haskelmoRoot root </> v) <$> _source_dirs)
    exposedModules
    dependencies

haskelm_deps = ["lamdera-haskelm-runtime", "base >=4.7 && <5"]

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

stackYaml :: Project.AppInfo -> FilePath -> Crawl.Graph a b -> List Text -> Task.Task StackYaml
stackYaml
  (Project.AppInfo _ _ dirDeps transDeps _ _)
  root
  (Crawl.Graph
  _args
  _locals
  _kernels
  _foreigns
  _problems)
  extraDeps =
    do
      pkgs <-
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
      extDeps <-
            (\(pkgName, vsn) ->
              do
                dir <- Task.getPackageCacheDirFor pkgName vsn
                pure $ (matchElmPkg
                     (\fqPkgName -> pack $ ".." </> "backend-runtime" </> "stdlib" </> T.unpack fqPkgName)) (T.pack dir)
            ) `mapM` (Map.toList (dirDeps `Map.union` transDeps))
      pure $ StackYaml pkgs (extDeps ++ extraDeps)

removeDuplicates = Prelude.foldr (\x seen -> if x `elem` seen then seen else x : seen) []


matchElmPkg :: (Text -> Text) -> Text -> Text
matchElmPkg onMatch pkgPath =
  do
    let
        withoutPrefix =
          pkgPath
          & T.splitOn "/package/"
          & Prelude.drop 1
          & T.intercalate "/package/"
          & T.replace "/haskelm" ""
          & onMatch
    if unsafePerformIO $ Dir.doesDirectoryExist (T.unpack withoutPrefix) then
        DT.trace ("found stdlib at " <> T.unpack withoutPrefix) $
        withoutPrefix
      else
        T.pack $ T.unpack pkgPath </> "haskelm"


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
  toJSON (StackYaml _ extDeps) =
    object
    [ "packages" .= array (Aeson.String <$> ["."])
    , "extra-deps" .=
      array
        ((Aeson.String <$> (extDeps ++ lamderaDeps)) <>
          [object
            [ "git" .= Aeson.String "https://github.com/supermario/hilt.git"
            , "commit" .= Aeson.String "f59eff3a1b4d2d897ddd1ce94c8f7e9a6b4eefea"
            ]
          , Aeson.String (T.pack $ ".." </> "backend-runtime" </> "shared")
          , Aeson.String (T.pack $ ".." </> "backend-runtime" </> "haskelm" </> "runtime")
          ]
        )
    , "resolver" .= Aeson.String "lts-13.1"
    , "require-stack-version" .= Aeson.String ">= 1.4.0"
    --, "flags" .= array ()
    --, "extra-package-dbs" .= array ()
    ]

lamderaDeps =
  [ "acid-state-0.14.3"
  , "regexpr-0.5.4"
  , "row-types-0.2.3.0"
  , "superrecord-0.5.0.1"
  , "unagi-chan-0.4.1.0"
  , "mtlparse-0.1.4.0"
  , "slave-thread-1.0.3"
  , "stm-containers-1.1.0.2"
  , "stm-hamt-1.2.0.2"
  , "primitive-extras-0.7.1"
  , "timers-0.2.0.3"
  , "suspend-0.2.0.0"
  ]
