{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Endpoint.Package (handlePost, reportOnInstalledPackages) where

{-
  1.

  This endpoint will respond to POST requests to "https://repl.lamdera.com/packageList"
  with a JSON body of the form:
  [
    { "name": "elm/core", "version": "1.0.5" },
    { "name": "elm/html", "version": "1.0.0" }
  ]
  It will write an elm.json file to the repl directory, and then reload the repl.
  This response is mediate by function `handlePost` below.

  2.

  In additon, this endpoint will respond to GET requests to "https://repl.lamdera.com/reportOnInstalledPackages"
  with a JSON body of the form:
  [
    { "name": "elm/core", "version": "1.0.5" },
    { "name": "elm/html", "version": "1.0.0" }
  ]
  The json body reports on the packages that are currently installed in the repl.
  This response is mediated by function `reportOnInstalledPackages` below.

  NOTE.  handlePost and reportOnInstalledPackages
         are referenced in the Snap webserver at Develop.runWithRoot
         via the code fragments

             SnapCore.path "packageList" $ Package.handlePost artifactRef)
             SnapCore.path "reportOnInstalledPackages" $ Package.reportOnInstalledPackages)
-}


import GHC.Generics (Generic)
import Snap.Core
import Snap.Http.Server
import Data.Aeson (FromJSON, eitherDecode, encode, ToJSON, toJSON, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as ByteString
import GHC.Generics
import System.IO (writeFile)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
---
import Snap.Util.FileServe
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.Text.Encoding (decodeUtf8)
import Snap.Http.Server.Config (setPort, defaultConfig)
import qualified ReplArtifacts
import Data.IORef


data Package = Package { name :: String, version :: String } deriving (Show, Generic)

instance FromJSON Package
instance ToJSON Package


type PackageList = [Package]

writeElmJson :: PackageList -> IO ()
writeElmJson pkgs = do
    let directDeps = Map.fromList $ ("elm/core", "1.0.5"):[(name p, version p) | p <- pkgs]
        elmJson = object [
            "type" .= ("application" :: String),
            "source-directories" .= (["../../repl-src"] :: [String]),
            "elm-version" .= ("0.19.1" :: String),
            "dependencies" .= object [
                "direct" .= directDeps,
                "indirect" .= object [
                    "elm/json" .= ("1.1.3" :: String)
                ]
            ],
            "test-dependencies" .= object [
                "direct" .= (Map.empty :: Map.Map String String),
                "indirect" .= (Map.empty :: Map.Map String String)
            ]
          ]
    writeFile "./outlines/repl/elm.json" ( BL.unpack $ encode elmJson)


handlePost :: IORef ReplArtifacts.Artifacts -> Snap ()
handlePost artifactRef = do
    body <- readRequestBody 10000
    let maybePackageList = eitherDecode body :: Either String PackageList
    case maybePackageList of
        Left err -> writeBS $ "Error: Could not decode JSON: " <> (ByteString.pack err)
        Right packages -> do
            liftIO $ writeElmJson packages
            let message = ByteString.pack $ "Packages added: " ++ (show $ length packages)
            writeBS message
            newArtifacts <- liftIO ReplArtifacts.loadRepl
            liftIO $ writeIORef artifactRef newArtifacts



data Dependencies = Dependencies {
    direct :: HM.HashMap String String
  } deriving (Generic, Show)

data TopLevel = TopLevel {
    dependencies :: Dependencies
  } deriving (Generic, Show)

instance FromJSON TopLevel

instance FromJSON Dependencies

--- curl -X POST -H "Content-Length: 0" http://localhost:8000/reportOnInstalledPackages

reportOnInstalledPackages :: Snap ()
reportOnInstalledPackages = do
    jsonData <- liftIO $ LBS.readFile "./outlines/repl/elm.json"
    case eitherDecode jsonData :: Either String TopLevel of
        Left err -> writeBS $ "Failed to parse JSON: " <> (LBS.toStrict jsonData)
        Right topLevel -> do
            let directDeps = HM.toList $ direct $ dependencies topLevel
            let outputList = map (\(name, version) -> object ["name" .= name, "version" .= version]) directDeps
            writeBS . LBS.toStrict . encode $ outputList
