{-# LANGUAGE OverloadedStrings #-}

module Endpoint.Package (handlePost) where

{-# LANGUAGE DeriveGeneric #-}

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

import Snap.Http.Server.Config (setPort, defaultConfig)

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
            "dep  endencies" .= object [
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
    writeFile "../../outlines/repl/elm.json" ( BL.unpack $ encode elmJson)


handlePost :: Snap ()
handlePost = do
    body <- readRequestBody 10000
    let maybePackageList = eitherDecode body :: Either String PackageList
    case maybePackageList of
        Left err -> writeBS $ "Error: Could not decode JSON: " <> (ByteString.pack err)
        Right packages -> do
            liftIO $ writeElmJson packages
            writeBS "Successfully received packages and wrote to elm.json"

--routes :: [(ByteString, Snap ())]
--routes = [("/packageList", method POST handlePost)]

