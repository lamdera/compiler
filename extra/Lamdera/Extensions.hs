module Lamdera.Extensions where

import qualified Data.Map as Map
import qualified System.Directory as Dir

import qualified Data.Utf8 as Utf8
import qualified Elm.Version as V
import qualified Elm.Package as Pkg
import qualified Data.ByteString.Char8 as BS

import Lamdera
import Lamdera.Project


-- The idea is to keep extension code in here, but so far the extensions have
-- required types from the files they were extending, causing cyclic refs.
-- So they've needed to go inline as a result.

-- So the only extensions going here are the ones that don't rely on local types


metadata :: Pkg.Name -> V.Version -> String -> String -> String
metadata pkg@(Pkg.Name author project) version file original =
  if author == Pkg.authorLamdera
    then
      "https://static.lamdera.com/r/" ++ Pkg.toUrl pkg ++ "/" ++ V.toChars version ++ "/" ++ file
    else
      original


elmJsonOverride :: Pkg.Name -> V.Version -> IO (Either e BS.ByteString) -> IO (Either e BS.ByteString)
elmJsonOverride pkg@(Pkg.Name author project) vsn original =
  if author == Pkg.authorLamdera
    then do
      pkgsPath <- Lamdera.getLamderaPkgPath -- @LAMDERA
      if pkgsPath /= Nothing
        then do
          let
            packageRoot = (pkgsPath & withDefault "<no-packages-path-override-set>") <> "/packages/" <> Pkg.toUrl pkg ++ "/" ++ V.toChars vsn
            elmJson = packageRoot <> "/elm.json"

          exists <- Dir.doesFileExist elmJson
          if exists
            then do
              res <- readUtf8 elmJson
              debug $ "🔁  Serving local elm.json: " <> elmJson
              pure (Right res)

            else original

        else
          original

    else
      original


endpointJsonOverride :: Pkg.Name -> V.Version -> IO (Either e BS.ByteString) -> IO (Either e BS.ByteString)
endpointJsonOverride pkg@(Pkg.Name author project) vsn original =
  if author == Pkg.authorLamdera
    then do
      pkgsPath <- Lamdera.getLamderaPkgPath -- @LAMDERA
      if pkgsPath /= Nothing
        then do
          let
            packageRoot = (pkgsPath & withDefault "<no-packages-path-override-set>") <> "/packages/" <> Pkg.toUrl pkg ++ "/" ++ V.toChars vsn
            endpointJson = packageRoot <> "/endpoint.json"

          exists <- Dir.doesFileExist endpointJson
          if exists
            then do
              res <- readUtf8 endpointJson
              debug $ "🔁  Serving local endpoint.json: " <> endpointJson
              pure (Right res)

            else original

        else
          original

    else
      original
