{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Packages where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Elm.Package as Pkg
import Elm.Package (Name(Name), Version)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.FilePath ((</>), splitFileName)
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Client as Client
import qualified Data.ByteString.Lazy as LBS
import qualified Reporting.Task.Http as Http
import qualified Json.Decode as D
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Http as E
import qualified Reporting.Progress as Progress

import Lamdera
import System.IO.Unsafe
import qualified System.Process
import Wire.PrettyPrint (sShow)



-- LOCAL PACKAGE OVERRIDES

getLamderaPkgPath = Env.lookupEnv "LOVR"

localPackages :: Map.Map Name [Version]
localPackages =
  Map.union (Map.fromList [(Name "lamdera" "codecs", [Pkg.Version 1 0 0]), (Name "lamdera" "core", [Pkg.Version 1 0 0])])
  $ unsafePerformIO $ do
    env <- getLamderaPkgPath
    case env of
      Just path ->
        do
          authors <- Dir.listDirectory (path </> "packages")
          authoredPkgs <- concat <$> mapM (\author -> do pkg <- Dir.listDirectory (path </> "packages" </> author); pure (((,) author) <$> pkg)) authors :: IO [(FilePath, FilePath)]
          versionedAuthoredPkgs <- mapM (\(author, pkg) -> do versions <- Dir.listDirectory (path </> "packages" </> author </> pkg); pure (( author, pkg, versions))) authoredPkgs :: IO [(FilePath, FilePath, [FilePath])]

          -- debug_note ("found local packages:" ++ sShow versionedAuthoredPkgs) $
          pure $
            catResultsOrCrashOnLeft <$> (Map.fromList $ ((\(author, pkg, versions) -> (Name (T.pack author) (T.pack pkg), versionFromText <$> T.pack <$> versions)) <$> versionedAuthoredPkgs))

      Nothing ->
        pure Map.empty

versionFromText t =
  case Pkg.versionFromText t of
    Just v -> Right v
    Nothing -> Left t

catResultsOrCrashOnLeft (Right a:rest) = (a:catResultsOrCrashOnLeft rest)
catResultsOrCrashOnLeft (Left a:_) = error ("pathfail:" <> T.unpack a <> ".")
catResultsOrCrashOnLeft [] = []



fetchLocal :: String -> IO (Maybe BS.ByteString)
fetchLocal url =
  do
    -- if $LOVR is set, and `$LOVR ++ url` exists, read that and return it instead
    env <- Lamdera.Packages.getLamderaPkgPath
    case env of
      Just path ->
        do
          exists <- Dir.doesFileExist (path </> url)
          if exists then
              -- debug_note ("using local file override at " ++ (path </> url)) $
              Just <$> BS.readFile (path </> url)
            else
              -- debug_note ("using web file; no local override found at " ++ (path </> url)) $
              pure Nothing
      Nothing ->
        pure $ case url of
          "packages/lamdera/core/1.0.0/endpoint.json" ->
            Just ("{\"url\":\"https://static.lamdera.com/r/lamdera/core/pack.zip\",\"hash\":\"ignored\"}")
          "packages/lamdera/codecs/1.0.0/endpoint.json" ->
            Just ("{\"url\":\"https://static.lamdera.com/r/lamdera/codecs/pack.zip\",\"hash\":\"ignored\"}")
          _ ->
            Nothing

redirectLamderaPaths :: String -> String
redirectLamderaPaths s =
  -- Since package.elm-lang.org doesn't contain copies of elm.json for lamdera packages, we redirect the requests to github directly.
  -- This is dangerous in general, since the author can change the elm.json files, but in this case we control all these packages, so we're all good.
  case s of
    "packages/lamdera/core/1.0.0/elm.json" ->
      "https://static.lamdera.com/r/lamdera/core/1.0.0/elm.json"
    "packages/lamdera/codecs/1.0.0/elm.json" ->
      "https://static.lamdera.com/r/lamdera/codecs/1.0.0/elm.json"
    _ ->
      s



fetchByteString :: String -> Http.Fetch BS.ByteString
fetchByteString path =
  case unsafePerformIO $ Lamdera.Packages.fetchLocal path of
    Just bs -> Http.self bs
    Nothing ->
      Http.package (Lamdera.Packages.redirectLamderaPaths path) [] $ \request manager ->
        do  response <- Client.httpLbs request manager
            return $ Right $ LBS.toStrict $ Client.responseBody response



fetchJson :: String -> (e -> [D.Doc]) -> D.Decoder e a -> String -> Http.Fetch a
fetchJson rootName errorToDocs decoder path =
  case unsafePerformIO $ Lamdera.Packages.fetchLocal path of
    Just bs ->
      case D.parse rootName errorToDocs decoder bs of
        Right value -> Http.self value
        Left v -> error ("fetchJson failed to decode local file: " ++ show v)
    Nothing ->
      Http.package (Lamdera.Packages.redirectLamderaPaths path) [] $ \request manager ->
        do  response <- Client.httpLbs request manager
            let bytes = LBS.toStrict (Client.responseBody response)
            case D.parse rootName errorToDocs decoder bytes of
              Right value ->
                return $ Right value

              Left jsonProblem ->
                return $ Left $ E.BadJson path jsonProblem



-- downloadHelp :: FilePath -> (Name, Version) -> Http.Fetch ()
downloadHelp cache (name, version) downloadArchive endpointDecoder =
  let
    fn =
      do
        -- if $LOVR is set, and `$LOVR ++ url` exists, read that and return it instead
        env <- Lamdera.Packages.getLamderaPkgPath
        case env of
          Just path ->
            do
              let fullPath = path </> "packages" </> Pkg.toUrl name </> Pkg.versionToString version
              exists <- Dir.doesDirectoryExist fullPath
              if exists then
                  --debug_note ("using local pkg override at " ++ fullPath) $
                  -- TODO: pretend it's a zip archive, or at least put it where writeArchive would've
                  let
                    from = fullPath
                    to = cache </> Pkg.toFilePath name
                  in
                    do
                      (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "rsync" ["-ptchr", from, to] ""
                      debug_note ("(roughly) rsync -ptchr " ++ from ++ " " ++ to ++ "\n") $
                      --  debug_note ("  exit code: " ++ show exit ++ "\n") $
                      --  debug_note ("  stdout: " ++ stdout ++ "\n") $
                      --  debug_note ("  stderr: " ++ stderr ++ "\n") $
                        pure (Just ())
                else
                  --debug_note ("using web pkg; no local override found at " ++ fullPath) $
                  pure Nothing
          Nothing ->
            pure Nothing
  in
    case unsafePerformIO fn of
      Just () -> Http.self ()
      Nothing ->
        let
          endpointUrl =
            "packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version ++ "/endpoint.json"
        in
          fetchJson "version" id endpointDecoder endpointUrl
          `Http.andThen` \(endpoint, hash) ->
              let
                start = Progress.DownloadPkgStart name version
                toEnd = Progress.DownloadPkgEnd name version
              in
                Http.report start toEnd $
                  Http.anything endpoint $ \request manager ->
                    Client.withResponse request manager (downloadArchive cache name version hash)
