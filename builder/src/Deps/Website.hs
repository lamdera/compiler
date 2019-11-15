{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Deps.Website
  ( getElmJson
  , getDocs
  , getNewPackages
  , getAllPackages
  , download
  , githubCommit
  , githubDownload
  , Sha
  , register
  )
  where


import Prelude hiding (zip)
import qualified Codec.Archive.Zip as Zip
import Control.Monad (void)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import qualified Network.HTTP.Types as Http
import qualified System.Directory as Dir
import System.FilePath ((</>), splitFileName)
import qualified System.Environment as Env

import Elm.Package (Name(Name), Version)
import qualified Elm.Package as Pkg
import qualified Json.Decode as D

import qualified Reporting.Doc as D
import qualified Reporting.Exit.Http as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Reporting.Task.Http as Http
import qualified Stuff.Paths as Path

import qualified Debug.Trace as DT
import System.IO.Unsafe
import qualified System.Process
import Transpile.PrettyPrint (sShow)

-- GET PACKAGE INFO


getElmJson :: Name -> Version -> Task.Task BS.ByteString
getElmJson name version =
  Http.run $ fetchByteString $
    "packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version ++ "/elm.json"


getDocs :: Name -> Version -> Task.Task BS.ByteString
getDocs name version =
  Http.run $ fetchByteString $
    "packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version ++ "/docs.json"

-- LOCAL PACKAGE OVERRIDES

getLamderaPkgPath = Env.lookupEnv "LAMDERA_PKG_PATH"

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

          -- DT.trace ("found local packages:" ++ sShow versionedAuthoredPkgs) $
          pure $
            catResultsOrCrashOnLeft <$> (Map.fromList $ ((\(author, pkg, versions) -> (Name (T.pack author) (T.pack pkg), versionFromText <$> T.pack <$> versions)) <$> versionedAuthoredPkgs))

      Nothing ->
        pure Map.empty

versionFromText t =
  case Pkg.versionFromText t of
    Just v -> Right v
    Nothing -> Left t

catResultsOrCrashOnLeft (Right a:rest) = (a:catResultsOrCrashOnLeft rest)
catResultsOrCrashOnLeft (Left a:_) = error ("failed to parse folder structure; did you accidentally end up with `$LAMDERA_PKG_PATH/packages/packages/...` or something similar? I expected something like `$LAMDERA_PKG_PATH/packages/Lamdera/core/1.0.0/...`, but where I expected the `1.0.0` part to be, there wasn't a valid elm semver, instead I saw `" <> T.unpack a <> "`.")
catResultsOrCrashOnLeft [] = []

-- NEW PACKAGES


getNewPackages :: Int -> Task.Task [(Name, Version)]
getNewPackages index =
  -- this runs when version solver tries to upgrade a dependency
  Http.run $ fetchJson "packages" E.badJsonToDocs (D.list newPkgDecoder) ("all-packages/since/" ++ show index)


newPkgDecoder :: D.Decoder E.BadJson ( Name, Version )
newPkgDecoder =
  do  txt <- D.text
      case T.splitOn "@" txt of
        [key, value] ->
          case Pkg.fromText key of
            Right pkg ->
              case Pkg.versionFromText value of
                Just vsn ->
                  D.succeed (pkg, vsn)

                Nothing ->
                  D.fail (E.BadNewPkg txt)

            Left _ ->
              D.fail (E.BadNewPkg txt)

        _ ->
          D.fail (E.BadNewPkg txt)



-- ALL PACKAGES


getAllPackages :: Task.Task (Map.Map Name [Version])
getAllPackages =
  Map.union localPackages <$>
  (Http.run $ fetchJson "packages" E.badJsonToDocs allPkgsDecoder "all-packages")


allPkgsDecoder :: D.Decoder E.BadJson (Map.Map Name [Version])
allPkgsDecoder =
  let
    checkKeys pairs pkgs =
      case pairs of
        [] ->
          D.succeed pkgs

        (key, versions) : rest ->
          case Pkg.fromText key of
            Left (msg, _) ->
              D.fail (E.BadAllPkg key msg)

            Right pkg ->
              checkKeys rest (Map.insert pkg versions pkgs)
  in
    do  dict <- D.dict (D.list (D.mapError E.BadAllVsn Pkg.versionDecoder))
        checkKeys (HashMap.toList dict) Map.empty



-- HELPERS


fetchLocal :: String -> IO (Maybe BS.ByteString)
fetchLocal url =
  do
    -- if $LAMDERA_PKG_PATH is set, and `$LAMDERA_PKG_PATH ++ url` exists, read that and return it instead
    env <- getLamderaPkgPath
    case env of
      Just path ->
        do
          exists <- Dir.doesFileExist (path </> url)
          if exists then
              -- DT.trace ("using local file override at " ++ (path </> url)) $
              Just <$> BS.readFile (path </> url)
            else
              -- DT.trace ("using web file; no local override found at " ++ (path </> url)) $
              pure Nothing
      Nothing ->
        pure $ case url of
          "packages/lamdera/core/1.0.0/endpoint.json" ->
            Just ("{\"url\":\"http://static.lamdera.com/r/lamdera/core/pack.zip\",\"hash\":\"ignored\"}")
          "packages/lamdera/codecs/1.0.0/endpoint.json" ->
            Just ("{\"url\":\"http://static.lamdera.com/r/lamdera/codecs/pack.zip\",\"hash\":\"ignored\"}")
          _ ->
            Nothing

redirectLamderaPaths :: String -> String
redirectLamderaPaths s =
  -- Since package.elm-lang.org doesn't contain copies of elm.json for lamdera packages, we redirect the requests to github directly.
  -- This is dangerous in general, since the author can change the elm.json files, but in this case we control all these packages, so we're all good.
  case s of
    "packages/lamdera/core/1.0.0/elm.json" ->
      "http://static.lamdera.com/r/lamdera/core/1.0.0/elm.json"
    "packages/lamdera/codecs/1.0.0/elm.json" ->
      "http://static.lamdera.com/r/lamdera/codecs/1.0.0/elm.json"
    _ ->
      s

fetchByteString :: String -> Http.Fetch BS.ByteString
fetchByteString path =
  case unsafePerformIO $ fetchLocal path of
    Just bs -> Http.self bs
    Nothing ->
      Http.package (redirectLamderaPaths path) [] $ \request manager ->
        do  response <- Client.httpLbs request manager
            return $ Right $ LBS.toStrict $ Client.responseBody response


fetchJson :: String -> (e -> [D.Doc]) -> D.Decoder e a -> String -> Http.Fetch a
fetchJson rootName errorToDocs decoder path =
  case unsafePerformIO $ fetchLocal path of
    Just bs ->
      case D.parse rootName errorToDocs decoder bs of
        Right value -> Http.self value
        Left v -> error ("fetchJson failed to decode local file: " ++ show v)
    Nothing ->
      Http.package (redirectLamderaPaths path) [] $ \request manager ->
        do  response <- Client.httpLbs request manager
            let bytes = LBS.toStrict (Client.responseBody response)
            case D.parse rootName errorToDocs decoder bytes of
              Right value ->
                return $ Right value

              Left jsonProblem ->
                return $ Left $ E.BadJson path jsonProblem



-- DOWNLOAD


download :: [(Name, Version)] -> Task.Task ()
download packages =
  case packages of
    [] ->
      Task.report Progress.DownloadSkip

    _ ->
      do  cache <- Task.getPackageCacheDir

          let start = Progress.DownloadStart packages
          let toEnd = Progress.DownloadEnd

          void $ Http.run $ Http.report start toEnd $
            Http.parallel $ map (downloadHelp cache) packages


downloadHelp :: FilePath -> (Name, Version) -> Http.Fetch ()
downloadHelp cache (name, version) =
  let
    fn =
      do
        -- if $LAMDERA_PKG_PATH is set, and `$LAMDERA_PKG_PATH ++ url` exists, read that and return it instead
        env <- getLamderaPkgPath
        case env of
          Just path ->
            do
              let fullPath = path </> "packages" </> Pkg.toUrl name </> Pkg.versionToString version
              exists <- Dir.doesDirectoryExist fullPath
              if exists then
                  --DT.trace ("using local pkg override at " ++ fullPath) $
                  -- TODO: pretend it's a zip archive, or at least put it where writeArchive would've
                  let
                    from = fullPath
                    to = cache </> Pkg.toFilePath name
                  in
                    do
                      (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "rsync" ["-ptchr", from, to] ""
                      DT.trace ("(roughly) rsync -ptchr " ++ from ++ " " ++ to ++ "\n") $
                      --  DT.trace ("  exit code: " ++ show exit ++ "\n") $
                      --  DT.trace ("  stdout: " ++ stdout ++ "\n") $
                      --  DT.trace ("  stderr: " ++ stderr ++ "\n") $
                        pure (Just ())
                else
                  --DT.trace ("using web pkg; no local override found at " ++ fullPath) $
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


endpointDecoder :: D.Decoder e (String, String)
endpointDecoder =
  D.map2 (,)
    (D.field "url" D.string)
    (D.field "hash" D.string)



-- DOWNLOAD ZIP ARCHIVE


downloadArchive :: FilePath -> Name -> Version -> String -> Client.Response Client.BodyReader -> IO (Either E.Exit ())
downloadArchive cache name version expectedHash response =
  do  result <- readArchive (Client.responseBody response) initialArchiveState
      case result of
        Left msg ->
          return (Left msg)

        Right (sha, archive) ->
          if expectedHash == "ignored" || expectedHash == SHA.showDigest sha then
            Right <$> writeArchive archive (cache </> Pkg.toFilePath name) (Pkg.versionToString version)
          else
            return $ Left $ E.BadZipSha expectedHash (SHA.showDigest sha)



-- READ ARCHIVE


data ArchiveState =
  AS
    { _len :: !Int
    , _sha :: !(Binary.Decoder SHA.SHA1State)
    , _zip :: !(Binary.Decoder Zip.Archive)
    }


initialArchiveState :: ArchiveState
initialArchiveState =
  AS 0 SHA.sha1Incremental (Binary.runGetIncremental Binary.get)


type Sha = SHA.Digest SHA.SHA1State


readArchive :: Client.BodyReader -> ArchiveState -> IO (Either E.Exit (Sha, Zip.Archive))
readArchive body (AS len sha zip) =
  case zip of
    Binary.Fail _ _ _ ->
      return $ Left E.BadZipData

    Binary.Partial k ->
      do  chunk <- Client.brRead body
          readArchive body $ AS
            { _len = len + BS.length chunk
            , _sha = Binary.pushChunk sha chunk
            , _zip = k (if BS.null chunk then Nothing else Just chunk)
            }

    Binary.Done _ _ archive ->
      return $ Right ( SHA.completeSha1Incremental sha len, archive )



-- WRITE ARCHIVE

writeArchive :: Zip.Archive -> FilePath -> FilePath -> IO ()
writeArchive archive destination newRoot =
  do  Dir.createDirectoryIfMissing True destination
      let opts = [Zip.OptDestination destination]
      mapM_ (Zip.writeEntry opts . replaceRoot newRoot) (Zip.zEntries archive)


replaceRoot :: String -> Zip.Entry -> Zip.Entry
replaceRoot root entry =
  let
    rootless =
      dropWhile (/='/') (Zip.eRelativePath entry)
  in
    entry { Zip.eRelativePath = root ++ rootless }



-- FIND TAGGED COMMIT ON GITHUB


githubCommit :: Name -> Version -> Task.Task String
githubCommit name version =
  let
    endpoint =
      "https://api.github.com/repos/" ++ Pkg.toUrl name ++ "/git/refs/tags/" ++ Pkg.versionToString version

    headers =
      [ ( Http.hUserAgent, "elm-cli" )
      , ( Http.hAccept, "application/json" )
      ]

    decoder =
      D.at ["object","sha"] D.string
  in
    Http.run $ Http.anything endpoint $ \request manager ->
      do  response <- Client.httpLbs (request { Client.requestHeaders = headers }) manager
          let bytes = LBS.toStrict (Client.responseBody response)
          case D.parse "github" id decoder bytes of
            Right value ->
              return $ Right value

            Left jsonProblem ->
              return $ Left $ E.BadJson "github.json" jsonProblem



-- DOWNLOAD FROM GITHUB


githubDownload :: Name -> Version -> FilePath -> Task.Task Sha
githubDownload name version dir =
  let
    endpoint =
      "https://github.com/" ++ Pkg.toUrl name ++ "/zipball/" ++ Pkg.versionToString version ++ "/"
  in
    Http.run $ Http.anything endpoint $ \request manager ->
      Client.withResponse request manager (githubDownloadHelp dir)


githubDownloadHelp :: FilePath -> Client.Response Client.BodyReader -> IO (Either E.Exit Sha)
githubDownloadHelp targetDir response =
  do  result <- readArchive (Client.responseBody response) initialArchiveState
      case result of
        Left msg ->
          return (Left msg)

        Right (sha, archive) ->
          do  let (dir, root) = splitFileName targetDir
              writeArchive archive dir root
              return $ Right sha



-- REGISTER PACKAGES


register :: Name -> Version -> String -> Sha -> Task.Task ()
register name version commitHash digest =
  let
    params =
      [ ("name", Pkg.toString name)
      , ("version", Pkg.versionToString version)
      , ("commit-hash", commitHash)
      ]

    files =
      [ Multi.partFileSource "elm.json" "elm.json"
      , Multi.partFileSource "docs.json" Path.docs
      , Multi.partFileSource "README.md" "README.md"
      , Multi.partFileRequestBody "github-hash" "github-hash" $
          Client.RequestBodyBS (BS.pack (SHA.showDigest digest))
      ]
  in
    Http.run $ Http.package "register" params $ \rawRequest manager ->
      do  requestWithBody <- Multi.formDataBody files rawRequest
          let request = requestWithBody { Client.responseTimeout = Client.responseTimeoutNone }
          void $ Client.httpLbs request manager
          return $ Right ()
