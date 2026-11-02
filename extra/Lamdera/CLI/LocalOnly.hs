{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.LocalOnly
  ( forbidRemoteDevTools
  , forbidRemoteDotfiles
  , localOnly
  , isLocalRequest
  , isLocal
  , isLocalHost
  , hasDotSegment
  , resolveWithin
  , isLoopbackBind
  ) where

{- The parts of `lamdera live` that read, write or open files on this computer:
   /_c (the REPL's view of the project and ~/.elm) and /_x/* (file read/write/list
   with EXPERIMENTAL=1, opening the editor, the backend model).

   They answer only this computer, also when `lamdera live --bind` serves the app
   to the network (a phone on the same Wi-Fi), and only for paths inside the
   directories they are for.

   The same goes for the project's files at their own path (/src/Backend.elm,
   /elm.json) and directory listings: a request that is not local only gets what the
   app itself needs, as in production (see localOnly, forbidRemoteDotfiles). That is
   what `lamdera live --serve` relies on: its tunnel forwards each request from this
   computer, but with the Host it was sent to (<app>-<random>.lamdera.dev), so
   none of it counts as local.
-}

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import Snap.Core hiding (path)


-- | Answers 403 for /_c and /_x/* unless the request comes from this computer, with
-- a Host a DNS rebinding page can't send (localhost, *.localhost or an IP address),
-- so neither another device on the network nor a website open in the browser can
-- reach them. Passes everything else on.
forbidRemoteDevTools :: Snap ()
forbidRemoteDevTools = do
  request <- getRequest
  let
    path = decodedPath request
    firstSegment = BS8.takeWhile (\c -> c /= '/' && c /= '\\') (BS8.dropWhile (\c -> c == '/' || c == '\\') path)
    local = isLocalRequest request
  if firstSegment `elem` ["_c", "_x"] && not local
    then do
      modifyResponse $ setResponseStatus 403 "Forbidden"
      modifyResponse $ setContentType "text/plain; charset=utf-8"
      writeBS "The lamdera live dev tools are only available on this computer, at http://localhost\n"
      getResponse >>= finishWith
    else
      pass


-- | Answers 404 for a request that is not local and names a dotfile or a dot
-- directory anywhere in its path (/.env, /.git/config, /public/.secret): never part
-- of what an app serves. Passes everything else on.
forbidRemoteDotfiles :: Snap ()
forbidRemoteDotfiles = do
  request <- getRequest
  if not (isLocalRequest request) && hasDotSegment (decodedPath request)
    then do
      modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/plain; charset=utf-8"
      writeBS "Not found\n"
      getResponse >>= finishWith
    else
      pass


-- | The handler only for local requests (see isLocalRequest), passing on the others:
-- for the parts of `lamdera live` that serve the project's own files at their path,
-- which the app does not need (it is served from public/ and the compiled harness).
localOnly :: Snap () -> Snap ()
localOnly handler = do
  request <- getRequest
  if isLocalRequest request then handler else pass


-- | Whether a request comes from this computer, with a Host a DNS rebinding page
-- can't send (localhost, *.localhost or an IP address). A request through the
-- `lamdera live --serve` tunnel comes from this computer, but with the preview's Host.
isLocalRequest :: Request -> Bool
isLocalRequest request =
  isLocal (rqClientAddr request) (getHeader "Host" request)


-- | isLocalRequest from the client address and the Host header.
isLocal :: BS8.ByteString -> Maybe BS8.ByteString -> Bool
isLocal clientAddr hostHeader =
  isLoopbackClient clientAddr && maybe True isLocalHost hostHeader


-- | Whether any segment of a path starts with a dot ("." and ".." included).
hasDotSegment :: BS8.ByteString -> Bool
hasDotSegment path =
  any (\segment -> "." `BS8.isPrefixOf` segment) (BS8.splitWith (\c -> c == '/' || c == '\\') path)


decodedPath :: Request -> BS8.ByteString
decodedPath request =
  maybe (rqPathInfo request) id (urlDecode (rqPathInfo request))


-- | The real path of `path` with symlinks resolved, if that is inside one of `roots`
-- (themselves resolved), otherwise Nothing. Absolute paths, `..` and symlinks out of
-- the roots all give Nothing.
resolveWithin :: [FilePath] -> FilePath -> IO (Maybe FilePath)
resolveWithin roots path
  | ".." `elem` FP.splitDirectories path = pure Nothing
  | otherwise = do
      result <- try $ do
        full <- Dir.canonicalizePath path
        roots_ <- mapM Dir.canonicalizePath roots
        pure $
          if any (\root -> FP.splitDirectories root `List.isPrefixOf` FP.splitDirectories full) roots_
            then Just full
            else Nothing
      case result of
        Right resolved -> pure resolved
        Left (_ :: SomeException) -> pure Nothing


isLoopbackBind :: String -> Bool
isLoopbackBind bind =
  isLoopbackClient (BS8.pack bind) || bind == "localhost"


isLoopbackClient :: BS8.ByteString -> Bool
isLoopbackClient addr =
     "127." `BS8.isPrefixOf` addr
  || addr == "::1"
  || "::ffff:127." `BS8.isPrefixOf` addr


-- The name in a Host header, without its port: a browser only sends one of these to
-- a page it loaded from this computer by that name, never from a website's domain.
isLocalHost :: BS8.ByteString -> Bool
isLocalHost hostHeader =
  let
    host = BS8.map Char.toLower $
      if "[" `BS8.isPrefixOf` hostHeader
        then BS8.takeWhile (/= ']') (BS8.drop 1 hostHeader) -- [::1]:8000
        else BS8.takeWhile (/= ':') hostHeader
    isIPv4 =
      let parts = BS8.split '.' host
      in length parts == 4 && all (\p -> not (BS8.null p) && BS8.all Char.isDigit p) parts
    isIPv6 = "[" `BS8.isPrefixOf` hostHeader
  in
  host == "localhost" || ".localhost" `BS8.isSuffixOf` host || isIPv4 || isIPv6
