{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.LocalOnly
  ( forbidCrossSite
  , isSameOrigin
  , socketHostAllowed
  , forbidRemoteDevTools
  , forbidRemoteDotfiles
  , localOnly
  , isLocalRequest
  , isLocal
  , isLocalHost
  , hasDotSegment
  , resolveWithin
  , isLoopbackBind
  , pathSegments
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

   A website open in the developer's browser is a different matter: its requests to
   http://localhost:8000 come from this computer with that Host, so they count as
   local. The browser keeps the page from reading the answers, but not from causing
   side effects, and a websocket is not limited by the same-origin policy at all. Only
   the Origin and Sec-Fetch-Site headers show where such a request comes from, see
   forbidCrossSite.
-}

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import Snap.Core hiding (path)


-- | Answers 403 for /_c, /_x/* and /_w when a page on another site sent the request
-- (see isSameOrigin): a website open in the developer's browser can reach
-- http://localhost:8000 like any other page, and its requests look local. /_r/* stays
-- open, as production takes RPC calls from other sites.
--
-- Also answers 403 for /_w when the Host is not a local name (see socketHostAllowed),
-- unless `shared` (--serve or --bind is on): a DNS rebinding page, with its own domain
-- pointed at 127.0.0.1, is same-origin with itself. Passes everything else on.
forbidCrossSite :: Bool -> Snap ()
forbidCrossSite shared = do
  request <- getRequest
  let
    first = take 1 (pathSegments request)
    host = getHeader "Host" request
    sameOrigin = isSameOrigin host (getHeader "Origin" request) (getHeader "Sec-Fetch-Site" request)
    forbid message = do
      modifyResponse $ setResponseStatus 403 "Forbidden"
      modifyResponse $ setContentType "text/plain; charset=utf-8"
      writeBS message
      getResponse >>= finishWith
  if first `elem` [["_c"], ["_x"], ["_w"]] && not sameOrigin
    then forbid "The lamdera live dev tools only answer the app's own pages, not other websites\n"
    else if first == ["_w"] && not (socketHostAllowed shared host)
      then forbid "lamdera live only answers the app's socket at localhost or an IP address. To open the app by another name, start lamdera live with --bind (e.g. --bind=127.0.0.1) or --serve\n"
      else pass


-- | Whether a request comes from a page of the same origin, or from no page at all:
--
--   * Origin is missing (a tool like curl, or a same-origin GET), or names the host and
--     port of the Host header;
--   * Sec-Fetch-Site is missing (an older browser, or not a browser), same-origin, or
--     none (the developer typed the URL).
--
-- The Sec-Fetch-Site check catches what has no Origin, like an <img src> GET. A
-- `lamdera live --serve` visitor's requests are same-origin too: the tunnel keeps the
-- preview's Host, and the visitor's page is at the preview's origin.
isSameOrigin :: Maybe BS8.ByteString -> Maybe BS8.ByteString -> Maybe BS8.ByteString -> Bool
isSameOrigin hostHeader originHeader fetchSite =
  let
    originOk =
      case originHeader of
        Nothing -> True
        Just origin ->
          case (parseOrigin origin, hostHeader) of
            (Just (scheme, originHost, originPort), Just host) ->
              let (hostName, hostPort) = splitHostPort host
              in originHost == hostName && Just originPort == maybe (defaultPort scheme) Just hostPort
            _ -> False -- Origin: null (a sandboxed or opaque origin), or no Host
    siteOk =
      case fmap (BS8.map Char.toLower . trim) fetchSite of
        Nothing -> True
        Just site -> site `elem` ["same-origin", "none"]
  in
  originOk && siteOk


-- | Whether /_w answers a request with this Host: always when `shared` (--serve or
-- --bind is on), otherwise only for a local name (see isLocalHost), which a DNS
-- rebinding page can't send.
socketHostAllowed :: Bool -> Maybe BS8.ByteString -> Bool
socketHostAllowed shared hostHeader =
  shared || maybe True isLocalHost hostHeader


-- An Origin header's scheme, host and port, the port filled in when it's the default.
parseOrigin :: BS8.ByteString -> Maybe (BS8.ByteString, BS8.ByteString, BS8.ByteString)
parseOrigin origin =
  let
    lower = BS8.map Char.toLower (trim origin)
    (scheme, rest) = BS8.breakSubstring "://" lower
    authority = BS8.drop 3 rest
    (host, port) = splitHostPort authority
  in
  if BS8.null scheme || BS8.null rest || BS8.null host || BS8.any (== '/') authority
    then Nothing
    else fmap (\p -> (scheme, host, p)) (maybe (defaultPort scheme) Just port)


-- A Host header (or an Origin's authority) as its lowercased name and its port, if it
-- has one. An IPv6 address keeps its brackets, as an Origin writes it: [::1]:8000.
splitHostPort :: BS8.ByteString -> (BS8.ByteString, Maybe BS8.ByteString)
splitHostPort hostHeader =
  let
    lower = BS8.map Char.toLower (trim hostHeader)
    (name, rest) =
      if "[" `BS8.isPrefixOf` lower
        then let (inside, after) = BS8.break (== ']') lower in (inside <> BS8.take 1 after, BS8.drop 1 after)
        else BS8.break (== ':') lower
  in
  case BS8.uncons rest of
    Just (':', port) | not (BS8.null port) -> (name, Just port)
    _ -> (name, Nothing)


defaultPort :: BS8.ByteString -> Maybe BS8.ByteString
defaultPort scheme =
  case scheme of
    "http" -> Just "80"
    "ws" -> Just "80"
    "https" -> Just "443"
    "wss" -> Just "443"
    _ -> Nothing


trim :: BS8.ByteString -> BS8.ByteString
trim =
  BS8.dropWhile Char.isSpace . BS8.reverse . BS8.dropWhile Char.isSpace . BS8.reverse


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


-- | The segments of a request's decoded path, split at / and \ (the dev tools take
-- either, see Lamdera.CLI.Live.serveExperimental), without empty ones.
pathSegments :: Request -> [BS8.ByteString]
pathSegments request =
  filter (not . BS8.null) (BS8.splitWith (\c -> c == '/' || c == '\\') (decodedPath request))


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
