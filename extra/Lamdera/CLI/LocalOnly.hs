{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.LocalOnly
  ( forbidRemoteDevTools
  , resolveWithin
  , isLoopbackBind
  ) where

{- The parts of `lamdera live` that read, write or open files on this computer:
   /_c (the REPL's view of the project and ~/.elm) and /_x/* (file read/write/list
   with EXPERIMENTAL=1, opening the editor, the backend model).

   They answer only this computer, also when `lamdera live --bind` serves the app
   to the network (a phone on the same Wi-Fi), and only for paths inside the
   directories they are for.
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
    path = maybe (rqPathInfo request) id (urlDecode (rqPathInfo request))
    firstSegment = BS8.takeWhile (\c -> c /= '/' && c /= '\\') (BS8.dropWhile (\c -> c == '/' || c == '\\') path)
    local = isLoopbackClient (rqClientAddr request) && maybe True isLocalHost (getHeader "Host" request)
  if firstSegment `elem` ["_c", "_x"] && not local
    then do
      modifyResponse $ setResponseStatus 403 "Forbidden"
      modifyResponse $ setContentType "text/plain; charset=utf-8"
      writeBS "The lamdera live dev tools are only available on this computer, at http://localhost\n"
      getResponse >>= finishWith
    else
      pass


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
