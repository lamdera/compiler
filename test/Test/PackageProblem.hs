{-# LANGUAGE OverloadedStrings #-}

{-| Coverage for the package-download failure reports.

These fire when a published Elm version can no longer be fetched from the GitHub
repo the registry points at. Published versions are immutable; the repos serving
them are not, so this happens for real and keeps happening.

Two distinct failures, with the same root cause:

  * the tag is gone          -> 404 on the zipball          -> PP_BadArchiveRequest
  * the author was renamed   -> archive bytes changed       -> PP_BadArchiveHash

The second one is the unobvious one. A GitHub zipball contains a top-level folder
named after the CURRENT owner and repo, so renaming an account changes the bytes,
and therefore the sha1, of every published version of that package at once - while
the code inside is untouched.

The wording IS the feature here, so it is asserted rather than eyeballed. Cases
below are modelled on real packages, verified by hand against the live registry:

  STTR13/ziplist 1.3.0              404,  now staeter,      re-published
  ryannhg/date-format 2.3.0         hash, now ryan-haskell, re-published
  y0hy0h/ordered-containers 2.0.0   hash, now j-maas,       NOT re-published
-}
module Test.PackageProblem where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Version as HTTP

import EasyTest

import qualified Data.Utf8 as Utf8
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Http
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help


all = EasyTest.run suite


-- FIXTURES


ziplist :: Pkg.Name
ziplist =
  Pkg.toName (Utf8.fromChars "STTR13") "ziplist"


dateFormat :: Pkg.Name
dateFormat =
  Pkg.toName (Utf8.fromChars "ryannhg") "date-format"


orderedContainers :: Pkg.Name
orderedContainers =
  Pkg.toName (Utf8.fromChars "y0hy0h") "ordered-containers"


movedAndPublished :: String -> Maybe Exit.PackageMoved
movedAndPublished newName =
  Just (Exit.PackageMoved newName True)


movedNotPublished :: String -> Maybe Exit.PackageMoved
movedNotPublished newName =
  Just (Exit.PackageMoved newName False)


-- Build a genuine Http.Error carrying an HTTP status, the same shape the archive
-- download produces on failure, so archiveNotFound's classification is exercised
-- for real rather than stubbed around.
httpStatusError :: Int -> String -> Http.Error
httpStatusError code url =
  Http.BadHttp url $
    HTTP.StatusCodeException
      (HTTP.Response
        { HTTP.responseStatus = HTTP.Status code (BS.pack (statusMessage code))
        , HTTP.responseVersion = HTTP.http11
        , HTTP.responseHeaders = []
        , HTTP.responseBody = ()
        , HTTP.responseCookieJar = mempty
        , HTTP.responseClose' = HTTP.ResponseClose (return ())
        , HTTP.responseOriginalRequest = HTTP.parseRequest_ url
        }
      )
      (BS.pack "")


statusMessage :: Int -> String
statusMessage code =
  case code of
    404 -> "Not Found"
    500 -> "Internal Server Error"
    _   -> "Unknown"


render :: Pkg.Name -> V.Version -> Exit.PackageProblem -> String
render pkg vsn problem =
  Help.toString $ Help.reportToDoc $ Exit.toPackageProblemReport pkg vsn problem


-- ASSERTIONS


-- Reports are hard-wrapped at render time, so a phrase can be split across lines.
-- Compare on a whitespace-normalised copy so the assertions describe the message
-- rather than the current wrap points.
contains :: String -> String -> Test ()
contains needle haystack =
  if squish needle `List.isInfixOf` squish haystack
    then ok
    else crash $ "expected to find:\n  " ++ needle ++ "\n\nin report:\n" ++ haystack


omits :: String -> String -> Test ()
omits needle haystack =
  if squish needle `List.isInfixOf` squish haystack
    then crash $ "expected NOT to find:\n  " ++ needle ++ "\n\nin report:\n" ++ haystack
    else ok


squish :: String -> String
squish =
  unwords . words


suite :: Test ()
suite = tests
  [ scope "404 + renamed + re-published -> name it, link it, give exact commands" $ do
      let report = render ziplist (V.Version 1 3 0) $
            Exit.PP_BadArchiveRequest
              (httpStatusError 404 "https://github.com/STTR13/ziplist/zipball/1.3.0/")
              (movedAndPublished "staeter/ziplist")

      contains "PACKAGE SOURCE UNAVAILABLE" report
      contains "404 Not Found" report
      contains "the project now lives here" report
      contains "https://github.com/staeter/ziplist" report
      contains "re-published to the Elm package registry" report
      contains "https://package.elm-lang.org/packages/staeter/ziplist/" report
      contains "npx elm-json-lamdera uninstall STTR13/ziplist" report
      contains "npx elm-json-lamdera install staeter/ziplist" report
      contains "version numbers may not line up" report

      -- the whole point of splitting this out of the generic HTTP report
      omits "firewall" report

  , scope "404 + renamed + NOT re-published -> say so, do not suggest installing it" $ do
      let report = render orderedContainers (V.Version 2 0 0) $
            Exit.PP_BadArchiveRequest
              (httpStatusError 404 "https://github.com/y0hy0h/ordered-containers/zipball/2.0.0/")
              (movedNotPublished "j-maas/ordered-containers")

      contains "PACKAGE SOURCE UNAVAILABLE" report
      contains "https://github.com/j-maas/ordered-containers" report
      contains "has not been re-published" report
      omits "npx elm-json-lamdera install j-maas/ordered-containers" report

  , scope "404 + no rename found -> fall back to picking a published version" $ do
      let report = render ziplist (V.Version 1 3 0) $
            Exit.PP_BadArchiveRequest
              (httpStatusError 404 "https://github.com/STTR13/ziplist/zipball/1.3.0/")
              Nothing

      contains "PACKAGE SOURCE UNAVAILABLE" report
      contains "Switch to a version that is still published" report
      contains "https://package.elm-lang.org/packages/STTR13/ziplist/" report
      contains "npx elm-json-lamdera install STTR13/ziplist@<version>" report
      omits "the project now lives here" report

  , scope "non-404 archive failure still gets the generic HTTP report" $ do
      let report = render ziplist (V.Version 1 3 0) $
            Exit.PP_BadArchiveRequest
              (httpStatusError 500 "https://github.com/STTR13/ziplist/zipball/1.3.0/")
              Nothing

      contains "PROBLEM DOWNLOADING PACKAGE" report
      omits "PACKAGE SOURCE UNAVAILABLE" report

  , scope "hash mismatch + renamed + re-published -> explain the rename, give commands" $ do
      let report = render dateFormat (V.Version 2 3 0) $
            Exit.PP_BadArchiveHash
              "https://github.com/ryannhg/date-format/zipball/2.3.0/"
              "70c67866fed499bec685f43f23fea279556757f2"
              "86534146f5a550bb8e87b87a7484ea5732090bb5"
              (movedAndPublished "ryan-haskell/date-format")

      contains "PACKAGE SOURCE HAS CHANGED" report
      contains "Expected: 70c67866fed499bec685f43f23fea279556757f2" report
      contains "Actual: 86534146f5a550bb8e87b87a7484ea5732090bb5" report
      contains "top-level folder named after the current owner" report
      contains "https://github.com/ryan-haskell/date-format" report
      contains "npx elm-json-lamdera uninstall ryannhg/date-format" report
      contains "npx elm-json-lamdera install ryan-haskell/date-format" report

      -- it downloaded fine; calling it corrupt sent people looking in the wrong place
      omits "CORRUPT PACKAGE DATA" report

  , scope "hash mismatch + renamed + NOT re-published -> no version can help" $ do
      let report = render orderedContainers (V.Version 2 0 0) $
            Exit.PP_BadArchiveHash
              "https://github.com/y0hy0h/ordered-containers/zipball/2.0.0/"
              "3de944919fd7d0204de1eb6462ecfede583844a8"
              "b7ff29c86bc63199324045f4ad55a475039ee6f4"
              (movedNotPublished "j-maas/ordered-containers")

      contains "PACKAGE SOURCE HAS CHANGED" report
      contains "https://github.com/j-maas/ordered-containers" report
      contains "has not been re-published" report
      -- a rename invalidates every published version at once, so "try another
      -- version" would be actively misleading here
      contains "Every published version of this package is affected" report
      omits "Switch to a version that is still published" report

  , scope "hash mismatch + no rename found -> tell them how to check for one" $ do
      let report = render dateFormat (V.Version 2 3 0) $
            Exit.PP_BadArchiveHash
              "https://github.com/ryannhg/date-format/zipball/2.3.0/"
              "70c67866fed499bec685f43f23fea279556757f2"
              "86534146f5a550bb8e87b87a7484ea5732090bb5"
              Nothing

      contains "PACKAGE SOURCE HAS CHANGED" report
      contains "Check whether the package has moved" report
      contains "https://package.elm-lang.org/packages/ryannhg/date-format/" report
      omits "the project now lives here" report

  , scope "every variant offers the ELM_HOME cache as an escape hatch" $ do
      let reports =
            [ render ziplist (V.Version 1 3 0) $
                Exit.PP_BadArchiveRequest
                  (httpStatusError 404 "https://github.com/STTR13/ziplist/zipball/1.3.0/")
                  (movedAndPublished "staeter/ziplist")
            , render ziplist (V.Version 1 3 0) $
                Exit.PP_BadArchiveRequest
                  (httpStatusError 404 "https://github.com/STTR13/ziplist/zipball/1.3.0/")
                  Nothing
            , render dateFormat (V.Version 2 3 0) $
                Exit.PP_BadArchiveHash "url" "aaa" "bbb" (movedAndPublished "ryan-haskell/date-format")
            , render dateFormat (V.Version 2 3 0) $
                Exit.PP_BadArchiveHash "url" "aaa" "bbb" Nothing
            ]

      mapM_ (contains "ELM_HOME cache") reports
  ]
