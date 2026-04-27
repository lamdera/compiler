{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Develop.Generate.Help
  ( makePageHtml
  , makeCodeHtml
  )
  where


import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Name as Name

import qualified Json.Encode as Encode
import Literals (b)

import qualified Lamdera.Live
import qualified Lamdera.UiSourceMap
import qualified Data.Text.Encoding as T

-- PAGES


makePageHtml :: Name.Name -> Maybe Encode.Value -> B.Builder
makePageHtml moduleName maybeFlags =
  [b|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=5.0, minimum-scale=1.0">
  <link type="text/css" rel="stylesheet" href="/_elm/styles.css">
  <script src="/_elm/elm.js"></script>
</head>
<body>
<script>
var app = // @LAMDERA
Elm.|] <> Name.toBuilder moduleName <> [b|.init({ flags: |] <> maybe "undefined" Encode.encode maybeFlags <> [b| });
if (app.ports && app.ports.jumpTo) app.ports.jumpTo.subscribe(node => { |] <> T.encodeUtf8Builder Lamdera.UiSourceMap.openEditorSrc <> [b| }); // @LAMDERA
|] <> Lamdera.Live.lamderaLiveSrc <> Lamdera.UiSourceMap.src <> [b|
</script>
</body>
</html>
|]



-- CODE


makeCodeHtml :: FilePath -> B.Builder -> B.Builder
makeCodeHtml title code =
  [b|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=5.0, minimum-scale=1.0">
  <title>|] <> B.stringUtf8 title <> [b|</title>
  <style type="text/css">
    @import url(/_elm/source-code-pro.ttf);
    html, head, body, pre { margin: 0; height: 100%; }
    body { font-family: "Source Code Pro", monospace; }
  </style>
  <link type="text/css" rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/styles/default.min.css">
  <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/highlight.min.js"></script>
  <script>if (hljs) { hljs.initHighlightingOnLoad(); }</script>
</head>
<body style="background-color: #F0F0F0;">
<pre><code>|] <> code <> Lamdera.UiSourceMap.src <> [b|</code></pre>
</body>
</html>
|]
