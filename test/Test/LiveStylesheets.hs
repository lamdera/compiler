{-# LANGUAGE OverloadedStrings #-}

module Test.LiveStylesheets where

import EasyTest

import qualified Lamdera.CLI.Live as Live


all = EasyTest.run suite


{-| Which file changes `lamdera live` swaps in the browser as stylesheets, without a
recompile or a page reload, see Live.stylesheetOnlyChange. -}
suite :: Test ()
suite = tests $
  let
    roots = ["/tmp/app", "/private/tmp/app"]
    change = Live.stylesheetOnlyChange roots
    present path = (path, True)
    gone path = (path, False)
  in
  [ scope "a stylesheet in public/ is served from /" $
      expectEqual (Just ["/style.css"]) (change [present "/tmp/app/public/style.css"])

  , scope "public/ subfolders keep their path" $
      expectEqual (Just ["/css/layout.css"]) (change [present "/tmp/app/public/css/layout.css"])

  , scope "a stylesheet outside public/ is served at its own path" $
      expectEqual (Just ["/assets/app.css"]) (change [present "/tmp/app/assets/app.css"])

  , scope "the canonical root, as the watcher reports on macOS" $
      expectEqual (Just ["/style.css"]) (change [present "/private/tmp/app/public/style.css"])

  , scope "several stylesheets, each once" $
      expectEqual (Just ["/a.css", "/b.CSS"])
        (change [present "/tmp/app/public/a.css", present "/tmp/app/public/b.CSS", present "/tmp/app/public/a.css"])

  , scope "any other file keeps the recompile" $ do
      expectEqual Nothing (change [present "/tmp/app/public/style.css", present "/tmp/app/src/Frontend.elm"])
      expectEqual Nothing (change [present "/tmp/app/public/style.css", present "/tmp/app/head.html"])
      expectEqual Nothing (change [present "/tmp/app/public/logo.png"])
      expectEqual Nothing (change [present "/tmp/app/public/app.js"])

  , scope "no changes keeps the recompile" $
      expectEqual Nothing (change [])

  , scope "what editors leave behind while saving is left out" $ do
      -- vim
      expectEqual (Just ["/style.css"])
        (change [gone "/tmp/app/public/4913", gone "/tmp/app/public/style.css~", present "/tmp/app/public/style.css"])
      -- JetBrains
      expectEqual (Just ["/style.css"])
        (change [gone "/tmp/app/public/style.css___jb_tmp___", gone "/tmp/app/public/style.css___jb_old___", present "/tmp/app/public/style.css"])
      -- sed -i
      expectEqual (Just ["/style.css"])
        (change [gone "/tmp/app/public/.!35924!style.css", present "/tmp/app/public/style.css"])

  , scope "a removed Elm source keeps the recompile" $
      expectEqual Nothing (change [gone "/tmp/app/src/Old.elm", present "/tmp/app/public/style.css"])

  , scope "a removed file alone keeps the recompile" $
      expectEqual Nothing (change [gone "/tmp/app/public/logo.png"])

  , scope "removed stylesheets are still swapped when nothing else is there" $
      expectEqual (Just ["/old.css"]) (change [gone "/tmp/app/public/old.css"])

  , scope "the stylesheets of a change that recompiles are swapped too" $
      expectEqual ["/style.css"]
        (Live.changedStylesheets roots
          [present "/tmp/app/src/Frontend.elm", present "/tmp/app/public/style.css", gone "/tmp/app/public/style.css~"])
  ]
