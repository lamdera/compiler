{-# LANGUAGE OverloadedStrings #-}

module Test.Ext.Filewatch where

import qualified System.Directory as Dir
import System.FilePath ((</>))

import EasyTest

import qualified Ext.Filewatch as Filewatch


all = EasyTest.run suite


suite :: Test ()
suite = tests $
  let
    -- A scratch dir to write .lamdera-ignore fixtures into.
    scratch = "./test/tmp-filewatch"

    withIgnoreFile :: String -> ([String] -> IO a) -> IO a
    withIgnoreFile contents f = do
      Dir.createDirectoryIfMissing True scratch
      writeFile (scratch </> Filewatch.ignoreFilename) contents
      patterns <- Filewatch.loadIgnorePatterns scratch
      result <- f patterns
      Dir.removeDirectoryRecursive scratch
      pure result
  in
  [ scope "loadIgnorePatterns: absent file yields no patterns" $ do
      patterns <- io $ do
        Dir.createDirectoryIfMissing True scratch
        let path = scratch </> Filewatch.ignoreFilename
        exists <- Dir.doesFileExist path
        onlyWhenIO exists $ Dir.removeFile path
        p <- Filewatch.loadIgnorePatterns scratch
        Dir.removeDirectoryRecursive scratch
        pure p
      expectEqual [] patterns

  , scope "loadIgnorePatterns: reads one pattern per line" $ do
      patterns <- io $ withIgnoreFile ".worktrees\ndist\n" pure
      expectEqual [".worktrees", "dist"] patterns

  , scope "loadIgnorePatterns: skips comments, blanks and surrounding whitespace" $ do
      patterns <- io $ withIgnoreFile "# a comment\n\n  .playwright-mcp  \n\n#another\ndist\n" pure
      expectEqual [".playwright-mcp", "dist"] patterns

  , scope "loadIgnorePatterns: a file of only comments yields no patterns" $ do
      patterns <- io $ withIgnoreFile "# nothing to see here\n\n" pure
      expectEqual [] patterns

  , scope "shouldRefreshPath: ordinary source changes still trigger" $ do
      expect $ Filewatch.shouldRefreshPath [] "/project/src/Frontend.elm"
      expect $ Filewatch.shouldRefreshPath [] "/project/src/Types.elm"
      expect $ Filewatch.shouldRefreshPath [] "/project/head.html"

  , scope "shouldRefreshPath: built-in ignores are skipped" $ do
      expect $ not $ Filewatch.shouldRefreshPath [] "/project/.git/index"
      expect $ not $ Filewatch.shouldRefreshPath [] "/project/elm-stuff/0.19.1/Main.elmo"
      expect $ not $ Filewatch.shouldRefreshPath [] "/project/node_modules/x/index.js"
      expect $ not $ Filewatch.shouldRefreshPath [] "/project/tests/Spec.elm"

  , scope "shouldRefreshPath: user patterns are skipped" $ do
      let ignores = [".worktrees", ".playwright-mcp"]
      expect $ not $ Filewatch.shouldRefreshPath ignores "/project/.worktrees/fork/src/Frontend.elm"
      expect $ not $ Filewatch.shouldRefreshPath ignores "/project/.playwright-mcp/console.log"

  , scope "shouldRefreshPath: user patterns don't over-match real source" $ do
      let ignores = [".worktrees", ".playwright-mcp"]
      expect $ Filewatch.shouldRefreshPath ignores "/project/src/Frontend.elm"
      expect $ Filewatch.shouldRefreshPath ignores "/project/src/Worktrees.elm"

  , scope "shouldRefreshPath: no patterns means built-in behaviour is unchanged" $ do
      let path = "/project/.playwright-mcp/console.log"
      expect $ Filewatch.shouldRefreshPath [] path

  , scope "loaded patterns feed shouldRefreshPath end to end" $ do
      (ignored, kept) <- io $ withIgnoreFile "# tooling output\n.playwright-mcp\n.worktrees\n" $ \patterns ->
        pure
          ( Filewatch.shouldRefreshPath patterns "/project/.playwright-mcp/console.log"
          , Filewatch.shouldRefreshPath patterns "/project/src/Frontend.elm"
          )
      expectEqual False ignored
      expectEqual True kept
  ]


onlyWhenIO :: Bool -> IO () -> IO ()
onlyWhenIO condition io_ =
  if condition then io_ else pure ()
