{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Format 
  ( Format(..)
  , run
  , command
  ) where

{- `lamdera format` functionality -}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Ext.ElmFormat as ElmFormat
import qualified System.Exit as Exit
import qualified System.IO as IO
import Terminal hiding (args)
import Terminal.Helpers
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Data.List as List
import qualified System.Directory as Dir
import qualified System.FilePath as FP

data Format = Format
  { _yes :: Bool
  , _validate :: Bool
  , _stdin :: Bool
  , _output :: Maybe FilePath
  , _help :: Bool
  }

yes_ :: Parser Bool
yes_ =
  Parser
    { _singular = "yes"
    , _plural = "yes"
    , _parser = \_ -> Just True
    , _suggest = \_ -> return []
    , _examples = \_ -> return []
    }

validate_ :: Parser Bool
validate_ =
  Parser
    { _singular = "validate"
    , _plural = "validate"
    , _parser = \_ -> Just True
    , _suggest = \_ -> return []
    , _examples = \_ -> return []
    }

stdin_ :: Parser Bool
stdin_ =
  Parser
    { _singular = "stdin"
    , _plural = "stdin"
    , _parser = \_ -> Just True
    , _suggest = \_ -> return []
    , _examples = \_ -> return []
    }

output_ :: Parser FilePath
output_ =
  Parser
    { _singular = "output"
    , _plural = "outputs"
    , _parser = Just
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["Main2.elm"]
    }

elmFileOrDir :: Parser FilePath
elmFileOrDir =
  Parser
    { _singular = "elm file or directory"
    , _plural = "elm files or directories"
    , _parser = Just  -- Accept any path
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["Main.elm", "src/Main.elm"]
    }

command :: Terminal.Command
command =
  let
    summary =
      "Format Elm source files."

    details =
      "Usage: lamdera format [INPUT] [--output FILE] [--yes] [--validate] [--stdin]\n\n" ++
      "  Format Elm source files."

    example =
      stack
        [ reflow "Examples:"
        , P.vcat [ P.indent 2 $ P.green "lamdera format Main.elm                     # formats Main.elm"
                 , P.indent 2 $ P.green "lamdera format Main.elm --output Main2.elm  # formats Main.elm as Main2.elm"
                 , P.indent 2 $ P.green "lamdera format src/                         # format all *.elm files in the src directory"
                 ]
        , reflow "Full guide to using elm-format at <https://github.com/avh4/elm-format>"
        ]

    formatFlags =
      flags Format
        |-- onOff "yes" "Reply 'yes' to all automated prompts."
        |-- onOff "validate" "Check if files are formatted without changing them."
        |-- onOff "stdin" "Read from stdin, output to stdout."
        |-- flag "output" output_ "Write output to FILE instead of overwriting the given source file."
        |-- onOff "help" "Show this help text."
  in
  Terminal.Command "format" (Common summary) details example (zeroOrMore elmFileOrDir) formatFlags run

run :: [FilePath] -> Format -> IO ()
run inputs flags =
  if _help flags || null inputs && not (_stdin flags)
    then do
      TIO.putStrLn "Usage: lamdera format [INPUT] [--output FILE] [--yes] [--validate] [--stdin]\n"
      TIO.putStrLn "  Format Elm source files.\n"
      TIO.putStrLn "Available options:"
      TIO.putStrLn "  --help                Show this help text"
      TIO.putStrLn "  --output FILE         Write output to FILE instead of overwriting the given"
      TIO.putStrLn "                        source file."
      TIO.putStrLn "  --yes                 Reply 'yes' to all automated prompts."
      TIO.putStrLn "  --validate            Check if files are formatted without changing them."
      TIO.putStrLn "  --stdin               Read from stdin, output to stdout.\n"
      TIO.putStrLn "Note: All flags must use double dashes (--flag). Single dashes (-flag) are not supported.\n"
      TIO.putStrLn "Examples:"
      TIO.putStrLn "  lamdera format Main.elm                     # formats Main.elm"
      TIO.putStrLn "  lamdera format Main.elm --output Main2.elm  # formats Main.elm as Main2.elm"
      TIO.putStrLn "  lamdera format src/                         # format all *.elm files in the src directory\n"
      TIO.putStrLn "Full guide to using elm-format at <https://github.com/avh4/elm-format>"
      Exit.exitSuccess
    else case inputs of
      [] | _stdin flags ->
        do
          input <- TIO.hGetContents IO.stdin
          formatText flags input
      [] ->
        do
          TIO.putStrLn "Please specify at least one .elm file to format."
          Exit.exitFailure
      paths ->
        do
          elmFilePaths <- concat <$> mapM expandPath paths
          if null elmFilePaths
            then return ()
            else do
              TIO.putStrLn "This will overwrite the following files to use Elm's preferred style:\n"
              mapM_ (\f -> TIO.putStrLn $ "    " <> T.pack f) elmFilePaths
              TIO.putStrLn "\nThis cannot be undone! Make sure to back up these files before proceeding.\n"
              if _yes flags
                then formatFiles flags elmFilePaths
                else do
                  TIO.putStrLn "Are you sure you want to overwrite these files with formatted versions? (y/n) "
                  answer <- getLine
                  case answer of
                    "Y" -> formatFiles flags elmFilePaths
                    "y" -> formatFiles flags elmFilePaths
                    "" -> formatFiles flags elmFilePaths
                    _ -> return ()

expandPath :: FilePath -> IO [FilePath]
expandPath path = do
  isDir <- Dir.doesDirectoryExist path
  if isDir
    then findElmFiles path
    else return [path | FP.isExtensionOf "elm" path]

formatText :: Format -> T.Text -> IO ()
formatText flags input =
  case ElmFormat.format "stdin" input of
    Right formatted ->
      TIO.putStr formatted
    Left err ->
      do
        TIO.putStrLn $ "Error: " <> err
        Exit.exitFailure

formatFile :: Format -> FilePath -> IO ()
formatFile flags path = do
  isDir <- Dir.doesDirectoryExist path
  if isDir
    then do
      elmFilePaths <- findElmFiles path
      if null elmFilePaths
        then return ()
        else do
          TIO.putStrLn "This will overwrite the following files to use Elm's preferred style:\n"
          mapM_ (\f -> TIO.putStrLn $ "    " <> T.pack f) elmFilePaths
          TIO.putStrLn "\nThis cannot be undone! Make sure to back up these files before proceeding.\n"
          if _yes flags
            then formatFiles flags elmFilePaths
            else do
              TIO.putStrLn "Are you sure you want to overwrite these files with formatted versions? (y/n) "
              answer <- getLine
              case answer of
                "Y" -> formatFiles flags elmFilePaths
                "y" -> formatFiles flags elmFilePaths
                "" -> formatFiles flags elmFilePaths
                _ -> return ()
    else formatSingleFile flags path

findElmFiles :: FilePath -> IO [FilePath]
findElmFiles dir = do
  contents <- Dir.listDirectory dir
  let baseName = FP.takeFileName dir
  if baseName `elem` ["elm-stuff", "node_modules"]
    then return []
    else do
      paths <- mapM (\f -> do
        let path = dir FP.</> f
        isDir <- Dir.doesDirectoryExist path
        if isDir
          then findElmFiles path
          else return [path | FP.isExtensionOf "elm" f]
        ) contents
      return $ concat paths

formatFiles :: Format -> [FilePath] -> IO ()
formatFiles flags = mapM_ (formatSingleFile flags)

formatSingleFile :: Format -> FilePath -> IO ()
formatSingleFile flags path = do
  TIO.putStrLn $ "Processing file " <> T.pack path
  input <- TIO.readFile path
  case ElmFormat.format path input of
    Right formatted -> 
      if _validate flags && formatted /= input
        then do
          TIO.putStrLn $ "File " <> T.pack path <> " would be reformatted"
          Exit.exitFailure
        else case _output flags of
          Just outputPath -> TIO.writeFile outputPath formatted
          Nothing -> TIO.writeFile path formatted
    Left err -> do
      TIO.putStrLn err
      if _validate flags
        then Exit.exitFailure
        else return ()

stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs

reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string