{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Annotate where

import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Stuff as PerUserCache
import qualified Reporting
import qualified Reporting.Doc as D
import Terminal (Parser(..))

import qualified Data.Name as Name
import qualified AST.Canonical as Can
import AST.Canonical (Type(..))
import qualified Compile

import Lamdera
import Lamdera.Progress
import qualified Ext.Query.Canonical
import qualified Ext.Common

import System.FilePath ((</>))
import qualified AST.Source as Src
import qualified Reporting.Doc
import qualified Reporting.Render.Type.Localizer as Localizer
import qualified Reporting.Render.Type
{-

Lookup and print out the type annotation for the given file:expression.

Note: currently Ext.Query.Canonical.loadSingleArtifacts will try compile the file,
so this annotation query will involve a full disk read of all .elm file metadata
for the entire project on every single query, as Elm tries to figure out if there's
any changes requiring an incremental compile.

@FUTURE in the in-memory daemon mode we will have equivalent functions that use the cache

-}
run :: Args -> () -> IO ()
run (Args file expressionName) () = do
  debug_ "Starting annotation..."

  elmHome <- PerUserCache.getElmHome
  root <- getProjectRoot "Lamdera.CLI.Annotate.run"
  printAnnotations root file expressionName


printAnnotationsTest :: FilePath -> FilePath -> String -> IO ()
printAnnotationsTest root file expressionName =
  printAnnotations root file (Name.fromChars expressionName)

printAnnotations :: FilePath -> FilePath -> Name.Name -> IO ()
printAnnotations root file expressionName = do
  -- we need to get the module so that we can localize the types based on the 
  -- imports of the current module
  (_, modul) <- Ext.Query.Canonical.loadFileSource (root </> file)

  Ext.Common.withProjectRoot root $ do
    debug_ "Getting artifacts..."

    (Compile.Artifacts canonical annotations objects) <- Ext.Query.Canonical.loadSingleArtifacts file

    case annotations & Map.lookup expressionName of
      Just annotation -> do
        hindentPrint annotation
        putStrLn $ "----------------------------------------"
        putStrLn $ canonicalAnnotationToString modul annotation

      Nothing ->
        putStrLn "Oops! Something went wrong!"

  pure ()


-- Args helpers

data Args = Args FilePath Name.Name

expressionName :: Parser Name.Name
expressionName =
  Parser
    { _singular = "expression name"
    , _plural = "expression names"
    , _parser = parseExpressionName
    , _suggest = suggestExpressionName
    , _examples = return . exampleExpressionNames
    }

parseExpressionName :: String -> Maybe Name.Name
parseExpressionName chars =
  Just $ Name.fromChars chars

suggestExpressionName :: String -> IO [String]
suggestExpressionName _ =
  return []


exampleExpressionNames :: String -> [String]
exampleExpressionNames chars =
  ["add", "addOneMore", "map3"]


-- Convert a Canonical Annotation into a String

canonicalAnnotationToString :: Src.Module -> Can.Annotation -> String
canonicalAnnotationToString modul (Can.Forall freeVars tipe) =
  let 
    localizer = 
      Localizer.fromModule modul
    
    context = 
      Reporting.Render.Type.None
  in
  tipe
  & Reporting.Render.Type.canToDoc localizer context
  & Reporting.Doc.toString
