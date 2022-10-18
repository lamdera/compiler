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

{-

Lookup and print out the type annotation for the given file:expression.

@TODO this only works if we've already compiled successfully (no errors) before!

If we instead want to do a check compile first, we'll need to use Lamdera.Compile.make.

Note: if we do this, this annotation query will involve a full disk read of all
.elm file metadata for the entire project on every single query, as Elm tries to
figure out if there's any changes requiring an incremental compile.

@FUTURE @TRACK inotify-mode feature (https://trello.com/c/S1LmVKbK) would solve this!

-}
run :: Args -> () -> IO ()
run (Args file expressionName) () = do
  debug_ "Starting annotation..."

  elmHome <- PerUserCache.getElmHome
  root <- getProjectRoot "Lamdera.CLI.Annotate.run"
  printAnnotations root file expressionName


printAnnotations :: FilePath -> FilePath -> Name.Name -> IO ()
printAnnotations root file expressionName = do
  withCurrentDirectory root $ do
    debug_ "Getting artifacts..."

    (Compile.Artifacts canonical annotations objects) <- Ext.Query.Canonical.loadSingleArtifacts file

    case annotations & Map.lookup expressionName of
      Just annotation -> do
        formatHaskellValue ("debug AST") annotation
        putStrLn $ T.unpack $ canonicalTypeToString annotation

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


{-

= TLambda Type Type
| TVar Name
| TType ModuleName.Canonical Name [Type]
| TRecord (Map.Map Name FieldType) (Maybe Name)
| TUnit
| TTuple Type Type (Maybe Type)
| TAlias ModuleName.Canonical Name [(Name, Type)] AliasType

For operations on Text
https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html

|> == &
<| == $

-}
canonicalTypeToString :: Can.Annotation -> Text
canonicalTypeToString (Can.Forall freeVars tipe) =
  case tipe of
    TLambda t1 t2 ->
      "single" <> "concatenation"

    TVar name ->
      ["how","to","add","dashes","between"] & T.intercalate "-"

    TType moduleName name paramTypes ->
      ["how","to","join"] & T.concat

    TRecord fieldTypes extensibleName ->
      "TODO"

    TUnit ->
      "TODO"

    TTuple t1 t2 mt3 ->
      "TODO"

    TAlias moduleName name namedParamTypes alias ->
      "TODO"
