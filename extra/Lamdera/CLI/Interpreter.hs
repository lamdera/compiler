{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Interpreter where

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
import Ext.Common (getProjectRootFor)
import qualified Lamdera.Evaluate

{-
-}
run :: Args -> () -> IO ()
run (Args file expressionName) () = do
  debug_ "Starting interpreter..."
  Lamdera.Evaluate.exec file expressionName



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
