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

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Package
import qualified Data.Utf8 as Utf8
import qualified AST.Source as Src
import qualified Reporting.Render.Type.Localizer as Localizer
import System.FilePath
import AST.Canonical (FieldType(..))

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
        putStrLn $ T.unpack $ canonicalAnnotationToString modul annotation

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

canonicalAnnotationToString :: Src.Module -> Can.Annotation -> Text
canonicalAnnotationToString modul (Can.Forall freeVars tipe) =
  let 
    localizer = 
      Localizer.fromModule modul
    
    localize moduleName name = 
      Localizer.toChars localizer moduleName name & T.pack
  in
  case tipe of 
    TLambda t1 t2 ->
      -- within a type annotation, lambdas need to be wrapped in parentheses, 
      -- but if the whole expression is a lambda, we can skip the parens.
      tLambdaToString localize t1 t2

    _ -> 
      -- otherwise, we can proceed as normal
      typeToString localize tipe


typeToString :: (ModuleName.Canonical -> Name.Name -> Text) -> Type -> Text
typeToString localize tipe = 
  case tipe of
    TLambda t1 t2 ->
      "(" <> tLambdaToString localize t1 t2 <> ")"

    TVar name ->
      nameToString name

    TType moduleName name paramTypes ->
      tTypeToString localize moduleName name paramTypes

    TRecord fieldTypes extensibleName ->
      tRecordToString localize fieldTypes extensibleName

    TUnit ->
      "()"

    TTuple t1 t2 mt3 ->
      tTupleToString localize t1 t2 mt3 

    TAlias moduleName name namedParamTypes alias ->
      tAliasToString localize moduleName name namedParamTypes alias 


-- typeToString helpers

tLambdaToString :: (ModuleName.Canonical -> Name.Name -> Text) -> Type -> Type -> Text
tLambdaToString localize t1 t2 =
  case t2 of 
    TLambda t2a t2b ->
      -- if the next thing is also a lambda, we don't need to wrap it in 
      -- parentheses because (->) is right-associative
      typeToString localize t1 <> " -> " <> tLambdaToString localize t2a t2b

    _ ->
      typeToString localize t1 <> " -> " <> typeToString localize t2


tTypeToString :: (ModuleName.Canonical -> Name.Name -> Text) -> ModuleName.Canonical -> Name.Name -> [Type] -> Text
tTypeToString localize moduleName name paramTypes = 
      let 
          paramString =
            paramTypes 
              & map (parenthesizeParams localize)
              & T.intercalate " "
          
          spacedParamString = 
            if paramString == "" then 
              "" 
            else 
              " " <> paramString
      in
      localize moduleName name <> spacedParamString


tRecordToString :: (ModuleName.Canonical -> Name.Name -> Text) -> Map.Map Name.Name FieldType -> Maybe Name.Name -> Text
tRecordToString localize fieldTypes extensibleName =
  let
    fieldsString = 
      fieldTypes
        & Map.toList
        & map fieldNameAndTypeToString
        & T.intercalate ", "
    
    fieldNameAndTypeToString (name, FieldType _ tipe) = 
      nameToString name <> " : " <> typeToString localize tipe
  in
  case extensibleName of 
    Nothing ->
      if fieldsString == "" then 
        "{}" 
      else 
        "{ " <> fieldsString <> " }"

    Just name ->
      "{ " <> nameToString name <> " | " <> fieldsString <> " }"


tTupleToString :: (ModuleName.Canonical -> Name.Name -> Text) -> Type -> Type -> Maybe Type -> Text
tTupleToString localize t1 t2 mt3 =
  case mt3 of
    Just t3 ->
      "( " 
        <> typeToString localize t1 
        <> ", " 
        <> typeToString localize t2 
        <> ", " 
        <> typeToString localize t3 
        <> " )"

    Nothing ->
      "( " 
        <> typeToString localize t1 
        <> ", " 
        <> typeToString localize t2 
        <> " )"


tAliasToString :: (ModuleName.Canonical -> Name.Name -> Text) -> ModuleName.Canonical -> Name.Name -> [( Name.Name, Type )] -> alias -> Text
tAliasToString localize moduleName name namedParamTypes alias =
  -- I don't understand what the alias is for, so it's unused in this function
  case namedParamTypes of 
    [] ->
      localize moduleName name 

    _ ->
      let 
        paramString = 
          namedParamTypes 
            -- I don't understand why the params need to have names.
            -- Let's throw the names away for now...
            & map snd
            & map (parenthesizeParams localize)
            & T.intercalate " " 
      in
      localize moduleName name  <> " " <> paramString


parenthesizeParams :: (ModuleName.Canonical -> Name.Name -> Text) -> Type -> Text
parenthesizeParams localize paramType = 
  -- the params of an alias or custom type need to be parenthesized if they 
  -- themselves are aliases or custom types that have params. For example, 
  -- `Maybe (Result x a)` needs parentheses, but `Maybe Char` does not.
  let 
    paramTypeString = 
      typeToString localize paramType
  in
    case paramType of 
      TType moduleName name (h:t) ->
        "(" <> paramTypeString <> ")"

      TAlias moduleName name (h:t) alias ->
        "(" <> paramTypeString <> ")"

      _ ->
        paramTypeString


nameToString :: Name.Name -> Text
nameToString name =
  T.pack $ Utf8.toChars name
