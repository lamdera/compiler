{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.TypeScript where

import Lamdera
import EasyTest
import Test.Helpers

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Process as Process
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import qualified System.Random as Random

import qualified Init
import Make (Flags(..))
import qualified Make
import qualified Ext.Common
import qualified Lamdera.Relative

-- Property-based test that generates random Elm modules
-- and verifies their TypeScript declarations compile
suite :: Test ()
suite =
  tests
    [ scope "simple TypeScript generation test" $ do
        -- Test with a known simple module first
        let simpleModule = Text.unlines
              [ "module TestModule exposing (..)"
              , ""
              , "type alias User = { name : String, age : Int }"
              , ""
              , "type Color = Red | Green | Blue"
              , ""
              , "greet : String -> String"
              , "greet name = \"Hello, \" ++ name"
              , ""
              , "addOne : Int -> Int"
              , "addOne x = x + 1"
              ]
        
        project <- io $ do
          tmpDir <- Dir.getTemporaryDirectory
          timestamp <- Random.randomRIO (1000000, 9999999) :: IO Int
          let projectDir = tmpDir </> ("elm-ts-simple-" ++ show timestamp)
          Dir.createDirectoryIfMissing True projectDir
          pure projectDir
        
        let elmHome = project </> "elm-home"
            elmStuff = project </> "elm-stuff"
            srcDir = project </> "src"
            modulePath = srcDir </> "TestModule.elm"
            jsOutput = project </> "output.js"
            dtsOutput = project </> "output.d.ts"
        
        success <- io $ do
          -- Create src directory and write module
          Dir.createDirectoryIfMissing True srcDir
          writeUtf8 modulePath simpleModule
          
          -- Initialize elm.json
          Test.Helpers.withElmHome elmHome $
            Ext.Common.withProjectRoot project $
              Init.init
          
          -- Compile with --export-all-functions
          Test.Helpers.withElmHome elmHome $
            Ext.Common.withProjectRoot project $
              Make.run ["src/TestModule.elm"] $
                Make.Flags
                  { _debug = False
                  , _optimize = False
                  , _output = Just (Make.JS jsOutput)
                  , _report = Nothing
                  , _docs = Nothing
                  , _noWire = True
                  , _optimizeLegible = False
                  , _experimentalJsTsExports = True
                  }
          
          -- Check if .d.ts was generated
          dtsExists <- Dir.doesFileExist dtsOutput
          if not dtsExists
            then do
              rmdir project
              pure False
            else do
              -- Read and verify content
              maybeDtsContent <- readUtf8Text dtsOutput
              
              case maybeDtsContent of
                Nothing -> do
                  rmdir project
                  pure False
                Just dtsContent -> do
                  -- Basic checks
                  let hasNamespace = Text.isInfixOf "export declare namespace TestModule" dtsContent
                      hasUserType = Text.isInfixOf "export type User" dtsContent
                      hasColorType = Text.isInfixOf "export type Color" dtsContent
                      hasGreetFunc = Text.isInfixOf "greet:" dtsContent
                      hasAddOneFunc = Text.isInfixOf "addOne:" dtsContent
                  
                  -- Clean up
                  rmdir project
                  
                  pure $ hasNamespace && hasUserType && hasColorType && hasGreetFunc && hasAddOneFunc
        
        expect success
        
    , scope "property: generated TypeScript declarations compile" $ do
        -- Run multiple random tests
        forM_ [1..5] $ \i -> do
          scope ("iteration " ++ show i) $ do
            -- Generate a random module
            elmModule <- io $ generateRandomElmModule
            
            -- Create a temporary project directory
            project <- io $ do
              tmpDir <- Dir.getTemporaryDirectory
              timestamp <- Random.randomRIO (1000000, 9999999) :: IO Int
              let projectDir = tmpDir </> ("elm-ts-test-" ++ show timestamp)
              Dir.createDirectoryIfMissing True projectDir
              pure projectDir
            
            let elmHome = project </> "elm-home"
                elmStuff = project </> "elm-stuff"
                srcDir = project </> "src"
                modulePath = srcDir </> "TestModule.elm"
                jsOutput = project </> "output.js"
                dtsOutput = project </> "output.d.ts"
            
            success <- io $ do
              -- Clean up any existing directories
              rmdir elmHome
              rmdir elmStuff
              rmdir srcDir
              
              -- Create src directory and write module
              Dir.createDirectoryIfMissing True srcDir
              writeUtf8 modulePath elmModule
              
              -- Initialize elm.json
              Test.Helpers.withElmHome elmHome $
                Ext.Common.withProjectRoot project $
                  Init.init
              
              -- Compile with --export-all-functions
              Test.Helpers.withElmHome elmHome $
                Ext.Common.withProjectRoot project $
                  Make.run ["src/TestModule.elm"] $
                    Make.Flags
                      { _debug = False
                      , _optimize = False
                      , _output = Just (Make.JS jsOutput)
                      , _report = Nothing
                      , _docs = Nothing
                      , _noWire = True
                      , _optimizeLegible = False
                      , _experimentalJsTsExports = True
                      }
              
              -- Check if .d.ts was generated
              dtsExists <- Dir.doesFileExist dtsOutput
              if not dtsExists
                then do
                  rmdir project
                  pure False
                else do
                  -- Verify TypeScript compilation
                  -- Create a minimal tsconfig.json
                  let tsConfig = "{ \"compilerOptions\": { \"noEmit\": true, \"strict\": true } }"
                  writeUtf8 (project </> "tsconfig.json") tsConfig
                  
                  -- Create a minimal package.json to avoid npx warnings
                  let packageJson = "{ \"name\": \"test\", \"version\": \"1.0.0\" }"
                  writeUtf8 (project </> "package.json") packageJson
                  
                  -- Run tsc directly if available, otherwise use npx
                  tscExists <- Dir.findExecutable "tsc"
                  (exitCode, _, stderr) <- case tscExists of
                    Just tscPath -> Process.readProcessWithExitCode 
                      tscPath 
                      ["--noEmit", dtsOutput] 
                      ""
                    Nothing -> Process.readProcessWithExitCode 
                      "npx" 
                      ["--yes", "typescript@latest", "tsc", "--noEmit", dtsOutput] 
                      ""
                  
                  -- Clean up
                  rmdir project
                  
                  case exitCode of
                    Exit.ExitSuccess -> pure True
                    Exit.ExitFailure _ -> do
                      -- For debugging
                      putStrLn $ "\nGenerated Elm module:"
                      Text.putStrLn elmModule
                      putStrLn $ "\nTypeScript compilation failed: " ++ stderr
                      pure False
            
            expect success
    ]

-- Generate a random valid Elm module with limited complexity
generateRandomElmModule :: IO Text.Text
generateRandomElmModule = do
  -- Generate random module components
  moduleName <- pure "TestModule"
  
  -- Generate random type definitions
  numTypes <- randomInt 0 3
  typeDecls <- mapM generateTypeDecl [1..numTypes]
  
  -- Generate random functions
  numFunctions <- randomInt 1 5
  functionDecls <- mapM generateFunctionDecl [1..numFunctions]
  
  -- Build the module
  let moduleText = Text.unlines $
        [ "module " <> moduleName <> " exposing (..)"
        , ""
        , "-- Auto-generated test module"
        , ""
        ] ++ typeDecls ++ [""] ++ functionDecls
  
  pure moduleText

-- Generate a random type declaration
generateTypeDecl :: Int -> IO Text.Text
generateTypeDecl n = do
  typeKind <- randomInt 1 3
  case typeKind of
    1 -> generateTypeAlias n
    2 -> generateCustomType n
    _ -> generateRecordAlias n

-- Generate a type alias
generateTypeAlias :: Int -> IO Text.Text
generateTypeAlias n = do
  baseType <- randomBaseType
  pure $ "type alias Type" <> Text.pack (show n) <> " = " <> baseType

-- Generate a custom type (ADT)
generateCustomType :: Int -> IO Text.Text
generateCustomType n = do
  numConstructors <- randomInt 1 3
  constructors <- mapM (generateConstructor n) [1..numConstructors]
  pure $ "type CustomType" <> Text.pack (show n) <> " = " <> 
         Text.intercalate " | " constructors

-- Generate a constructor
generateConstructor :: Int -> Int -> IO Text.Text
generateConstructor typeNum ctorNum = do
  numArgs <- randomInt 0 2
  if numArgs == 0
    then pure $ "Ctor" <> Text.pack (show typeNum) <> "_" <> Text.pack (show ctorNum)
    else do
      args <- mapM (const randomBaseType) [1..numArgs]
      pure $ "Ctor" <> Text.pack (show typeNum) <> "_" <> Text.pack (show ctorNum) <> 
             " " <> Text.intercalate " " args

-- Generate a record type alias
generateRecordAlias :: Int -> IO Text.Text
generateRecordAlias n = do
  numFields <- randomInt 1 3
  fields <- mapM generateRecordField [1..numFields]
  pure $ "type alias Record" <> Text.pack (show n) <> " = { " <> 
         Text.intercalate ", " fields <> " }"

-- Generate a record field
generateRecordField :: Int -> IO Text.Text
generateRecordField n = do
  fieldType <- randomBaseType
  pure $ "field" <> Text.pack (show n) <> " : " <> fieldType

-- Generate a function declaration
generateFunctionDecl :: Int -> IO Text.Text
generateFunctionDecl n = do
  numArgs <- randomInt 0 3
  resultType <- randomBaseType
  
  let funcName = "func" <> Text.pack (show n)
      args = map (\i -> "arg" <> Text.pack (show i)) [1..numArgs]
      argTypes = replicate numArgs "Int"
      
      typeSignature = funcName <> " : " <> 
                      Text.intercalate " -> " (argTypes ++ [resultType])
      
      implementation = if numArgs == 0
                       then funcName <> " = " <> defaultValueForType resultType
                       else funcName <> " " <> Text.intercalate " " args <> 
                            " = " <> defaultValueForType resultType
  
  pure $ Text.unlines [typeSignature, implementation]

-- Generate a random base type
randomBaseType :: IO Text.Text
randomBaseType = do
  typeChoice <- randomInt 1 6
  case typeChoice of
    1 -> pure "Int"
    2 -> pure "String"
    3 -> pure "Bool"
    4 -> pure "Float"
    5 -> do
      innerType <- randomSimpleType
      pure $ "List " <> innerType
    _ -> do
      innerType <- randomSimpleType
      pure $ "Maybe " <> innerType

-- Generate a simple type (no nested generics)
randomSimpleType :: IO Text.Text
randomSimpleType = do
  typeChoice <- randomInt 1 4
  case typeChoice of
    1 -> pure "Int"
    2 -> pure "String"
    3 -> pure "Bool"
    _ -> pure "Float"

-- Get default value for a type
defaultValueForType :: Text.Text -> Text.Text
defaultValueForType t
  | t == "Int" = "0"
  | t == "Float" = "0.0"
  | t == "String" = "\"\""
  | t == "Bool" = "False"
  | Text.isPrefixOf "List" t = "[]"
  | Text.isPrefixOf "Maybe" t = "Nothing"
  | otherwise = "0"  -- fallback

-- Generate a random integer in range
randomInt :: Int -> Int -> IO Int
randomInt minVal maxVal = Random.randomRIO (minVal, maxVal)