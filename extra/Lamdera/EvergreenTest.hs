{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.EvergreenTest where


import qualified AST.Canonical as Can
import AST.Module.Name (Canonical(..))
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type


import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Char
import qualified Data.Map as Map
import Data.Map.Strict (unionWithKey)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import Data.List.Index (imap)
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Elm.Interface as Interface
-- import qualified Reporting.Progress as Progress
-- import qualified Stuff.Paths as Paths

import qualified Data.ByteString.Char8 as BS8

-- import qualified File.IO as File
import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))
import CanSer.CanSer as CanSer

import qualified AST.Valid as Valid
import qualified Elm.Compiler.Module as Module
import qualified Reporting.Result as Result

import qualified Reporting.Error.LamderaError as LamderaError
import qualified Reporting.Doc as D
import qualified Reporting.Error as Error

--- Need to clean up the above types, many are unused.
import System.FilePath ((</>))
import System.Process (readProcess)
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map

import Lamdera
import Lamdera.Types

import NeatInterpolation
import EasyTest
import qualified Lamdera.Test
import Lamdera.Evergreen


generationFileCheck originalFile generatedFile expectedOutput = do
  -- original <- liftIO $ readUtf8Text originalFile
  generatedM <- liftIO $ readUtf8Text generatedFile

  case generatedM of
    Just generated ->
      expectEqualTextTrimmed generated expectedOutput

    Nothing ->
      crash $ "Could not read generated file: " <> generatedFile


suite :: Test ()
suite = tests
  [ scope "mergeAllImports" $ do
      expectEqual
        (mergeAllImports [Map.fromList [("Derp","Derp")], Map.fromList [("Herp","Herp")]])
        (Map.fromList [("Derp","Derp"), ("Herp","Herp")])

  , scope "addImports" $ do
      let
        moduleName = (ModuleName.Canonical (Pkg.Name "author" "pkg") (N.Name "Types"))
        imports = Map.singleton "Derping" "Derping"

        fts :: ElmFilesText
        fts =
          (Map.fromList
            [ ( "Something"
              , ElmFileText
                  { imports = ["Existing"]
                  , types = []
                  }
              )
            ]
          )

        expected :: ElmFilesText
        expected =
          (Map.fromList
            [ ( "Types"
              , ElmFileText
                  { imports = ["Derping"]
                  , types = []
                  }
              )
            , ( "Something"
              , ElmFileText
                  { imports = ["Existing"]
                  , types = []
                  }
              )
            ]
          )
      expectEqual
        (addImports moduleName imports fts)
        expected


  , scope "alltypes e2e to disk for lamdera/test/v1/" $ do

      liftIO $ Lamdera.Test.checkWithParams "/Users/mario/lamdera/test/v1" "testapp"

      generationFileCheck
        "/Users/mario/lamdera/test/v1/src/Types.elm"
        "/Users/mario/lamdera/test/v1/src/Evergreen/Types/V1/Types.elm"
        [text|
          module Evergreen.Types.V1.Types exposing (..)

          import Browser.Navigation
          import Dict
          import Http
          import Lamdera
          import Set
          import Time
          import WireTypes


          type alias FrontendModel =
              { key : Browser.Navigation.Key
              , counter : Int
              , sessionId : String
              , clientId : String
              , timestamps : (List
              { label : String
              , time : Time.Posix
              })
              , lastReceived : Time.Posix
              , subCounter : Int
              , rpcRes : (Result Http.Error Int)
              }


          type Role
              = Admin
              | User


          type alias Record =
              { name : String
              , age : Int
              , roles : (List Role)
              }


          type alias BackendModel =
              { counter : Int
              , clients : (Set Lamdera.ClientId)
              , currentTime : Time.Posix
              , benchList : (List Int)
              , benchDictRec : (Dict String Record)
              }


          type FrontendMsg
              = Increment
              | Decrement
              | RequestedSnapshot
              | RequestedRestore
              | RequestedMemcheck
              | RequestedGc
              | RequestedUpgrade
              | TestFEWire
              | HttpFinished (Result Http.Error ())
              | Stamp FrontendMsg
              | Test1000
              | Test10000
              | Start FrontendMsg Time.Posix
              | Finish String Time.Posix
              | SubCounterIncremented Time.Posix
              | GrowBenchListClicked Int
              | ClearBenchListClicked
              | GrowBenchDictRecClicked Int
              | ClearBenchDictRecClicked
              | FNoop
              | ExternalCustom_ (WireTypes.ExternalCustom String)
              | ExternalRecord_ (WireTypes.ExternalRecord (WireTypes.AnotherParamRecord Int))
              | ExternalAlias WireTypes.ExternalAliasTuple
              | TestRPC
              | RPCRes (Result Http.Error Int)


          type ToBackend
              = ClientJoin
              | CounterIncremented
              | CounterDecremented
              | GrowBenchList Int
              | GrowBenchDictRec Int
              | ClearBenchList
              | ClearBenchDictRec


          type BackendMsg
              = NewTime Time.Posix
              | DelayedNewValue Lamdera.SessionId Lamdera.ClientId
              | Noop


          type ToFrontend
              = CounterNewValue Int Lamdera.SessionId Lamdera.ClientId
              | BenchStats
              { benchListSize : Int
              }

        |]

      generationFileCheck
        "/Users/mario/lamdera/test/v1/src/WireTypes.elm"
        "/Users/mario/lamdera/test/v1/src/Evergreen/Types/V1/WireTypes.elm"
        [text|
          module Evergreen.Types.V1.WireTypes exposing (..)

          import Array
          import Dict
          import Set
          import Time


          type ExternalCustom threadedTvar
              = AlphabeticallyLast
              | AlphabeticallyFirst
              | AlphabeticallyKMiddleThreaded threadedTvar


          type alias SubRecord threadedTvar =
              { subtype : Int
              , threaded : threadedTvar
              }


          type alias SubSubRecordAlias threadedTvar = (SubRecord threadedTvar)


          type alias SubRecordAlias threadedTvar = (SubSubRecordAlias threadedTvar)


          type alias AliasInt = Int


          type alias AllTypes =
              { int : Int
              , float : Float
              , bool : Bool
              , char : Char
              , string : String
              , listInt : (List Int)
              , setFloat : (Set Float)
              , arrayString : (Array String)
              , dict : (Dict String (List Int))
              , time : Time.Posix
              , order : Order
              , union : AllUnion
              , unit : ()
              }


          type alias SubRecursiveRecord =
              { recurse : AllTypes
              }


          type AllUnion
              = ValueStandalone
              | ValueInt Int
              | ValueFloat Float
              | ValueBool Bool
              | ValueChar Char
              | ValueString String
              | ValueListBool (List Bool)
              | ValueSetFloat (Set Float)
              | ValueArrayString (Array String)
              | ValueDict (Dict String (List Int))
              | ValueTime Time.Posix
              | ValueOrder Order
              | ValueUnit ()
              | ValueAliased AliasInt
              | ValueRecursive AllUnion
              | ValueSubRecursive SubRecursiveRecord
              | ValueTwoParams Bool Char
              | ValueTuple (Int, String)
              | ValueTriple (Int, String)
              | ValueResult (Result String Int)
              | ValueAll AllTypes


          type alias ExternalRecord threadedTvar =
              { ext : (ExternalCustom threadedTvar)
              , sub : (SubRecordAlias threadedTvar)
              , alphaFirst : Int
              , allUnion : AllUnion
              }


          type alias ExternalAliasTuple = (Float, Bool)
        |]

  , scope "testing2" $ do

      expectEqual "blah" "blah"
  , scope "testing3" $ do

      expectEqual "blah" "blah"
      -- migrations <- io $ getMigrationsSequence ["V2.elm"] (WithoutMigrations 3)
      -- show migrations
      --   & expectEqual "[[WithMigrations 1,WithMigrations 2,WithoutMigrations 3],[WithMigrations 2,WithoutMigrations 3],[WithoutMigrations 3]]"

  ]
