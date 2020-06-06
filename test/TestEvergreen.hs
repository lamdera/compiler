{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module TestEvergreen where


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
import qualified Data.Set as Set

import Lamdera
import Lamdera.Types

import NeatInterpolation
import EasyTest
import qualified TestLamdera
import Lamdera.Evergreen


all = run suite


generationFileCheck originalFile generatedFile expectedOutput = do
  -- original <- liftIO $ readUtf8Text originalFile
  generatedM <- liftIO $ readUtf8Text generatedFile

  case generatedM of
    Just generated ->
      expectEqualTextTrimmed generated expectedOutput

    Nothing ->
      crash $ "Could not read generated file: " <> generatedFile


suite :: Test ()
suite = tests $
  let
    derpImp = ModuleName.Canonical (Pkg.Name "author" "project") (N.Name "Derp")
    herpImp = ModuleName.Canonical (Pkg.Name "author" "project") (N.Name "Herp")
  in
  [ scope "mergeAllImports" $ do
      expectEqual
        (mergeAllImports [Set.singleton derpImp, Set.singleton herpImp])
        (Set.fromList [derpImp, herpImp])

  , scope "addImports" $ do
      let
        moduleName = (ModuleName.Canonical (Pkg.Name "author" "pkg") (N.Name "Types"))
        imports = Set.singleton derpImp

        fts :: ElmFilesText
        fts =
          (Map.fromList
            [ ( "Something", ElmFileText { imports = Set.singleton herpImp , types = [] } ) ]
          )

        expected :: ElmFilesText
        expected =
          (Map.fromList
            [ ( "Types", ElmFileText { imports = Set.singleton derpImp , types = [] } )
            , ( "Something", ElmFileText { imports = Set.singleton herpImp , types = [] } )
            ]
          )
      expectEqual
        (addImports moduleName imports fts)
        expected


  , scope "alltypes e2e to disk for lamdera/test/v1/" $ do

      liftIO $ TestLamdera.snapshotWithParams 1 "/Users/mario/lamdera/test/v1" "test-local"

      scope "-> Types.elm" $
        generationFileCheck
        "/Users/mario/lamdera/test/v1/src/Types.elm"
        "/Users/mario/lamdera/test/v1/src/Evergreen/V1/Types.elm"
        [text|
          module Evergreen.V1.Types exposing (..)

          import Browser.Navigation
          import Dict
          import Evergreen.V1.Fusion
          import Http
          import Lamdera
          import Set
          import Time
          import Evergreen.V1.WireTypes


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
              , backendTtype : (Maybe Evergreen.V1.Fusion.TType)
              , backendVal : Evergreen.V1.Fusion.VType
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
              , clients : (Set.Set Lamdera.ClientId)
              , currentTime : Time.Posix
              , benchList : (List Int)
              , benchDictRec : (Dict.Dict String Record)
              , allTypes : Evergreen.V1.WireTypes.AllTypes
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
              | ExternalCustom_ (Evergreen.V1.WireTypes.ExternalCustom String)
              | ExternalCustomBasic_ Evergreen.V1.WireTypes.ExternalCustomBasic
              | ExternalRecord_ (Evergreen.V1.WireTypes.ExternalRecord (Evergreen.V1.WireTypes.AnotherParamRecord Int))
              | ExternalAlias Evergreen.V1.WireTypes.ExternalAliasTuple
              | TestRPC
              | RPCRes (Result Http.Error Int)
              | EditLocal Evergreen.V1.Fusion.FType
              | FusionQuery (List Evergreen.V1.Fusion.FType)


          type ToBackend
              = ClientJoin
              | CounterIncremented
              | CounterDecremented
              | GrowBenchList Int
              | GrowBenchDictRec Int
              | ClearBenchList
              | ClearBenchDictRec
              | PatchedQuery Evergreen.V1.Fusion.FType
              | FusionedQuery (List Evergreen.V1.Fusion.FType)


          type BackendMsg
              = NewTime Time.Posix
              | DelayedNewValue Lamdera.SessionId Lamdera.ClientId
              | Noop


          type ToFrontend
              = CounterNewValue Int Lamdera.SessionId Lamdera.ClientId
              | BackendModelTypeInfo Evergreen.V1.Fusion.TType
              | ReceivedBackendLType Evergreen.V1.Fusion.VType
              | BenchStats
              { benchListSize : Int
              }

        |]

      scope "-> WireTypes.elm" $
        generationFileCheck
        "/Users/mario/lamdera/test/v1/src/WireTypes.elm"
        "/Users/mario/lamdera/test/v1/src/Evergreen/V1/WireTypes.elm"
        [text|
          module Evergreen.V1.WireTypes exposing (..)

          import Array
          import Dict
          import Set
          import Evergreen.V1.Subdir.Subsubdir.SubsubdirType
          import Time


          type alias AliasInt = Int


          type alias AllTypes =
              { int : Int
              , float : Float
              , bool : Bool
              , char : Char
              , string : String
              , listInt : (List Int)
              , setFloat : (Set.Set Float)
              , arrayString : (Array.Array String)
              , dict : (Dict.Dict String (List Int))
              , time : Time.Posix
              , order : Order
              , union : AllUnion
              , unit : ()
              }


          type alias SubRecursiveRecord =
              { recurse : AllTypes
              }


          type ExternalCustom threadedTvar
              = AlphabeticallyLast
              | AlphabeticallyFirst
              | AlphabeticallyKMiddleThreaded threadedTvar


          type ExternalCustomBasic
              = Custom1
              | Custom2


          type OnlyUsedInPhantom
              = OnlyUsedInPhantom


          type Phantom a
              = Phantom String


          type alias ExternalAliasTuple = (Float, Bool)


          type AllUnion
              = ValueStandalone
              | ValueInt Int
              | ValueFloat Float
              | ValueBool Bool
              | ValueChar Char
              | ValueString String
              | ValueListBool (List Bool)
              | ValueSetFloat (Set.Set Float)
              | ValueArrayString (Array.Array String)
              | ValueDict (Dict.Dict String (List Int))
              | ValueTime Time.Posix
              | ValueOrder Order
              | ValueUnit ()
              | ValueAliased AliasInt
              | ValueRecursive AllUnion
              | ValueSubRecursive SubRecursiveRecord
              | ValueDeep Evergreen.V1.Subdir.Subsubdir.SubsubdirType.DeepRec
              | ValueTwoParams Bool Char
              | ValueTuple (Int, String)
              | ValueTriple (Int, String)
              | ValueResult (Result String Int)
              | ValueCustom (ExternalCustom Int)
              | ValueCustomDeep Evergreen.V1.Subdir.Subsubdir.SubsubdirType.DeepCustom
              | ValueCustomBasic ExternalCustomBasic
              | ValuePhantom (Phantom OnlyUsedInPhantom)
              | ValueAliasTuple ExternalAliasTuple
              | ValueAll AllTypes


          type alias AnotherParamRecord threadedTvar =
              { threaded2 : threadedTvar
              }


          type alias SubRecord threadedTvar =
              { subtype : Int
              , threaded : threadedTvar
              }


          type alias SubSubRecordAlias threadedTvar = (SubRecord threadedTvar)


          type alias SubRecordAlias threadedTvar = (SubSubRecordAlias threadedTvar)


          type alias ExternalRecord threadedTvar =
              { ext : (ExternalCustom threadedTvar)
              , sub : (SubRecordAlias threadedTvar)
              , alphaFirst : Int
              , allUnion : AllUnion
              }

        |]

  ]
