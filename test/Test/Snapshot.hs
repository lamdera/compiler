{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Snapshot where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import EasyTest
import Lamdera
import Lamdera.Evergreen.Snapshot
import NeatInterpolation
import qualified Test.Lamdera
import qualified Lamdera.Compile
import qualified Lamdera.Relative


all = EasyTest.run suite


generationFileCheck originalFile generatedFile expectedOutput = do
  -- original <- io $ readUtf8Text originalFile
  generatedM <- io $ Lamdera.Relative.readFile generatedFile

  case generatedM of
    Just generated -> do
      -- Make sure our generation is as expected
      expectEqualTextTrimmed generated expectedOutput
      -- Make sure we compile the file to catch any bad generations as well
      io $ Lamdera.Compile.makeDev_ generatedFile

    Nothing ->
      crash $ "Could not read generated file: " <> generatedFile


suite :: Test ()
suite = tests $
  let
    derpImp = ModuleName.Canonical (Pkg.Name "author" "project") "Derp"
    herpImp = ModuleName.Canonical (Pkg.Name "author" "project") "Herp"
  in
  [ scope "mergeAllImports" $ do
      expectEqual
        (mergeAllImports [Set.singleton derpImp, Set.singleton herpImp])
        (Set.fromList [derpImp, herpImp])

  , scope "addImports" $ do
      let
        moduleName = (ModuleName.Canonical (Pkg.Name "author" "pkg") "Types")
        imps = Set.singleton derpImp

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
        (addImports moduleName imps fts)
        expected


  , scope "alltypes e2e to disk for test/scenario-alltypes" $ do

      io $ Test.Lamdera.snapshotWithParams 1 "test/scenario-alltypes" "test-local"

      scope "-> Types.elm" $
        generationFileCheck
        "test/scenario-alltypes/src/Types.elm"
        "test/scenario-alltypes/src/Evergreen/V1/Types.elm"
        [text|
          module Evergreen.V1.Types exposing (..)

          import Browser
          import Browser.Navigation
          import Url


          type alias FrontendModel =
              { key : Browser.Navigation.Key
              , message : String
              }


          type alias BackendModel =
              { message : String
              }


          type FrontendMsg
              = UrlClicked Browser.UrlRequest
              | UrlChanged Url.Url
              | NoOpFrontendMsg


          type ToBackend
              = NoOpToBackend


          type BackendMsg
              = NoOpBackendMsg


          type ToFrontend
              = NoOpToFrontend

        |]

      scope "-> WireTypes.elm" $
        generationFileCheck
        "test/scenario-alltypes/src/WireTypes.elm"
        "test/scenario-alltypes/src/Evergreen/V1/WireTypes.elm"
        [text|
          module Evergreen.V1.WireTypes exposing (..)

          import Array
          import Dict
          import Evergreen.V1.Subdir.Subsubdir.SubsubdirType
          import Set
          import Time


          type alias AliasInt =
              Int


          type ExternalCustom threadedTvar
              = AlphabeticallyLast
              | AlphabeticallyFirst
              | AlphabeticallyKMiddleThreaded threadedTvar


          type alias Emoji =
              { available : OptionalData Bool
              }


          type OptionalData a
              = Included a


          type alias AllTypes =
              { int : Int
              , float : Float
              , bool : Bool
              , char : Char
              , string : String
              , listInt : List Int
              , setFloat : Set.Set Float
              , arrayString : Array.Array String
              , dict : Dict.Dict String (List Int)
              , time : Time.Posix
              , order : Order
              , union : AllUnion
              , unit : ()
              , externalCustom : ExternalCustom Int
              , reactions : OptionalData (List Emoji)
              }


          type alias SubRecursiveRecord =
              { recurse : AllTypes
              }


          type ExternalCustomBasic
              = Custom1
              | Custom2


          type OnlyUsedInPhantom
              = OnlyUsedInPhantom


          type Phantom a
              = Phantom String


          type alias ExternalAliasTuple =
              ( Float, Bool )


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
              | ValueTuple ( Int, String )
              | ValueTriple ( Int, String, Bool )
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


          type alias SubSubRecordAlias threadedTvar =
              SubRecord threadedTvar


          type alias SubRecordAlias threadedTvar =
              SubSubRecordAlias threadedTvar


          type alias ExternalRecord threadedTvar =
              { ext : ExternalCustom threadedTvar
              , sub : SubRecordAlias threadedTvar
              , alphaFirst : Int
              , allUnion : AllUnion
              }

        |]

  ]
