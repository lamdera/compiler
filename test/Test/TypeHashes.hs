{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.TypeHashes where

import qualified System.Directory as Dir
import System.FilePath ((</>))

import EasyTest
import Test.Helpers
import NeatInterpolation

import Lamdera
import Lamdera.Compile
import qualified Lamdera.Types
import qualified Lamdera.Relative
import qualified Ext.Query.Interfaces as Interfaces
import qualified Lamdera.TypeHash
import qualified Ext.Common

all = EasyTest.run suite

suite :: Test ()
suite = tests
  [ scope "e2e test" $ do

      project <- io $ Lamdera.Relative.requireDir "test/scenario-alltypes"
      let
        expected =
          [text|
            ["64db237c2087232331047b907cade9a262601159","728c05b17597cebaad4eee0d43febe29298b0f0c","fa92f6879a3929d7992e184cf89bd93fea3e094b","b1675b2dec2ee3f023cd958159060fd7d5c5c21c","b1675b2dec2ee3f023cd958159060fd7d5c5c21c","b1675b2dec2ee3f023cd958159060fd7d5c5c21c"]
          |]

      io $ withDebug $ Ext.Common.withProjectRoot project $ do
        Lamdera.TypeHash.calculateAndWrite


      Lamdera.Types.core & mapM
        (\coreType ->
          io $ do
            (thash, ttext) <- withDebug $ Ext.Common.withProjectRoot project $ Lamdera.TypeHash.calculateHashPair "src/Types.elm" "Types" coreType
            atomicPutStrLn $ show ttext
        )

      actual <- io $ readUtf8Text $ lamderaHashesPath project

      expectEqualTextTrimmed expected (actual & withDefault "<failed to read file>")


  , scope "all types" $ do
      project <- io $ Lamdera.Relative.requireDir "test/scenario-alltypes"
      let
        file = project </> "src/Test/Wire_Alias_2_Record.elm"
        moduleName = "Test.Wire_Alias_2_Record"
        typeName = "AllTypes"

        expectedTypeText =
          [
            "A[S]",     -- { arrayString : Array String
            "B",        -- , bool : Bool
            "Ch",       -- , char : Char
            "D[S,L[I]]", -- , dict : Dict String (List Int)
            "F",        -- , float : Float
            "I",        -- , int : Int
            "L[I]",     -- , listInt : List Int
            "Ord",      -- , order : Order
            "S[F]",     -- , setFloat : Set Float
            "S",        -- , string : String
            "C[[I]]",   -- , time : Time.Posix
            "()",       -- , unit : ()
            ""          -- }
          ]
          & (\fields ->
                mconcat fields & (\all_ -> "R[" <> all_ <> "]")
            )

      io $ Lamdera.Compile.makeDev_ file

      (thash, ttext) <- io $ withDebug $ Ext.Common.withProjectRoot project $ do
        Lamdera.TypeHash.calculateHashPair "src/Test/Wire_Alias_2_Record.elm" moduleName typeName

      expectEqualTextTrimmed thash "0b5ace6c03f080a53d547cda99731442119db2de"
      expectEqualTextTrimmed ttext expectedTypeText

  , scope "extensible record" $ do
      project <- io $ Lamdera.Relative.requireDir "test/scenario-alltypes"
      let
        modulePath = "src/Test/Wire_Record_Extensible1_Basic.elm"
        moduleName = "Test.Wire_Record_Extensible1_Basic"
        typeName = "ColorOverlap"

      (thash, ttext) <- io $ withDebug $ Ext.Common.withProjectRoot project $ do
        Lamdera.TypeHash.calculateHashPair modulePath moduleName typeName

      expectEqualTextTrimmed thash "4bef3232374b3dfe84546f3f132ad4eaaa2cbb2f"
      expectEqualTextTrimmed ttext "R[FIIIS]"

  , scope "lamdera/containers types" $ do
      project <- io $ Lamdera.Relative.requireDir "test/scenario-alltypes"

      scope "Custom type usage" $ do
        let
          modulePath = "src/Test/Wire_Package_Types.elm"
          moduleName = "Test.Wire_Package_Types"
          typeName = "PackageTypes"

        (thash, ttext) <- io $ withDebug $ Ext.Common.withProjectRoot project $ do
          Lamdera.TypeHash.calculateHashPair modulePath moduleName typeName

        expectEqualTextTrimmed thash "57c1aaab0b97e6ded9efa0a45eafa79a6f42c3f2"
        expectEqualTextTrimmed ttext "C[[LD[C[[][]],S]][LS[C[[][]]]]]"

      scope "Record usage" $ do
        let
          modulePath = "src/Test/Wire_Package_Types.elm"
          moduleName = "Test.Wire_Package_Types"
          typeName = "PackageTypesRecord"

        (thash, ttext) <- io $ withDebug $ Ext.Common.withProjectRoot project $ do
          Lamdera.TypeHash.calculateHashPair modulePath moduleName typeName

        expectEqualTextTrimmed thash "4684f0e30ca7421c497e79b5dfc88cec77010699"
        expectEqualTextTrimmed ttext "R[LD[C[[][]],S]LS[C[[][]]]]"


  , scope "sha1 should not collide" $ do

      -- This seems insane to believe it could ever fail, but let me tell you a story.

      -- This is the exact behavior I was observing one evening from the type hashing.
      -- TypeHash was reporting no type changes for these two different strings (see end)

      -- After a couple of hours debugging and banging my head against the wall, I notied this∷

      -- DEBUG: Writing out hashes:["481e7534534066e769a8a38c6c7c3168f1e7e854","fbdea5a22e006302b28362a2d768a9c6e7485e70","41c557a264cf74a0443d7f6e21b9671283d7e12f","181616e561d53e4a2a6e1483e1dba2174895d428","ff98601e1e2d26dc80ec9f79200584a2ed5156dc","505b3928015cd8f0f2a1d2b7bf4e1d4afb2e407a"]
      -- DEBUG: writeUtf8: "/Users/mario/dev/projects/lamdera-dashboard/lamdera-stuff/.lamdera-hashes"
      -- DEBUG: writeUtf8: "/Users/mario/dev/projects/lamdera-dashboard/lamdera-stuff/.lamdera-external-warnings"
      -- Success! Compiled 22 modules.
      -- Success! Compiled 1 module.
      -- ───> Checking Evergreen migrations...
      -- DEBUG: app name:"dashboard"
      -- DEBUG: Reading local types from /Users/mario/dev/projects/lamdera-dashboard/lamdera-stuff/.lamdera-hashes
      -- DEBUG: local types:["6739f4abe527a2d2cab754d35b9b290c75b65298","a212b0d84bb10b592887775a331d9e9f80c1cf89","0bbc44da7904a09d5d3e9b148e021c08d50c2bda","784d73d97807ce1435f7df7612c44d30788e0126","077c6ce09c972a6caa121c88c8c8de7185ec8e08","ab5cdc0c2e5b19eea28bd77d3991df342c254c82"]

      -- Write the types, and then immediately read them back, and somehow they are changed!?
      -- Nowhere else in the code reads/writes this file except these two points
      -- Also seemingly nothing I changed in the compiler

      -- In frustration I decided to close all my shells, and I spot this...

      -- # mario ~/dev/projects/lamdera-dashboard ± master x                                                                                                  22:00:12
      -- $ fg
      -- [1]  + 69313 running    lamdera live

      -- I still had an instance of lamdera live running...
      -- Yep.
      -- The other process was picking up the change from the write, and itself compiling a type hash based on it's old logic
      -- It was perfectly injecting it between the write+read of my own lamdera check in a beautiful race condition that
      -- held stable across many repeats

      -- I figured this one was worth writing down!

      let
        sha_first = textSha1 "C[[R[SS]][S][Res[S,L[R[SS]]]][Res[S,R[SL[C[[][]]]SS]]][D[S,I]][D[S,R[C[[I]]SC[[I]]SC[[S]]L[C[[][]]]SS]]][D[S,R[C[[I]]SSS]]][][][Res[S,R[SL[C[[][]]]SS]]]]"
        sha_second = textSha1 "C[[R[SS]][S][Res[S,L[R[SS]]]][Res[S,R[SL[C[[][]]]SS]]][D[S,I]][D[S,R[C[[I]]SC[[I]]SC[[S]]L[C[[][]]]SS]]][D[S,R[C[[I]]SSS]]][][][][Res[S,R[SL[C[[][]]]SS]]]]"

      io $ atomicPutStrLn $ show sha_first
      io $ atomicPutStrLn $ show sha_second

      expect $ sha_first /= sha_second
  ]



debugProject project = do
  withDebug $ Ext.Common.withProjectRoot project $ do
    res <- Lamdera.TypeHash.calculateLamderaHashes
    case res of
      Right hashes -> do
        -- hindentPrintLabelled "hash" hashes
        atomicPutStrLn $ show hashes
        pure ()
      Left err -> do
        -- hindentPrintLabelled "hash-err" err
        pure ()

    pure ()
