{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.TypeHashes where

import qualified System.Directory as Dir
import System.FilePath ((</>))

import EasyTest
import Test.Helpers

import Lamdera
import Lamdera.Compile
import NeatInterpolation
import qualified Ext.Query.Interfaces as Interfaces
import qualified Lamdera.TypeHash

all = EasyTest.run suite

suite :: Test ()
suite = tests
  [ scope "e2e test" $ do
      let
        project = "/Users/mario/lamdera/test/v1"
        expected =
          [text|
            ["a57ac2f8269a6929598785f3a9789cdb7c528e8e","c6705c231ea1753f3bfc4e54191ddde603bac315","3fbceb448ef53c7bbb655e6d49d94c927cc3caa0","6bf58721649b39b399ac7ec2f6b162baceae03d6","a6b615774ea87d310518531985a84cb7b8c9bb75","a83d62ff8afc0e6ca4b0cd6a544d023ef6718807"]
          |]

      liftIO $ withDebug $ Dir.withCurrentDirectory project $ do
        Lamdera.TypeHash.calculateAndWrite

      actual <- liftIO $ readUtf8Text $ lamderaHashesPath project

      expectEqualTextTrimmed (actual & withDefault "<failed to read file>") expected


  , scope "all types" $ do
      let
        file = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-alltypes/src/Test/Wire_Alias_2_Record.elm"
        project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-alltypes"
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
                mconcat fields & (\all -> "R[" <> all <> "]")
            )

      liftIO $ Lamdera.Compile.makeDev_ file

      (thash, ttext) <- liftIO $ withDebug $ Dir.withCurrentDirectory project $ do
        Lamdera.TypeHash.calculateHashPair "src/Test/Wire_Alias_2_Record.elm" moduleName typeName

      expectEqualTextTrimmed thash "0b5ace6c03f080a53d547cda99731442119db2de"
      expectEqualTextTrimmed ttext expectedTypeText

  , scope "extensible record" $ do
      let
        file = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-alltypes/src/Test/Wire_Record_Extensible1_Basic.elm"
        project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-alltypes"
        moduleName = "Test.Wire_Record_Extensible1_Basic"
        typeName = "ColorOverlap"

      liftIO $ Lamdera.Compile.makeDev_ file

      (thash, ttext) <- liftIO $ withDebug $ Dir.withCurrentDirectory project $ do
        Lamdera.TypeHash.calculateHashPair "src/Test/Wire_Record_Extensible1_Basic.elm" moduleName typeName

      expectEqualTextTrimmed thash "4bef3232374b3dfe84546f3f132ad4eaaa2cbb2f"
      expectEqualTextTrimmed ttext "R[FIIIS]"

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

      liftIO $ putStrLn $ show sha_first
      liftIO $ putStrLn $ show sha_second

      expect $ sha_first /= sha_second
  ]



debugProject project = do
  withDebug $ Dir.withCurrentDirectory project $ do
    res <- Lamdera.TypeHash.calculateLamderaHashes
    case res of
      Right hashes -> do
        -- hindentPrintValue "hash" hashes
        atomicPutStrLn $ show hashes
        pure ()
      Left err -> do
        -- hindentPrintValue "hash-err" err
        pure ()

    pure ()
