{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Lamdera.Live where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.Vector as Vector
import Data.Function ((&))

import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String
import qualified Data.Utf8 as Utf8

import qualified Snap.Core as Snap

import EasyTest
import Test.Helpers

-- Import the module we're testing
import qualified Lamdera.CLI.Live as Live

suite :: Test ()
suite = tests
  [ scope "generateRpcRequestPayload form-urlencoded parsing" $ do
      testFormUrlencodedParsing
  ]

-- Test the form-urlencoded parsing logic
testFormUrlencodedParsing :: Test ()
testFormUrlencodedParsing = do
  let
    -- Test data: "singlevalue=test&multivalue=test1&multivalue=test2"
    formData = "singlevalue=test&multivalue=test1&multivalue=test2"
    contentType = Just "application/x-www-form-urlencoded"
    rbody = BSL.fromStrict $ T.encodeUtf8 formData
    endpoint = "test-endpoint"
    sid = "test-session"
    reqId = "test-request-id"
    
    -- Create a simple headers JSON for testing
    requestHeadersJson = E.object [("content-type", E.string "application/x-www-form-urlencoded")]

  -- Generate the payload
  let finalPayload = Live.generateRpcRequestPayload contentType rbody endpoint sid reqId requestHeadersJson

  -- Test 1: The result should be a valid JSON string
  case Aeson.decode (BSL.fromStrict $ T.encodeUtf8 finalPayload) of
    Nothing -> crash $ "Final payload is not valid JSON: " ++ T.unpack finalPayload
    Just (outerJson :: Aeson.Value) -> do
      
      -- Test 2: Check that the outer JSON has the expected structure
      case outerJson of
        Aeson.Object outerObj -> do
          -- Check for required fields
          if not (KeyMap.member "t" outerObj) then crash "Missing 't' field" else ok
          if not (KeyMap.member "s" outerObj) then crash "Missing 's' field" else ok
          if not (KeyMap.member "e" outerObj) then crash "Missing 'e' field" else ok
          if not (KeyMap.member "r" outerObj) then crash "Missing 'r' field" else ok
          if not (KeyMap.member "h" outerObj) then crash "Missing 'h' field" else ok
          if not (KeyMap.member "j" outerObj) then crash "Missing 'j' field" else ok
          
          -- Test 3: Check that the "j" field contains a valid JSON string
          case KeyMap.lookup "j" outerObj of
            Just (Aeson.String jValue) -> do
              let jString = T.unpack jValue
              case Aeson.decode (BSL.fromStrict $ T.encodeUtf8 jValue) of
                Nothing -> crash $ "The 'j' field does not contain valid JSON: " ++ jString
                Just (innerJson :: Aeson.Value) -> do
                  
                  -- Test 4: Check that the inner JSON has the expected form data structure
                  case innerJson of
                    Aeson.Object innerObj -> do
                      -- Check for singlevalue
                      case KeyMap.lookup "singlevalue" innerObj of
                        Just (Aeson.String val) -> 
                          if val == "test" then ok else crash "singlevalue should be 'test'"
                        _ -> crash "singlevalue field missing or not a string"
                      
                      -- Check for multivalue (should be an array)
                      case KeyMap.lookup "multivalue" innerObj of
                        Just (Aeson.Array arr) -> do
                          let arrList = Vector.toList arr
                          if length arrList == 2 then ok else crash "multivalue should have 2 elements"
                          case arrList of
                            [Aeson.String val1, Aeson.String val2] -> do
                              if val1 == "test1" then ok else crash "first multivalue should be 'test1'"
                              if val2 == "test2" then ok else crash "second multivalue should be 'test2'"
                            _ -> crash "multivalue array elements are not strings"
                        _ -> crash "multivalue field missing or not an array"
                    
                    _ -> crash "Inner JSON is not an object"
            
            _ -> crash "The 'j' field is missing or not a string"
        
        _ -> crash "Outer JSON is not an object" 