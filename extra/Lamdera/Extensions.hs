module Lamdera.Extensions where

import qualified Data.Utf8 as Utf8
import qualified Elm.Version as V
import qualified Elm.Package as Pkg

import qualified Data.Map as Map

import Lamdera
import Lamdera.Project


-- The idea is to keep extenion code in here, but so far the extensions have
-- required types from the files they were extending, causing cyclic refs.
-- So they've needed to go inline as a result.

-- So the only extensions going here are the ones that don't rely on local types


metadata :: Pkg.Name -> V.Version -> String -> String -> String
metadata name version file original =
  if name == lamderaCore || name == lamderaCodecs
    then
      "https://static.lamdera.com/r/" ++ Pkg.toUrl name ++ "/" ++ V.toChars version ++ "/" ++ file
    else
      original
