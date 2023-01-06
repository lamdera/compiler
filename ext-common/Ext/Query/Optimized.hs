module Ext.Query.Optimized where


import qualified Data.Map as Map
import Data.Text (Text)

import Data.Name (Name)
import qualified AST.Optimized as Opt

import Lamdera
import qualified Ext.Query.Canonical


findString :: Name -> FilePath -> FilePath -> IO (Maybe Text)
findString valueName project filename = do
  stringDef <- Ext.Query.Canonical.showDefOptimized project filename valueName
  pure $ case Map.toList stringDef of
    (global, node@(Opt.Define (Opt.Str contents) _)):_ ->
      Just $ utf8ToText contents
    _ -> Nothing
