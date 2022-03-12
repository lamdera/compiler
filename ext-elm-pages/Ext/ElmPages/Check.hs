module Ext.ElmPages.Check where

import qualified Reporting.Error as E
import qualified Reporting.Doc as D
import qualified Ext.TypeHash

isWireCompatible target ifaces inDebug = do

  -- calculateHashes :: Interfaces -> Bool -> IO (Either Exit.BuildProblem ([Text], [(Text, [Text], DiffableType)]))
  -- calculateHashes interfaces inDebug = do

  let inDebug = False
      x = Ext.TypeHash.calculateHashes [target] ifaces inDebug


  Left $ E.BadLamdera $ D.fromChars "Ext.ElmPages.Check.isWireCompatible error!"
