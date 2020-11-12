{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Wire.Interfaces where

import qualified AST.Source as Src
import AST.Source
import qualified Data.Name
import qualified Reporting.Annotation as A
import Reporting.Annotation
import qualified Data.Utf8 as Utf8
import qualified Data.Map as Map
import Elm.Package
import qualified Elm.Package as Pkg
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName

import Lamdera
import qualified Lamdera.Wire.Helpers
import StandaloneInstances

{- Heper functions for AST.Source modifications -}

-- modul =
--   data Module =
--     Module
--       { _name    :: Maybe (A.Located Name)
--       , _exports :: A.Located Exposing
--       , _docs    :: Docs
--       , _imports :: [Import]
--       , _values  :: [A.Located Value]
--       , _unions  :: [A.Located Union]
--       , _aliases :: [A.Located Alias]
--       , _binops  :: [A.Located Infix]
--       , _effects :: Effects
--       }

-- ifaces =
--   map for each module except current one:
--   data Interface =
--     Interface
--       { _home    :: Pkg.Name
--       , _values  :: Map.Map Name.Name Can.Annotation
--       , _unions  :: Map.Map Name.Name Union
--       , _aliases :: Map.Map Name.Name Alias
--       , _binops  :: Map.Map Name.Name Binop
--       }
--     deriving (Eq)

modifyModul :: Name -> Map.Map ModuleName.Raw I.Interface -> Module -> Module
modifyModul pkg ifaces modul =
  unsafePerformIO $ do
    if (Lamdera.Wire.Helpers.shouldHaveCodecsGenerated pkg)
      then do
        -- atomicPutStrLn $ tShow "🧡" $ Src.getName modul
        -- atomicPutStrLn $ tShow "💚" $ Src._exports modul
        -- atomicPutStrLn $ tShow "💙" $ Map.keys ifaces
        -- Src._values modul
        --   & mapM showValue

        let
          newModul =
            modul {
              Src._values =
                (Src._values modul)
                -- These stubs are added in so that the functions (which are not yet generated!)
                -- can be referenced by user-code that is already in the file. This allows us to
                -- write the `expected_w2_[en|de]code_TYPENAME` style tests, which ensures our
                -- expected final Elm generation is identical to what would have been ingested
                -- had the source actually been literally written in the file as text to begin with!
                ++ (unionStubs (Src._unions modul))
                ++ (aliasStubs (Src._aliases modul))
            }

        -- debugPassText "💙" (hindentFormatValue $ Src._values modul) (pure ())
        -- atomicPutStrLn $ tShow "💙" $ newModul
        -- debugHaskellPass "💙" newModul (pure newModul)

        if (pkg == (Pkg.Name "author" "project") && (getName modul == "Env"))
          then do
            -- We are in the Env.elm module, inject the `type Mode = ...` and
            -- `mode = ...` code if it is not already defined.
            -- NOTE: this should be part of AppConfig but for simplicity it's here so all injections are together.

            envMode <- Lamdera.getEnvMode
            pure $
              newModul
                { Src._unions =
                    -- Source for `type Mode = Production | Development`
                    if hasModeDef (Src._unions newModul)
                      then Src._unions newModul
                      else Src._unions newModul ++ [(a (Union (a ("Mode")) [] [((a ("Production")), []), ((a ("Development")), [])]))]
                , Src._values =
                    -- Source for `mode = Development`
                    Src._values newModul
                      & listUpsert
                          isModeValue
                          (a (Value (a ("mode")) [] (a (Var CapVar (Data.Name.fromChars $ show envMode))) Nothing))
                }

          else pure newModul

      else pure modul


listUpsert :: (a -> Bool) -> a -> [a] -> [a]
listUpsert check item collection =
    if any check collection then
      fmap (\v ->
        if check v then
          item
        else
          v
      )
      collection
    else
      collection ++ [item]


hasModeDef :: [Located Union] -> Bool
hasModeDef unions =
  unions
    & any (\union ->
      case union of
        A.At _ (Union (A.At _ name) params constructors) ->
          name == "Mode"
    )


isModeValue :: Located Value -> Bool
isModeValue value =
  case value of
    A.At _ (Value (A.At _ name) params value typeAnnotation) -> name == "mode"


unionStubs :: [Located Union] -> [Located Value]
unionStubs unions =
  unions
    & concatMap (\(A.At _ (Src.Union (A.At _ name) _ _)) ->
      [ _Debug_todo $ Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars name
      , _Debug_todo $ Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars name
      ]
    )


aliasStubs :: [Located Alias] -> [Located Value]
aliasStubs aliases =
  aliases
    & concatMap (\(A.At _ (Src.Alias (A.At _ name) _ _)) ->
      [ _Debug_todo $ Data.Name.fromChars $ "w2_encode_" ++ Data.Name.toChars name
      , _Debug_todo $ Data.Name.fromChars $ "w2_decode_" ++ Data.Name.toChars name
      ]
    )


a v =
  A.at (A.Position 0 0) (A.Position 0 10) v

showValue (A.At region (Src.Value name pattern expr mtype)) = do
  -- data Value = Value (A.Located Name) [Pattern] Expr (Maybe Type)
  atomicPutStrLn $ "❤️name: " ++ show name
  atomicPutStrLn $ "💛pattern: " ++ show pattern
  atomicPutStrLn $ "💙expr: " ++ show expr
  atomicPutStrLn $ "💜mtype: " ++ show mtype

tShow x v =
  x ++ show v

x =
  "❤️ 💔 ♥️ 💗 💓 💕 💖  💛 💙 💜 💚"

_Debug_todo :: Data.Name.Name -> A.Located Src.Value
_Debug_todo functionName =
  let functionName_ = Utf8.fromChars . Data.Name.toChars $ functionName
  in
  a $ Src.Value
        (a functionName)
        []
        (a (Src.Call (a (Src.VarQual Src.LowVar "Debug" "todo")) [a (Src.Str functionName_)]))
        Nothing
