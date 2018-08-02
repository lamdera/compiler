{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wire where

-- import AST.Source (VarType(..), Type_(..), Pattern_(..))
-- import AST.Valid
import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N
-- import qualified AST.Valid as AS (Module(..))
import AST.Canonical
import qualified AST.Module.Name as ModuleName

import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Prettyprint


-- modify canonical flag pkg importDict interfaces source =
--   canonical

-- Our injection point POC for AllTypes. Search for `Wire.modify`
modify canonical flag pkg importDict interfaces source =
  case canonical of
    Module name docs exports decls unions aliases binops effects ->
      case name of
        ModuleName.Canonical pkg n ->
          if N.toString n == "AllTypes_Gen" then

            -- tracef ("-" ++ N.toString n) valid
            -- tracef ("-" ++ N.toString n) (canonical { _decls = [ encoder, decoder, evg_e_Union, evg_d_Union ] })
            tracef ("-" ++ N.toString n) canonical

          else if N.toString n == "AllTypes" then

            tracef ("-" ++ N.toString n) (canonical { _decls =
              case _decls canonical of
                DeclareRec d x -> DeclareRec (d ++ [ staticX ]) x
                d -> d
            })
            -- tracef ("-" ++ N.toString n) canonical

          else
            canonical




staticX =
  (Def (At (Region {_start = Position {_line = 33
                                      ,_column = 1}
                   ,_end = Position {_line = 33
                                    ,_column = 4}})
           (N.fromString "evg"))
       []
       (At (Region {_start = Position {_line = 34
                                      ,_column = 5}
                   ,_end = Position {_line = 34
                                    ,_column = 6}})
           (Int 123)))




-- x =
--   case canonical of
--     AS.Module n _ _ _ _ _ _ _ _ _ ->
--       -- case n of
--       --   (ModuleName.Canonical pkg modl) ->
--           if N.toString n == "AllTypes_Gen" then
--
--             -- tracef ("-" ++ N.toString n) valid
--             -- tracef ("-" ++ N.toString n) (canonical { _decls = [ encoder, decoder, evg_e_Union, evg_d_Union ] })
--             tracef ("-" ++ N.toString n) (canonical { _decls = [  ] })
--
--           else if N.toString n == "AllTypes" then
--
--             tracef ("-" ++ N.toString n) canonical
--
--           else
--             canonical



-- AST to file debugger
tracef n a =
  unsafePerformIO $ do
    putStrLn ("can-" ++ n ++ ".txt")
    writeFile ("can-" ++ n ++ ".txt") $ prettyShow a
    pure a
