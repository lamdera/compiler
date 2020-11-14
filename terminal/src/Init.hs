{-# LANGUAGE OverloadedStrings #-}
module Init
  ( run
  , init -- @LAMDERA exposed
  )
  where


import Prelude hiding (init)
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir

import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit


import qualified Lamdera.Checks

-- RUN


run :: () -> () -> IO ()
run () () =
  Reporting.attempt Exit.initToReport $
  do  exists <- Dir.doesFileExist "elm.json"
      if exists
        then return (Left Exit.InitAlreadyExists)
        else
          do  approved <- Reporting.ask question
              if approved
                then init
                else
                  do  putStrLn "Okay, I did not make any changes!"
                      return (Right ())


question :: D.Doc
question =
  D.stack
    [ D.fillSep
        ["Hello!"
        ,"Lamdera","projects","always","start","with","an",D.green "elm.json","file,"
        ,"as","well","as","three","source","files:",D.green "Frontend.elm",",",D.green "Backend.elm","and",D.green "Types.elm"
        ]
    , D.fillSep
        ["If","you're","new","to","Elm,","the","best","starting","point","is"
        , D.cyan (D.fromChars (D.makeLink "init"))
        ]
    , D.fillSep
        ["Otherwise","check","out",D.cyan ("<https://dashboard.lamdera.app/docs/starting>")
        ,"for","Lamdera","specific","information!"
        ]
    , "Knowing all that, would you like me to create a starter implementation? [Y/n]: "
    ]



-- INIT


init :: IO (Either Exit.Init ())
init =
  do  eitherEnv <- Solver.initEnv
      case eitherEnv of
        Left problem ->
          return (Left (Exit.InitRegistryProblem problem))

        Right (Solver.Env cache _ connection registry) ->
          do  result <- Solver.verify cache connection registry defaults
              case result of
                Solver.Err exit ->
                  return (Left (Exit.InitSolverProblem exit))

                Solver.NoSolution ->
                  return (Left (Exit.InitNoSolution (Map.keys defaults)))

                Solver.NoOfflineSolution ->
                  return (Left (Exit.InitNoOfflineSolution (Map.keys defaults)))

                Solver.Ok details ->
                  let
                    solution = Map.map (\(Solver.Details vsn _) -> vsn) details
                    directs = Map.intersection solution defaults
                    indirects = Map.difference solution defaults
                  in
                  do  Dir.createDirectoryIfMissing True "src"
                      Outline.write "." $ Outline.App $
                        Outline.AppOutline V.compiler (NE.List (Outline.RelativeSrcDir "src") []) directs indirects Map.empty Map.empty
                      Lamdera.Checks.writeDefaultImplementations
                      putStrLn "Okay, I created it. Now read that link!"
                      return (Right ())


defaults :: Map.Map Pkg.Name Con.Constraint
defaults =
  Map.fromList
    [ (Pkg.core, Con.anything)
    , (Pkg.browser, Con.anything)
    , (Pkg.html, Con.anything)
    -- @LAMDERA
    , (Pkg.url, Con.anything)
    , (Pkg.http, Con.anything)
    , (Pkg.json, Con.anything)
    , (Pkg.time, Con.anything)
    , (Pkg.lamderaCore, Con.exactly (V.Version 1 0 0))
    , (Pkg.lamderaCodecs, Con.exactly (V.Version 1 0 0))
    ]
