module Compiler.Compile exposing (Artifacts(..), compile)

import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Module as Canonicalize
import Compiler.Data.Name as Name
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Nitpick.PatternMatches as PatternMatches
import Compiler.Optimize.Module as Optimize
import Compiler.Reporting.Error as E
import Compiler.Reporting.Render.Type.Localizer as Localizer
import Compiler.Reporting.Result as R
import Compiler.Type.Constrain.Module as Type
import Compiler.Type.Solve as Solve
import Extra.System.IO.Pure as IO
import Extra.Type.Either as Either exposing (Either(..))
import Extra.Type.Map as Map


type Artifacts
    = Artifacts
        --{ modul : Can.Module
        --, types : Map.Map Name.Name Can.Annotation
        --, graph : Opt.LocalGraph
        --}
        Can.Module
        (Map.Map Name.Name Can.Annotation)
        Opt.LocalGraph


compile : Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Artifacts
compile pkg ifaces modul =
    Either.bind (canonicalize pkg ifaces modul) <|
        \canonical ->
            Either.bind (typeCheck modul canonical) <|
                \annotations ->
                    Either.bind (nitpick canonical) <|
                        \() ->
                            Either.bind (optimize modul annotations canonical) <|
                                \objects ->
                                    Right (Artifacts canonical annotations objects)



-- PHASES


canonicalize : Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Can.Module
canonicalize pkg ifaces modul =
    case Tuple.second <| R.run <| Canonicalize.canonicalize pkg ifaces modul of
        Right canonical ->
            Right canonical

        Left errors ->
            Left <| E.BadNames errors


typeCheck : Src.Module -> Can.Module -> Either E.Error (Map.Map Name.Name Can.Annotation)
typeCheck modul canonical =
    case IO.performIO (IO.andThen Solve.run <| Type.constrain canonical) Solve.init of
        Right annotations ->
            Right annotations

        Left errors ->
            Left (E.BadTypes (Localizer.fromModule modul) errors)


nitpick : Can.Module -> Either E.Error ()
nitpick canonical =
    case PatternMatches.check canonical of
        Right () ->
            Right ()

        Left errors ->
            Left (E.BadPatterns errors)


optimize : Src.Module -> Map.Map Name.Name Can.Annotation -> Can.Module -> Either E.Error Opt.LocalGraph
optimize modul annotations canonical =
    case Tuple.second <| R.run <| Optimize.optimize annotations canonical of
        Right localGraph ->
            Right localGraph

        Left errors ->
            Left (E.BadMains (Localizer.fromModule modul) errors)
