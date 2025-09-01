module Compiler.Elm.Interface exposing
    ( Alias(..)
    , Binop(..)
    , DependencyInterface(..)
    , Interface(..)
    , Union(..)
    , bDependencyInterface
    , bInterface
    , extractAlias
    , extractUnion
    , fromModule
    , private
    , privatize
    , public
    , toPublicAlias
    , toPublicUnion
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Binop as Binop
import Compiler.Data.Name as Name
import Compiler.Elm.Package as Pkg
import Compiler.Reporting.Annotation as A
import Extra.Data.Binary as B
import Extra.Type.Map as Map



-- INTERFACE


type Interface
    = Interface
        --{ home    : Pkg.Name
        --, values  : Map.Map Name.Name Can.Annotation
        --, unions  : Map.Map Name.Name Union
        --, aliases : Map.Map Name.Name Alias
        --, binops  : Map.Map Name.Name Binop
        --}
        Pkg.Name
        (Map.Map Name.Name Can.Annotation)
        (Map.Map Name.Name Union)
        (Map.Map Name.Name Alias)
        (Map.Map Name.Name Binop)


type Union
    = OpenUnion Can.Union
    | ClosedUnion Can.Union
    | PrivateUnion Can.Union


type Alias
    = PublicAlias Can.Alias
    | PrivateAlias Can.Alias


type Binop
    = Binop
        --{ op_name : Name.Name
        --, op_annotation : Can.Annotation
        --, op_associativity : Binop.Associativity
        --, op_precedence : Binop.Precedence
        --}
        Name.Name
        Can.Annotation
        Binop.Associativity
        Binop.Precedence



-- FROM MODULE


fromModule : Pkg.Name -> Can.Module -> Map.Map Name.Name Can.Annotation -> Interface
fromModule home (Can.Module _ exports _ unions aliases binops _) annotations =
    Interface
        home
        (restrict exports annotations)
        (restrictUnions exports unions)
        (restrictAliases exports aliases)
        (restrict exports (Map.map (toOp annotations) binops))


restrict : Can.Exports -> Map.Map Name.Name a -> Map.Map Name.Name a
restrict exports dict =
    case exports of
        Can.ExportEverything ->
            dict

        Can.Export explicitExports ->
            Map.intersection dict explicitExports


toOp : Map.Map Name.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
    Binop name (Map.ex types name) associativity precedence


restrictUnions : Can.Exports -> Map.Map Name.Name Can.Union -> Map.Map Name.Name Union
restrictUnions exports unions =
    case exports of
        Can.ExportEverything ->
            Map.map OpenUnion unions

        Can.Export explicitExports ->
            let
                onLeft =
                    Map.dropMissing identity

                onRight =
                    Map.mapMissing identity (\_ union -> PrivateUnion union)

                onBoth =
                    Map.zipWithMatched identity <|
                        \_ (A.At _ export) union ->
                            case export of
                                Can.ExportUnionOpen ->
                                    OpenUnion union

                                Can.ExportUnionClosed ->
                                    ClosedUnion union

                                _ ->
                                    Debug.todo "impossible exports discovered in restrictUnions"
            in
            Map.mergeA identity identity onLeft onRight onBoth explicitExports unions


restrictAliases : Can.Exports -> Map.Map Name.Name Can.Alias -> Map.Map Name.Name Alias
restrictAliases exports aliases =
    case exports of
        Can.ExportEverything ->
            Map.map PublicAlias aliases

        Can.Export explicitExports ->
            let
                onLeft =
                    Map.dropMissing identity

                onRight =
                    Map.mapMissing identity (\_ a -> PrivateAlias a)

                onBoth =
                    Map.zipWithMatched identity (\_ _ a -> PublicAlias a)
            in
            Map.mergeA identity identity onLeft onRight onBoth explicitExports aliases



-- TO PUBLIC


toPublicUnion : Union -> Maybe Can.Union
toPublicUnion iUnion =
    case iUnion of
        OpenUnion union ->
            Just union

        ClosedUnion (Can.Union vars _ _ opts) ->
            Just (Can.Union vars [] 0 opts)

        PrivateUnion _ ->
            Nothing


toPublicAlias : Alias -> Maybe Can.Alias
toPublicAlias iAlias =
    case iAlias of
        PublicAlias alias ->
            Just alias

        PrivateAlias _ ->
            Nothing



-- DEPENDENCY INTERFACE


type DependencyInterface
    = Public Interface
    | Private Pkg.Name (Map.Map Name.Name Can.Union) (Map.Map Name.Name Can.Alias)


public : Interface -> DependencyInterface
public =
    Public


private : Interface -> DependencyInterface
private (Interface pkg _ unions aliases _) =
    Private pkg (Map.map extractUnion unions) (Map.map extractAlias aliases)


extractUnion : Union -> Can.Union
extractUnion iUnion =
    case iUnion of
        OpenUnion union ->
            union

        ClosedUnion union ->
            union

        PrivateUnion union ->
            union


extractAlias : Alias -> Can.Alias
extractAlias iAlias =
    case iAlias of
        PublicAlias alias ->
            alias

        PrivateAlias alias ->
            alias


privatize : DependencyInterface -> DependencyInterface
privatize di =
    case di of
        Public i ->
            private i

        Private _ _ _ ->
            di



-- BINARY


bInterface : B.Binary Interface
bInterface =
    B.bin5 Interface
        (\(Interface a b c d e) -> B.T5 a b c d e)
        Pkg.bName
        (B.bMap Name.bName Can.bAnnotation)
        (B.bMap Name.bName bUnion)
        (B.bMap Name.bName bAlias)
        (B.bMap Name.bName bBinop)


bUnion : B.Binary Union
bUnion =
    B.custom "binary encoding of Union was corrupted"
        (\p0 p1 p2 union ->
            case union of
                OpenUnion u ->
                    p0 u

                ClosedUnion u ->
                    p1 u

                PrivateUnion u ->
                    p2 u
        )
        |> B.var1 0 OpenUnion Can.bUnion
        |> B.var1 1 ClosedUnion Can.bUnion
        |> B.var1 2 PrivateUnion Can.bUnion
        |> B.finish


bAlias : B.Binary Alias
bAlias =
    B.custom "binary encoding of Alias was corrupted"
        (\p0 p1 alias_ ->
            case alias_ of
                PublicAlias a ->
                    p0 a

                PrivateAlias a ->
                    p1 a
        )
        |> B.var1 0 PublicAlias Can.bAlias
        |> B.var1 1 PrivateAlias Can.bAlias
        |> B.finish


bBinop : B.Binary Binop
bBinop =
    B.bin4 Binop
        (\(Binop a b c d) -> B.T4 a b c d)
        Name.bName
        Can.bAnnotation
        Binop.bAssociativity
        Binop.bPrecedence


bDependencyInterface : B.Binary DependencyInterface
bDependencyInterface =
    B.custom "binary encoding of DependencyInterface was corrupted"
        (\p0 p1 dependencyInterface ->
            case dependencyInterface of
                Public a ->
                    p0 a

                Private a b c ->
                    p1 a b c
        )
        |> B.var1 0 Public bInterface
        |> B.var3 1 Private Pkg.bName (B.bMap Name.bName Can.bUnion) (B.bMap Name.bName Can.bAlias)
        |> B.finish
