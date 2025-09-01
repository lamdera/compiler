module Compiler.AST.Canonical exposing
    ( Alias(..)
    , AliasType(..)
    , Annotation(..)
    , Binop(..)
    , CaseBranch(..)
    , Ctor(..)
    , CtorOpts(..)
    , Decls(..)
    , Def(..)
    , Effects(..)
    , Export(..)
    , Exports(..)
    , Expr
    , Expr_(..)
    , FieldType(..)
    , FieldUpdate(..)
    , Manager(..)
    , Module(..)
    , Pattern
    , PatternCtorArg(..)
    , Pattern_(..)
    , Port(..)
    , Type(..)
    , Union(..)
    , bAlias
    , bAnnotation
    , bCtorOpts
    , bType
    , bUnion
    , ctorOptsToString
    , fieldsToList
    , getName
    )

{- Creating a canonical AST means finding the home module for all variables.
   So if you have L.map, you need to figure out that it is from the elm/core
   package in the List module.

   In later phases (e.g. type inference, exhaustiveness checking, optimization)
   you need to look up additional info from these modules. What is the type?
   What are the alternative type constructors? These lookups can be quite costly,
   especially in type inference. To reduce costs the canonicalization phase
   caches info needed in later phases. This means we no longer build large
   dictionaries of metadata with O(log(n)) lookups in those phases. Instead
   there is an O(1) read of an existing field! I have tried to mark all
   cached data with comments like:

   -- CACHE for exhaustiveness
   -- CACHE for inference

   So it is clear why the data is kept around.
-}

import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.Float as EF
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.String as ES
import Compiler.Reporting.Annotation as A
import Extra.Data.Binary as B
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- EXPRESSIONS


type alias Expr =
    A.Located Expr_



-- CACHE Annotations for type inference


type Expr_
    = VarLocal Name.Name
    | VarTopLevel ModuleName.Canonical Name.Name
    | VarKernel Name.Name Name.Name
    | VarForeign ModuleName.Canonical Name.Name Annotation
    | VarCtor CtorOpts ModuleName.Canonical Name.Name Index.ZeroBased Annotation
    | VarDebug ModuleName.Canonical Name.Name Annotation
    | VarOperator Name.Name ModuleName.Canonical Name.Name Annotation -- CACHE real Name.Name for optimization
    | Chr ES.TString
    | Str ES.TString
    | CInt Int
    | CFloat EF.TFloat
    | CList (TList Expr)
    | Negate Expr
    | Binop Name.Name ModuleName.Canonical Name.Name Annotation Expr Expr -- CACHE real Name.Name for optimization
    | Lambda (TList Pattern) Expr
    | Call Expr (TList Expr)
    | If (TList ( Expr, Expr )) Expr
    | Let Def Expr
    | LetRec (TList Def) Expr
    | LetDestruct Pattern Expr Expr
    | Case Expr (TList CaseBranch)
    | Accessor Name.Name
    | Access Expr (A.Located Name.Name)
    | Update Name.Name Expr (Map.Map Name.Name FieldUpdate)
    | Record (Map.Map Name.Name Expr)
    | Unit
    | Tuple Expr Expr (Maybe Expr)
    | Shader Shader.Source Shader.Types


type CaseBranch
    = CaseBranch Pattern Expr


type FieldUpdate
    = FieldUpdate A.Region Expr



-- DEFS


type Def
    = Def (A.Located Name.Name) (TList Pattern) Expr
    | TypedDef (A.Located Name.Name) FreeVars (TList ( Pattern, Type )) Expr Type



-- DECLARATIONS


type Decls
    = Declare Def Decls
    | DeclareRec Def (TList Def) Decls
    | SaveTheEnvironment



-- PATTERNS


type alias Pattern =
    A.Located Pattern_


type Pattern_
    = PAnything
    | PVar Name.Name
    | PRecord (TList Name.Name)
    | PAlias Pattern Name.Name
    | PUnit
    | PTuple Pattern Pattern (Maybe Pattern)
    | PList (TList Pattern)
    | PCons Pattern Pattern
    | PBool Union Bool
    | PChr ES.TString
    | PStr ES.TString
    | PInt Int
    | PCtor
        --{ p_home : ModuleName.Canonical
        --, p_type : Name.Name
        --, p_union : Union
        --, p_name : Name.Name
        --, p_index : Index.ZeroBased
        --, p_args : List_ PatternCtorArg
        --}
        -- CACHE _p_home, _p_type, and _p_vars for type inference
        -- CACHE _p_index to replace _p_name in PROD code gen
        -- CACHE _p_opts to allocate less in PROD code gen
        -- CACHE _p_alts and _p_numAlts for exhaustiveness checker
        ModuleName.Canonical
        Name.Name
        Union
        Name.Name
        Index.ZeroBased
        (TList PatternCtorArg)


type PatternCtorArg
    = PatternCtorArg
        --{ index : Index.ZeroBased -- CACHE for destructors/errors
        --, type : Type             -- CACHE for type inference
        --, arg : Pattern
        --}
        Index.ZeroBased
        Type
        Pattern



-- TYPES


type Annotation
    = Forall FreeVars Type


type alias FreeVars =
    Map.Map Name.Name ()


type Type
    = TLambda Type Type
    | TVar Name.Name
    | TType ModuleName.Canonical Name.Name (TList Type)
    | TRecord (Map.Map Name.Name FieldType) (Maybe Name.Name)
    | TUnit
    | TTuple Type Type (Maybe Type)
    | TAlias ModuleName.Canonical Name.Name (TList ( Name.Name, Type )) AliasType


type AliasType
    = Holey Type
    | Filled Type


type FieldType
    = FieldType Int Type



-- NOTE: The Word16 marks the source order, but it may not be available
-- for every canonical type. For example, if the canonical type is inferred
-- the orders will all be zeros.


fieldsToList : Map.Map Name.Name FieldType -> TList ( Name.Name, Type )
fieldsToList fields =
    let
        getIndex ( _, FieldType index _ ) =
            index

        dropIndex ( name, FieldType _ tipe ) =
            ( name, tipe )
    in
    MList.map dropIndex (MList.sortOn getIndex (Map.toList fields))



-- MODULES


type Module
    = Module
        --{ name    : ModuleName.Canonical
        --, exports : Exports
        --, decls   : Decls
        --, unions  : Map.Map Name.Name Union
        --, aliases : Map.Map Name.Name Alias
        --, binops  : Map.Map Name.Name Binop
        --, effects : Effects
        --}
        ModuleName.Canonical
        Exports
        Decls
        (Map.Map Name.Name Union)
        (Map.Map Name.Name Alias)
        (Map.Map Name.Name Binop)
        Effects


getName : Module -> ModuleName.Canonical
getName (Module name _ _ _ _ _ _) =
    name


type Alias
    = Alias (TList Name.Name) Type


type Binop
    = Binop_ Binop.Associativity Binop.Precedence Name.Name


type Union
    = Union
        --{ u_vars : List_ Name.Name
        --, u_alts : List_ Ctor
        --, u_numAlts : Int -- CACHE numAlts for exhaustiveness checking
        --, u_opts : CtorOpts -- CACHE which optimizations are available
        --}
        (TList Name.Name)
        (TList Ctor)
        Int
        CtorOpts


type CtorOpts
    = Normal
    | Enum
    | Unbox


ctorOptsToString : CtorOpts -> String
ctorOptsToString ctorOpts =
    case ctorOpts of
        Normal ->
            "Normal"

        Enum ->
            "Enum"

        Unbox ->
            "Unbox"


type Ctor
    = Ctor Name.Name Index.ZeroBased Int (TList Type) -- CACHE length args



-- EXPORTS


type Exports
    = ExportEverything
    | Export (Map.Map Name.Name (A.Located Export))


type Export
    = ExportValue
    | ExportBinop
    | ExportAlias
    | ExportUnionOpen
    | ExportUnionClosed
    | ExportPort



-- EFFECTS


type Effects
    = NoEffects
    | Ports (Map.Map Name.Name Port)
    | Manager A.Region A.Region A.Region Manager


type Port
    = Incoming FreeVars Type Type -- { freeVars : FreeVars, payload : Type, func : Type }
    | Outgoing FreeVars Type Type -- { freeVars : FreeVars, payload : Type, func : Type }


type Manager
    = CCmd Name.Name
    | CSub Name.Name
    | Fx Name.Name Name.Name



-- BINARY


bAlias : B.Binary Alias
bAlias =
    B.bin2 Alias
        (\(Alias a b) -> B.T2 a b)
        (B.bTList Name.bName)
        bType


bUnion : B.Binary Union
bUnion =
    B.bin4 Union
        (\(Union a b c d) -> B.T4 a b c d)
        (B.bTList Name.bName)
        (B.bTList bCtor)
        B.bWord64
        bCtorOpts


bCtor : B.Binary Ctor
bCtor =
    B.bin4 Ctor
        (\(Ctor a b c d) -> B.T4 a b c d)
        Name.bName
        Index.bZeroBased
        B.bWord64
        (B.bTList bType)


bCtorOpts : B.Binary CtorOpts
bCtorOpts =
    B.enum "binary encoding of CtorOpts was corrupted"
        [ Normal, Enum, Unbox ]


bAnnotation : B.Binary Annotation
bAnnotation =
    B.bin2 Forall
        (\(Forall a b) -> B.T2 a b)
        (B.bMap Name.bName B.bUnit)
        bType


bType : B.Binary Type
bType =
    B.custom "can't happen"
        (\p0 p1 p2 p3 p4 p5 p6 p7 tipe ->
            case tipe of
                TLambda a b ->
                    p0 a b

                TVar a ->
                    p1 a

                TRecord a b ->
                    p2 a b

                TUnit ->
                    p3

                TTuple a b c ->
                    p4 a b c

                TAlias a b c d ->
                    p5 a b c d

                TType home name ts ->
                    let
                        potentialWord =
                            MList.length ts + 7
                    in
                    if potentialWord <= 255 then
                        p7 potentialWord home name ts

                    else
                        p6 home name ts
        )
        |> B.var2 0 TLambda (B.lazy <| \() -> bType) (B.lazy <| \() -> bType)
        |> B.var1 1 TVar Name.bName
        |> B.var2 2 TRecord (B.bMap Name.bName bFieldType) (B.bMaybe Name.bName)
        |> B.var0 3 TUnit
        |> B.var3 4 TTuple (B.lazy <| \() -> bType) (B.lazy <| \() -> bType) (B.bMaybe (B.lazy <| \() -> bType))
        |> B.var4 5 TAlias ModuleName.bCanonical Name.bName (B.bTList (B.bTuple Name.bName (B.lazy <| \() -> bType))) bAliasType
        |> B.var3 6 TType ModuleName.bCanonical Name.bName (B.bTList (B.lazy <| \() -> bType))
        |> B.customVar3 (\id -> id > 6) TType (\_ -> ModuleName.bCanonical) (\_ -> Name.bName) (\id -> B.bSequence (B.lazy <| \() -> bType) (id - 7))
        |> B.finish


bAliasType : B.Binary AliasType
bAliasType =
    B.custom "binary encoding of AliasType was corrupted"
        (\p0 p1 aliasType ->
            case aliasType of
                Holey tipe ->
                    p0 tipe

                Filled tipe ->
                    p1 tipe
        )
        |> B.var1 0 Holey (B.lazy <| \() -> bType)
        |> B.var1 1 Filled (B.lazy <| \() -> bType)
        |> B.finish


bFieldType : B.Binary FieldType
bFieldType =
    B.bin2 FieldType (\(FieldType a b) -> B.T2 a b) B.bWord16 (B.lazy <| \() -> bType)
