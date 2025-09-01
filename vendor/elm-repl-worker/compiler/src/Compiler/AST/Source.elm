module Compiler.AST.Source exposing
    ( Alias(..)
    , Comment(..)
    , Def(..)
    --, Docs(..)
    , Effects(..)
    , Exposed(..)
    , Exposing(..)
    , Expr
    , Expr_(..)
    , Import(..)
    , Infix(..)
    , Manager(..)
    , Module(..)
    , Pattern
    , Pattern_(..)
    , Port(..)
    , Privacy(..)
    , Type
    , Type_(..)
    , Union(..)
    , Value(..)
    , VarType(..)
    , getImportName
    , getName
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Name as Name
import Compiler.Elm.Float as EF
import Compiler.Elm.String as ES
import Compiler.Reporting.Annotation as A
import Extra.Type.List exposing (TList)



-- EXPRESSIONS


type alias Expr =
    A.Located Expr_


type Expr_
    = Chr ES.TString
    | Str ES.TString
    | CInt Int
    | CFloat EF.TFloat
    | Var VarType Name.Name
    | VarQual VarType Name.Name Name.Name
    | CList (TList Expr)
    | Op Name.Name
    | Negate Expr
    | Binops (TList ( Expr, A.Located Name.Name )) Expr
    | Lambda (TList Pattern) Expr
    | Call Expr (TList Expr)
    | If (TList ( Expr, Expr )) Expr
    | Let (TList (A.Located Def)) Expr
    | Case Expr (TList ( Pattern, Expr ))
    | Accessor Name.Name
    | Access Expr (A.Located Name.Name)
    | Update (A.Located Name.Name) (TList ( A.Located Name.Name, Expr ))
    | Record (TList ( A.Located Name.Name, Expr ))
    | Unit
    | Tuple Expr Expr (TList Expr)
    | Shader Shader.Source Shader.Types


type VarType
    = LowVar
    | CapVar



-- DEFINITIONS


type Def
    = Define (A.Located Name.Name) (TList Pattern) Expr (Maybe Type)
    | Destruct Pattern Expr



-- PATTERN


type alias Pattern =
    A.Located Pattern_


type Pattern_
    = PAnything
    | PVar Name.Name
    | PRecord (TList (A.Located Name.Name))
    | PAlias Pattern (A.Located Name.Name)
    | PUnit
    | PTuple Pattern Pattern (TList Pattern)
    | PCtor A.Region Name.Name (TList Pattern)
    | PCtorQual A.Region Name.Name Name.Name (TList Pattern)
    | PList (TList Pattern)
    | PCons Pattern Pattern
    | PChr ES.TString
    | PStr ES.TString
    | PInt Int



-- TYPE


type alias Type =
    A.Located Type_


type Type_
    = TLambda Type Type
    | TVar Name.Name
    | TType A.Region Name.Name (TList Type)
    | TTypeQual A.Region Name.Name Name.Name (TList Type)
    | TRecord (TList ( A.Located Name.Name, Type )) (Maybe (A.Located Name.Name))
    | TUnit
    | TTuple Type Type (TList Type)



-- MODULE


type Module
    = Module
        --{ name    : Maybe (A.Located Name)
        --, exports : A.Located Exposing
        --, imports : List_ Import
        --, values  : List_ (A.Located Value)
        --, unions  : List_ (A.Located Union)
        --, aliases : List_ (A.Located Alias)
        --, binops  : List_ (A.Located Infix)
        --, effects : Effects
        --}
        (Maybe (A.Located Name.Name))
        (A.Located Exposing)
        (TList Import)
        (TList (A.Located Value))
        (TList (A.Located Union))
        (TList (A.Located Alias))
        (TList (A.Located Infix))
        Effects


getName : Module -> Name.Name
getName (Module maybeName _ _ _ _ _ _ _) =
    case maybeName of
        Just (A.At _ name) ->
            name

        Nothing ->
            Name.u_Main


getImportName : Import -> Name.Name
getImportName (Import (A.At _ name) _ _) =
    name


type Import
    = Import
        --{ import : A.Located Name
        --, alias : Maybe Name
        --, exposing : Exposing
        --}
        (A.Located Name.Name)
        (Maybe Name.Name)
        Exposing


type Value
    = Value (A.Located Name.Name) (TList Pattern) Expr (Maybe Type)


type Union
    = Union (A.Located Name.Name) (TList (A.Located Name.Name)) (TList ( A.Located Name.Name, TList Type ))


type Alias
    = Alias (A.Located Name.Name) (TList (A.Located Name.Name)) Type


type Infix
    = Infix Name.Name Binop.Associativity Binop.Precedence Name.Name


type Port
    = Port (A.Located Name.Name) Type


type Effects
    = NoEffects
    | Ports (TList Port)
    | Manager A.Region Manager


type Manager
    = CCmd (A.Located Name.Name)
    | CSub (A.Located Name.Name)
    | Fx (A.Located Name.Name) (A.Located Name.Name)


type Comment
    = Comment



-- EXPOSING


type Exposing
    = Open
    | Explicit (TList Exposed)


type Exposed
    = Lower (A.Located Name.Name)
    | Upper (A.Located Name.Name) Privacy
    | Operator A.Region Name.Name


type Privacy
    = Public A.Region
    | Private
