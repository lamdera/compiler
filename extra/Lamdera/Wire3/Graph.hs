{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Wire3.Graph where

import qualified Data.Map as Map
import qualified Data.Graph as Graph

import qualified Data.Name
import AST.Canonical
import qualified Reporting.Annotation as A

import Lamdera
import Lamdera.Wire3.Helpers


-- The Decls data structure must be topologically sorted by LocalVar refs,
-- otherwise type inference will throw Map.! errors and not be able to see sub-functions
stronglyConnCompDefs defs =
  defs
    & fmap defToNode
    & Graph.stronglyConnComp


addGraphDefsToDecls decls defsGraph =
  defsGraph
    & foldr (\scc decls ->
        case scc of
          Graph.AcyclicSCC def ->
            addDef def decls
          Graph.CyclicSCC defs ->
            addRecDef defs decls
      ) decls


defToNode :: Def -> (Def, Data.Name.Name, [Data.Name.Name])
defToNode def =
  ( def, defName def, defGetEdges def )


defGetEdges :: Def -> [Data.Name.Name]
defGetEdges def =
  case def of
    Def (A.At region name_) pvars expr ->
      getLvars expr
    TypedDef (A.At region name_) freeVars pvars expr tipe ->
      getLvars expr


getLvars :: Expr -> [Data.Name.Name]
getLvars (A.At _ expr) =
  case expr of
    VarLocal name -> []
    VarTopLevel cname name -> [name]
    VarKernel module_ name -> []
    VarForeign cname name annotation -> []
    VarCtor ctorOpts cname name index annotation -> []
    VarDebug cname name annotation -> []
    VarOperator name cname name2 annotation -> []
    Chr s -> []
    Str s -> []
    Int i -> []
    Float f -> []
    List exprs -> exprs & concatMap getLvars
    Negate expr -> getLvars expr
    Binop name cname name2 annotation e1 e2 -> [e1, e2] & concatMap getLvars
    Lambda pvars expr -> getLvars expr
    Call expr params -> getLvars expr ++ concatMap getLvars params
    If [(e1, e2)] e3 -> [e1, e2, e3] & concatMap getLvars
    Let def expr -> defGetEdges def ++ getLvars expr
    LetRec defs expr -> concatMap defGetEdges defs ++ getLvars expr
    LetDestruct pat e1 e2 -> [e1, e2] & concatMap getLvars
    Case expr branches -> branches & concatMap (\(CaseBranch pat expr) -> getLvars expr)
    Accessor name -> []
    Access expr aName -> getLvars expr
    Update name expr fieldUpdates -> getLvars expr ++
      (fieldUpdates & Map.toList & concatMap (\(n, (FieldUpdate region expr)) -> getLvars expr))
    Record fields ->
      fields & Map.toList & concatMap (\(n, expr) -> getLvars expr)
    Unit -> []
    Tuple e1 e2 me3 ->
      case me3 of
        Just e3 -> [e1, e2, e3] & concatMap getLvars
        Nothing -> [e1, e2] & concatMap getLvars
    Shader source types -> []
    _ -> error $ "getLvars: impossible expr: " ++ show expr
