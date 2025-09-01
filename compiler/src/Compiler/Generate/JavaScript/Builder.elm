{- MANUALLY FORMATTED -}
module Compiler.Generate.JavaScript.Builder exposing
  ( stmtToBuilder
  , exprToBuilder
  , Expr(..), LValue(..)
  , Stmt(..), Case(..)
  , InfixOp(..), PrefixOp(..)
  )

-- Based on the language-ecmascript package.
-- https://hackage.haskell.org/package/language-ecmascript
-- They did the hard work of reading the spec to figure out
-- how all the types should fit together.

import Compiler.Generate.JavaScript.Name as Name exposing (Name)
import Compiler.Json.Encode as Json
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe



-- EXPRESSIONS


-- NOTE: I tried making this create a B.Builder directly.
--
-- The hope was that it'd allocate less and speed things up, but it seemed
-- to be neutral for perf.
--
-- The downside is that Generate.JavaScript.Expression inspects the
-- structure of Expr and Stmt on some occassions to try to strip out
-- unnecessary closures. I think these closures are already avoided
-- by other logic in code gen these days, but I am not 100% certain.
--
-- For this to be worth it, I think it would be necessary to avoid
-- returning tuples when generating expressions.
--
{- NEW: AsyncCall, AsyncFunction -}
type Expr
  = CString String
  | CFloat String
  | CInt Int
  | CBool Bool
  | Json Json.Value
  | Array (TList Expr)
  | Object (TList (Name, Expr))
  | Ref Name
  | Access Expr Name -- foo.bar
  | Index  Expr Expr -- foo[bar]
  | Prefix PrefixOp Expr
  | Infix InfixOp Expr Expr
  | If Expr Expr Expr
  | Assign LValue Expr
  | Call Expr (TList Expr)
  | Function (Maybe Name) (TList Name) (TList Stmt)
  | AsyncCall Expr (TList Expr)
  | AsyncFunction (Maybe Name) (TList Name) (TList Stmt)


type LValue
  = LRef Name
  | LBracket Expr Expr



-- STATEMENTS


type Stmt
  = Block (TList Stmt)
  | EmptyStmt
  | ExprStmt Expr
  | IfStmt Expr Stmt Stmt
  | Switch Expr (TList Case)
  | While Expr Stmt
  | Break (Maybe Name)
  | Continue (Maybe Name)
  | Labelled Name Stmt
  | Try Stmt Name Stmt
  | Throw Expr
  | Return Expr
  | Var Name Expr
  | Vars (TList (Name, Expr))
  | FunctionStmt Name (TList Name) (TList Stmt)


type Case
  = Case Expr (TList Stmt)
  | Default (TList Stmt)



-- OPERATORS


type InfixOp
  = OpAdd -- +
  | OpSub -- -
  | OpMul -- *
  | OpDiv -- /
  | OpMod -- %
  | OpEq -- ===
  | OpNe -- !==
  | OpLt -- <
  | OpLe -- <=
  | OpGt -- >
  | OpGe -- >=
  | OpAnd -- &&
  | OpOr  -- ||
  | OpBitwiseAnd -- &
  | OpBitwiseXor -- ^
  | OpBitwiseOr  -- |
  | OpLShift     -- <<
  | OpSpRShift   -- >>
  | OpZfRShift   -- >>>


type PrefixOp
  = PrefixNot        -- !
  | PrefixNegate     -- -
  | PrefixComplement -- ~



-- ENCODE


stmtToBuilder : Stmt -> String
stmtToBuilder stmts =
  fromStmt levelZero stmts


exprToBuilder : Expr -> String
exprToBuilder expr =
  Tuple.second <| fromExpr levelZero Whatever expr



-- INDENT LEVEL


type Level =
  Level String (() -> Level)


levelZero : Level
levelZero =
  Level "" (\() -> makeLevel "\t" "\t")


makeLevel : String -> String -> Level
makeLevel increase value =
  Level value (\() -> makeLevel increase (value ++ increase))



-- HELPERS


commaSep : TList String -> String
commaSep builders =
  String.concat (MList.intersperse ", " builders)


commaNewlineSep : Level -> TList String -> String
commaNewlineSep (Level _ toDeeperIndent) builders =
  let (Level deeperIndent _) = toDeeperIndent () in
  String.concat (MList.intersperse (",\n" ++ deeperIndent) builders)



-- STATEMENTS


fromStmtBlock : Level -> TList Stmt -> String
fromStmtBlock level stmts =
  String.concat (MList.map (fromStmt level) stmts)


fromStmt : Level -> Stmt -> String
fromStmt ((Level indent toNextLevel) as level) statement =
  case statement of
    Block stmts ->
      fromStmtBlock level stmts

    EmptyStmt ->
      ""

    ExprStmt expr ->
      indent ++ Tuple.second (fromExpr level Whatever expr) ++ ";\n"

    IfStmt condition thenStmt elseStmt ->
      String.concat
        [ indent, "if (", Tuple.second (fromExpr level Whatever condition), ") {\n"
        , fromStmt (toNextLevel ()) thenStmt
        , indent, "} else {\n"
        , fromStmt (toNextLevel ()) elseStmt
        , indent, "}\n"
        ]

    Switch expr clauses ->
      String.concat
        [ indent, "switch (", Tuple.second (fromExpr level Whatever expr), ") {\n"
        , String.concat (MList.map (fromClause (toNextLevel ())) clauses)
        , indent, "}\n"
        ]

    While expr stmt ->
      String.concat
        [ indent, "while (", Tuple.second (fromExpr level Whatever expr), ") {\n"
        , fromStmt (toNextLevel ()) stmt
        , indent, "}\n"
        ]

    Break Nothing ->
      indent ++ "break;\n"

    Break (Just label) ->
      indent ++ "break " ++ Name.toBuilder label ++ ";\n"

    Continue Nothing ->
      indent ++ "continue;\n"

    Continue (Just label) ->
      indent ++ "continue " ++ Name.toBuilder label ++ ";\n"

    Labelled label stmt ->
      String.concat
        [ indent, Name.toBuilder label, ":\n"
        , fromStmt level stmt
        ]

    Try tryStmt errorName catchStmt ->
      String.concat
        [ indent, "try {\n"
        , fromStmt (toNextLevel ()) tryStmt
        , indent, "} catch (", Name.toBuilder errorName, ") {\n"
        , fromStmt (toNextLevel ()) catchStmt
        , indent, "}\n"
        ]

    Throw expr ->
      indent ++ "throw " ++ Tuple.second (fromExpr level Whatever expr) ++ ";"

    Return expr ->
      indent ++ "return " ++ Tuple.second (fromExpr level Whatever expr) ++ ";\n"

    Var name expr ->
      indent ++ "var " ++ Name.toBuilder name ++ " = " ++ Tuple.second (fromExpr level Whatever expr) ++ ";\n"

    Vars [] ->
      ""

    Vars vars ->
      indent ++ "var " ++ commaNewlineSep level (MList.map (varToBuilder level) vars) ++ ";\n"

    FunctionStmt name args stmts ->
      indent ++ "function " ++ Name.toBuilder name ++ "(" ++ commaSep (MList.map Name.toBuilder args) ++ ") {\n"
      ++
        fromStmtBlock (toNextLevel ()) stmts
      ++
      indent ++ "}\n"



-- SWITCH CLAUSES


fromClause : Level -> Case -> String
fromClause ((Level indent toNextLevel) as level) clause =
  case clause of
    Case expr stmts ->
      indent ++ "case " ++ Tuple.second (fromExpr level Whatever expr) ++ ":\n"
      ++ fromStmtBlock (toNextLevel ()) stmts

    Default stmts ->
      indent ++ "default:\n"
      ++ fromStmtBlock (toNextLevel ()) stmts



-- VAR DECLS


varToBuilder : Level -> (Name, Expr) -> String
varToBuilder level (name, expr) =
  Name.toBuilder name ++ " = " ++ Tuple.second (fromExpr level Whatever expr)



-- EXPRESSIONS


type Lines = One | Many


merge : Lines -> Lines -> Lines
merge a b =
  if a == Many || b == Many then Many else One


linesMap : (a -> (Lines, b)) -> TList a -> (Bool, TList b)
linesMap func xs =
  let
    pairs = MList.map func xs
  in
  ( MList.any ((==) Many << Tuple.first) pairs
  , MList.map Tuple.second pairs
  )


type Grouping = Atomic | Whatever


parensFor : Grouping -> String -> String
parensFor grouping builder =
  case grouping of
    Atomic ->
      "(" ++ builder ++ ")"

    Whatever ->
      builder


fromExpr : Level -> Grouping -> Expr -> (Lines, String)
fromExpr ((Level indent toNextLevel) as level) grouping expression =
  let ((Level deeperIndent _) as nextLevel) = toNextLevel () in
  case expression of
    CString string ->
      ( One, "'" ++ string ++ "'" )

    CFloat float ->
      ( One, float )

    CInt n ->
      ( One, String.fromInt n )

    CBool bool ->
      ( One, if bool then "true" else "false" )

    Json json ->
      ( One, Json.encodeUgly json )

    Array exprs ->
      Tuple.pair Many <|
        let
          (anyMany, builders) = linesMap (fromExpr level Whatever) exprs
        in
        if anyMany then
          "[\n"
          ++ deeperIndent
          ++ commaNewlineSep level builders
          ++ "\n" ++ indent ++ "]"
        else
          "[" ++ commaSep builders ++ "]"

    Object fields ->
      Tuple.pair Many <|
        let
          (anyMany, builders) = linesMap (fromField nextLevel) fields
        in
        if anyMany then
          "{\n"
          ++ deeperIndent
          ++ commaNewlineSep level builders
          ++ "\n" ++ indent ++ "}"
        else
          "{" ++ commaSep builders ++ "}"

    Ref name ->
      ( One, Name.toBuilder name )

    Access expr field ->
      makeDot level expr field

    Index expr bracketedExpr ->
      makeBracketed level expr bracketedExpr

    Prefix op expr ->
      let
        (lines, builder) = fromExpr level Atomic expr
      in
      ( lines
      , parensFor grouping (fromPrefix op ++ builder)
      )

    Infix op leftExpr rightExpr ->
      let
        (leftLines , left ) = fromExpr level Atomic leftExpr
        (rightLines, right) = fromExpr level Atomic rightExpr
      in
      ( merge leftLines rightLines
      , parensFor grouping (left ++ fromInfix op ++ right)
      )

    If condExpr thenExpr elseExpr ->
      let
        condB = Tuple.second (fromExpr level Atomic condExpr)
        thenB = Tuple.second (fromExpr level Atomic thenExpr)
        elseB = Tuple.second (fromExpr level Atomic elseExpr)
      in
      ( Many
      , parensFor grouping (condB ++ " ? " ++ thenB ++ " : " ++ elseB)
      )

    Assign lValue expr ->
      let
        (leftLines , left ) = fromLValue level lValue
        (rightLines, right) = fromExpr level Whatever expr
      in
      ( merge leftLines rightLines
      , parensFor grouping (left ++ " = " ++ right)
      )

    Call function args ->
      Tuple.pair Many <|
        let
          (_      , funcB) = fromExpr level Atomic function
          (anyMany, argsB) = linesMap (fromExpr nextLevel Whatever) args
        in
        if anyMany then
          funcB ++ "(\n" ++ deeperIndent ++ commaNewlineSep level argsB ++ ")"
        else
          funcB ++ "(" ++ commaSep argsB ++ ")"

    Function maybeName args stmts ->
      Tuple.pair Many <|
        "function " ++ MMaybe.maybe "" Name.toBuilder maybeName ++ "(" ++ commaSep (MList.map Name.toBuilder args) ++ ") {\n"
        ++
          fromStmtBlock nextLevel stmts
        ++
        indent ++ "}"

    AsyncCall function args ->
      Tuple.pair Many <|
        let
          (_      , funcB) = fromExpr level Atomic function
          (anyMany, argsB) = linesMap (fromExpr nextLevel Whatever) args
        in
        "await " ++ if anyMany then
          funcB ++ "(\n" ++ deeperIndent ++ commaNewlineSep level argsB ++ ")"
        else
          funcB ++ "(" ++ commaSep argsB ++ ")"

    AsyncFunction maybeName args stmts ->
      Tuple.pair Many <|
        "async function " ++ MMaybe.maybe "" Name.toBuilder maybeName ++ "(" ++ commaSep (MList.map Name.toBuilder args) ++ ") {\n"
        ++
          fromStmtBlock nextLevel stmts
        ++
        indent ++ "}"



-- FIELDS


fromField : Level -> (Name, Expr) -> (Lines, String)
fromField level (field, expr) =
  let
    (lines, builder) = fromExpr level Whatever expr
  in
  ( lines
  , Name.toBuilder field ++ ": " ++ builder
  )



-- VALUES


fromLValue : Level -> LValue -> (Lines, String)
fromLValue level lValue =
  case lValue of
    LRef name ->
      (One, Name.toBuilder name)

    LBracket expr bracketedExpr ->
      makeBracketed level expr bracketedExpr


makeDot : Level -> Expr -> Name -> (Lines, String)
makeDot level expr field =
  let
    (lines, builder) = fromExpr level Atomic expr
  in
  (lines, builder ++ "." ++ Name.toBuilder field)


makeBracketed : Level -> Expr -> Expr -> (Lines, String)
makeBracketed level expr bracketedExpr =
  let
    (lines         , builder         ) = fromExpr level Atomic expr
    (bracketedLines, bracketedBuilder) = fromExpr level Whatever bracketedExpr
  in
  ( merge lines bracketedLines
  , builder ++ "[" ++ bracketedBuilder ++ "]"
  )



-- OPERATORS


fromPrefix : PrefixOp -> String
fromPrefix op =
  case op of
    PrefixNot        -> "!"
    PrefixNegate     -> "-"
    PrefixComplement -> "~"


fromInfix : InfixOp -> String
fromInfix op =
  case op of
    OpAdd        -> " + "
    OpSub        -> " - "
    OpMul        -> " * "
    OpDiv        -> " / "
    OpMod        -> " % "
    OpEq         -> " === "
    OpNe         -> " !== "
    OpLt         -> " < "
    OpLe         -> " <= "
    OpGt         -> " > "
    OpGe         -> " >= "
    OpAnd        -> " && "
    OpOr         -> " || "
    OpBitwiseAnd -> " & "
    OpBitwiseXor -> " ^ "
    OpBitwiseOr  -> " | "
    OpLShift     -> " << "
    OpSpRShift   -> " >> "
    OpZfRShift   -> " >>> "
