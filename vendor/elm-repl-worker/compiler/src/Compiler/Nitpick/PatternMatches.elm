{- MANUALLY FORMATTED -}
module Compiler.Nitpick.PatternMatches exposing
  ( check
  , Error(..)
  , Context(..)
  , Pattern(..)
  , Literal(..)
  )


{- The algorithm used here comes from "Warnings for Pattern Matching"
by Luc Maranget. Check it out for more information!

http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

-}

import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.String as ES
import Compiler.Reporting.Annotation as A
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe



-- PATTERN


type Pattern
  = Anything
  | Literal Literal
  | Ctor Can.Union Name.Name (TList Pattern)


type Literal
  = Chr ES.TString
  | Str ES.TString
  | CInt Int



-- CREATE SIMPLIFIED PATTERNS


simplify : Can.Pattern -> Pattern
simplify (A.At _ pattern) =
  case pattern of
    Can.PAnything ->
      Anything

    Can.PVar _ ->
      Anything

    Can.PRecord _ ->
      Anything

    Can.PUnit ->
      Ctor unit unitName []

    Can.PTuple a b Nothing ->
      Ctor pair pairName [ simplify a, simplify b ]

    Can.PTuple a b (Just c) ->
      Ctor triple tripleName [ simplify a, simplify b, simplify c ]

    Can.PCtor _ _ union name _ args ->
      Ctor union name <|
        MList.map (\(Can.PatternCtorArg _ _ arg) -> simplify arg) args

    Can.PList entries ->
      MList.foldr cons nil entries

    Can.PCons hd tl ->
      cons hd (simplify tl)

    Can.PAlias subPattern _ ->
      simplify subPattern

    Can.PInt int ->
      Literal (CInt int)

    Can.PStr str ->
      Literal (Str str)

    Can.PChr chr ->
      Literal (Chr chr)

    Can.PBool union bool ->
      Ctor union (if bool then Name.true else Name.false) []


cons : Can.Pattern -> Pattern -> Pattern
cons hd tl =
  Ctor list consName [ simplify hd, tl ]


nil : Pattern
nil =
  Ctor list nilName []



-- BUILT-IN UNIONS


unit : Can.Union
unit =
  let
    ctor =
      Can.Ctor unitName Index.first 0 []
  in
  Can.Union [] [ ctor ] 1 Can.Normal


pair : Can.Union
pair =
  let
    ctor =
      Can.Ctor pairName Index.first 2 [Can.TVar "a", Can.TVar "b"]
  in
  Can.Union ["a","b"] [ ctor ] 1 Can.Normal


triple : Can.Union
triple =
  let
    ctor =
      Can.Ctor tripleName Index.first 3 [Can.TVar "a", Can.TVar "b", Can.TVar "c"]
  in
  Can.Union ["a","b","c"] [ ctor ] 1 Can.Normal


list : Can.Union
list =
  let
    nilCtor =
      Can.Ctor nilName Index.first 0 []

    consCtor =
      Can.Ctor consName Index.second 2
        [ Can.TVar "a"
        , Can.TType ModuleName.list Name.list [Can.TVar "a"]
        ]
  in
  Can.Union ["a"] [ nilCtor, consCtor ] 2 Can.Normal


unitName : Name.Name
unitName = "#0"


pairName : Name.Name
pairName = "#2"


tripleName : Name.Name
tripleName = "#3"


consName : Name.Name
consName = "::"


nilName : Name.Name
nilName = "[]"



-- ERROR


type Error
  = Incomplete A.Region Context (TList Pattern)
  | Redundant A.Region A.Region Int


type Context
  = BadArg
  | BadDestruct
  | BadCase



-- CHECK


check : Can.Module -> Either (NE.TList Error) ()
check (Can.Module _ _ decls _ _ _ _) =
  case checkDecls decls [] identity of
    [] ->
      Right ()

    e :: es ->
      Left (NE.CList e es)



-- CHECK DECLS


checkDecls : Can.Decls -> TList Error -> (TList Error -> TList Error) -> TList Error
checkDecls decls errors cont =
  case decls of
    Can.Declare def subDecls ->
      checkDecls subDecls errors (checkDef def >> cont)

    Can.DeclareRec def defs subDecls ->
      MList.foldr checkDef (checkDecls subDecls errors (checkDef def >> cont)) defs

    Can.SaveTheEnvironment ->
      cont errors



-- CHECK DEFS


checkDef : Can.Def -> TList Error -> TList Error
checkDef def errors =
  case def of
    Can.Def _ args body ->
      MList.foldr checkArg (checkExpr body errors) args

    Can.TypedDef _ _ args body _ ->
      MList.foldr checkTypedArg (checkExpr body errors) args


checkArg : Can.Pattern -> TList Error -> TList Error
checkArg ((A.At region _) as pattern) errors =
  checkPatterns region BadArg [pattern] errors


checkTypedArg : (Can.Pattern, tipe) -> TList Error -> TList Error
checkTypedArg (((A.At region _) as pattern), _) errors =
  checkPatterns region BadArg [pattern] errors



-- CHECK EXPRESSIONS


checkExpr : Can.Expr -> TList Error -> TList Error
checkExpr (A.At region expression) errors =
  case expression of
    Can.VarLocal _ ->
      errors

    Can.VarTopLevel _ _ ->
      errors

    Can.VarKernel _ _ ->
      errors

    Can.VarForeign _ _ _ ->
      errors

    Can.VarCtor _ _ _ _ _ ->
      errors

    Can.VarDebug _ _ _ ->
      errors

    Can.VarOperator _ _ _ _ ->
      errors

    Can.Chr _ ->
      errors

    Can.Str _ ->
      errors

    Can.CInt _ ->
      errors

    Can.CFloat _ ->
      errors

    Can.CList entries ->
      MList.foldr checkExpr errors entries

    Can.Negate expr ->
      checkExpr expr errors

    Can.Binop _ _ _ _ left right ->
      checkExpr left <|
        checkExpr right errors

    Can.Lambda args body ->
      MList.foldr checkArg (checkExpr body errors) args

    Can.Call func args ->
      checkExpr func <| MList.foldr checkExpr errors args

    Can.If branches finally ->
      MList.foldr checkIfBranch (checkExpr finally errors) branches

    Can.Let def body ->
      checkDef def <| checkExpr body errors

    Can.LetRec defs body ->
      MList.foldr checkDef (checkExpr body errors) defs

    Can.LetDestruct ((A.At reg _) as pattern) expr body ->
      checkPatterns reg BadDestruct [pattern] <|
        checkExpr expr <| checkExpr body errors

    Can.Case expr branches ->
      checkExpr expr <| checkCases region branches errors

    Can.Accessor _ ->
      errors

    Can.Access record _ ->
      checkExpr record errors

    Can.Update _ record fields ->
      checkExpr record <| Map.foldr checkField errors fields

    Can.Record fields ->
      Map.foldr checkExpr errors fields

    Can.Unit ->
      errors

    Can.Tuple a b maybeC ->
      checkExpr a <|
        checkExpr b <|
          case maybeC of
            Nothing ->
              errors

            Just c ->
              checkExpr c errors

    Can.Shader _ _ ->
      errors



-- CHECK FIELD


checkField : Can.FieldUpdate -> TList Error -> TList Error
checkField (Can.FieldUpdate _ expr) errors =
  checkExpr expr errors



-- CHECK IF BRANCH


checkIfBranch : (Can.Expr, Can.Expr) -> TList Error -> TList Error
checkIfBranch (condition, branch) errs =
  checkExpr condition <| checkExpr branch errs



-- CHECK CASE EXPRESSION


checkCases : A.Region -> TList Can.CaseBranch -> TList Error -> TList Error
checkCases region branches errors =
  let
    (patterns, newErrors) =
      MList.foldr checkCaseBranch ([], errors) branches
  in
  checkPatterns region BadCase patterns newErrors


checkCaseBranch : Can.CaseBranch -> (TList Can.Pattern, TList Error) -> (TList Can.Pattern, TList Error)
checkCaseBranch (Can.CaseBranch pattern expr) (patterns, errors) =
  ( pattern::patterns
  , checkExpr expr errors
  )



-- CHECK PATTERNS


checkPatterns : A.Region -> Context -> TList Can.Pattern -> TList Error -> TList Error
checkPatterns region context patterns errors =
  case toNonRedundantRows region patterns of
    Left err ->
      err::errors

    Right matrix ->
      case isExhaustive matrix 1 of
        [] ->
          errors

        badPatterns ->
          Incomplete region context (MList.map MList.head badPatterns) :: errors



-- EXHAUSTIVE PATTERNS


-- INVARIANTS:
--
--   The initial rows "matrix" are all of length 1
--   The initial count of items per row "n" is also 1
--   The resulting rows are examples of missing patterns
--
isExhaustive : TList (TList Pattern) -> Int -> TList (TList Pattern)
isExhaustive matrix n =
  case matrix of
    [] ->
      [MList.replicate n Anything]

    _ ->
      if n == 0 then
        []
      else
      let
        ctors = collectCtors matrix
        numSeen = Map.size ctors
      in
      if numSeen == 0 then
        MList.map ((::) Anything)
          <| isExhaustive (MMaybe.mapMaybe specializeRowByAnything matrix) (n - 1)

      else
        let ((Can.Union _ altList numAlts _) as alts) = Tuple.second (Map.findMin ctors) in
        if numSeen < numAlts then
          MList.pure (::)
            |> MList.andMap (MMaybe.mapMaybe (isMissing alts ctors) altList)
            |> MList.andMap (isExhaustive (MMaybe.mapMaybe specializeRowByAnything matrix) (n - 1))

        else
          let
            isAltExhaustive (Can.Ctor name _ arity _) =
              MList.map (recoverCtor alts name arity) <|
              isExhaustive
                (MMaybe.mapMaybe (specializeRowByCtor name arity) matrix)
                (arity + n - 1)
          in
          MList.concatMap isAltExhaustive altList


isMissing : Can.Union -> Map.Map Name.Name a -> Can.Ctor -> Maybe Pattern
isMissing union ctors (Can.Ctor name _ arity _) =
  if Map.member name ctors then
    Nothing
  else
    Just (Ctor union name (MList.replicate arity Anything))


recoverCtor : Can.Union -> Name.Name -> Int -> TList Pattern -> TList Pattern
recoverCtor union name arity patterns =
  let
    (args, rest) =
      MList.splitAt arity patterns
  in
  Ctor union name args :: rest



-- REDUNDANT PATTERNS


-- INVARIANT: Produces a list of rows where (forall row. length row == 1)
toNonRedundantRows : A.Region -> TList Can.Pattern -> Either Error (TList (TList Pattern))
toNonRedundantRows region patterns =
  toSimplifiedUsefulRows region [] patterns


-- INVARIANT: Produces a list of rows where (forall row. length row == 1)
toSimplifiedUsefulRows : A.Region -> TList (TList Pattern) -> TList Can.Pattern -> Either Error (TList (TList Pattern))
toSimplifiedUsefulRows overallRegion checkedRows uncheckedPatterns =
  case uncheckedPatterns of
    [] ->
      Right checkedRows

    ((A.At region _) as pattern) :: rest ->
      let nextRow = [simplify pattern] in
      if isUseful checkedRows nextRow then
        toSimplifiedUsefulRows overallRegion (nextRow :: checkedRows) rest
      else
        Left (Redundant overallRegion region (MList.length checkedRows + 1))


-- Check if a new row "vector" is useful given previous rows "matrix"
isUseful : TList (TList Pattern) -> TList Pattern -> Bool
isUseful matrix vector =
  case matrix of
    [] ->
      -- No rows are the same as the new vector! The vector is useful!
      True

    _ ->
      case vector of
        [] ->
          -- There is nothing left in the new vector, but we still have
          -- rows that match the same things. This is not a useful vector!
          False

        firstPattern :: patterns ->
          case firstPattern of
            Ctor _ name args ->
              -- keep checking rows that start with this Ctor or Anything
              isUseful
                (MMaybe.mapMaybe (specializeRowByCtor name (MList.length args)) matrix)
                (args ++ patterns)

            Anything ->
              -- check if all alts appear in matrix
              case isComplete matrix of
                No ->
                  -- This Anything is useful because some Ctors are missing.
                  -- But what if a previous row has an Anything?
                  -- If so, this one is not useful.
                  isUseful (MMaybe.mapMaybe specializeRowByAnything matrix) patterns

                Yes alts ->
                  -- All Ctors are covered, so this Anything is not needed for any
                  -- of those. But what if some of those Ctors have subpatterns
                  -- that make them less general? If so, this actually is useful!
                  let
                    isUsefulAlt (Can.Ctor name _ arity _) =
                      isUseful
                        (MMaybe.mapMaybe (specializeRowByCtor name arity) matrix)
                        (MList.replicate arity Anything ++ patterns)
                  in
                    MList.any isUsefulAlt alts

            Literal literal ->
              -- keep checking rows that start with this Literal or Anything
              isUseful
                (MMaybe.mapMaybe (specializeRowByLiteral literal) matrix)
                patterns


-- INVARIANT: (length row == N) ==> (length result == arity + N - 1)
specializeRowByCtor : Name.Name -> Int -> TList Pattern -> Maybe (TList Pattern)
specializeRowByCtor ctorName arity row =
  case row of
    Ctor _ name args :: patterns ->
      if name == ctorName then
        Just (args ++ patterns)
      else
        Nothing

    Anything :: patterns ->
      Just (MList.replicate arity Anything ++ patterns)

    Literal _ :: _ ->
      Debug.todo <|
        "Compiler bug! After type checking, constructors and literals"
        ++ " should never align in pattern match exhaustiveness checks."

    [] ->
      Debug.todo "Compiler error! Empty matrices should not get specialized."


-- INVARIANT: (length row == N) ==> (length result == N-1)
specializeRowByLiteral : Literal -> TList Pattern -> Maybe (TList Pattern)
specializeRowByLiteral literal row =
  case row of
    Literal lit :: patterns ->
      if lit == literal then
        Just patterns
      else
        Nothing

    Anything :: patterns ->
      Just patterns

    Ctor _ _ _ :: _ ->
      Debug.todo <|
        "Compiler bug! After type checking, constructors and literals"
        ++ " should never align in pattern match exhaustiveness checks."

    [] ->
      Debug.todo "Compiler error! Empty matrices should not get specialized."


-- INVARIANT: (length row == N) ==> (length result == N-1)
specializeRowByAnything : TList Pattern -> Maybe (TList Pattern)
specializeRowByAnything row =
  case row of
    [] ->
      Nothing

    Ctor _ _ _ :: _ ->
      Nothing

    Anything :: patterns ->
      Just patterns

    Literal _ :: _ ->
      Nothing



-- ALL CONSTRUCTORS ARE PRESENT?


type Complete
  = Yes (TList Can.Ctor)
  | No


isComplete : TList (TList Pattern) -> Complete
isComplete matrix =
  let
    ctors = collectCtors matrix
    numSeen = Map.size ctors
  in
    if numSeen == 0 then
      No
    else
      let (Can.Union _ alts numAlts _) = Tuple.second (Map.findMin ctors) in
      if numSeen == numAlts then Yes alts else No



-- COLLECT CTORS


collectCtors : TList (TList Pattern) -> Map.Map Name.Name Can.Union
collectCtors matrix =
  MList.foldl collectCtorsHelp Map.empty matrix


collectCtorsHelp : Map.Map Name.Name Can.Union -> TList Pattern -> Map.Map Name.Name Can.Union
collectCtorsHelp ctors row =
  case row of
    Ctor union name _ :: _ ->
      Map.insert name union ctors

    _ ->
      ctors
