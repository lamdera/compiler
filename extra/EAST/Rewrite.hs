{-# LANGUAGE OverloadedStrings #-}
module East.Rewrite where


import qualified Debug.Trace as DT
import qualified Data.Text.Lazy as T
import qualified Data.Text as Text

import qualified AST.Canonical as C
import qualified Reporting.Annotation as A
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified AST.Module.Name as ModuleName
import Data.List (intercalate)

import qualified Data.Map.Strict as Map
import Data.Function ((&))
import Data.Monoid ((<>))

import Transpile.PrettyPrint

type List a = [a]

-- RECORD ARGUMENTS TO LET EXPR

recordArgsToLet :: List C.Pattern -> C.Expr -> (List C.Pattern, C.Expr)
recordArgsToLet [] e = ([], e)
recordArgsToLet (pat:pats) e =
  let
    (np, ne) = recordArgToLet pat e
    (rpats, re) = recordArgsToLet pats ne
  in (np : rpats, re)

recordArgToLet :: C.Pattern -> C.Expr -> (C.Pattern, C.Expr) -- TODO: we're replacing records in record replacements as well, so we never construct the record anywhere :(
recordArgToLet pat expr@(A.At meta e) =
  let
    at x = (A.At meta x)
    (np, recordChanges) = recordPatNames pat
    recordChangeMap = concat $ fmap (\(recName, fields) -> fmap (\f -> (recName, f)) fields) recordChanges
  in
  ( np
  , at $ C.LetRec
      ((\(recVar, fieldName) ->
          (C.Def (A.At meta fieldName) [] (at $ C.Access (at $ C.VarLocal recVar) (at fieldName)))
      ) <$> recordChangeMap)
      expr
  )

removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []


-- replace any record pattern with its record types. Return rewritten pattern and list of record variables
recordPat :: C.Pattern_ -> C.Pattern_
recordPat p =
  case p of
    (C.PRecord names) -> C.PVar (patVarFromRecordNames names)
    p -> p

patVarFromRecordNames names = N.fromText $ "rec'" <> Text.intercalate "'" (N.toText <$> names)

recordPatNames :: C.Pattern -> (C.Pattern, List (N.Name, [N.Name]))
recordPatNames p =
  -- 1. walk over pattern, recursively
  -- 2. when we hit a new PRecord, replace it with a variable `rec'a'b`, then add `("rec'a'b", ["a", "b"])` to the list of record updates
  -- 3. return (newPattern, List (Text, List Text))
  --
  let
    pRecFetchRecordFields p = case p of
      (C.PRecord names) -> [(patVarFromRecordNames names, names)]
      _ -> []
    recordFieldsInPattern = removeDuplicates $! fPattern pRecFetchRecordFields (++) p
    patternWithReplacedRecordFields = rPattern recordPat $! p
  in
    -- DT.trace (sShow ("recordPatNames", p, "into", patternWithReplacedRecordFields, recordFieldsInPattern)) $
    (patternWithReplacedRecordFields, recordFieldsInPattern)



-- REWRITE PRIMITIVES

rPattern f (A.At m p) = A.At m (rPattern' f (f p))

rPattern' :: (C.Pattern_ -> C.Pattern_) -> C.Pattern_ -> C.Pattern_
rPattern' f p =
  case p of
    (C.PAnything) -> (C.PAnything)
    (C.PUnit) -> (C.PUnit)
    (C.PVar name) -> (C.PVar name)
    (C.PChr text) -> (C.PChr text)
    (C.PStr text) -> (C.PStr text)
    (C.PInt int) -> (C.PInt int)
    (C.PRecord names) -> (C.PRecord names)
    (C.PBool union bool) -> (C.PBool union bool)
    -- recursive
    (C.PAlias pattern name) -> (C.PAlias (rPattern f pattern) name)
    (C.PTuple p1 p2 Nothing) -> (C.PTuple (rPattern f p1) (rPattern f p2) Nothing)
    (C.PTuple p1 p2 (Just p3)) -> (C.PTuple (rPattern f p1) (rPattern f p2) (Just (rPattern f p3)))
    (C.PList pats) -> (C.PList (rPattern f <$> pats))
    (C.PCons p1 p2) -> (C.PCons (rPattern f p1) (rPattern f p2))
    (C.PCtor
      _p_home_moduleName -- :: ModuleName.Canonical
      _p_type_name -- :: N.Name
      _p_union -- :: Union
      _p_constructor_name -- :: N.Name
      _p_index -- :: Index.ZeroBased
      _p_args -- :: [PatternCtorArg]
      ) -> (C.PCtor
      _p_home_moduleName -- :: ModuleName.Canonical
      _p_type_name -- :: N.Name
      _p_union -- :: Union
      _p_constructor_name -- :: N.Name
      _p_index -- :: Index.ZeroBased
      (rPatternCtorArg f <$> _p_args) -- :: [PatternCtorArg]
      )

rPatternCtorArg f (C.PatternCtorArg idx tipe arg) = (C.PatternCtorArg idx tipe (rPattern f arg))

-- foldPattern, uses a `merge :: a -> a -> a` and a `map f -> a` function
fPattern :: (C.Pattern_ -> a) -> (a -> a -> a) -> C.Pattern -> a
fPattern f m (A.At _ p) =
  let
    fm things = (f <$> tat <$> things) ++ (fPattern f m <$> things)
  in
  foldr1 m (f p :
  case p of
    (C.PAnything) -> []
    (C.PUnit) -> []
    (C.PVar name) -> []
    (C.PChr text) -> []
    (C.PStr text) -> []
    (C.PInt int) -> []
    (C.PRecord names) -> []
    (C.PBool union bool) -> []
    -- recursive
    (C.PAlias p1 name) -> fm [p1]
    (C.PTuple p1 p2 Nothing) -> fm [p1, p2]
    (C.PTuple p1 p2 (Just p3)) -> fm [p1, p2, p3]
    (C.PList pats) -> fm pats
    (C.PCons p1 p2) -> fm [p1, p2]
    (C.PCtor _ _ _ _ _ _p_args) ->
      fm $ tPatternCtorArg <$> _p_args
  )

tPatternCtorArg (C.PatternCtorArg idx tipe arg) = arg

tat (A.At _ a) = a
