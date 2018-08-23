{-# LANGUAGE DuplicateRecordFields #-}

module East.Conversion where
{-
import qualified AST.Declaration as A
import qualified AST.Expression as A
import qualified AST.Module as A
import qualified AST.Pattern as AP
import qualified AST.V0_16 as A
import qualified AST.Variable as A
import qualified Reporting.Annotation as A
import qualified Reporting.Region as A

import qualified East.V0_18 as E

import qualified Cheapskate.Types as Markdown
import qualified Data.Char as Char
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Arrow (first, second, (<<<), (>>>))
import Data.Function ((&))
import Data.Monoid ((<>))

{-| This module handles conversion between East and the elm-format ast.

fromX is conversion from elm-ast to   east
  toX is conversion to   elm-ast from east
-}
-- Module
fromModule :: A.Module -> E.Module
fromModule (A.Module _ header docs pcMapUcisPcImportMethod decls) =
  E.Module
    (fromHeader header)
    (pcMapUcisPcImportMethod & snd & Map.toList &
     fmap
       (\(ucis, pcImportMethod) ->
          (fmap fromUci ucis, fromImportMethod $ snd pcImportMethod)) &
     Map.fromList)
    (concatMap fromDecl decls)

fromImportMethod :: A.ImportMethod -> E.ImportMethod
fromImportMethod (A.ImportMethod mPcPcUci pcPcDetailedListing) =
  E.ImportMethod
    (mPcPcUci & fmap snd & fmap snd & fmap fromUci)
    (pcPcDetailedListing & snd & snd & fromDetailedListing)

fromHeader :: A.Header -> E.Header
fromHeader (A.Header sourceTag cUcis mKwcSourceSettings kwcDetailedListing) =
  E.Header
    (fromSourceTag sourceTag)
    (cUcis & rc & fmap fromUci)
    (mKwcSourceSettings & fmap fromKwc & fmap fromSourceSettings)
    (kwcDetailedListing & fromKwc & fromDetailedListing)

fromKwc :: A.KeywordCommented a -> a
fromKwc (A.KeywordCommented _ _ a) = a

fromDetailedListing :: A.Listing A.DetailedListing -> E.DetailedListing
fromDetailedListing (A.OpenListing _) = E.DetailedOpenListing
fromDetailedListing (A.ClosedListing) = E.DetailedClosedListing
fromDetailedListing (A.ExplicitListing (A.DetailedListing cMaplowercaseIdentifier cMapsymbolIdentifier cMapUciPcUciListing) _) =
  E.DetailedListing
    (cMaplowercaseIdentifier & fromCmap & Map.keys & fmap fromLci)
    (cMapsymbolIdentifier & fromCmap & Map.keys & fmap fromSym)
    (cMapUciPcUciListing & fromCmap & Map.toList &
     fmap
       (\(uci, pcUciListing) ->
          (fromUci uci, pcUciListing & snd & fromUciListing)) &
     Map.fromList)

fromUciListing ::
     (A.Listing (A.CommentedMap A.UppercaseIdentifier ())) -> E.UciListing
fromUciListing (A.ExplicitListing cMapUciUnit _) =
  E.ExplicitListing (cMapUciUnit & fromCmap & Map.keys & fmap fromUci)
fromUciListing (A.OpenListing _) = E.UciOpenListing
fromUciListing (A.ClosedListing) = E.UciClosedListing

fromCmap :: A.CommentedMap k v -> Map k v
fromCmap = fmap rc

fromSourceSettings :: A.SourceSettings -> E.SourceSettings
fromSourceSettings =
  fmap (\(cLci, cUci) -> (cLci & rc & fromLci, cUci & rc & fromUci))

fromSourceTag (A.Normal)   = E.Normal
fromSourceTag (A.Effect _) = E.Effect
fromSourceTag (A.Port _)   = E.Port

-- TopLevelStructure
fromDecl :: A.TopLevelStructure A.Declaration -> [E.Declaration]
fromDecl (A.DocComment _)     = []
fromDecl (A.BodyComment _)    = []
fromDecl (A.Entry locatedDecl) = [fromDeclaration $ tat locatedDecl]

fromDeclaration :: A.Declaration -> E.Declaration
fromDeclaration (A.Definition pat pcPats _ e) =
  E.Definition
    (fromPattern pat)
    (pcPats & fmap snd & fmap fromPattern)
    (fromExpr e)
fromDeclaration (A.TypeAnnotation (ref, _) (_, t)) =
  E.TypeAnnotation (fromRef ref) (fromType t)
fromDeclaration (A.Datatype cNwargsUciLci oclNwargsUciType) =
  E.Datatype
    (cNwargsUciLci & rc & fromNameWithArgs fromUci fromLci)
    (oclNwargsUciType & fromOcl & fmap (fromNameWithArgs fromUci fromType))
fromDeclaration (A.TypeAlias _ cNwargsUciLci pcType) =
  E.TypeAlias
    (cNwargsUciLci & rc & fromNameWithArgs fromUci fromLci)
    (pcType & snd & fromType)
fromDeclaration (A.PortAnnotation cLci _ t) =
  E.PortAnnotation (cLci & rc & fromLci) (fromType t)
fromDeclaration (A.PortDefinition cLci _ e) =
  E.PortDefinition (cLci & rc & fromLci) (fromExpr e)
fromDeclaration (A.Fixity assoc _ i _ (A.OpRef sym)) =
  E.Fixity (fromAssoc assoc) i (fromSym sym)

fromNameWithArgs :: (a -> a') -> (b -> b') -> (a, [(comment, b)]) -> (a', [b'])
fromNameWithArgs fa fb = first fa >>> second (fmap snd) >>> second (fmap fb)

fromOcl :: A.OpenCommentedList a -> [a]
fromOcl (A.OpenCommentedList cWeolList pcWeol) =
  ((fmap rc cWeolList) <> [snd pcWeol]) & fmap weol

fromAssoc :: A.Assoc -> E.Assoc
fromAssoc A.L = E.L
fromAssoc A.N = E.N
fromAssoc A.R = E.R

-- Expr
fromExpr :: A.Expr -> E.Expr
fromExpr e = fromExpr' $ tat e

fromExpr' :: A.Expr' -> E.Expr
fromExpr' (A.Unit _) = E.EUnit
fromExpr' (A.Literal lit) = E.ELiteral $ fromLiteral lit
fromExpr' (A.VarExpr ref) = E.EVar $ fromQRef ref
fromExpr' (A.App e pcExprs _) =
  E.EApp (fromExpr e) (pcExprs & fmap snd & fmap fromExpr)
fromExpr' (A.Unary A.Negative e) = E.ENegative (fromExpr e)
fromExpr' (A.Binop ref e1 e2) =
  E.EBinOp (fromOpRef ref) (fromExpr e1) (fromExpr e2)
fromExpr' (A.Parens ce) = E.EParens (ce & rc & fromExpr)
fromExpr' (A.ExplicitList exprSeq _ _) =
  E.EExplicitList (exprSeq & fromSequence & fmap fromExpr)
fromExpr' (A.Range ce1 ce2 _) =
  E.ERange (ce1 & rc & fromExpr) (ce2 & rc & fromExpr)
fromExpr' (A.Tuple cexprs _) = E.ETuple (cexprs & fmap rc & fmap fromExpr)
fromExpr' (A.TupleFunction i) = E.ETupleFunc i
fromExpr' (A.Access e lci) = E.EAccess (fromExpr e) (fromLci lci)
fromExpr' (A.AccessFunction lci) = E.EAccessFunc (fromLci lci)
fromExpr' (A.Lambda cpList _ e _) =
  E.ELambda (cpList & fmap snd & fmap fromPattern) (fromExpr e)
fromExpr' (A.If ifClause pcIfClauseList (_, e)) =
  E.EIf
    ([fromIfClause ifClause] <> (pcIfClauseList & fmap snd & fmap fromIfClause))
    (fromExpr e)
fromExpr' (A.Let letDecls _ e) =
  E.ELet (concatMap fromLetDecls letDecls) (fromExpr e)
fromExpr' (A.Case (ce, _) cppceList) =
  E.ECase
    (ce & rc & fromExpr)
    -- [(Commented Pattern.Pattern, (Comments, Expr))]
    (cppceList & fmap (first rc) & fmap (second snd) & fmap (first fromPattern) &
     fmap (second fromExpr))
fromExpr' (A.Record mcLci seqLciExprPair _ _) =
  E.ERecord
    (mcLci & fmap rc & fmap fromLci)
    (seqLciExprPair & fromSequence & fmap (fromPair fromLci fromExpr))

fromQRef :: A.Ref -> E.QRef
fromQRef (A.VarRef [] lci) = E.UnQRef $ E.VarRef (fromLci lci)
fromQRef (A.TagRef [] uci) = E.UnQRef $ E.TagRef (fromUci uci)
fromQRef (A.OpRef sym) = E.UnQRef $ E.OpRef (fromSym sym)
fromQRef (A.VarRef ucis lci) =
  E.QRef (fmap fromUci ucis) $ E.VarRef (fromLci lci)
fromQRef (A.TagRef ucis uci) =
  E.QRef (fmap fromUci ucis) $ E.TagRef (fromUci uci)

fromRef :: A.Ref -> E.Ref
fromRef (A.VarRef [] lci) = E.VarRef (fromLci lci)
fromRef (A.TagRef [] uci) = E.TagRef (fromUci uci)
fromRef (A.OpRef sym)     = E.OpRef (fromSym sym)

fromOpRef :: A.Ref -> E.SymbolIdentifier
fromOpRef (A.VarRef ucis lci) = error "unexpected VarRef; expected OpRef"
fromOpRef (A.TagRef ucis uci) = error "unexpected TagRef; expected OpRef"
fromOpRef (A.OpRef sym)       = fromSym sym

fromIfClause :: A.IfClause -> E.IfClause
fromIfClause (ce1, ce2) = (ce1 & rc & fromExpr, ce2 & rc & fromExpr)

fromLetDecls :: A.LetDeclaration -> [E.LetDeclaration]
fromLetDecls (A.LetDefinition pat pcPats _ e) =
  [ E.LetDefinition
      ([fromPattern pat] <> (pcPats & fmap snd & fmap fromPattern))
      (fromExpr e)
  ]
fromLetDecls (A.LetAnnotation (ref, _) (_, t)) =
  [E.LetAnnotation (fromRef ref) (fromType t)]
fromLetDecls (A.LetComment _) = []

-- Pattern
fromPattern :: AP.Pattern -> E.Pattern
fromPattern p = fromPattern' $ tat p

fromPattern' :: AP.Pattern' -> E.Pattern
fromPattern' AP.Anything = E.PAnything
fromPattern' (AP.UnitPattern _) = E.PUnitPattern
fromPattern' (AP.Literal lit) = E.PLiteral $ fromLiteral lit
fromPattern' (AP.VarPattern lci) = E.PVar $ fromLci lci
fromPattern' (AP.OpPattern sym) = E.POp $ fromSym sym
fromPattern' (AP.PatternParens cPat) = E.PParens (cPat & rc & fromPattern)
fromPattern' (AP.EmptyListPattern _) = E.PEmptyList
fromPattern' (AP.Tuple cPats) = E.PTuple (cPats & fmap rc & fmap fromPattern)
fromPattern' (AP.List cPats) = E.PList (cPats & fmap rc & fmap fromPattern)
fromPattern' (AP.Data ucis cPats) =
  E.PData (fmap fromUci ucis) (cPats & fmap snd & fmap fromPattern)
fromPattern' (AP.ConsPattern (pat, mString) ccpatmsList) =
  E.PCons $
  (fromPattern pat, mString) :
  (ccpatmsList & fmap (\(c1, c2, p, mString) -> (fromPattern p, mString)))
fromPattern' (AP.Record clcis) = E.PRecord (clcis & fmap rc & fmap fromLci)
fromPattern' (AP.Alias (pat, _) (_, lci)) =
  E.PAlias (fromPattern pat) (fromLci lci)

fromLiteral :: A.Literal -> E.Literal
fromLiteral (A.IntNum x _)   = E.IntNum x
fromLiteral (A.FloatNum x _) = E.FloatNum x
fromLiteral (A.Chr x)      = E.Chr x
fromLiteral (A.Str x _)      = E.Str x
fromLiteral (A.Boolean x)  = E.Boolean x

-- Type
fromType :: A.Type -> E.Type
fromType t = fromType' $ tat t

fromType' :: A.Type' -> E.Type
fromType' (A.UnitType _) = E.UnitType
fromType' (A.TypeVariable lci) = E.TypeVariable (fromLci lci)
fromType' (A.TypeParens ct) = E.TypeParens (ct & rc & fromType)
fromType' (A.TupleType cweolTypes) =
  E.TupleType (cweolTypes & fmap rc & fmap weol & fmap fromType)
fromType' (A.TypeConstruction typeConstructor pcTypes) =
  E.TypeConstruction
    (fromTypeConstructor typeConstructor)
    (pcTypes & fmap snd & fmap fromType)
fromType' (A.RecordType mcLci seqPairLciType _ _) =
  E.RecordType
    (mcLci & fmap rc & fmap fromLci)
    (seqPairLciType & fromSequence & fmap (fromPair fromLci fromType))
fromType' (A.FunctionType weolType cctmsList _) =
  E.FunctionType
    (weolType & weol & fromType)
    (cctmsList & fmap (\(c1, c2, t, ms) -> (fromType t, ms)))

fromTypeConstructor (A.NamedConstructor ucis) =
  E.NamedConstructor (fmap fromUci ucis)
fromTypeConstructor (A.TupleConstructor i) = (E.TupleConstructor i)

fromSequence :: A.Sequence a -> E.List a
fromSequence s = s & fmap snd & fmap snd & fmap weol

fromPair :: (a -> a') -> (b -> b') -> A.Pair a b -> (a', b')
fromPair fromKey fromVal (A.Pair key val forceMultiline) =
  (fromKey $ fst key, fromVal $ snd val)

fromLci :: A.LowercaseIdentifier -> E.LowercaseIdentifier
fromLci (A.LowercaseIdentifier a) = a

fromUci :: A.UppercaseIdentifier -> E.UppercaseIdentifier
fromUci (A.UppercaseIdentifier a) = a

fromSym :: A.SymbolIdentifier -> E.SymbolIdentifier
fromSym (A.SymbolIdentifier a) = a

-- ### TO
-- Module
toModule :: E.Module -> A.Module
toModule (E.Module header mapUcisImportMethod decls) =
  A.Module
    []
    (toHeader header)
    (at Nothing)
    (pre $
     Map.fromList $
     fmap
       (\(ucis, importMethod) ->
          (fmap toUci ucis, pre $ toImportMethod $ importMethod)) $
     Map.toList $ mapUcisImportMethod)
    (fmap toDecl decls)

toImportMethod :: E.ImportMethod -> A.ImportMethod
toImportMethod (E.ImportMethod mUci detailedListing) =
  A.ImportMethod
    (fmap pre $ fmap pre $ fmap toUci $ mUci)
    (pre $ pre $ toDetailedListing $ detailedListing)

toHeader :: E.Header -> A.Header
toHeader (E.Header sourceTag ucis mSourceSettings detailedListing) =
  A.Header
    (toSourceTag sourceTag)
    (c $ fmap toUci $ ucis)
    (fmap toKwc $ fmap toSourceSettings $ mSourceSettings)
    (toKwc $ toDetailedListing $ detailedListing)

toKwc :: a -> A.KeywordCommented a
toKwc a = A.KeywordCommented [] [] a

toDetailedListing :: E.DetailedListing -> A.Listing A.DetailedListing
toDetailedListing E.DetailedOpenListing = A.OpenListing (c ())
toDetailedListing E.DetailedClosedListing = A.ClosedListing
toDetailedListing (E.DetailedListing lcis syms mapUciUciListing) =
  A.ExplicitListing
    (A.DetailedListing
       (toCmap $ Map.fromList $ fmap (\a -> (a, ())) $ fmap toLci $ lcis)
       (toCmap $ Map.fromList $ fmap (\a -> (a, ())) $ fmap toSym $ syms)
       (toCmap $
        Map.fromList $
        fmap
          (\(uci, uciListing) -> (toUci uci, pre $ toUciListing $ uciListing)) $
        Map.toList $ mapUciUciListing))
    False

toUciListing ::
     E.UciListing -> (A.Listing (A.CommentedMap A.UppercaseIdentifier ()))
toUciListing (E.ExplicitListing ucis) =
  A.ExplicitListing
    (toCmap $ Map.fromList $ fmap (\a -> (a, ())) $ fmap toUci $ ucis)
    False
toUciListing E.UciOpenListing = (A.OpenListing (c ()))
toUciListing E.UciClosedListing = (A.ClosedListing)

toCmap :: Map k v -> A.CommentedMap k v
toCmap = fmap c

toSourceSettings :: E.SourceSettings -> A.SourceSettings
toSourceSettings = fmap (\(lci, uci) -> (lci & toLci & c, uci & toUci & c))

toSourceTag E.Normal = (A.Normal)
toSourceTag E.Effect = (A.Effect [])
toSourceTag E.Port   = (A.Port [])

-- TopLevelStructure
toDecl :: E.Declaration -> A.TopLevelStructure A.Declaration
toDecl decl = (A.Entry $ at $ toDeclaration decl)

toDeclaration :: E.Declaration -> A.Declaration
toDeclaration (E.Definition pat pats e) =
  A.Definition (toPattern pat) (pats & fmap toPattern & fmap pre) [] (toExpr e)
toDeclaration (E.TypeAnnotation ref t) =
  A.TypeAnnotation (post $ toRef ref) (pre $ toType t)
toDeclaration (E.Datatype nwargs uciTypesList) =
  A.Datatype
    (c $ toNameWithArgs toUci toLci nwargs)
    (uciTypesList & fmap (toNameWithArgs toUci toType) & toOcl)
toDeclaration (E.TypeAlias nwargs t) =
  A.TypeAlias [] (c $ toNameWithArgs toUci toLci nwargs) ([], toType t)
toDeclaration (E.PortAnnotation lci t) =
  A.PortAnnotation (c $ toLci lci) [] (toType t)
toDeclaration (E.PortDefinition lci e) =
  A.PortDefinition (c $ toLci lci) [] (toExpr e)
toDeclaration (E.Fixity assoc int sym) =
  A.Fixity (toAssoc assoc) [] int [] (A.OpRef $ toSym sym)

toNameWithArgs :: (a' -> a) -> (b' -> b) -> (a', [b']) -> A.NameWithArgs a b
toNameWithArgs fa fb (a, bs) = (fa a, fmap pre $ fmap fb bs)

toOcl :: [a] -> A.OpenCommentedList a
toOcl a =
  let last = a & reverse & head
      rest = a & reverse & drop 1 & reverse
  in A.OpenCommentedList (rest & fmap toWithEol & fmap c) (pre $ toWithEol last)

toAssoc :: E.Assoc -> A.Assoc
toAssoc E.L = A.L
toAssoc E.N = A.N
toAssoc E.R = A.R

-- Expr
toExpr :: E.Expr -> A.Expr
toExpr e = at $ toExpr' e

toExpr' :: E.Expr -> A.Expr'
toExpr' (E.EUnit) = A.Unit []
toExpr' (E.ELiteral lit) = A.Literal $ toLiteral lit
toExpr' (E.EVar qref) = A.VarExpr $ toQRef qref
toExpr' (E.EApp e exprs) =
  A.App (toExpr e) (fmap pre $ fmap toExpr $ exprs) functionApplicationMultiline
toExpr' (E.ENegative e) = A.Unary A.Negative $ toExpr e
toExpr' (E.EBinOp opRef e1 e2) =
  A.Binop (toRef $ E.OpRef opRef) (toExpr e1) (toExpr e2)
toExpr' (E.EParens e) = A.Parens $ c $ toExpr e
toExpr' (E.EExplicitList exprs) =
  A.ExplicitList (toSequence $ fmap toExpr exprs) [] forceMultiline
toExpr' (E.ERange e1 e2) = A.Range (c $ toExpr e1) (c $ toExpr e2) False
toExpr' (E.ETuple exprs) = A.Tuple (fmap c $ fmap toExpr exprs) False
toExpr' (E.ETupleFunc i) = A.TupleFunction i
toExpr' (E.EAccess e lci) = A.Access (toExpr e) (toLci lci)
toExpr' (E.EAccessFunc lci) = (A.AccessFunction $ toLci lci)
toExpr' (E.ELambda pats e) =
  A.Lambda (fmap pre $ fmap toPattern pats) [] (toExpr e) False
toExpr' (E.EIf (ifc:ifcs) e) =
  A.If (ifc & toIfClause) (ifcs & fmap toIfClause & fmap pre) ([], toExpr e)
toExpr' (E.ELet letDecls e) = A.Let (fmap toLetDecl letDecls) [] (toExpr e)
toExpr' (E.ECase e patExprList) =
  A.Case
    (e & toExpr & c, False)
    (patExprList & fmap (\(pat, e) -> (c $ toPattern pat, pre $ toExpr e)))
toExpr' (E.ERecord mLci lciExprList) =
  A.Record
    (mLci & fmap toLci & fmap c)
    (lciExprList & fmap (toPair toLci toExpr) & toSequence)
    []
    forceMultiline

toQRef :: E.QRef -> A.Ref
toQRef (E.UnQRef ref) = toRef ref
toQRef (E.QRef ucis ref) =
  case toRef ref of
    A.VarRef [] lci -> A.VarRef (fmap toUci ucis) lci
    A.TagRef [] uci -> A.TagRef (fmap toUci ucis) uci

toRef :: E.Ref -> A.Ref
toRef (E.VarRef lci) = A.VarRef [] (toLci lci)
toRef (E.TagRef uci) = A.TagRef [] (toUci uci)
toRef (E.OpRef sym)  = A.OpRef (toSym sym)

toOpRef :: E.SymbolIdentifier -> A.Ref
toOpRef sym = (A.OpRef $ toSym sym)

toIfClause :: E.IfClause -> A.IfClause
toIfClause (e1, e2) = (c $ toExpr e1, c $ toExpr e2)

toLetDecl :: E.LetDeclaration -> A.LetDeclaration
toLetDecl (E.LetDefinition (pat:pats) e) =
  A.LetDefinition
    (toPattern pat)
    (fmap pre $ fmap toPattern $ pats)
    []
    (toExpr e)
toLetDecl (E.LetAnnotation ref t) =
  A.LetAnnotation (toRef ref, []) ([], toType t)

-- Pattern
toPattern :: E.Pattern -> AP.Pattern
toPattern p = at $ toPattern' p

toPattern' :: E.Pattern -> AP.Pattern'
toPattern' (E.PAnything) = AP.Anything
toPattern' (E.PUnitPattern) = AP.UnitPattern []
toPattern' (E.PLiteral lit) = AP.Literal $ toLiteral lit
toPattern' (E.PVar lci) = AP.VarPattern $ toLci lci
toPattern' (E.POp sym) = AP.OpPattern $ toSym sym
toPattern' (E.PParens pat) = AP.PatternParens $ c $ toPattern pat
toPattern' (E.PEmptyList) = AP.EmptyListPattern []
toPattern' (E.PTuple pats) = AP.Tuple (fmap c $ fmap toPattern $ pats)
toPattern' (E.PList pats) = AP.List (fmap c $ fmap toPattern $ pats)
toPattern' (E.PData ucis pats) =
  AP.Data (fmap toUci ucis) (fmap pre $ fmap toPattern $ pats)
toPattern' (E.PCons ((pat, mString):patmsList)) =
  AP.ConsPattern
    (toPattern pat, mString)
    (patmsList & fmap (\(p, mString) -> ([], [], toPattern p, mString)))
toPattern' (E.PRecord lcis) = (AP.Record $ fmap c $ fmap toLci lcis)
toPattern' (E.PAlias pat lci) = AP.Alias (toPattern pat, []) ([], toLci lci)

toLiteral :: E.Literal -> A.Literal
toLiteral (E.IntNum x)   = A.IntNum x A.DecimalInt
toLiteral (E.FloatNum x) = A.FloatNum x A.DecimalFloat
toLiteral (E.Chr x)      = A.Chr x
toLiteral (E.Str x)      = A.Str x False
toLiteral (E.Boolean x)  = A.Boolean x

-- Type
toType :: E.Type -> A.Type
toType t = at $ toType' t

toType' :: E.Type -> A.Type'
toType' E.UnitType = (A.UnitType [])
toType' (E.TypeVariable lci) = (A.TypeVariable $ toLci lci)
toType' (E.TypeParens t) = (A.TypeParens $ c $ toType t)
toType' (E.TupleType cweolTypes) =
  A.TupleType (fmap c $ fmap toWithEol $ fmap toType $ cweolTypes)
toType' (E.TypeConstruction typeConstructor types) =
  A.TypeConstruction
    (toTypeConstructor typeConstructor)
    (fmap pre $ fmap toType $ types)
toType' (E.RecordType mLci lciTypeList) =
  A.RecordType
    (fmap c $ fmap toLci $ mLci)
    (toSequence $ fmap (toPair toLci toType) $ lciTypeList)
    []
    forceMultiline
toType' (E.FunctionType weolType cctmsList) =
  A.FunctionType
    (toWithEol $ toType $ weolType)
    (cctmsList & fmap (\(t, ms) -> ([], [], toType t, ms)))
    forceMultiline

toTypeConstructor (E.NamedConstructor ucis) =
  A.NamedConstructor (fmap toUci ucis)
toTypeConstructor (E.TupleConstructor i) = A.TupleConstructor i

toSequence :: E.List a -> A.Sequence a
toSequence s = s & fmap toWithEol & fmap pre & fmap pre

toPair :: (a' -> a) -> (b' -> b) -> (a', b') -> A.Pair a b
toPair toKey toVal (key, val) =
  A.Pair (toKey key, []) ([], toVal val) forceMultiline

toLci :: E.LowercaseIdentifier -> A.LowercaseIdentifier
toLci a = (A.LowercaseIdentifier a)

toUci :: E.UppercaseIdentifier -> A.UppercaseIdentifier
toUci a = (A.UppercaseIdentifier a)

toSym :: E.SymbolIdentifier -> A.SymbolIdentifier
toSym a = (A.SymbolIdentifier a)

toWithEol :: a -> A.WithEol a
toWithEol a = (a, Nothing)

forceMultiline = A.ForceMultiline False

functionApplicationMultiline = A.FAJoinFirst A.SplitAll

-- Helpers
-- convert to/from annotated
tat (A.A _ a) = a

at a = A.A (A.Region (A.Position 0 0) (A.Position 0 0)) a

rc (A.Commented _ a _) = a

weol = fst

c a = A.Commented [] a []

pre a = ([], a)

post a = (a, [])


-}
