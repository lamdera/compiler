module Plate where

import Multiplate

-- Multiplate
data Plate f = Plate
  { _module          :: Module -> f Module
  , _sourceTag       :: SourceTag -> f SourceTag
  , _header          :: Header -> f Header
  , _detailedListing :: DetailedListing -> f DetailedListing
  , _sourceSettings  :: SourceSettings -> f SourceSettings
  , _importMethod    :: ImportMethod -> f ImportMethod
  , _literal         :: Literal -> f Literal
  , _typeConstructor :: TypeConstructor -> f TypeConstructor
  , _type            :: Type -> f Type
  , _pattern         :: Pattern -> f Pattern
  , _letDeclaration  :: LetDeclaration -> f LetDeclaration
  , _expr            :: Expr -> f Expr
  , _ifClause        :: IfClause -> f IfClause
  , _declaration     :: Declaration -> f Declaration
  , _assoc           :: Assoc -> f Assoc
  , _ref             :: Ref -> f Ref
  , _qref            :: QRef -> f QRef
  , _uciListing      :: UciListing -> f UciListing
  , _uci             :: UppercaseIdentifier -> f UppercaseIdentifier
  , _lci             :: LowercaseIdentifier -> f LowercaseIdentifier
  , _sym             :: SymbolIdentifier -> f SymbolIdentifier
  }

instance Multiplate Plate where
  multiplate child = Plate b_module b_sourceTag b_header b_detailedListing b_sourceSettings b_importMethod b_literal b_typeConstructor b_type b_pattern b_letDeclaration b_expr b_ifClause b_declaration b_assoc b_ref b_qref b_uciListing b_uci b_lci b_sym
    where
      b_module (Module header mapUciImportMethod declarations) =
        Module <$> _header child header <*> (Map.fromList <$> traverse (\(ucis, importMethod) -> sequenceA2 (traverse (_uci child) ucis, _importMethod child importMethod)) (Map.toList mapUciImportMethod)) <*> traverse (_declaration child) declarations
         --
      b_header (Header sourceTag ucis mSourceSettings detailedListing) = Header <$> _sourceTag child sourceTag <*> traverse (_uci child) ucis <*> traverse (_sourceSettings child) mSourceSettings <*> _detailedListing child detailedListing
         --
      b_importMethod (ImportMethod (mUci) detailedListing) = ImportMethod <$> traverse (_uci child) mUci <*> _detailedListing child detailedListing
         --
      b_detailedListing DetailedOpenListing                          = pure DetailedOpenListing
      b_detailedListing DetailedClosedListing                        = pure DetailedClosedListing
      b_detailedListing (DetailedListing lcis syms uciUciListingMap) = DetailedListing <$> traverse (_lci child) lcis <*> traverse (_sym child) syms <*> (Map.fromList <$> traverse (\(k, v) -> sequenceA2 (_uci child k, _uciListing child v)) (Map.toList uciUciListingMap))
         --
      b_sourceSettings sourceSettings = traverse (\(lci, uci) -> sequenceA2 (_lci child lci, _uci child uci)) sourceSettings
         --
      b_type (UnitType)                        = pure UnitType
      b_type (TypeVariable lci)                = TypeVariable <$> _lci child lci
      b_type (TypeConstruction tc types)       = TypeConstruction <$> _typeConstructor child tc <*> traverse (_type child) types
      b_type (TypeParens t)                    = TypeParens <$> _type child t
      b_type (TupleType types)                 = TupleType <$> traverse (_type child) types
      b_type (RecordType mLci lciTypeList)     = RecordType <$> traverse (_lci child) mLci <*> traverse (\(lci, t) -> sequenceA2 (_lci child lci, _type child t)) lciTypeList
      b_type (FunctionType t typeMaybeStrList) = FunctionType <$> _type child t <*> traverse (\(t, mString) -> sequenceA2 (_type child t, pure mString)) typeMaybeStrList
         --
      b_pattern (PAnything)       = pure PAnything
      b_pattern (PUnitPattern)    = pure PUnitPattern
      b_pattern (PLiteral lit)    = PLiteral <$> _literal child lit
      b_pattern (PVar lci)        = PVar <$> _lci child lci
      b_pattern (POp sym)         = POp <$> _sym child sym
      b_pattern (PData ucis pats) = PData <$> traverse (_uci child) ucis <*> traverse (_pattern child) pats
      b_pattern (PParens p)       = PParens <$> _pattern child p
      b_pattern (PEmptyList)      = pure PEmptyList
      b_pattern (PList pats)      = PList <$> traverse (_pattern child) pats
      b_pattern (PTuple pats)     = PTuple <$> traverse (_pattern child) pats
      b_pattern (PCons patmsList) = PCons <$> traverse (\(p, mString) -> sequenceA2 (_pattern child p, pure mString)) patmsList
      b_pattern (PRecord lcis)    = PRecord <$> traverse (_lci child) lcis
      b_pattern (PAlias p lci)    = PAlias <$> _pattern child p <*> _lci child lci
         --
      b_letDeclaration (LetDefinition patterns e) = LetDefinition <$> traverse (_pattern child) patterns <*> _expr child e
      b_letDeclaration (LetAnnotation ref t)      = LetAnnotation <$> _ref child ref <*> _type child t
         --
      b_expr (EUnit)                        = pure EUnit
      b_expr (ELiteral lit)                 = ELiteral <$> _literal child lit
      b_expr (EVar qref)                    = EVar <$> _qref child qref
      b_expr (EApp e exprs)                 = EApp <$> _expr child e <*> traverse (_expr child) exprs
      b_expr (ENegative e)                  = ENegative <$> _expr child e
      b_expr (EBinOp sym e1 e2)             = EBinOp <$> _sym child sym <*> _expr child e1 <*> _expr child e2
      b_expr (EParens e)                    = EParens <$> _expr child e
      b_expr (EExplicitList exprs)          = EExplicitList <$> traverse (_expr child) exprs
      b_expr (ERange e1 e2)                 = ERange <$> _expr child e1 <*> _expr child e2
      b_expr (ETuple exprs)                 = ETuple <$> traverse (_expr child) exprs
      b_expr (ETupleFunc int)               = ETupleFunc <$> pure int
      b_expr (ERecord mLci lciExprPairList) = ERecord <$> traverse (_lci child) mLci <*> traverse (\(lci, expr) -> sequenceA2 (_lci child lci, _expr child expr)) lciExprPairList
      b_expr (EAccess e lci)                = EAccess <$> _expr child e <*> _lci child lci
      b_expr (EAccessFunc lci)              = EAccessFunc <$> _lci child lci
      b_expr (ELambda pats e)               = ELambda <$> traverse (_pattern child) pats <*> _expr child e
      b_expr (EIf ifClauses e)              = EIf <$> traverse (_ifClause child) ifClauses <*> _expr child e
      b_expr (ELet letDecls e)              = ELet <$> traverse (_letDeclaration child) letDecls <*> _expr child e
      b_expr (ECase e patExprPairList)      = ECase <$> _expr child e <*> traverse (\(p, e) -> sequenceA2 (_pattern child p, _expr child e)) patExprPairList
         --
      b_ifClause (e1, e2) = sequenceA2 (_expr child e1, _expr child e2)
         --
      b_declaration (Definition p pats e)                   = Definition <$> _pattern child p <*> traverse (_pattern child) pats <*> _expr child e
      b_declaration (TypeAnnotation ref t)                  = TypeAnnotation <$> _ref child ref <*> _type child t
      b_declaration (Datatype (uci, lcis) uciTypesPairList) = Datatype <$> sequenceA2 (_uci child uci, traverse (_lci child) lcis) <*> traverse (\(uci, t) -> sequenceA2 (_uci child uci, traverse (_type child) t)) uciTypesPairList
      b_declaration (TypeAlias (uci, lcis) t)               = TypeAlias <$> sequenceA2 (_uci child uci, traverse (_lci child) lcis) <*> _type child t
      b_declaration (PortAnnotation lci t)                  = PortAnnotation <$> _lci child lci <*> _type child t
      b_declaration (PortDefinition lci e)                  = PortDefinition <$> _lci child lci <*> _expr child e
      b_declaration (Fixity assoc int sym)                  = Fixity <$> _assoc child assoc <*> pure int <*> _sym child sym
         --
      b_ref (VarRef lci) = VarRef <$> _lci child lci
      b_ref (TagRef uci) = TagRef <$> _uci child uci
      b_ref (OpRef sym)  = OpRef <$> _sym child sym
         --
      b_qref (QRef ucis ref) = QRef <$> traverse (_uci child) ucis <*> _ref child ref
      b_qref (UnQRef ref)    = UnQRef <$> _ref child ref
         --
      b_uciListing (ExplicitListing uciSet) = ExplicitListing <$> traverse (_uci child) uciSet
      b_uciListing UciOpenListing           = pure UciOpenListing
      b_uciListing UciClosedListing         = pure UciClosedListing
         --
      b_assoc = pure
         --
      b_literal = pure
         --
      b_sourceTag = pure
         --
      b_typeConstructor = pure
         --
      b_uci = pure
         --
      b_lci = pure
         --
      b_sym = pure
  mkPlate build =
    Plate
      (build _module)
      (build _sourceTag)
      (build _header)
      (build _detailedListing)
      (build _sourceSettings)
      (build _importMethod)
      (build _literal)
      (build _typeConstructor)
      (build _type)
      (build _pattern)
      (build _letDeclaration)
      (build _expr)
      (build _ifClause)
      (build _declaration)
      (build _assoc)
      (build _ref)
      (build _qref)
      (build _uciListing)
      (build _uci)
      (build _lci)
      (build _sym)

sequenceA2 :: Applicative f => (f a, f b) -> f (a, b)
sequenceA2 (a, b) = liftA2 (,) a b

