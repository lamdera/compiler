{-# OPTIONS_GHC -Wall #-}
module Transpile.Instances where

import qualified Reporting.Annotation as Ann
import qualified Data.Text as Text
import qualified Language.Haskell.Exts.Simple.Syntax as Hs
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Reporting.Region as RR
import qualified Data.Set as Set

import Data.Text (pack)
import Data.List (intercalate, isPrefixOf)
import Data.Monoid ((<>))
import Data.Function ((&))

import qualified Debug.Trace
--
-- data Declaration
--   = Definition Pattern.Pattern [PreCommented Pattern.Pattern] Comments Expression.Expr
--   | TypeAnnotation (PostCommented Var.Ref) (PreCommented Type)

--   | Datatype
--       Commented (NameWithArgs UppercaseIdentifier LowercaseIdentifier)
--       OpenCommentedList (NameWithArgs UppercaseIdentifier Type)

--   | TypeAlias Comments
--       (Commented (NameWithArgs UppercaseIdentifier LowercaseIdentifier))
--       (PreCommented Type)
--   | PortAnnotation (Commented LowercaseIdentifier) Comments Type
--   | PortDefinition (Commented LowercaseIdentifier) Comments Expression.Expr
--   | Fixity Assoc Comments Int Comments Var.Ref

-- OpenCommentedList [Commented (WithEol a)] (PreCommented (WithEol a))


 -- ##############################################
tDataToGADT :: String -> Hs.Decl -> Hs.Decl
tDataToGADT moduName (Hs.DataDecl dataOrNew mContext declHead qualConDecls derive) =
  -- TODO: deriving / instance
  let
    qConUnwrap (Hs.QualConDecl Nothing Nothing conDecl) = conDecl
    qualConDecls2 = qualConDecls & fmap qConUnwrap & fmap conDeclToGadtDecl
    ctx vars = Hs.TyForall Nothing (Just (Hs.CxTuple (fmap (\ident -> Hs.ClassA (Hs.UnQual (Hs.Ident "ElmVal'")) [Hs.TyVar ident]) vars)))
    conDeclToGadtDecl (Hs.ConDecl name types) =
      Hs.GadtDecl name Nothing (ctx (dedup $ concatMap dTypeVars types) $ foldr1 Hs.TyFun (types ++ [dHeadToType moduName declHead]))
    conDeclToGadtDecl a = error ("expected ConDecl: " ++ show a)
  in
    Hs.GDataDecl dataOrNew mContext declHead Nothing (qualConDecls2) derive
tDataToGADT moduName a = a
-- ##############################################

tDataToInstDecl :: String -> Hs.Decl -> [Hs.Decl]
tDataToInstDecl moduName (Hs.DataDecl dataOrNew mContext declHead qualConDecls derive) =
  let
    qConUnwrap (Hs.QualConDecl Nothing Nothing conDecl) = conDecl
    conDeclToInstDecl (Hs.ConDecl name types) = Hs.GadtDecl name Nothing (ctx (dHeadVars declHead) $ foldr1 Hs.TyFun (types ++ [dHeadToType moduName declHead]))
    conDeclToInstDecl a = error ("missing ConDecl: " ++ show a)
    ctx vars = Hs.TyForall Nothing (Just (Hs.CxTuple (fmap (\ident -> Hs.ClassA (Hs.UnQual (Hs.Ident "ElmVal'")) [Hs.TyVar ident]) vars)))
    --
    dh = instDecl (declHead & dHeadVars) (declHead & dHeadToType moduName)
    instDecl names t =
      (Hs.IRule Nothing
        (Nothing)
        (Hs.IHApp
          (Hs.IHCon (Hs.UnQual (Hs.Ident "ElmVal'")))
          (Hs.TyParen t)
        )
      )
    toString (Hs.ConDecl name types) =
      let
        vars = [0..(length types-1)] & fmap (\i -> "v" ++ show i) & fmap Hs.Ident
        rawName (Hs.Ident n) = n
      in
      Hs.InsDecl
        (Hs.FunBind
          [ Hs.Match (Hs.Ident "toString")
              [ Hs.PApp (Hs.Qual (Hs.ModuleName moduName) name) (fmap Hs.PVar vars) ]
              (Hs.UnGuardedRhs
                (Hs.App (Hs.App (Hs.Var (Hs.Qual (Hs.ModuleName "String") (Hs.Ident "join"))) (Hs.Lit (Hs.String " "))) (Hs.List $
                  (Hs.Lit (Hs.String (rawName name))) :
                  fmap (\ident -> Hs.App (Hs.Var (Hs.UnQual (Hs.Ident "toString"))) (Hs.Var (Hs.UnQual ident))) vars
              )))
              Nothing
          ]
        )
    tEquals (Hs.ConDecl name types) =
          let
            vars = [0..(length types-1)] & fmap (\i -> "v" ++ show i)
            rawName (Hs.Ident n) = n
          in
          Hs.Match (Hs.Ident "equals")
            [ Hs.PApp (Hs.Qual (Hs.ModuleName moduName) name) (vars & fmap (\v -> v ++ "x") & fmap Hs.Ident & fmap Hs.PVar)
            , Hs.PApp (Hs.Qual (Hs.ModuleName moduName) name) (vars & fmap (\v -> v ++ "y") & fmap Hs.Ident & fmap Hs.PVar)
            ]
            (Hs.UnGuardedRhs
              ( if vars == [] then
                  Hs.Con (Hs.UnQual (Hs.Ident "True"))
                else
                  foldl1 (\a b -> Hs.InfixApp a (Hs.QVarOp (Hs.UnQual (Hs.Symbol "&&"))) b)
                    (fmap
                      (\v ->
                        Hs.Paren
                          (Hs.App
                            (Hs.App
                              (Hs.Var
                                (Hs.UnQual
                                  (Hs.Ident "equals")
                                )
                              )
                              (Hs.Var
                                (Hs.UnQual
                                  (Hs.Ident (v ++ "x"))
                                )
                              )
                            )
                            (Hs.Var
                              (Hs.UnQual
                                (Hs.Ident (v ++ "y"))
                              )
                            )
                          )
                        )
                      vars
                    )
              )
            )
            Nothing

    --
    q2 = qualConDecls & fmap qConUnwrap
  in
    [Hs.InstDecl Nothing dh
      (Just
        (
          (fmap toString q2) <>
          [Hs.InsDecl
            (Hs.FunBind
              ((fmap tEquals q2) <>
                (if length q2 > 1 then
                  [Hs.Match (Hs.Ident "equals") [ Hs.PWildCard, Hs.PWildCard ] (Hs.UnGuardedRhs (Hs.Con (Hs.UnQual (Hs.Ident "False")))) Nothing]
                 else [])
              )
            )
          ]
        )
      )
    ]

tDataToInstDecl _ _ = []

-- ##############################################

dHeadToType :: String -> Hs.DeclHead -> Hs.Type
dHeadToType moduName (Hs.DHead name) = Hs.TyCon (Hs.Qual (Hs.ModuleName moduName) name)
dHeadToType moduName dh@(Hs.DHInfix (Hs.UnkindedVar v) name) = error ("dHeadToType infix notimpl: " ++ show dh)
  -- Hs.TyInfix (Hs.TyVar v) (Hs.UnpromotedName (Hs.UnQual )) (Hs.TyVar name)
dHeadToType moduName (Hs.DHParen dh) = Hs.TyParen (dHeadToType moduName dh)
dHeadToType moduName (Hs.DHApp dh (Hs.UnkindedVar v)) = Hs.TyApp (dHeadToType moduName dh) (Hs.TyVar v)

dHeadVars :: Hs.DeclHead -> [Hs.Name]
dHeadVars (Hs.DHead name) = []
dHeadVars dh@(Hs.DHInfix (Hs.UnkindedVar v) name) = error ("dHeadVars infix notimpl: " ++ show dh)
  -- Hs.TyInfix (Hs.TyVar v) (Hs.UnpromotedName (Hs.UnQual )) (Hs.TyVar name)
dHeadVars (Hs.DHParen dh) = dHeadVars dh
dHeadVars (Hs.DHApp dh (Hs.UnkindedVar v)) = v : dHeadVars dh

-- | dTypeVars picks out all Hs.Idents and Hs.Symbols from a type, i.e. unqualified variables and symbols
dTypeVars :: Hs.Type -> [Hs.Name]
dTypeVars (Hs.TyFun t1 t2) = dTypeVars t1 ++ dTypeVars t2
dTypeVars (Hs.TyApp t1 t2) = dTypeVars t1 ++ dTypeVars t2
dTypeVars (Hs.TyTuple _ types) = concatMap dTypeVars types
dTypeVars (Hs.TyList types) = dTypeVars types
dTypeVars (Hs.TyInfix t1 _ t2) = dTypeVars t1 ++ dTypeVars t2
dTypeVars (Hs.TyParen t) = dTypeVars t
-- promoted types; probably superrecords
dTypeVars (Hs.TyPromoted (Hs.PromotedInteger int rawStr)) = []
dTypeVars (Hs.TyPromoted (Hs.PromotedString str rawStr)) = []
dTypeVars (Hs.TyPromoted (Hs.PromotedCon hadLeadingSingleQuote qName)) = []
dTypeVars (Hs.TyPromoted (Hs.PromotedList hadLeadingSingleQuote types)) = concatMap dTypeVars types
dTypeVars (Hs.TyPromoted (Hs.PromotedTuple types)) = concatMap dTypeVars types
dTypeVars (Hs.TyPromoted (Hs.PromotedUnit)) = []
-- base-cases
dTypeVars (Hs.TyVar n) = [n]
dTypeVars (Hs.TyCon _) = []
dTypeVars tv = error ("dTypeVars: " ++ show tv)


dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = x : filter (/= x) (dedup xs)

{-

  type LazyListView a
    = Nil
    | Cons a (LazyList a)

  data LazyListView a
    = Nil
    | Cons a (Lazy.List.LazyList a)
    deriving (Haskelm.Core.Show, Haskelm.Core.Eq, ElmVal')

  data LazyListView a where
    Nil :: ElmVal' a => LazyListView a
    Cons :: ElmVal' a => a -> (LazyList a) -> LazyListView a
-}
