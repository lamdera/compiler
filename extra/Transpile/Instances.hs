{-# OPTIONS_GHC -Wall #-}
module Transpile.Instances (tDataToGADT, tDataToInstDecl) where

import qualified Language.Haskell.Exts.Simple.Syntax as Hs

import Data.Monoid ((<>))
import Data.Function ((&))


{-
Useful example when first developing this module:

  type LazyListView a
    = Nil
    | Cons a (LazyList a)

  data LazyListView a
    = Nil
    | Cons a (Lazy.List.LazyList a)
    deriving (Lamdera.Haskelm.Core.Show, Lamdera.Haskelm.Core.Eq, Lamdera.Haskelm.Core.ElmVal')

  data LazyListView a where
    Nil :: (Lamdera.Haskelm.Core.ElmVal' a) => LazyListView a
    Cons :: (Lamdera.Haskelm.Core.ElmVal' a) => a -> (LazyList a) -> LazyListView a

Also, ident(ifier) /= indent(ation).
-}


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

tDataToGADT :: String -> Hs.Decl -> Hs.Decl
tDataToGADT moduName (Hs.DataDecl dataOrNew mContext declHead qualConDecls derive) =
  -- translate a data definition to a gadt, so we can have different type instances for different ctors of the same data type, like in elm.
  let
    qualConDecls2 = qualConDecls & fmap qConUnwrap & fmap conDeclToGadtDecl
    ctx vars = Hs.TyForall Nothing (Just (Hs.CxTuple (fmap (\ident -> Hs.ClassA (haskelmCoreIdent "ElmVal'") [Hs.TyVar ident]) vars)))
    conDeclToGadtDecl (Hs.ConDecl name types) =
      Hs.GadtDecl name Nothing (ctx (dedup $ concatMap dTypeVars types) $ foldr1 Hs.TyFun (types ++ [dHeadToType moduName declHead]))
    conDeclToGadtDecl a = error ("expected ConDecl: " ++ show a)
  in
    Hs.GDataDecl dataOrNew mContext declHead Nothing (qualConDecls2) derive
tDataToGADT _ a = a



tDataToInstDecl :: String -> Hs.Decl -> [Hs.Decl]
tDataToInstDecl moduName (Hs.DataDecl _ _ declHead qualConDecls _) =
  -- generate instance declarations for data types for the super types and global elm fns;
  -- Monoid/appendable, Ord/comparable, Eq and Show.
  -- We don't need a num instance since all Elm numbers are doubles under the hood, so we type aliased them both to Double.
  -- This function is really opaque; sorry about that. When modifying, pretty-print the result and iterate until it seems correct.
  let
    dh = instDecl (declHead & dHeadToType moduName)
    instDecl t =
      (Hs.IRule Nothing
        (Nothing)
        (Hs.IHApp
          (Hs.IHCon (haskelmCoreIdent "ElmVal'"))
          (Hs.TyParen t)
        )
      )
    toString :: Hs.ConDecl -> Hs.InstDecl
    toString (Hs.ConDecl name types) =
      let
        vars = [0..(length types-1)] & fmap (\i -> "v" ++ show i) & fmap Hs.Ident
        rawName (Hs.Ident n) = n
        rawName _ = error "unexpected symbol in ConDecl"
      in
      Hs.InsDecl
        (Hs.FunBind
          [ Hs.Match (Hs.Ident "toString")
              [ Hs.PApp (Hs.Qual (Hs.ModuleName moduName) name) (fmap Hs.PVar vars) ]
              (Hs.UnGuardedRhs
                (Hs.App (Hs.App (Hs.Var (Hs.Qual (Hs.ModuleName "String") (Hs.Ident "join"))) (Hs.Lit (Hs.String " "))) (Hs.List $
                  (Hs.Lit (Hs.String (rawName name))) :
                  fmap (\ident -> Hs.App (Hs.Var (haskelmCoreIdent "toString")) (Hs.Var (Hs.UnQual ident))) vars
              )))
              Nothing
          ]
        )
    toString _ = error "unexpected argument; is the ext src ast corrupt?"

    tEquals :: Hs.ConDecl -> Hs.Match
    tEquals (Hs.ConDecl name types) =
          let
            vars = [0..(length types-1)] & fmap (\i -> "v" ++ show i)
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
                                (haskelmCoreIdent "equals")
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
    tEquals _ = error "unexpected argument; is the ext src ast corrupt?"

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

-- INTERNALS

haskelmCoreIdent :: String -> Hs.QName
haskelmCoreIdent i = Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Ident i)

dHeadToType :: String -> Hs.DeclHead -> Hs.Type
dHeadToType moduName (Hs.DHead name) = Hs.TyCon (Hs.Qual (Hs.ModuleName moduName) name)
dHeadToType _ dh@(Hs.DHInfix (Hs.UnkindedVar _) _) = error ("dHeadToType infix notimpl: " ++ show dh)
  -- Hs.TyInfix (Hs.TyVar v) (Hs.UnpromotedName (Hs.UnQual )) (Hs.TyVar name)
dHeadToType moduName (Hs.DHParen dh) = Hs.TyParen (dHeadToType moduName dh)
dHeadToType moduName (Hs.DHApp dh (Hs.UnkindedVar v)) = Hs.TyApp (dHeadToType moduName dh) (Hs.TyVar v)
dHeadToType _ _ = error "unexpected DeclHead"

-- dHeadVars :: Hs.DeclHead -> [Hs.Name]
-- dHeadVars (Hs.DHead _) = []
-- dHeadVars dh@(Hs.DHInfix (Hs.UnkindedVar _) _) = error ("dHeadVars infix notimpl: " ++ show dh)
--   -- Hs.TyInfix (Hs.TyVar v) (Hs.UnpromotedName (Hs.UnQual )) (Hs.TyVar name)
-- dHeadVars (Hs.DHParen dh) = dHeadVars dh
-- dHeadVars (Hs.DHApp dh (Hs.UnkindedVar v)) = v : dHeadVars dh
-- dHeadVars _ = error "unexpected DeclHead"

-- | dTypeVars picks out all Hs.Idents and Hs.Symbols from a type, i.e. unqualified variables and symbols
dTypeVars :: Hs.Type -> [Hs.Name]
dTypeVars (Hs.TyFun t1 t2) = dTypeVars t1 ++ dTypeVars t2
dTypeVars (Hs.TyApp t1 t2) = dTypeVars t1 ++ dTypeVars t2
dTypeVars (Hs.TyTuple _ types) = concatMap dTypeVars types
dTypeVars (Hs.TyList types) = dTypeVars types
dTypeVars (Hs.TyInfix t1 _ t2) = dTypeVars t1 ++ dTypeVars t2
dTypeVars (Hs.TyParen t) = dTypeVars t
-- promoted types; probably superrecords
dTypeVars (Hs.TyPromoted (Hs.PromotedInteger _ _)) = []
dTypeVars (Hs.TyPromoted (Hs.PromotedString _ _)) = []
dTypeVars (Hs.TyPromoted (Hs.PromotedCon _ _)) = []
dTypeVars (Hs.TyPromoted (Hs.PromotedList _ types)) = concatMap dTypeVars types
dTypeVars (Hs.TyPromoted (Hs.PromotedTuple types)) = concatMap dTypeVars types
dTypeVars (Hs.TyPromoted (Hs.PromotedUnit)) = []
-- base-cases
dTypeVars (Hs.TyVar n) = [n]
dTypeVars (Hs.TyCon _) = []
dTypeVars tv = error ("dTypeVars: " ++ show tv)


dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = x : filter (/= x) (dedup xs)

qConUnwrap :: Hs.QualConDecl -> Hs.ConDecl
qConUnwrap (Hs.QualConDecl Nothing Nothing conDecl) = conDecl
qConUnwrap _ = error "unexpected pattern"


