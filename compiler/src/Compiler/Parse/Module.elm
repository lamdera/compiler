{- MANUALLY FORMATTED -}
module Compiler.Parse.Module exposing
  ( fromByteString
  , ProjectType(..)
  , isKernel
  , chompImports
  , chompImport
  )


import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Imports as Imports
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Declaration as Decl
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Variable as Var
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Extra.Type.Either as Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)



-- FROM BYTE STRING


fromByteString : ProjectType -> String -> Either E.Error Src.Module
fromByteString projectType source =
  case P.fromByteString (chompModule projectType) E.ModuleBadEnd source of
    Right modul -> checkModule projectType modul
    Left err    -> Left (E.ParseError err)



-- PROJECT TYPE


type ProjectType
  = Package Pkg.Name
  | Application


isCore : ProjectType -> Bool
isCore projectType =
  case projectType of
    Package pkg -> pkg == Pkg.core
    Application -> False


isKernel : ProjectType -> Bool
isKernel projectType =
  case projectType of
    Package pkg -> Pkg.isKernel pkg
    Application -> False



-- MODULE


type Module =
  Module
    {- header -} (Maybe Header)
    {- imports -} (TList Src.Import)
    {- infixes -} (TList (A.Located Src.Infix))
    {- decls -} (TList Decl.Decl)


chompModule : ProjectType -> P.Parser E.Module Module
chompModule projectType =
  P.bind chompHeader <| \header ->
  P.bind (chompImports (if isCore projectType then [] else Imports.defaults)) <| \imports ->
  P.bind (if isKernel projectType then chompInfixes [] else P.return []) <| \infixes ->
  P.bind (P.specialize (\decl _ _ -> E.Declarations decl) <| chompDecls) <| \decls ->
  P.return (Module header imports infixes decls)



-- CHECK MODULE


checkModule : ProjectType -> Module -> Either E.Error Src.Module
checkModule projectType (Module maybeHeader imports infixes decls) =
  let
    ((values, unions), (aliases, ports)) = categorizeDecls [] [] [] [] decls
  in
  case maybeHeader of
    Just (Header name effects exports) ->
      Either.fmap (Src.Module (Just name) exports imports values unions aliases infixes)
        <| checkEffects projectType ports effects

    Nothing ->
      Right <|
        Src.Module Nothing (A.At A.one Src.Open) imports values unions aliases infixes <|
          case ports of
            [] -> Src.NoEffects
            _::_ -> Src.Ports ports


checkEffects : ProjectType -> TList Src.Port -> Effects -> Either E.Error Src.Effects
checkEffects projectType ports effects =
  case effects of
    NoEffects region ->
      case ports of
        [] ->
          Right Src.NoEffects

        Src.Port name _ :: _ ->
          case projectType of
            Package _   -> Left (E.NoPortsInPackage name)
            Application -> Left (E.UnexpectedPort region)

    Ports region ->
      case projectType of
        Package _ ->
          Left (E.NoPortModulesInPackage region)

        Application ->
          case ports of
            []  -> Left (E.NoPorts region)
            _::_ -> Right (Src.Ports ports)

    Manager region manager ->
      if isKernel projectType then
        case ports of
          []  -> Right (Src.Manager region manager)
          _::_ -> Left (E.UnexpectedPort region)
      else
        Left (E.NoEffectsOutsideKernel region)



categorizeDecls : TList (A.Located Src.Value) -> TList (A.Located Src.Union) -> TList (A.Located Src.Alias) -> TList Src.Port -> TList Decl.Decl -> ( ( TList (A.Located Src.Value), TList (A.Located Src.Union) ), ( TList (A.Located Src.Alias), TList Src.Port ) )
categorizeDecls values unions aliases ports decls =
  case decls of
    [] ->
      ((values, unions), (aliases, ports))

    decl::otherDecls ->
      case decl of
        Decl.Value value -> categorizeDecls (value::values) unions aliases ports otherDecls
        Decl.Union union -> categorizeDecls values (union::unions) aliases ports otherDecls
        Decl.Alias alias_ -> categorizeDecls values unions (alias_::aliases) ports otherDecls
        Decl.Port  port_ -> categorizeDecls values unions aliases (port_::ports) otherDecls



-- FRESH LINES


freshLine : (P.Row -> P.Col -> E.Module) -> P.Parser E.Module ()
freshLine toFreshLineError =
  P.bind (Space.chomp E.ModuleSpace) <| \_ ->
  Space.checkFreshLine toFreshLineError



-- CHOMP DECLARATIONS


chompDecls : P.Parser E.Decl (TList Decl.Decl)
chompDecls =
  P.bind Decl.declaration <| \(decl, _) ->
  P.loop chompDeclsHelp [decl]


chompDeclsHelp : TList Decl.Decl -> P.Parser E.Decl (P.Step (TList Decl.Decl) (TList Decl.Decl))
chompDeclsHelp decls =
  P.oneOfWithFallback
    [ P.bind (Space.checkFreshLine E.DeclStart) <| \_ ->
      P.bind Decl.declaration <| \(decl, _) ->
      P.return (P.Loop (decl::decls))
    ]
    (P.Done (MList.reverse decls))


chompInfixes : TList (A.Located Src.Infix) -> P.Parser E.Module (TList (A.Located Src.Infix))
chompInfixes infixes =
  P.oneOfWithFallback
    [ P.bind Decl.infix_ <| \binop ->
      chompInfixes (binop::infixes)
    ]
    infixes



-- MODULE DOC COMMENT


chompModuleDocCommentSpace : P.Parser E.Module (Either A.Region Src.Comment)
chompModuleDocCommentSpace =
  P.bind (P.addLocation (freshLine E.FreshLine)) <| \(A.At region ()) ->
  P.oneOfWithFallback
    [
      P.bind (Space.docComment E.ImportStart E.ModuleSpace) <| \docComment ->
      P.bind (Space.chomp E.ModuleSpace) <| \_ ->
      P.bind (Space.checkFreshLine E.FreshLine) <| \_ ->
      P.return (Right docComment)
    ]
    (Left region)



-- HEADER


type Header =
  Header (A.Located Name.Name) Effects (A.Located Src.Exposing)


type Effects
  = NoEffects A.Region
  | Ports A.Region
  | Manager A.Region Src.Manager


chompHeader : P.Parser E.Module (Maybe Header)
chompHeader =
  P.bind (freshLine E.FreshLine) <| \_ ->
  P.bind P.getPosition <| \start ->
  P.oneOfWithFallback
    [
      -- module MyThing exposing (..)
      P.bind (Keyword.module_ E.ModuleProblem) <| \_ ->
      P.bind P.getPosition <| \effectEnd ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem) <| \_ ->
      P.bind (P.addLocation (Var.moduleName E.ModuleName)) <| \name ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem) <| \_ ->
      P.bind (Keyword.exposing_ E.ModuleProblem) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem) <| \_ ->
      P.bind (P.addLocation (P.specialize E.ModuleExposing exposing_)) <| \exports ->
      P.bind chompModuleDocCommentSpace <| \_ ->
      P.return <| Just <|
        Header name (NoEffects (A.Region start effectEnd)) exports
    ,
      -- port module MyThing exposing (..)
      P.bind (Keyword.port_ E.PortModuleProblem) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem) <| \_ ->
      P.bind (Keyword.module_ E.PortModuleProblem) <| \_ ->
      P.bind P.getPosition <| \effectEnd ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem) <| \_ ->
      P.bind (P.addLocation (Var.moduleName E.PortModuleName)) <| \name ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem) <| \_ ->
      P.bind (Keyword.exposing_ E.PortModuleProblem) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem) <| \_ ->
      P.bind (P.addLocation (P.specialize E.PortModuleExposing exposing_)) <| \exports ->
      P.bind chompModuleDocCommentSpace <| \_ ->
      P.return <| Just <|
        Header name (Ports (A.Region start effectEnd)) exports
    ,
      -- effect module MyThing where { command = MyCmd } exposing (..)
      P.bind (Keyword.effect_ E.Effect) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.Effect) <| \_ ->
      P.bind (Keyword.module_ E.Effect) <| \_ ->
      P.bind P.getPosition <| \effectEnd ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.Effect) <| \_ ->
      P.bind (P.addLocation (Var.moduleName E.ModuleName)) <| \name ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.Effect) <| \_ ->
      P.bind (Keyword.where_ E.Effect) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.Effect) <| \_ ->
      P.bind chompManager <| \manager ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.Effect) <| \_ ->
      P.bind (Keyword.exposing_ E.Effect) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ModuleSpace E.Effect) <| \_ ->
      P.bind (P.addLocation (P.specialize (always E.Effect) exposing_)) <| \exports ->
      P.bind chompModuleDocCommentSpace <| \_ ->
      P.return <| Just <|
        Header name (Manager (A.Region start effectEnd) manager) exports
    ]
    -- default header
    Nothing


chompManager : P.Parser E.Module Src.Manager
chompManager =
  P.bind (P.word1 0x7B {- { -} E.Effect) <| \_ ->
  P.bind spaces_em <| \_ ->
  P.oneOf E.Effect
    [ P.bind chompCommand <| \cmd ->
      P.bind spaces_em <| \_ ->
      P.oneOf E.Effect
        [ P.bind (P.word1 0x7D {-}-} E.Effect) <| \_ ->
          P.bind spaces_em <| \_ ->
          P.return (Src.CCmd cmd)
        , P.bind (P.word1 0x2C {-,-} E.Effect) <| \_ ->
          P.bind spaces_em <| \_ ->
          P.bind chompSubscription <| \sub ->
          P.bind spaces_em <| \_ ->
          P.bind (P.word1 0x7D {-}-} E.Effect) <| \_ ->
          P.bind spaces_em <| \_ ->
          P.return (Src.Fx cmd sub)
        ]
    , P.bind chompSubscription <| \sub ->
      P.bind spaces_em <| \_ ->
      P.oneOf E.Effect
        [ P.bind (P.word1 0x7D {-}-} E.Effect) <| \_ ->
          P.bind spaces_em <| \_ ->
          P.return (Src.CSub sub)
        , P.bind (P.word1 0x2C {-,-} E.Effect) <| \_ ->
          P.bind spaces_em <| \_ ->
          P.bind chompCommand <| \cmd ->
          P.bind spaces_em <| \_ ->
          P.bind (P.word1 0x7D {-}-} E.Effect) <| \_ ->
          P.bind spaces_em <| \_ ->
          P.return (Src.Fx cmd sub)
        ]
    ]


chompCommand : P.Parser E.Module (A.Located Name.Name)
chompCommand =
  P.bind (Keyword.command_ E.Effect) <| \_ ->
  P.bind spaces_em <| \_ ->
  P.bind (P.word1 0x3D {-=-} E.Effect) <| \_ ->
  P.bind spaces_em <| \_ ->
  P.addLocation (Var.upper E.Effect)


chompSubscription : P.Parser E.Module (A.Located Name.Name)
chompSubscription =
  P.bind (Keyword.subscription_ E.Effect) <| \_ ->
  P.bind spaces_em <| \_ ->
  P.bind (P.word1 0x3D {-=-} E.Effect) <| \_ ->
  P.bind spaces_em <| \_ ->
  P.addLocation (Var.upper E.Effect)


spaces_em : P.Parser E.Module ()
spaces_em =
  Space.chompAndCheckIndent E.ModuleSpace E.Effect



-- IMPORTS


chompImports : TList Src.Import -> P.Parser E.Module (TList Src.Import)
chompImports is =
  P.oneOfWithFallback
    [ P.bind chompImport <| \i ->
      chompImports (i::is)
    ]
    (MList.reverse is)


chompImport : P.Parser E.Module Src.Import
chompImport =
  P.bind (Keyword.import_ E.ImportStart) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName) <| \_ ->
  P.bind (P.addLocation (Var.moduleName E.ImportName)) <| \((A.At (A.Region _ end) _) as name) ->
  P.bind (Space.chomp E.ModuleSpace) <| \_ ->
  P.oneOf E.ImportEnd
    [ P.bind (Space.checkFreshLine E.ImportEnd) <| \_ ->
      P.return <| Src.Import name Nothing (Src.Explicit [])
    , P.bind (Space.checkIndent end E.ImportEnd) <| \_ ->
      P.oneOf E.ImportAs
        [ chompAs name
        , chompExposing name Nothing
        ]
    ]


chompAs : A.Located Name.Name -> P.Parser E.Module Src.Import
chompAs name =
  P.bind (Keyword.as_ E.ImportAs) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias) <| \_ ->
  P.bind (Var.upper E.ImportAlias) <| \alias_ ->
  P.bind P.getPosition <| \end ->
  P.bind (Space.chomp E.ModuleSpace) <| \_ ->
  P.oneOf E.ImportEnd
    [ P.bind (Space.checkFreshLine E.ImportEnd) <| \_ ->
      P.return <| Src.Import name (Just alias_) (Src.Explicit [])
    , P.bind (Space.checkIndent end E.ImportEnd) <| \_ ->
      chompExposing name (Just alias_)
    ]


chompExposing : A.Located Name.Name -> Maybe Name.Name -> P.Parser E.Module Src.Import
chompExposing name maybeAlias =
  P.bind (Keyword.exposing_ E.ImportExposing) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingList) <| \_ ->
  P.bind (P.specialize E.ImportExposingList exposing_) <| \exposed ->
  P.bind (freshLine E.ImportEnd) <| \_ ->
  P.return <| Src.Import name maybeAlias exposed



-- LISTING


exposing_ : P.Parser E.Exposing Src.Exposing
exposing_ =
  P.bind (P.word1 0x28 {-(-} E.ExposingStart) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue) <| \_ ->
  P.oneOf E.ExposingValue
    [ P.bind (P.word2 0x2E 0x2E {-..-} E.ExposingValue) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd) <| \_ ->
      P.bind (P.word1 0x29 {-)-} E.ExposingEnd) <| \_ ->
      P.return Src.Open
    , P.bind chompExposed <| \exposed ->
      P.bind (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd) <| \_ ->
      P.loop exposingHelp [exposed]
    ]


exposingHelp : TList Src.Exposed -> P.Parser E.Exposing (P.Step (TList Src.Exposed) Src.Exposing)
exposingHelp revExposed =
  P.oneOf E.ExposingEnd
    [ P.bind (P.word1 0x2C {-,-} E.ExposingEnd) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue) <| \_ ->
      P.bind chompExposed <| \exposed ->
      P.bind (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd) <| \_ ->
      P.return (P.Loop (exposed::revExposed))
    , P.bind (P.word1 0x29 {-)-} E.ExposingEnd) <| \_ ->
      P.return (P.Done (Src.Explicit (MList.reverse revExposed)))
    ]


chompExposed : P.Parser E.Exposing Src.Exposed
chompExposed =
  P.bind P.getPosition <| \start ->
  P.oneOf E.ExposingValue
    [ P.bind (Var.lower E.ExposingValue) <| \name ->
      P.bind P.getPosition <| \end ->
      P.return <| Src.Lower <| A.at start end name
    , P.bind (P.word1 0x28 {-(-} E.ExposingValue) <| \_ ->
      P.bind (Symbol.operator E.ExposingOperator E.ExposingOperatorReserved) <| \op ->
      P.bind (P.word1 0x29 {-)-} E.ExposingOperatorRightParen) <| \_ ->
      P.bind P.getPosition <| \end ->
      P.return <| Src.Operator (A.Region start end) op
    , P.bind (Var.upper E.ExposingValue) <| \name ->
      P.bind P.getPosition <| \end ->
      P.bind (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd) <| \_ ->
      P.fmap (Src.Upper (A.at start end name)) privacy
    ]


privacy : P.Parser E.Exposing Src.Privacy
privacy =
  P.oneOfWithFallback
    [ P.bind (P.word1 0x28 {-(-} E.ExposingTypePrivacy) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy) <| \_ ->
      P.bind P.getPosition <| \start ->
      P.bind (P.word2 0x2E 0x2E {-..-} E.ExposingTypePrivacy) <| \_ ->
      P.bind P.getPosition <| \end ->
      P.bind (Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy) <| \_ ->
      P.bind (P.word1 0x29 {-)-} E.ExposingTypePrivacy) <| \_ ->
      P.return <| Src.Public (A.Region start end)
    ]
    Src.Private
