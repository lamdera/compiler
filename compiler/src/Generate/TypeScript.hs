{-# LANGUAGE OverloadedStrings #-}
module Generate.TypeScript
  ( generate
  , generateForModule
  )
  where

import Prelude hiding (cycle)
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Name as JsName


-- GENERATE

type Interfaces = Map.Map ModuleName.Canonical I.Interface

generate :: Interfaces -> Map.Map ModuleName.Canonical Opt.Main -> Opt.GlobalGraph -> B.Builder
generate ifaces mains (Opt.GlobalGraph graph _) =
  Map.foldlWithKey' (addModuleDeclarations ifaces graph) "" mains <> "\n"


generateForModule :: Interfaces -> Opt.GlobalGraph -> ModuleName.Canonical -> B.Builder
generateForModule ifaces (Opt.GlobalGraph graph _) home =
  case Map.lookup home ifaces of
    Nothing -> ""
    Just iface -> generateModuleInterface ifaces graph home iface


-- MODULE DECLARATIONS

addModuleDeclarations :: Interfaces -> Map.Map Opt.Global Opt.Node -> B.Builder -> ModuleName.Canonical -> Opt.Main -> B.Builder
addModuleDeclarations ifaces graph acc home _ =
  case Map.lookup home ifaces of
    Nothing -> acc
    Just iface ->
      if B.toLazyByteString acc == ""
      then generateModuleInterface ifaces graph home iface
      else acc <> "\n\n" <> generateModuleInterface ifaces graph home iface


generateModuleInterface :: Interfaces -> Map.Map Opt.Global Opt.Node -> ModuleName.Canonical -> I.Interface -> B.Builder
generateModuleInterface ifaces graph home iface =
  let
    ModuleName.Canonical _ moduleName = home
    moduleNameStr = Name.toBuilder moduleName
    declarations = generateInterfaceDeclarations ifaces home iface
    exports = generateModuleExports graph home iface
  in
  "export declare namespace " <> moduleNameStr <> " {\n"
  <> declarations
  <> "}\n\n"
  <> "declare const " <> moduleNameStr <> ": {\n"
  <> exports
  <> "};\n\n"
  <> "export default " <> moduleNameStr <> ";"


-- INTERFACE DECLARATIONS

generateInterfaceDeclarations :: Interfaces -> ModuleName.Canonical -> I.Interface -> B.Builder
generateInterfaceDeclarations ifaces home (I.Interface _ values unions aliases _) =
  let
    typeDecls = Map.foldlWithKey' (addUnionDeclaration ifaces home) "" unions
                <> Map.foldlWithKey' (addAliasDeclaration ifaces home) "" aliases
  in
  typeDecls


addUnionDeclaration :: Interfaces -> ModuleName.Canonical -> B.Builder -> Name.Name -> I.Union -> B.Builder
addUnionDeclaration ifaces home acc name union =
  let decl = generateUnionDeclaration ifaces home name union
  in if B.toLazyByteString acc == ""
     then decl
     else acc <> "\n" <> decl


addAliasDeclaration :: Interfaces -> ModuleName.Canonical -> B.Builder -> Name.Name -> I.Alias -> B.Builder
addAliasDeclaration ifaces home acc name alias =
  let decl = generateAliasDeclaration ifaces home name alias
  in if B.toLazyByteString acc == ""
     then decl
     else acc <> "\n" <> decl


-- UNION TYPES

generateUnionDeclaration :: Interfaces -> ModuleName.Canonical -> Name.Name -> I.Union -> B.Builder
generateUnionDeclaration ifaces home name union =
  case union of
    I.OpenUnion (Can.Union vars ctors _ _) ->
      generateUnionType home name vars ctors
    I.ClosedUnion (Can.Union vars ctors _ _) ->
      generateUnionType home name vars ctors
    I.PrivateUnion _ ->
      ""  -- Don't generate private unions


generateUnionType :: ModuleName.Canonical -> Name.Name -> [Name.Name] -> [Can.Ctor] -> B.Builder
generateUnionType home name vars ctors =
  let
    nameStr = Name.toBuilder name
    typeParams = if null vars then "" else "<" <> joinWith ", " (map (Name.toBuilder) vars) <> ">"
    variants = map (generateVariant home) ctors
  in
  "  export type " <> nameStr <> typeParams <> " = " <> joinWith " | " variants <> ";\n"


generateVariant :: ModuleName.Canonical -> Can.Ctor -> B.Builder
generateVariant home (Can.Ctor ctorName _ _ args) =
  let
    ctorNameStr = Name.toBuilder ctorName
    fields = if null args
             then ""
             else ", " <> joinWith ", " (zipWith (generateCtorField home) [0..] args)
  in
  "{ $: \"" <> ctorNameStr <> "\"" <> fields <> " }"


generateCtorField :: ModuleName.Canonical -> Int -> Can.Type -> B.Builder
generateCtorField home index tipe =
  let fieldName = if index == 0 then "a" else B.charUtf8 (toEnum (97 + index))
  in fieldName <> ": " <> generateTypeForNamespace home Map.empty tipe


-- ALIAS TYPES

generateAliasDeclaration :: Interfaces -> ModuleName.Canonical -> Name.Name -> I.Alias -> B.Builder
generateAliasDeclaration ifaces home name alias =
  case alias of
    I.PublicAlias (Can.Alias vars tipe) ->
      generateAliasType home name vars tipe
    I.PrivateAlias _ ->
      ""  -- Don't generate private aliases


generateAliasType :: ModuleName.Canonical -> Name.Name -> [Name.Name] -> Can.Type -> B.Builder
generateAliasType home name vars tipe =
  let
    nameStr = Name.toBuilder name
    typeParams = if null vars then "" else "<" <> joinWith ", " (map Name.toBuilder vars) <> ">"
  in
  "  export type " <> nameStr <> typeParams <> " = " <> generateTypeForNamespace home Map.empty tipe <> ";\n"


-- MODULE EXPORTS

generateModuleExports :: Map.Map Opt.Global Opt.Node -> ModuleName.Canonical -> I.Interface -> B.Builder
generateModuleExports graph home (I.Interface _ values unions aliases _) =
  let
    valueExports = Map.foldlWithKey' (addValueExport graph home) "" values
    ctorExports = Map.foldlWithKey' (addCtorExports graph home) "" unions
  in
  valueExports <> ctorExports


addValueExport :: Map.Map Opt.Global Opt.Node -> ModuleName.Canonical -> B.Builder -> Name.Name -> Can.Annotation -> B.Builder
addValueExport graph home acc name (Can.Forall freeVars tipe) =
  if shouldExportName name
  then
    let
      nameStr = Name.toBuilder name
      typeVars = Map.keys freeVars
      typeStr = generateAnnotatedTypeForExport home typeVars tipe
      export = "  " <> nameStr <> ": " <> typeStr <> ";\n"
    in
    if B.toLazyByteString acc == ""
    then export
    else acc <> export
  else
    acc


-- Check if a name should be exported (filter out internal functions)
shouldExportName :: Name.Name -> Bool
shouldExportName name =
  let nameStr = Name.toChars name
  in not (List.isPrefixOf "w3_" nameStr)  -- Filter out wire3 encode/decode functions


addCtorExports :: Map.Map Opt.Global Opt.Node -> ModuleName.Canonical -> B.Builder -> Name.Name -> I.Union -> B.Builder
addCtorExports graph home acc unionName union =
  case union of
    I.PrivateUnion _ -> acc
    I.OpenUnion (Can.Union vars cs _ _) -> 
      Map.foldl' (addCtorExport graph home unionName vars) acc (ctorsToMap cs)
    I.ClosedUnion (Can.Union vars cs _ _) -> 
      Map.foldl' (addCtorExport graph home unionName vars) acc (ctorsToMap cs)


ctorsToMap :: [Can.Ctor] -> Map.Map Name.Name Can.Ctor
ctorsToMap ctors =
  Map.fromList [(name, ctor) | ctor@(Can.Ctor name _ _ _) <- ctors]


addCtorExport :: Map.Map Opt.Global Opt.Node -> ModuleName.Canonical -> Name.Name -> [Name.Name] -> B.Builder -> Can.Ctor -> B.Builder
addCtorExport graph home unionName typeVars acc (Can.Ctor ctorName index numAlts args) =
  let
    nameStr = Name.toBuilder ctorName
    -- For constructors, use namespace syntax
    ModuleName.Canonical _ moduleName = home
    fullTypeName = Name.toBuilder moduleName <> "." <> Name.toBuilder unionName
    -- Add type parameters to the result type if needed
    fullTypeNameWithParams = if null typeVars
                             then fullTypeName
                             else if null args
                                  -- For constant constructors of generic types, use 'any' for type parameters
                                  then fullTypeName <> "<" <> joinWith ", " (replicate (length typeVars) "any") <> ">"
                                  else fullTypeName <> "<" <> joinWith ", " (map Name.toBuilder typeVars) <> ">"
    ctorType = generateCtorTypeForExport home fullTypeNameWithParams typeVars args
    export = "  " <> nameStr <> ": " <> ctorType <> ";\n"
  in
  if B.toLazyByteString acc == ""
  then export
  else acc <> export


generateCtorTypeForExport :: ModuleName.Canonical -> B.Builder -> [Name.Name] -> [Can.Type] -> B.Builder
generateCtorTypeForExport currentModule fullTypeName typeVars args =
  if null args
  then 
    -- For constant constructors, we need to handle generics differently
    -- If there are type vars, the constant needs to work for any type
    if null typeVars
    then fullTypeName
    else fullTypeName  -- For now, use the concrete type without parameters for constants
  else generateCtorFunctionTypeForExport currentModule fullTypeName typeVars args

generateCtorFunctionTypeForExport :: ModuleName.Canonical -> B.Builder -> [Name.Name] -> [Can.Type] -> B.Builder
generateCtorFunctionTypeForExport currentModule resultType typeVars args =
  let
    genericParams = if null typeVars
                    then ""
                    else "<" <> joinWith ", " (map Name.toBuilder typeVars) <> ">"
    argTypes = map (generateTypeForExport currentModule Map.empty) args
    paramNames = map (\i -> "arg" <> B.intDec i) [0..length args - 1]
    directParams = joinWith ", " (zipWith (\name typ -> name <> ": " <> typ) paramNames argTypes)
    -- For generic constructors, put type params before the function
    directSig = genericParams <> "(" <> directParams <> ") => " <> resultType
    currySig = generateCurriedSignature argTypes resultType
  in
  if null typeVars
  then "(" <> directSig <> ") & { curry: " <> currySig <> " }"
  else directSig  -- For generic constructors, omit curry for now to simplify

generateCtorType :: B.Builder -> [Name.Name] -> [Can.Type] -> B.Builder
generateCtorType fullTypeName typeVars args =
  if null args
  then 
    -- For constant constructors, we need to handle generics differently
    -- If there are type vars, the constant needs to work for any type
    if null typeVars
    then fullTypeName
    else fullTypeName  -- For now, use the concrete type without parameters for constants
  else generateCtorFunctionType fullTypeName typeVars args


generateCtorFunctionType :: B.Builder -> [Name.Name] -> [Can.Type] -> B.Builder
generateCtorFunctionType resultType typeVars args =
  let
    genericParams = if null typeVars
                    then ""
                    else "<" <> joinWith ", " (map Name.toBuilder typeVars) <> ">"
    argTypes = map (generateType Map.empty) args
    paramNames = map (\i -> "arg" <> B.intDec i) [0..length args - 1]
    directParams = joinWith ", " (zipWith (\name typ -> name <> ": " <> typ) paramNames argTypes)
    -- For generic constructors, put type params before the function
    directSig = genericParams <> "(" <> directParams <> ") => " <> resultType
    currySig = generateCurriedSignature argTypes resultType
  in
  if null typeVars
  then "(" <> directSig <> ") & { curry: " <> currySig <> " }"
  else directSig  -- For generic constructors, omit curry for now to simplify


-- TYPE GENERATION

generateAnnotatedTypeForExport :: ModuleName.Canonical -> [Name.Name] -> Can.Type -> B.Builder
generateAnnotatedTypeForExport currentModule typeVars tipe =
  case tipe of
    Can.TLambda _ _ ->
      -- For functions, pass the type vars to the function generator
      generateFunctionTypeWithGenericsForExport currentModule typeVars tipe
    _ ->
      -- For non-functions, just generate the type normally
      generateTypeForExport currentModule Map.empty tipe

generateAnnotatedType :: [Name.Name] -> Can.Type -> B.Builder
generateAnnotatedType typeVars tipe =
  case tipe of
    Can.TLambda _ _ ->
      -- For functions, pass the type vars to the function generator
      generateFunctionTypeWithGenerics typeVars tipe
    _ ->
      -- For non-functions, just generate the type normally
      generateType Map.empty tipe

generateTypeForExport :: ModuleName.Canonical -> Map.Map Name.Name Name.Name -> Can.Type -> B.Builder
generateTypeForExport currentModule typeVarMap tipe =
  case tipe of
    Can.TLambda arg result ->
      generateFunctionTypeForExport currentModule typeVarMap tipe
    
    Can.TVar name ->
      Name.toBuilder name
    
    Can.TType home name args ->
      generateNamedTypeForExport currentModule home name args typeVarMap
    
    Can.TRecord fields Nothing ->
      generateRecordTypeForExport currentModule typeVarMap fields
    
    Can.TRecord fields (Just ext) ->
      generateExtensibleRecordTypeForExport currentModule typeVarMap ext fields
    
    Can.TUnit ->
      "null"
    
    Can.TTuple a b Nothing ->
      "[" <> generateTypeForExport currentModule typeVarMap a <> ", " <> generateTypeForExport currentModule typeVarMap b <> "]"
    
    Can.TTuple a b (Just c) ->
      "[" <> generateTypeForExport currentModule typeVarMap a <> ", " <> generateTypeForExport currentModule typeVarMap b <> ", " <> generateTypeForExport currentModule typeVarMap c <> "]"
    
    Can.TAlias _ _ _ (Can.Filled resolved) ->
      generateTypeForExport currentModule typeVarMap resolved
    
    Can.TAlias home name args _ ->
      generateNamedTypeForExport currentModule home name (map snd args) typeVarMap

generateType :: Map.Map Name.Name Name.Name -> Can.Type -> B.Builder
generateType typeVarMap tipe =
  case tipe of
    Can.TLambda arg result ->
      generateFunctionType typeVarMap tipe
    
    Can.TVar name ->
      Name.toBuilder name
    
    Can.TType home name args ->
      generateNamedType home name args typeVarMap
    
    Can.TRecord fields Nothing ->
      generateRecordType typeVarMap fields
    
    Can.TRecord fields (Just ext) ->
      generateExtensibleRecordType typeVarMap ext fields
    
    Can.TUnit ->
      "null"
    
    Can.TTuple a b Nothing ->
      "[" <> generateType typeVarMap a <> ", " <> generateType typeVarMap b <> "]"
    
    Can.TTuple a b (Just c) ->
      "[" <> generateType typeVarMap a <> ", " <> generateType typeVarMap b <> ", " <> generateType typeVarMap c <> "]"
    
    Can.TAlias _ _ _ (Can.Filled resolved) ->
      generateType typeVarMap resolved
    
    Can.TAlias home name args _ ->
      generateNamedType home name (map snd args) typeVarMap


generateFunctionTypeWithGenericsForExport :: ModuleName.Canonical -> [Name.Name] -> Can.Type -> B.Builder
generateFunctionTypeWithGenericsForExport currentModule typeVars tipe =
  let
    genericParams = if null typeVars
                    then ""
                    else "<" <> joinWith ", " (map Name.toBuilder typeVars) <> ">"
    (args, result) = collectFunctionArgs tipe
    argTypes = map (generateTypeForExport currentModule Map.empty) args
    resultType = generateTypeForExport currentModule Map.empty result
    paramNames = map (\i -> "arg" <> B.intDec i) [0..length args - 1]
    directParams = joinWith ", " (zipWith (\name typ -> name <> ": " <> typ) paramNames argTypes)
  in
  -- For generic functions, omit curry to keep things simple
  genericParams <> "(" <> directParams <> ") => " <> resultType

generateFunctionTypeWithGenerics :: [Name.Name] -> Can.Type -> B.Builder
generateFunctionTypeWithGenerics typeVars tipe =
  let
    genericParams = if null typeVars
                    then ""
                    else "<" <> joinWith ", " (map Name.toBuilder typeVars) <> ">"
    (args, result) = collectFunctionArgs tipe
    argTypes = map (generateType Map.empty) args
    resultType = generateType Map.empty result
    paramNames = map (\i -> "arg" <> B.intDec i) [0..length args - 1]
    directParams = joinWith ", " (zipWith (\name typ -> name <> ": " <> typ) paramNames argTypes)
  in
  -- For generic functions, omit curry to keep things simple
  genericParams <> "(" <> directParams <> ") => " <> resultType

generateFunctionTypeForExport :: ModuleName.Canonical -> Map.Map Name.Name Name.Name -> Can.Type -> B.Builder
generateFunctionTypeForExport currentModule typeVarMap tipe =
  let
    (args, result) = collectFunctionArgs tipe
    argTypes = map (generateTypeForExport currentModule typeVarMap) args
    resultType = generateTypeForExport currentModule typeVarMap result
  in
  if length args > 1
  then
    -- For multi-arg functions, use intersection type
    let
      paramNames = map (\i -> "arg" <> B.intDec i) [0..length args - 1]
      directParams = joinWith ", " (zipWith (\name typ -> name <> ": " <> typ) paramNames argTypes)
      directSig = "(" <> directParams <> ") => " <> resultType
      currySig = generateCurriedSignature argTypes resultType
    in
    "(" <> directSig <> ") & { curry: " <> currySig <> " }"
  else
    let
      paramName = if null args then "" else "arg0: " <> head argTypes
    in
    "(" <> paramName <> ") => " <> resultType

generateFunctionType :: Map.Map Name.Name Name.Name -> Can.Type -> B.Builder
generateFunctionType typeVarMap tipe =
  let
    (args, result) = collectFunctionArgs tipe
    argTypes = map (generateType typeVarMap) args
    resultType = generateType typeVarMap result
  in
  if length args > 1
  then
    -- For multi-arg functions, use intersection type
    let
      paramNames = map (\i -> "arg" <> B.intDec i) [0..length args - 1]
      directParams = joinWith ", " (zipWith (\name typ -> name <> ": " <> typ) paramNames argTypes)
      directSig = "(" <> directParams <> ") => " <> resultType
      currySig = generateCurriedSignature argTypes resultType
    in
    "(" <> directSig <> ") & { curry: " <> currySig <> " }"
  else
    let
      paramName = if null args then "" else "arg0: " <> head argTypes
    in
    "(" <> paramName <> ") => " <> resultType


collectFunctionArgs :: Can.Type -> ([Can.Type], Can.Type)
collectFunctionArgs tipe =
  case tipe of
    Can.TLambda arg rest ->
      let (args, result) = collectFunctionArgs rest
      in (arg : args, result)
    _ ->
      ([], tipe)


generateCurriedSignature :: [B.Builder] -> B.Builder -> B.Builder
generateCurriedSignature [] result = result
generateCurriedSignature (arg:args) result =
  -- Always use proper parameter syntax for curry
  "(arg: " <> arg <> ") => " <> generateCurriedSignature args result


generateNamedTypeForExport :: ModuleName.Canonical -> ModuleName.Canonical -> Name.Name -> [Can.Type] -> Map.Map Name.Name Name.Name -> B.Builder
generateNamedTypeForExport currentModule home@(ModuleName.Canonical pkg moduleName) name args typeVarMap =
  let
    argTypes = if null args
               then ""
               else "<" <> joinWith ", " (map (generateTypeForExport currentModule typeVarMap) args) <> ">"
  in
  if isBuiltinType home name
  then generateBuiltinType name argTypes
  else if pkg == Pkg.core && moduleName == Name.string && name == Name.string
  then "string"  -- Special case for String.String
  else if pkg == Pkg.core && moduleName == Name.list && name == Name.list
  then "Array" <> argTypes  -- Special case for List.List -> Array with type params
  else if home == currentModule
  then 
    -- When referencing types from the same module, use namespace syntax
    let ModuleName.Canonical _ currentModuleName = currentModule
    in Name.toBuilder currentModuleName <> "." <> Name.toBuilder name <> argTypes
  else 
    let modulePrefix = if pkg == Pkg.core && moduleName == Name.basics
                       then ""
                       -- Avoid dots in type names for TypeScript
                       else Name.toBuilder moduleName <> "_"
    in modulePrefix <> Name.toBuilder name <> argTypes

generateNamedType :: ModuleName.Canonical -> Name.Name -> [Can.Type] -> Map.Map Name.Name Name.Name -> B.Builder
generateNamedType home@(ModuleName.Canonical pkg moduleName) name args typeVarMap =
  let
    argTypes = if null args
               then ""
               else "<" <> joinWith ", " (map (generateType typeVarMap) args) <> ">"
  in
  if isBuiltinType home name
  then generateBuiltinType name argTypes
  else if pkg == Pkg.core && moduleName == Name.string && name == Name.string
  then "string"  -- Special case for String.String
  else if pkg == Pkg.core && moduleName == Name.list && name == Name.list
  then "Array" <> argTypes  -- Special case for List.List -> Array with type params
  else 
    let modulePrefix = if pkg == Pkg.core && moduleName == Name.basics
                       then ""
                       -- Avoid dots in type names for TypeScript
                       else Name.toBuilder moduleName <> "_"
    in modulePrefix <> Name.toBuilder name <> argTypes


isBuiltinType :: ModuleName.Canonical -> Name.Name -> Bool
isBuiltinType (ModuleName.Canonical pkg moduleName) name =
  pkg == Pkg.core && moduleName == Name.basics &&
  (name `elem` [Name.int, Name.float, Name.bool, Name.string, Name.list])


generateBuiltinType :: Name.Name -> B.Builder -> B.Builder
generateBuiltinType name typeArgs
  | name == Name.int = "number"
  | name == Name.float = "number"
  | name == Name.bool = "boolean"
  | name == Name.string = "string"
  | name == Name.list = "Array" <> typeArgs
  | otherwise = Name.toBuilder name <> typeArgs


generateRecordTypeForExport :: ModuleName.Canonical -> Map.Map Name.Name Name.Name -> Map.Map Name.Name Can.FieldType -> B.Builder
generateRecordTypeForExport currentModule typeVarMap fields =
  let
    fieldList = Map.toList fields
    fieldDecls = map (generateRecordFieldForExport currentModule typeVarMap) fieldList
  in
  "{ " <> joinWith "; " fieldDecls <> " }"

generateRecordFieldForExport :: ModuleName.Canonical -> Map.Map Name.Name Name.Name -> (Name.Name, Can.FieldType) -> B.Builder
generateRecordFieldForExport currentModule typeVarMap (name, Can.FieldType _ tipe) =
  Name.toBuilder name <> ": " <> generateTypeForExport currentModule typeVarMap tipe

generateExtensibleRecordTypeForExport :: ModuleName.Canonical -> Map.Map Name.Name Name.Name -> Name.Name -> Map.Map Name.Name Can.FieldType -> B.Builder
generateExtensibleRecordTypeForExport currentModule typeVarMap ext fields =
  Name.toBuilder ext <> " & " <> generateRecordTypeForExport currentModule typeVarMap fields

generateRecordType :: Map.Map Name.Name Name.Name -> Map.Map Name.Name Can.FieldType -> B.Builder
generateRecordType typeVarMap fields =
  let
    fieldList = Map.toList fields
    fieldDecls = map (generateRecordField typeVarMap) fieldList
  in
  "{ " <> joinWith "; " fieldDecls <> " }"


generateRecordField :: Map.Map Name.Name Name.Name -> (Name.Name, Can.FieldType) -> B.Builder
generateRecordField typeVarMap (name, Can.FieldType _ tipe) =
  Name.toBuilder name <> ": " <> generateType typeVarMap tipe


generateExtensibleRecordType :: Map.Map Name.Name Name.Name -> Name.Name -> Map.Map Name.Name Can.FieldType -> B.Builder
generateExtensibleRecordType typeVarMap ext fields =
  Name.toBuilder ext <> " & " <> generateRecordType typeVarMap fields


-- Namespace type generation (for types inside namespace declarations)
generateTypeForNamespace :: ModuleName.Canonical -> Map.Map Name.Name Name.Name -> Can.Type -> B.Builder
generateTypeForNamespace currentModule typeVarMap tipe =
  case tipe of
    Can.TLambda arg result ->
      generateFunctionType typeVarMap tipe
    
    Can.TVar name ->
      Name.toBuilder name
    
    Can.TType home name args ->
      generateNamedTypeForNamespace currentModule home name args typeVarMap
    
    Can.TRecord fields Nothing ->
      generateRecordTypeForNamespace currentModule typeVarMap fields
    
    Can.TRecord fields (Just ext) ->
      generateExtensibleRecordTypeForNamespace currentModule typeVarMap ext fields
    
    Can.TUnit ->
      "null"
    
    Can.TTuple a b Nothing ->
      "[" <> generateTypeForNamespace currentModule typeVarMap a <> ", " <> generateTypeForNamespace currentModule typeVarMap b <> "]"
    
    Can.TTuple a b (Just c) ->
      "[" <> generateTypeForNamespace currentModule typeVarMap a <> ", " <> generateTypeForNamespace currentModule typeVarMap b <> ", " <> generateTypeForNamespace currentModule typeVarMap c <> "]"
    
    Can.TAlias _ _ _ (Can.Filled resolved) ->
      generateTypeForNamespace currentModule typeVarMap resolved
    
    Can.TAlias home name args _ ->
      generateNamedTypeForNamespace currentModule home name (map snd args) typeVarMap

generateNamedTypeForNamespace :: ModuleName.Canonical -> ModuleName.Canonical -> Name.Name -> [Can.Type] -> Map.Map Name.Name Name.Name -> B.Builder
generateNamedTypeForNamespace currentModule home@(ModuleName.Canonical pkg moduleName) name args typeVarMap =
  let
    argTypes = if null args
               then ""
               else "<" <> joinWith ", " (map (generateTypeForNamespace currentModule typeVarMap) args) <> ">"
  in
  if isBuiltinType home name
  then generateBuiltinType name argTypes
  else if pkg == Pkg.core && moduleName == Name.string && name == Name.string
  then "string"  -- Special case for String.String
  else if pkg == Pkg.core && moduleName == Name.list && name == Name.list
  then "Array" <> argTypes  -- Special case for List.List -> Array with type params
  else if home == currentModule
  then 
    -- When referencing types from the same module within namespace, use direct name
    Name.toBuilder name <> argTypes
  else 
    let modulePrefix = if pkg == Pkg.core && moduleName == Name.basics
                       then ""
                       -- Avoid dots in type names for TypeScript
                       else Name.toBuilder moduleName <> "_"
    in modulePrefix <> Name.toBuilder name <> argTypes

generateRecordTypeForNamespace :: ModuleName.Canonical -> Map.Map Name.Name Name.Name -> Map.Map Name.Name Can.FieldType -> B.Builder
generateRecordTypeForNamespace currentModule typeVarMap fields =
  let
    fieldList = Map.toList fields
    fieldDecls = map (generateRecordFieldForNamespace currentModule typeVarMap) fieldList
  in
  "{ " <> joinWith "; " fieldDecls <> " }"

generateRecordFieldForNamespace :: ModuleName.Canonical -> Map.Map Name.Name Name.Name -> (Name.Name, Can.FieldType) -> B.Builder
generateRecordFieldForNamespace currentModule typeVarMap (name, Can.FieldType _ tipe) =
  Name.toBuilder name <> ": " <> generateTypeForNamespace currentModule typeVarMap tipe

generateExtensibleRecordTypeForNamespace :: ModuleName.Canonical -> Map.Map Name.Name Name.Name -> Name.Name -> Map.Map Name.Name Can.FieldType -> B.Builder
generateExtensibleRecordTypeForNamespace currentModule typeVarMap ext fields =
  Name.toBuilder ext <> " & " <> generateRecordTypeForNamespace currentModule typeVarMap fields

-- HELPERS

joinWith :: B.Builder -> [B.Builder] -> B.Builder
joinWith sep builders =
  mconcat (List.intersperse sep builders)