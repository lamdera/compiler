module Generate.Types exposing
    ( ComparableType(..)
    , ElmType(..)
    , GeneratedModule
    , TypeDecl(..)
    , TypeRef
    , elmTypeToAnnotation
    , generateModules
    , renderDecl
    , renderModule
    )

{-| Type AST, random type generation with weighted edge-case scenarios,
and rendering to elm-codegen declarations.
-}

import Elm
import Elm.Annotation as Type
import Generate.Names as Names
import Random


{-| A type that can appear in a field, variant parameter, or collection.
-}
type ElmType
    = TInt
    | TFloat
    | TBool
    | TChar
    | TString
    | TUnit
    | TOrder
    | TTimePosix
    | TMaybe ElmType
    | TResult ElmType ElmType
    | TList ElmType
    | TSet ComparableType
    | TArray ElmType
    | TDict ComparableType ElmType
    | TSeqDict ComparableType ElmType
    | TSeqSet ComparableType
    | TTuple2 ElmType ElmType
    | TTuple3 ElmType ElmType ElmType
    | TRecord (List ( String, ElmType ))
    | TRef TypeRef


{-| Comparable types for Set/Dict keys.
-}
type ComparableType
    = CInt
    | CFloat
    | CChar
    | CString


{-| Reference to a named type (possibly in another module).
-}
type alias TypeRef =
    { modulePath : List String
    , typeName : String
    }


{-| A type declaration to generate.
-}
type TypeDecl
    = UnionDecl
        { name : String
        , variants : List ( String, List ElmType )
        }
    | RecordAliasDecl
        { name : String
        , fields : List ( String, ElmType )
        }
    | ExtensibleRecordDecl
        { name : String
        , extensionVar : String
        , fields : List ( String, ElmType )
        }
    | ConcreteExtensionDecl
        { name : String
        , baseRef : TypeRef
        , baseFields : List ( String, ElmType )
        , extraFields : List ( String, ElmType )
        }
    | AliasChainDecl
        { name : String
        , targetRef : TypeRef
        }
    | ExtensibleExtensionDecl
        { name : String
        , extensionVar : String
        , baseRef : TypeRef
        , ownFields : List ( String, ElmType )
        }


{-| A generated module with its declarations.
-}
type alias GeneratedModule =
    { name : String
    , modulePath : List String
    , decls : List TypeDecl
    }


{-| Generate a list of modules from a seed.
-}
generateModules : Int -> Int -> List GeneratedModule
generateModules seed count =
    let
        initialSeed =
            Random.initialSeed seed
    in
    generateModulesHelp initialSeed count 0 []
        |> List.reverse


generateModulesHelp : Random.Seed -> Int -> Int -> List GeneratedModule -> List GeneratedModule
generateModulesHelp seed remaining index acc =
    if remaining <= 0 then
        acc

    else
        let
            ( mod, nextSeed ) =
                Random.step (moduleGenerator index acc) seed
        in
        generateModulesHelp nextSeed (remaining - 1) (index + 1) (mod :: acc)


{-| Generate a single module. Earlier modules from `priorModules` can be referenced.
-}
moduleGenerator : Int -> List GeneratedModule -> Random.Generator GeneratedModule
moduleGenerator index priorModules =
    let
        modName =
            Names.moduleName index

        modulePath =
            [ "Generated", modName ]
    in
    Random.int 5 12
        |> Random.andThen
            (\declCount ->
                generateDeclsForModule index modulePath priorModules declCount
            )
        |> Random.map
            (\decls ->
                { name = modName
                , modulePath = modulePath
                , decls = decls
                }
            )


generateDeclsForModule : Int -> List String -> List GeneratedModule -> Int -> Random.Generator (List TypeDecl)
generateDeclsForModule moduleIndex modulePath priorModules count =
    generateDeclsHelp moduleIndex modulePath priorModules count 0 []
        |> Random.map List.reverse


generateDeclsHelp :
    Int
    -> List String
    -> List GeneratedModule
    -> Int
    -> Int
    -> List TypeDecl
    -> Random.Generator (List TypeDecl)
generateDeclsHelp moduleIndex modulePath priorModules remaining typeIndex acc =
    if remaining <= 0 then
        Random.constant acc

    else
        scenarioGenerator moduleIndex modulePath priorModules typeIndex acc
            |> Random.andThen
                (\newDecls ->
                    generateDeclsHelp
                        moduleIndex
                        modulePath
                        priorModules
                        (remaining - List.length newDecls)
                        (typeIndex + List.length newDecls)
                        (List.reverse newDecls ++ acc)
                )



-- SCENARIO GENERATORS


{-| All the context a scenario generator needs. Avoids threading 6+ parameters.
-}
type alias ScenarioContext =
    { moduleIndex : Int
    , modulePath : List String
    , typeIndex : Int
    , allRefs : List TypeRef
    , allExtensibleRefs : List ExtensibleRef
    , priorModules : List GeneratedModule
    }


type Scenario
    = ExtensibleChain
    | TwoLevelExtensibleChain
    | CrossModuleExtensibleChain
    | OverlappingExtensibleFields
    | NestedExtension
    | SimpleUnion
    | UnionWithParams
    | MultiRefUnion
    | ManyVariantUnion
    | RecordAlias
    | LargeRecord
    | RecordWithRefs
    | AliasChain
    | DeepAliasChain
    | CrossModuleRef


scenarioGenerator :
    Int
    -> List String
    -> List GeneratedModule
    -> Int
    -> List TypeDecl
    -> Random.Generator (List TypeDecl)
scenarioGenerator moduleIndex modulePath priorModules typeIndex localDecls =
    let
        localRefs =
            declsToRefs modulePath localDecls

        ctx =
            { moduleIndex = moduleIndex
            , modulePath = modulePath
            , typeIndex = typeIndex
            , allRefs = priorModuleRefs priorModules ++ localRefs
            , allExtensibleRefs = priorExtensibleRefs priorModules ++ declsToExtensibleRefs modulePath localDecls
            , priorModules = priorModules
            }

        {- Build the weight table dynamically.
           Scenarios that require prior modules/refs are only included when available.
           This avoids the confusing "fallback to a different scenario" pattern.
        -}
        alwaysAvailable =
            [ ( 15, ExtensibleChain )
            , ( 10, TwoLevelExtensibleChain )
            , ( 10, SimpleUnion )
            , ( 10, UnionWithParams )
            , ( 8, RecordAlias )
            , ( 2, ManyVariantUnion )
            ]

        needsRefs =
            if List.isEmpty ctx.allRefs then
                []

            else
                [ ( 8, AliasChain )
                , ( 5, DeepAliasChain )
                , ( 5, MultiRefUnion )
                , ( 4, LargeRecord )
                , ( 2, RecordWithRefs )
                ]

        needsPriorModules =
            if List.isEmpty priorModules then
                []

            else
                [ ( 8, CrossModuleRef )
                ]

        needsExtensibleRefs =
            if List.isEmpty ctx.allExtensibleRefs then
                []

            else
                [ ( 8, OverlappingExtensibleFields )
                , ( 3, NestedExtension )
                ]

        needsPriorExtensibleRefs =
            if List.isEmpty (priorExtensibleRefs priorModules) then
                []

            else
                [ ( 12, CrossModuleExtensibleChain )
                ]

        allWeights =
            alwaysAvailable
                ++ needsRefs
                ++ needsPriorModules
                ++ needsExtensibleRefs
                ++ needsPriorExtensibleRefs
    in
    case allWeights of
        first :: rest ->
            Random.weighted first rest
                |> Random.andThen (runScenario ctx)

        [] ->
            -- Should never happen since alwaysAvailable is non-empty
            simpleUnionScenario moduleIndex typeIndex


runScenario : ScenarioContext -> Scenario -> Random.Generator (List TypeDecl)
runScenario ctx scenario =
    case scenario of
        ExtensibleChain ->
            extensibleRecordChainScenario ctx.moduleIndex ctx.modulePath ctx.typeIndex

        TwoLevelExtensibleChain ->
            twoLevelExtensibleScenario ctx.moduleIndex ctx.modulePath ctx.typeIndex

        CrossModuleExtensibleChain ->
            crossModuleExtensibleScenario ctx.moduleIndex ctx.modulePath ctx.priorModules ctx.typeIndex

        OverlappingExtensibleFields ->
            overlappingFieldsScenario ctx.moduleIndex ctx.modulePath ctx.typeIndex ctx.allExtensibleRefs

        NestedExtension ->
            nestedExtensionScenario ctx.moduleIndex ctx.modulePath ctx.typeIndex ctx.allExtensibleRefs ctx.allRefs

        SimpleUnion ->
            simpleUnionScenario ctx.moduleIndex ctx.typeIndex

        UnionWithParams ->
            unionWithParamsScenario ctx.moduleIndex ctx.typeIndex

        MultiRefUnion ->
            multiRefUnionScenario ctx.moduleIndex ctx.typeIndex ctx.allRefs

        ManyVariantUnion ->
            manyVariantUnionScenario ctx.moduleIndex ctx.typeIndex

        RecordAlias ->
            recordAliasScenario ctx.moduleIndex ctx.typeIndex

        LargeRecord ->
            largeRecordScenario ctx.moduleIndex ctx.typeIndex ctx.allRefs

        RecordWithRefs ->
            recordWithRefsScenario ctx.moduleIndex ctx.typeIndex ctx.allRefs

        AliasChain ->
            aliasChainScenario ctx.moduleIndex ctx.modulePath ctx.typeIndex ctx.allRefs

        DeepAliasChain ->
            deepAliasChainScenario ctx.moduleIndex ctx.modulePath ctx.priorModules ctx.typeIndex ctx.allRefs

        CrossModuleRef ->
            crossModuleScenario ctx.moduleIndex ctx.modulePath ctx.priorModules ctx.typeIndex


{-| Scenario 1: Extensible record base + concrete extension (+ optional alias chain)
This is the highest-weighted scenario because extensible record alias chains
have been the source of real bugs.
-}
extensibleRecordChainScenario : Int -> List String -> Int -> Random.Generator (List TypeDecl)
extensibleRecordChainScenario moduleIndex modulePath typeIndex =
    Random.int 2 4
        |> Random.andThen
            (\fieldCount ->
                randomFields fieldCount (typeIndex * 10)
                    |> Random.andThen
                        (\baseFields ->
                            Random.int 1 3
                                |> Random.andThen
                                    (\extraFieldCount ->
                                        randomFieldsAvoiding extraFieldCount (typeIndex * 10 + fieldCount) (List.map Tuple.first baseFields)
                                            |> Random.andThen
                                                (\extraFields ->
                                                    let
                                                        baseName =
                                                            Names.typeName moduleIndex typeIndex ++ "Base"

                                                        concreteName =
                                                            Names.typeName moduleIndex typeIndex

                                                        chainName =
                                                            Names.typeName moduleIndex typeIndex ++ "Chain"

                                                        baseDecl =
                                                            ExtensibleRecordDecl
                                                                { name = baseName
                                                                , extensionVar = "compatible"
                                                                , fields = baseFields
                                                                }

                                                        concreteDecl =
                                                            ConcreteExtensionDecl
                                                                { name = concreteName
                                                                , baseRef =
                                                                    { modulePath = modulePath
                                                                    , typeName = baseName
                                                                    }
                                                                , baseFields = baseFields
                                                                , extraFields = extraFields
                                                                }

                                                        chainDecl =
                                                            AliasChainDecl
                                                                { name = chainName
                                                                , targetRef =
                                                                    { modulePath = modulePath
                                                                    , typeName = concreteName
                                                                    }
                                                                }
                                                    in
                                                    Random.weighted ( 60, True ) [ ( 40, False ) ]
                                                        |> Random.map
                                                            (\includeChain ->
                                                                if includeChain then
                                                                    [ baseDecl, concreteDecl, chainDecl ]

                                                                else
                                                                    [ baseDecl, concreteDecl ]
                                                            )
                                                )
                                    )
                        )
            )


{-| Scenario 2: Simple enum union (no parameters).
-}
simpleUnionScenario : Int -> Int -> Random.Generator (List TypeDecl)
simpleUnionScenario moduleIndex typeIndex =
    Random.int 2 6
        |> Random.map
            (\variantCount ->
                let
                    variants =
                        List.range 0 (variantCount - 1)
                            |> List.map (\i -> ( Names.constructorName (typeIndex * 10 + i), [] ))
                in
                [ UnionDecl
                    { name = Names.typeName moduleIndex typeIndex
                    , variants = variants
                    }
                ]
            )


{-| Scenario 3: Record type alias with various field types.
-}
recordAliasScenario : Int -> Int -> Random.Generator (List TypeDecl)
recordAliasScenario moduleIndex typeIndex =
    Random.int 2 8
        |> Random.andThen
            (\fieldCount ->
                randomFields fieldCount (typeIndex * 10)
                    |> Random.map
                        (\fields ->
                            [ RecordAliasDecl
                                { name = Names.typeName moduleIndex typeIndex
                                , fields = fields
                                }
                            ]
                        )
            )


{-| Scenario 4: Union with diverse parameter types.
-}
unionWithParamsScenario : Int -> Int -> Random.Generator (List TypeDecl)
unionWithParamsScenario moduleIndex typeIndex =
    Random.int 3 8
        |> Random.andThen
            (\variantCount ->
                randomVariants variantCount (typeIndex * 10)
                    |> Random.map
                        (\variants ->
                            [ UnionDecl
                                { name = Names.typeName moduleIndex typeIndex
                                , variants = variants
                                }
                            ]
                        )
            )


{-| Scenario 5: Cross-module reference - union or record wrapping a type from a prior module.
-}
crossModuleScenario : Int -> List String -> List GeneratedModule -> Int -> Random.Generator (List TypeDecl)
crossModuleScenario moduleIndex modulePath priorModules typeIndex =
    let
        allPriorRefs =
            priorModuleRefs priorModules
    in
    case allPriorRefs of
        [] ->
            simpleUnionScenario moduleIndex typeIndex

        firstRef :: restRefs ->
            Random.uniform firstRef restRefs
                |> Random.andThen
                    (\ref ->
                        Random.weighted ( 50, True ) [ ( 50, False ) ]
                            |> Random.map
                                (\asUnion ->
                                    if asUnion then
                                        [ UnionDecl
                                            { name = Names.typeName moduleIndex typeIndex
                                            , variants =
                                                [ ( "Wrap" ++ ref.typeName, [ TRef ref ] )
                                                , ( "Other" ++ Names.constructorName typeIndex, [ TInt ] )
                                                ]
                                            }
                                        ]

                                    else
                                        [ RecordAliasDecl
                                            { name = Names.typeName moduleIndex typeIndex
                                            , fields =
                                                [ ( "wrapped", TRef ref )
                                                , ( "label", TString )
                                                ]
                                            }
                                        ]
                                )
                    )


{-| Scenario 6: Alias chain pointing to an existing type.
-}
aliasChainScenario : Int -> List String -> Int -> List TypeRef -> Random.Generator (List TypeDecl)
aliasChainScenario moduleIndex modulePath typeIndex allRefs =
    case allRefs of
        [] ->
            simpleUnionScenario moduleIndex typeIndex

        firstRef :: restRefs ->
            Random.uniform firstRef restRefs
                |> Random.map
                    (\ref ->
                        [ AliasChainDecl
                            { name = Names.typeName moduleIndex typeIndex
                            , targetRef = ref
                            }
                        ]
                    )


{-| Scenario 7: Union with many variants (tests multi-byte tag encoding).
-}
manyVariantUnionScenario : Int -> Int -> Random.Generator (List TypeDecl)
manyVariantUnionScenario moduleIndex typeIndex =
    Random.int 257 300
        |> Random.map
            (\variantCount ->
                let
                    prefix =
                        "M" ++ String.fromInt moduleIndex ++ "T" ++ String.fromInt typeIndex ++ "V"

                    variants =
                        List.range 0 (variantCount - 1)
                            |> List.map
                                (\i ->
                                    ( prefix ++ String.padLeft 3 '0' (String.fromInt i)
                                    , []
                                    )
                                )
                in
                [ UnionDecl
                    { name = Names.typeName moduleIndex typeIndex ++ "Massive"
                    , variants = variants
                    }
                ]
            )


{-| Scenario 9: Cross-module extensible record chain.
Base extensible in a prior module, concrete extension here.
This is the exact pattern that caused the recent TAlias Filled bug.
-}
crossModuleExtensibleScenario : Int -> List String -> List GeneratedModule -> Int -> Random.Generator (List TypeDecl)
crossModuleExtensibleScenario moduleIndex modulePath priorModules typeIndex =
    let
        extRefs =
            priorExtensibleRefs priorModules
    in
    case extRefs of
        [] ->
            extensibleRecordChainScenario moduleIndex modulePath typeIndex

        firstExt :: restExt ->
            Random.uniform firstExt restExt
                |> Random.andThen
                    (\extRef ->
                        Random.int 1 3
                            |> Random.andThen
                                (\extraFieldCount ->
                                    randomFieldsAvoiding extraFieldCount (typeIndex * 10 + 50) (List.map Tuple.first extRef.fields)
                                        |> Random.andThen
                                            (\extraFields ->
                                                let
                                                    concreteName =
                                                        Names.typeName moduleIndex typeIndex

                                                    chainName =
                                                        Names.typeName moduleIndex typeIndex ++ "Chain"

                                                    concreteDecl =
                                                        ConcreteExtensionDecl
                                                            { name = concreteName
                                                            , baseRef =
                                                                { modulePath = extRef.modulePath
                                                                , typeName = extRef.typeName
                                                                }
                                                            , baseFields = extRef.fields
                                                            , extraFields = extraFields
                                                            }

                                                    chainDecl =
                                                        AliasChainDecl
                                                            { name = chainName
                                                            , targetRef =
                                                                { modulePath = modulePath
                                                                , typeName = concreteName
                                                                }
                                                            }

                                                    -- Also wrap in a union to test union-of-cross-module-extension
                                                    unionDecl =
                                                        UnionDecl
                                                            { name = Names.typeName moduleIndex typeIndex ++ "Wrap"
                                                            , variants =
                                                                [ ( "Wrap" ++ concreteName
                                                                  , [ TRef { modulePath = modulePath, typeName = concreteName } ]
                                                                  )
                                                                , ( "WrapChain" ++ chainName
                                                                  , [ TRef { modulePath = modulePath, typeName = chainName } ]
                                                                  )
                                                                , ( "Plain" ++ Names.constructorName typeIndex
                                                                  , [ TInt ]
                                                                  )
                                                                ]
                                                            }
                                                in
                                                Random.weighted ( 40, True ) [ ( 60, False ) ]
                                                    |> Random.map
                                                        (\includeUnion ->
                                                            if includeUnion then
                                                                [ concreteDecl, chainDecl, unionDecl ]

                                                            else
                                                                [ concreteDecl, chainDecl ]
                                                        )
                                            )
                                )
                    )


{-| Scenario 10: Overlapping extensible record fields.
Extension includes fields that already exist in the base.
This tests the deduplication logic in wire codec generation.
-}
overlappingFieldsScenario : Int -> List String -> Int -> List ExtensibleRef -> Random.Generator (List TypeDecl)
overlappingFieldsScenario moduleIndex modulePath typeIndex extensibleRefs =
    case extensibleRefs of
        [] ->
            extensibleRecordChainScenario moduleIndex modulePath typeIndex

        firstExt :: restExt ->
            Random.uniform firstExt restExt
                |> Random.andThen
                    (\extRef ->
                        Random.int 1 2
                            |> Random.andThen
                                (\extraFieldCount ->
                                    -- Use unique field names that don't clash with base fields
                                    let
                                        baseFieldNames =
                                            List.map Tuple.first extRef.fields

                                        safeStartIndex =
                                            typeIndex * 10 + 70
                                    in
                                    randomFieldsAvoiding extraFieldCount safeStartIndex baseFieldNames
                                        |> Random.map
                                            (\extraFields ->
                                                let
                                                    -- Overlapping fields: same name AND same type from the base
                                                    overlappingFields =
                                                        List.take 1 extRef.fields

                                                    allExtraFields =
                                                        overlappingFields ++ extraFields
                                                in
                                                [ ConcreteExtensionDecl
                                                    { name = Names.typeName moduleIndex typeIndex ++ "Overlap"
                                                    , baseRef =
                                                        { modulePath = extRef.modulePath
                                                        , typeName = extRef.typeName
                                                        }
                                                    , baseFields = extRef.fields
                                                    , extraFields = allExtraFields
                                                    }
                                                ]
                                            )
                                )
                    )


{-| Scenario 11: Deep alias chain (3-4 levels across modules).
A -> B -> C -> D where each is an alias of the previous.
-}
deepAliasChainScenario : Int -> List String -> List GeneratedModule -> Int -> List TypeRef -> Random.Generator (List TypeDecl)
deepAliasChainScenario moduleIndex modulePath priorModules typeIndex allRefs =
    case allRefs of
        [] ->
            recordAliasScenario moduleIndex typeIndex

        firstRef :: restRefs ->
            Random.uniform firstRef restRefs
                |> Random.map
                    (\ref ->
                        let
                            level1Name =
                                Names.typeName moduleIndex typeIndex ++ "L1"

                            level2Name =
                                Names.typeName moduleIndex typeIndex ++ "L2"

                            level3Name =
                                Names.typeName moduleIndex typeIndex ++ "L3"
                        in
                        [ AliasChainDecl
                            { name = level1Name
                            , targetRef = ref
                            }
                        , AliasChainDecl
                            { name = level2Name
                            , targetRef = { modulePath = modulePath, typeName = level1Name }
                            }
                        , AliasChainDecl
                            { name = level3Name
                            , targetRef = { modulePath = modulePath, typeName = level2Name }
                            }
                        ]
                    )


{-| Scenario 12: Union with multiple variants each wrapping different cross-module types.
Tests that the compiler handles diverse cross-module codec delegation within one type.
-}
multiRefUnionScenario : Int -> Int -> List TypeRef -> Random.Generator (List TypeDecl)
multiRefUnionScenario moduleIndex typeIndex allRefs =
    if List.isEmpty allRefs then
        simpleUnionScenario moduleIndex typeIndex

    else
        let
            -- Pick a random subset by dropping from the front
            dropCount =
                modBy (List.length allRefs) (typeIndex * 7)

            selectedRefs =
                List.drop dropCount allRefs |> List.take 4
        in
        if List.isEmpty selectedRefs then
            simpleUnionScenario moduleIndex typeIndex

        else
            Random.constant
                [ UnionDecl
                    { name = Names.typeName moduleIndex typeIndex ++ "Multi"
                    , variants =
                        List.indexedMap
                            (\i ref ->
                                ( "T" ++ String.fromInt typeIndex ++ "R" ++ String.fromInt i ++ ref.typeName
                                , [ TRef ref ]
                                )
                            )
                            selectedRefs
                            ++ [ ( "T" ++ String.fromInt typeIndex ++ "None", [] ) ]
                    }
                ]


{-| Scenario 13: Record with many fields (8-14).
Tests the andMap chaining for large records.
-}
largeRecordScenario : Int -> Int -> List TypeRef -> Random.Generator (List TypeDecl)
largeRecordScenario moduleIndex typeIndex allRefs =
    Random.int 8 14
        |> Random.andThen
            (\fieldCount ->
                randomFieldsWithRefs fieldCount (typeIndex * 10) allRefs
                    |> Random.map
                        (\fields ->
                            [ RecordAliasDecl
                                { name = Names.typeName moduleIndex typeIndex ++ "Large"
                                , fields = fields
                                }
                            ]
                        )
            )


{-| Scenario 14: Nested extension scenario.
Take an extensible base, create a concrete extension, then wrap it in a record
whose fields include both the extension and other types, creating nested
cross-module references through alias chains.
-}
nestedExtensionScenario : Int -> List String -> Int -> List ExtensibleRef -> List TypeRef -> Random.Generator (List TypeDecl)
nestedExtensionScenario moduleIndex modulePath typeIndex extensibleRefs allRefs =
    case extensibleRefs of
        [] ->
            extensibleRecordChainScenario moduleIndex modulePath typeIndex

        firstExt :: restExt ->
            Random.uniform firstExt restExt
                |> Random.andThen
                    (\extRef ->
                        Random.int 1 2
                            |> Random.andThen
                                (\extraFieldCount ->
                                    randomFieldsAvoiding extraFieldCount (typeIndex * 10 + 90) (List.map Tuple.first extRef.fields)
                                        |> Random.map
                                            (\extraFields ->
                                                let
                                                    concreteName =
                                                        Names.typeName moduleIndex typeIndex ++ "Ext"

                                                    wrapperName =
                                                        Names.typeName moduleIndex typeIndex ++ "Nested"

                                                    concreteDecl =
                                                        ConcreteExtensionDecl
                                                            { name = concreteName
                                                            , baseRef =
                                                                { modulePath = extRef.modulePath
                                                                , typeName = extRef.typeName
                                                                }
                                                            , baseFields = extRef.fields
                                                            , extraFields = extraFields
                                                            }

                                                    -- Wrap in a record with additional fields
                                                    wrapperFields =
                                                        ( "nested"
                                                        , TRef { modulePath = modulePath, typeName = concreteName }
                                                        )
                                                            :: ( "label", TString )
                                                            :: List.indexedMap
                                                                (\i ref ->
                                                                    ( "ref" ++ String.fromInt i, TRef ref )
                                                                )
                                                                (List.filter
                                                                    (\ref -> ref.modulePath /= modulePath)
                                                                    (List.take 2 allRefs)
                                                                )

                                                    wrapperDecl =
                                                        RecordAliasDecl
                                                            { name = wrapperName
                                                            , fields = wrapperFields
                                                            }
                                                in
                                                [ concreteDecl, wrapperDecl ]
                                            )
                                )
                    )


{-| Scenario 15: Two-level extensible record chain.
Level1 compatible = { compatible | field1 : ... }
Level2 compatible = Level1 { compatible | field2 : ... }
Concrete = Level2 { field3 : ... }

This is a KNOWN BUG pattern — the wire codec generator only resolves one level
of TAlias chain, losing the outermost extension fields.
-}
twoLevelExtensibleScenario : Int -> List String -> Int -> Random.Generator (List TypeDecl)
twoLevelExtensibleScenario moduleIndex modulePath typeIndex =
    Random.int 1 3
        |> Random.andThen
            (\level1FieldCount ->
                randomFields level1FieldCount (typeIndex * 10)
                    |> Random.andThen
                        (\level1Fields ->
                            let
                                level1FieldNames =
                                    List.map Tuple.first level1Fields
                            in
                            Random.int 1 2
                                |> Random.andThen
                                    (\level2FieldCount ->
                                        randomFieldsAvoiding level2FieldCount (typeIndex * 10 + 20) level1FieldNames
                                            |> Random.andThen
                                                (\level2Fields ->
                                                    let
                                                        allAvoid =
                                                            level1FieldNames ++ List.map Tuple.first level2Fields
                                                    in
                                                    Random.int 1 2
                                                        |> Random.andThen
                                                            (\concreteFieldCount ->
                                                                randomFieldsAvoiding concreteFieldCount (typeIndex * 10 + 40) allAvoid
                                                                    |> Random.map
                                                                        (\concreteFields ->
                                                                            let
                                                                                level1Name =
                                                                                    Names.typeName moduleIndex typeIndex ++ "L1Base"

                                                                                level2Name =
                                                                                    Names.typeName moduleIndex typeIndex ++ "L2Base"

                                                                                concreteName =
                                                                                    Names.typeName moduleIndex typeIndex ++ "TwoLevel"

                                                                                -- Level1: { compatible | field1 : ... }
                                                                                level1Decl =
                                                                                    ExtensibleRecordDecl
                                                                                        { name = level1Name
                                                                                        , extensionVar = "compatible"
                                                                                        , fields = level1Fields
                                                                                        }

                                                                                -- Level2: Level1 { compatible | field2 : ... }
                                                                                -- This is extensible AND extends Level1
                                                                                level2Decl =
                                                                                    ExtensibleExtensionDecl
                                                                                        { name = level2Name
                                                                                        , extensionVar = "compatible"
                                                                                        , baseRef =
                                                                                            { modulePath = modulePath
                                                                                            , typeName = level1Name
                                                                                            }
                                                                                        , ownFields = level2Fields
                                                                                        }

                                                                                -- Concrete: Level2 { field3 : ... }
                                                                                concreteDecl =
                                                                                    ConcreteExtensionDecl
                                                                                        { name = concreteName
                                                                                        , baseRef =
                                                                                            { modulePath = modulePath
                                                                                            , typeName = level2Name
                                                                                            }
                                                                                        , baseFields = level1Fields ++ level2Fields
                                                                                        , extraFields = concreteFields
                                                                                        }
                                                                            in
                                                                            [ level1Decl, level2Decl, concreteDecl ]
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


{-| Scenario 8: Record with references to existing types.
-}
recordWithRefsScenario : Int -> Int -> List TypeRef -> Random.Generator (List TypeDecl)
recordWithRefsScenario moduleIndex typeIndex allRefs =
    Random.int 2 5
        |> Random.andThen
            (\fieldCount ->
                randomFieldsWithRefs fieldCount (typeIndex * 10) allRefs
                    |> Random.map
                        (\fields ->
                            [ RecordAliasDecl
                                { name = Names.typeName moduleIndex typeIndex
                                , fields = fields
                                }
                            ]
                        )
            )



-- RANDOM HELPERS


{-| Generate random primitive/collection fields.
-}
randomFields : Int -> Int -> Random.Generator (List ( String, ElmType ))
randomFields count startIndex =
    List.range 0 (count - 1)
        |> List.map
            (\i ->
                randomType 0
                    |> Random.map (\t -> ( Names.fieldName (startIndex + i), t ))
            )
        |> sequenceGenerators


{-| Generate random fields avoiding certain names (for overlapping field scenarios).
-}
randomFieldsAvoiding : Int -> Int -> List String -> Random.Generator (List ( String, ElmType ))
randomFieldsAvoiding count startIndex avoidNames =
    List.range 0 (count - 1)
        |> List.map
            (\i ->
                randomType 0
                    |> Random.map
                        (\t ->
                            let
                                baseName =
                                    Names.fieldName (startIndex + i)

                                safeName =
                                    if List.member baseName avoidNames then
                                        "extra_" ++ baseName

                                    else
                                        baseName
                            in
                            ( safeName, t )
                        )
            )
        |> sequenceGenerators


{-| Generate random fields that may include type references.
-}
randomFieldsWithRefs : Int -> Int -> List TypeRef -> Random.Generator (List ( String, ElmType ))
randomFieldsWithRefs count startIndex refs =
    List.range 0 (count - 1)
        |> List.map
            (\i ->
                randomTypeWithRefs 0 refs
                    |> Random.map (\t -> ( Names.fieldName (startIndex + i), t ))
            )
        |> sequenceGenerators


{-| Generate random union variants with parameters.
-}
randomVariants : Int -> Int -> Random.Generator (List ( String, List ElmType ))
randomVariants count startIndex =
    List.range 0 (count - 1)
        |> List.map
            (\i ->
                Random.int 0 3
                    |> Random.andThen
                        (\paramCount ->
                            randomList paramCount (randomType 0)
                                |> Random.map
                                    (\params ->
                                        ( Names.constructorName (startIndex + i), params )
                                    )
                        )
            )
        |> sequenceGenerators


{-| Generate a random type at a given depth (depth limits nesting).
-}
randomType : Int -> Random.Generator ElmType
randomType depth =
    if depth >= 2 then
        randomAtom

    else
        Random.weighted
            ( 50, 0 )
            -- atom
            [ ( 10, 1 )
            -- maybe
            , ( 10, 2 )
            -- list
            , ( 5, 3 )
            -- set
            , ( 5, 4 )
            -- array
            , ( 5, 5 )
            -- dict
            , ( 5, 6 )
            -- result
            , ( 3, 7 )
            -- tuple2
            , ( 2, 8 )
            -- tuple3
            , ( 2, 9 )
            -- seqDict
            , ( 2, 10 )
            -- seqSet
            , ( 1, 11 )
              -- inline record
            ]
            |> Random.andThen
                (\choice ->
                    case choice of
                        1 ->
                            randomType (depth + 1) |> Random.map TMaybe

                        2 ->
                            randomType (depth + 1) |> Random.map TList

                        3 ->
                            randomComparable |> Random.map TSet

                        4 ->
                            randomType (depth + 1) |> Random.map TArray

                        5 ->
                            Random.map2 TDict randomComparable (randomType (depth + 1))

                        6 ->
                            Random.map2 TResult (randomType (depth + 1)) (randomType (depth + 1))

                        7 ->
                            Random.map2 TTuple2 (randomType (depth + 1)) (randomType (depth + 1))

                        8 ->
                            Random.map3 TTuple3 (randomType (depth + 1)) (randomType (depth + 1)) (randomType (depth + 1))

                        9 ->
                            Random.map2 TSeqDict randomComparable (randomType (depth + 1))

                        10 ->
                            randomComparable |> Random.map TSeqSet

                        11 ->
                            Random.int 2 4
                                |> Random.andThen
                                    (\fieldCount ->
                                        randomFields fieldCount (depth * 100)
                                            |> Random.map TRecord
                                    )

                        _ ->
                            randomAtom
                )


{-| Like randomType but with a chance to reference existing types.
-}
randomTypeWithRefs : Int -> List TypeRef -> Random.Generator ElmType
randomTypeWithRefs depth refs =
    case refs of
        [] ->
            randomType depth

        firstRef :: restRefs ->
            Random.weighted ( 70, False ) [ ( 30, True ) ]
                |> Random.andThen
                    (\useRef ->
                        if useRef then
                            Random.uniform firstRef restRefs
                                |> Random.map TRef

                        else
                            randomType depth
                    )


randomAtom : Random.Generator ElmType
randomAtom =
    Random.uniform TInt
        [ TFloat
        , TBool
        , TChar
        , TString
        , TUnit
        , TOrder
        , TTimePosix
        ]


randomComparable : Random.Generator ComparableType
randomComparable =
    Random.uniform CInt [ CFloat, CChar, CString ]



-- TYPE REF HELPERS


{-| Reference to an extensible record base, for cross-module concrete extensions.
-}
type alias ExtensibleRef =
    { modulePath : List String
    , typeName : String
    , fields : List ( String, ElmType )
    }


{-| Collect type refs from a list of declarations (for cross-referencing).
Only includes concrete types that can be roundtrip tested (no extensible records).
-}
declsToRefs : List String -> List TypeDecl -> List TypeRef
declsToRefs modulePath decls =
    List.filterMap
        (\decl ->
            case decl of
                UnionDecl { name } ->
                    Just { modulePath = modulePath, typeName = name }

                RecordAliasDecl { name } ->
                    Just { modulePath = modulePath, typeName = name }

                ConcreteExtensionDecl { name } ->
                    Just { modulePath = modulePath, typeName = name }

                AliasChainDecl { name } ->
                    Just { modulePath = modulePath, typeName = name }

                ExtensibleRecordDecl _ ->
                    Nothing

                ExtensibleExtensionDecl _ ->
                    Nothing
        )
        decls


{-| Collect extensible record bases from declarations.
-}
declsToExtensibleRefs : List String -> List TypeDecl -> List ExtensibleRef
declsToExtensibleRefs modulePath decls =
    List.filterMap
        (\decl ->
            case decl of
                ExtensibleRecordDecl { name, fields } ->
                    Just { modulePath = modulePath, typeName = name, fields = fields }

                ExtensibleExtensionDecl { name, ownFields } ->
                    Just { modulePath = modulePath, typeName = name, fields = ownFields }

                _ ->
                    Nothing
        )
        decls


priorModuleRefs : List GeneratedModule -> List TypeRef
priorModuleRefs modules =
    List.concatMap (\m -> declsToRefs m.modulePath m.decls) modules


priorExtensibleRefs : List GeneratedModule -> List ExtensibleRef
priorExtensibleRefs modules =
    List.concatMap (\m -> declsToExtensibleRefs m.modulePath m.decls) modules



-- RENDERING


{-| Render a GeneratedModule to an Elm.File.
-}
renderModule : GeneratedModule -> Elm.File
renderModule mod =
    Elm.file mod.modulePath
        (List.concatMap renderDecl mod.decls)


renderDecl : TypeDecl -> List Elm.Declaration
renderDecl decl =
    case decl of
        UnionDecl { name, variants } ->
            [ Elm.customType name
                (List.map
                    (\( ctorName, params ) ->
                        case params of
                            [] ->
                                Elm.variant ctorName

                            _ ->
                                Elm.variantWith ctorName (List.map elmTypeToAnnotation params)
                    )
                    variants
                )
                |> Elm.expose
            ]

        RecordAliasDecl { name, fields } ->
            [ Elm.alias name
                (Type.record (List.map (\( n, t ) -> ( n, elmTypeToAnnotation t )) fields))
                |> Elm.expose
            ]

        ExtensibleRecordDecl { name, extensionVar, fields } ->
            [ Elm.alias name
                (Type.extensible extensionVar
                    (List.map (\( n, t ) -> ( n, elmTypeToAnnotation t )) fields)
                )
                |> Elm.expose
            ]

        ConcreteExtensionDecl { name, baseRef, extraFields } ->
            [ Elm.alias name
                (Type.namedWith baseRef.modulePath
                    baseRef.typeName
                    [ Type.record
                        (List.map (\( n, t ) -> ( n, elmTypeToAnnotation t )) extraFields)
                    ]
                )
                |> Elm.expose
            ]

        AliasChainDecl { name, targetRef } ->
            [ Elm.alias name
                (Type.named targetRef.modulePath targetRef.typeName)
                |> Elm.expose
            ]

        ExtensibleExtensionDecl { name, extensionVar, baseRef, ownFields } ->
            [ Elm.alias name
                (Type.namedWith baseRef.modulePath
                    baseRef.typeName
                    [ Type.extensible extensionVar
                        (List.map (\( n, t ) -> ( n, elmTypeToAnnotation t )) ownFields)
                    ]
                )
                |> Elm.expose
            ]


elmTypeToAnnotation : ElmType -> Type.Annotation
elmTypeToAnnotation elmType =
    case elmType of
        TInt ->
            Type.int

        TFloat ->
            Type.float

        TBool ->
            Type.bool

        TChar ->
            Type.char

        TString ->
            Type.string

        TUnit ->
            Type.unit

        TOrder ->
            Type.named [ "Basics" ] "Order"

        TTimePosix ->
            Type.named [ "Time" ] "Posix"

        TMaybe inner ->
            Type.maybe (elmTypeToAnnotation inner)

        TResult err ok ->
            Type.namedWith [ "Result" ] "Result" [ elmTypeToAnnotation err, elmTypeToAnnotation ok ]

        TList inner ->
            Type.list (elmTypeToAnnotation inner)

        TSet comp ->
            Type.namedWith [ "Set" ] "Set" [ comparableToAnnotation comp ]

        TArray inner ->
            Type.namedWith [ "Array" ] "Array" [ elmTypeToAnnotation inner ]

        TDict key val ->
            Type.namedWith [ "Dict" ] "Dict" [ comparableToAnnotation key, elmTypeToAnnotation val ]

        TSeqDict key val ->
            Type.namedWith [ "SeqDict" ] "SeqDict" [ comparableToAnnotation key, elmTypeToAnnotation val ]

        TSeqSet comp ->
            Type.namedWith [ "SeqSet" ] "SeqSet" [ comparableToAnnotation comp ]

        TTuple2 a b ->
            Type.tuple (elmTypeToAnnotation a) (elmTypeToAnnotation b)

        TTuple3 a b c ->
            Type.triple (elmTypeToAnnotation a) (elmTypeToAnnotation b) (elmTypeToAnnotation c)

        TRecord fields ->
            Type.record (List.map (\( n, t ) -> ( n, elmTypeToAnnotation t )) fields)

        TRef ref ->
            Type.named ref.modulePath ref.typeName


comparableToAnnotation : ComparableType -> Type.Annotation
comparableToAnnotation comp =
    case comp of
        CInt ->
            Type.int

        CFloat ->
            Type.float

        CChar ->
            Type.char

        CString ->
            Type.string



-- SEQUENCE HELPER


sequenceGenerators : List (Random.Generator a) -> Random.Generator (List a)
sequenceGenerators gens =
    case gens of
        [] ->
            Random.constant []

        first :: rest ->
            Random.map2 (::) first (sequenceGenerators rest)


randomList : Int -> Random.Generator a -> Random.Generator (List a)
randomList n gen =
    if n <= 0 then
        Random.constant []

    else
        Random.map2 (::) gen (randomList (n - 1) gen)
