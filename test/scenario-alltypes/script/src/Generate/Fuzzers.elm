module Generate.Fuzzers exposing (fuzzerForDecl, fuzzerForType)

{-| Maps ElmType to Fuzz.Fuzzer Elm.Expression for code generation.
-}

import Elm
import Elm.Annotation as Type
import Gen.Array
import Gen.Dict
import Gen.Fuzz
import Gen.Set
import Gen.Time
import Generate.Types exposing (ComparableType(..), ElmType(..), TypeDecl(..), TypeRef, elmTypeToAnnotation)
import Generate.Wire3


{-| Generate a fuzzer expression for an ElmType.
-}
fuzzerForType : ElmType -> Elm.Expression
fuzzerForType elmType =
    case elmType of
        TInt ->
            Gen.Fuzz.int

        TFloat ->
            Gen.Fuzz.niceFloat

        TBool ->
            Gen.Fuzz.bool

        TChar ->
            Gen.Fuzz.char

        TString ->
            Gen.Fuzz.string

        TUnit ->
            Gen.Fuzz.unit

        TOrder ->
            Gen.Fuzz.order

        TTimePosix ->
            Gen.Fuzz.call_.map
                (Elm.value
                    { importFrom = [ "Time" ]
                    , name = "millisToPosix"
                    , annotation = Nothing
                    }
                )
                Gen.Fuzz.int

        TMaybe inner ->
            Gen.Fuzz.maybe (fuzzerForType inner)

        TResult err ok ->
            Gen.Fuzz.result (fuzzerForType err) (fuzzerForType ok)

        TList inner ->
            Gen.Fuzz.list (fuzzerForType inner)

        TSet comp ->
            Gen.Fuzz.call_.map
                (Elm.value
                    { importFrom = [ "Set" ]
                    , name = "fromList"
                    , annotation = Nothing
                    }
                )
                (Gen.Fuzz.list (fuzzerForComparable comp))

        TArray inner ->
            Gen.Fuzz.call_.map
                (Elm.value
                    { importFrom = [ "Array" ]
                    , name = "fromList"
                    , annotation = Nothing
                    }
                )
                (Gen.Fuzz.list (fuzzerForType inner))

        TDict key val ->
            Gen.Fuzz.call_.map
                (Elm.value
                    { importFrom = [ "Dict" ]
                    , name = "fromList"
                    , annotation = Nothing
                    }
                )
                (Gen.Fuzz.list (Gen.Fuzz.pair (fuzzerForComparable key) (fuzzerForType val)))

        TSeqDict key val ->
            Gen.Fuzz.call_.map
                (Elm.value
                    { importFrom = [ "SeqDict" ]
                    , name = "fromList"
                    , annotation = Nothing
                    }
                )
                (Gen.Fuzz.list (Gen.Fuzz.pair (fuzzerForComparable key) (fuzzerForType val)))

        TSeqSet comp ->
            Gen.Fuzz.call_.map
                (Elm.value
                    { importFrom = [ "SeqSet" ]
                    , name = "fromList"
                    , annotation = Nothing
                    }
                )
                (Gen.Fuzz.list (fuzzerForComparable comp))

        TTuple2 a b ->
            Gen.Fuzz.pair (fuzzerForType a) (fuzzerForType b)

        TTuple3 a b c ->
            Gen.Fuzz.triple (fuzzerForType a) (fuzzerForType b) (fuzzerForType c)

        TRecord fields ->
            fuzzerForRecord fields

        TRef ref ->
            fuzzerRef ref


fuzzerForComparable : ComparableType -> Elm.Expression
fuzzerForComparable comp =
    case comp of
        CInt ->
            Gen.Fuzz.int

        CFloat ->
            Gen.Fuzz.niceFloat

        CChar ->
            Gen.Fuzz.char

        CString ->
            Gen.Fuzz.string


{-| Generate a fuzzer for a record type using andMap chaining:
Fuzz.constant (\p0 p1 -> { f1 = p0, f2 = p1 }) |> andMap fuzz1 |> andMap fuzz2
-}
fuzzerForRecord : List ( String, ElmType ) -> Elm.Expression
fuzzerForRecord fields =
    let
        sortedFields =
            List.sortBy Tuple.first fields

        -- Use p0, p1, p2... as param names to avoid clashing with field names
        -- which elm-codegen might interpret as type variables
        paramNames =
            List.indexedMap (\i _ -> "p" ++ String.fromInt i) sortedFields

        constructorFn =
            Elm.function
                (List.map (\pName -> ( pName, Nothing )) paramNames)
                (\args ->
                    Elm.record
                        (List.map2 (\( name, _ ) arg -> ( name, arg )) sortedFields args)
                )
    in
    List.foldl
        (\( _, fieldType ) accFuzzer ->
            Gen.Fuzz.call_.andMap (fuzzerForType fieldType) accFuzzer
        )
        (Gen.Fuzz.constant constructorFn)
        sortedFields


{-| Reference to a fuzzer defined elsewhere (for cross-module type refs).
The fuzzer is expected to be named fuzzer\_TypeName in the module.
-}
fuzzerRef : TypeRef -> Elm.Expression
fuzzerRef ref =
    Elm.value
        { importFrom = ref.modulePath
        , name = "fuzzer_" ++ ref.typeName
        , annotation = Nothing
        }


{-| Build a fuzzer for a union variant with the given constructor name and params.
-}
variantFuzzer : String -> List ElmType -> Elm.Expression
variantFuzzer ctorName params =
    let
        ctor =
            Elm.value
                { importFrom = []
                , name = ctorName
                , annotation = Nothing
                }
    in
    case params of
        [] ->
            Gen.Fuzz.constant ctor

        _ ->
            -- Use andMap chaining for all param counts
            List.foldl
                (\paramType acc -> Gen.Fuzz.call_.andMap (fuzzerForType paramType) acc)
                (Gen.Fuzz.constant ctor)
                params


{-| Build a Fuzz.Fuzzer annotation for a given type annotation.
-}
fuzzerAnnotation : Type.Annotation -> Type.Annotation
fuzzerAnnotation innerType =
    Type.namedWith [ "Fuzz" ] "Fuzzer" [ innerType ]


{-| Deduplicate fields by name, keeping the last occurrence.
-}
deduplicateFields : List ( String, ElmType ) -> List ( String, ElmType )
deduplicateFields fields =
    List.foldl
        (\( name, t ) acc ->
            if List.any (\( n, _ ) -> n == name) acc then
                List.map
                    (\( n, existing ) ->
                        if n == name then
                            ( n, t )

                        else
                            ( n, existing )
                    )
                    acc

            else
                acc ++ [ ( name, t ) ]
        )
        []
        fields


{-| Generate a fuzzer declaration for a TypeDecl.
Returns Nothing for extensible record types (can't be instantiated).
-}
fuzzerForDecl : List String -> TypeDecl -> Maybe ( Elm.Declaration, String )
fuzzerForDecl modulePath decl =
    case decl of
        UnionDecl { name, variants } ->
            let
                typeAnnotation =
                    Type.named modulePath name

                variantFuzzers =
                    List.map
                        (\( ctorName, params ) -> variantFuzzer ctorName params)
                        variants

                fuzzerExpr =
                    Gen.Fuzz.oneOf variantFuzzers
                        |> Elm.withType (fuzzerAnnotation typeAnnotation)

                fuzzerDecl =
                    Elm.declaration ("fuzzer_" ++ name) fuzzerExpr
                        |> Elm.expose
            in
            Just ( fuzzerDecl, name )

        RecordAliasDecl { name, fields } ->
            let
                typeAnnotation =
                    Type.named modulePath name

                fuzzerExpr =
                    fuzzerForRecord fields
                        |> Elm.withType (fuzzerAnnotation typeAnnotation)

                fuzzerDecl =
                    Elm.declaration ("fuzzer_" ++ name) fuzzerExpr
                        |> Elm.expose
            in
            Just ( fuzzerDecl, name )

        ConcreteExtensionDecl { name, baseFields, extraFields } ->
            let
                typeAnnotation =
                    Type.named modulePath name

                -- Deduplicate fields: extra fields override base fields with same name
                allFields =
                    deduplicateFields (baseFields ++ extraFields)

                fuzzerExpr =
                    fuzzerForRecord allFields
                        |> Elm.withType (fuzzerAnnotation typeAnnotation)

                fuzzerDecl =
                    Elm.declaration ("fuzzer_" ++ name) fuzzerExpr
                        |> Elm.expose
            in
            Just ( fuzzerDecl, name )

        AliasChainDecl { name, targetRef } ->
            let
                typeAnnotation =
                    Type.named modulePath name

                fuzzerExpr =
                    fuzzerRef targetRef
                        |> Elm.withType (fuzzerAnnotation typeAnnotation)

                fuzzerDecl =
                    Elm.declaration ("fuzzer_" ++ name) fuzzerExpr
                        |> Elm.expose
            in
            Just ( fuzzerDecl, name )

        ExtensibleRecordDecl _ ->
            -- Can't instantiate extensible records directly
            Nothing

        ExtensibleExtensionDecl _ ->
            -- Still extensible, can't instantiate directly
            Nothing
