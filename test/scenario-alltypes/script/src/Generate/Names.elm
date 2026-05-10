module Generate.Names exposing
    ( constructorName
    , fieldName
    , moduleName
    , typeName
    )

{-| Deterministic name pools for generated identifiers.
-}


moduleName : Int -> String
moduleName index =
    "Mod" ++ String.fromInt index


typeName : Int -> Int -> String
typeName moduleIndex typeIndex =
    let
        names =
            [ "Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta" ]

        base =
            List.drop (modBy (List.length names) typeIndex) names
                |> List.head
                |> Maybe.withDefault "Type"
    in
    base ++ String.fromInt moduleIndex ++ "_" ++ String.fromInt typeIndex


constructorName : Int -> String
constructorName index =
    let
        prefixes =
            [ "Ctor", "Tag", "Variant", "Case", "Option", "Kind" ]

        prefix =
            List.drop (modBy (List.length prefixes) index) prefixes
                |> List.head
                |> Maybe.withDefault "Ctor"
    in
    prefix ++ String.fromInt index


fieldName : Int -> String
fieldName index =
    let
        names =
            [ "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa" ]

        base =
            List.drop (modBy (List.length names) index) names
                |> List.head
                |> Maybe.withDefault "field"

        suffix =
            index // List.length names
    in
    if suffix == 0 then
        base

    else
        base ++ String.fromInt suffix
