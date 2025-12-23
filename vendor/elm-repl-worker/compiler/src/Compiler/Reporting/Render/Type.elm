module Compiler.Reporting.Render.Type exposing
    ( Context(..)
    , apply
    , canToDoc
    , lambda
    , record
    , srcToDoc
    , tuple
    , vrecord
    , vrecordSnippet
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (d)
import Compiler.Reporting.Render.Type.Localizer as L
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe



-- TO DOC


type Context
    = None
    | Func
    | App


lambda : Context -> D.Doc -> D.Doc -> TList D.Doc -> D.Doc
lambda context arg1 arg2 args =
    let
        lambdaDoc =
            D.align <| D.sep (arg1 :: MList.map (\s -> D.hsep [ D.fromChars "->", s ]) (arg2 :: args))
    in
    case context of
        None ->
            lambdaDoc

        Func ->
            D.hcat [ D.fromChars "(", lambdaDoc, D.fromChars ")" ]

        App ->
            D.hcat [ D.fromChars "(", lambdaDoc, D.fromChars ")" ]


apply : Context -> D.Doc -> TList D.Doc -> D.Doc
apply context name args =
    case args of
        [] ->
            name

        _ :: _ ->
            let
                applyDoc =
                    D.hang 4 (D.sep (name :: args))
            in
            case context of
                App ->
                    D.cat [ D.fromChars "(", applyDoc, D.fromChars ")" ]

                Func ->
                    applyDoc

                None ->
                    applyDoc


tuple : D.Doc -> D.Doc -> TList D.Doc -> D.Doc
tuple a b cs =
    let
        entries =
            zipWithHelp "(" "," (a :: b :: cs)
    in
    D.align <| D.sep [ D.cat entries, D.fromChars ")" ]


record : TList ( D.Doc, D.Doc ) -> Maybe D.Doc -> D.Doc
record entries maybeExt =
    case ( MList.map entryToDoc entries, maybeExt ) of
        ( [], Nothing ) ->
            D.fromChars "{}"

        ( fields, Nothing ) ->
            D.align <|
                D.sep <|
                    [ D.cat (zipWithHelp "{" "," fields)
                    , D.fromChars "}"
                    ]

        ( fields, Just ext ) ->
            D.align <|
                D.sep <|
                    [ D.hang 4 <|
                        D.sep <|
                            [ D.hsep [ D.fromChars "{", ext ]
                            , D.cat (zipWithHelp "|" "," fields)
                            ]
                    , D.fromChars "}"
                    ]


entryToDoc : ( D.Doc, D.Doc ) -> D.Doc
entryToDoc ( fieldName, fieldType ) =
    D.hang 4 (D.sep [ D.hsep [ fieldName, D.fromChars ":" ], fieldType ])


vrecordSnippet : ( D.Doc, D.Doc ) -> TList ( D.Doc, D.Doc ) -> D.Doc
vrecordSnippet entry entries =
    let
        field =
            D.hsep [ d "{", entryToDoc entry ]

        fields =
            zipWithHelp "," "," (MList.map entryToDoc entries ++ [ d "..." ])
    in
    D.vcat (field :: fields ++ [ d "}" ])


vrecord : TList ( D.Doc, D.Doc ) -> Maybe D.Doc -> D.Doc
vrecord entries maybeExt =
    case ( MList.map entryToDoc entries, maybeExt ) of
        ( [], Nothing ) ->
            d "{}"

        ( fields, Nothing ) ->
            D.vcat <|
                zipWithHelp "{" "," fields
                    ++ [ d "}" ]

        ( fields, Just ext ) ->
            D.vcat
                [ D.hang 4 <|
                    D.vcat <|
                        [ D.hsep [ d "{", ext ]
                        , D.cat (zipWithHelp "|" "," fields)
                        ]
                , d "}"
                ]


zipWithHelp : String -> String -> TList D.Doc -> TList D.Doc
zipWithHelp headPrefix tailPrefix docs =
    case docs of
        [] ->
            []

        head :: tail ->
            D.hsep [ D.fromChars headPrefix, head ]
                :: MList.map (\t -> D.hsep [ D.fromChars tailPrefix, t ]) tail



-- SOURCE TYPE TO DOC


srcToDoc : (Name.Name -> D.Doc) -> Context -> Src.Type -> D.Doc
srcToDoc tagger context (A.At _ tipe) =
    case tipe of
        Src.TLambda arg1 result ->
            let
                ( arg2, rest ) =
                    collectSrcArgs result
            in
            lambda context
                (srcToDoc tagger Func arg1)
                (srcToDoc tagger Func arg2)
                (MList.map (srcToDoc tagger Func) rest)

        Src.TVar name ->
            tagger <| name

        Src.TType _ name args ->
            apply context
                (tagger <| name)
                (MList.map (srcToDoc tagger App) args)

        Src.TTypeQual _ home name args ->
            apply context
                (D.hcat <| MList.map tagger [ home, ".", name ])
                (MList.map (srcToDoc tagger App) args)

        Src.TRecord fields ext ->
            record
                (MList.map (srcFieldToDocs tagger) fields)
                (Maybe.map (tagger << A.toValue) ext)

        Src.TUnit ->
            tagger "()"

        Src.TTuple a b cs ->
            tuple
                (srcToDoc tagger None a)
                (srcToDoc tagger None b)
                (MList.map (srcToDoc tagger None) cs)


srcFieldToDocs : (Name.Name -> D.Doc) -> ( A.Located Name.Name, Src.Type ) -> ( D.Doc, D.Doc )
srcFieldToDocs tagger ( A.At _ fieldName, fieldType ) =
    ( tagger <| fieldName
    , srcToDoc tagger None fieldType
    )


collectSrcArgs : Src.Type -> ( Src.Type, TList Src.Type )
collectSrcArgs tipe =
    case tipe of
        A.At _ (Src.TLambda a result) ->
            let
                ( b, cs ) =
                    collectSrcArgs result
            in
            ( a, b :: cs )

        _ ->
            ( tipe, [] )



-- CANONICAL TYPE TO DOC


canToDoc : L.Localizer -> Context -> Can.Type -> D.Doc
canToDoc localizer context tipe =
    case tipe of
        Can.TLambda arg1 result ->
            let
                ( arg2, rest ) =
                    collectArgs result
            in
            lambda context
                (canToDoc localizer Func arg1)
                (canToDoc localizer Func arg2)
                (MList.map (canToDoc localizer Func) rest)

        Can.TVar name ->
            D.fromName name

        Can.TType home name args ->
            apply context
                (L.toDoc localizer home name)
                (MList.map (canToDoc localizer App) args)

        Can.TRecord fields ext ->
            record
                (MList.map (canFieldToDoc localizer) (Can.fieldsToList fields))
                (Maybe.map D.fromName ext)

        Can.TUnit ->
            d "()"

        Can.TTuple a b maybeC ->
            tuple
                (canToDoc localizer None a)
                (canToDoc localizer None b)
                (MList.map (canToDoc localizer None) (MMaybe.maybeToList maybeC))

        Can.TAlias home name args _ ->
            apply context
                (L.toDoc localizer home name)
                (MList.map (canToDoc localizer App << Tuple.second) args)


canFieldToDoc : L.Localizer -> ( Name.Name, Can.Type ) -> ( D.Doc, D.Doc )
canFieldToDoc localizer ( name, tipe ) =
    ( D.fromName name
    , canToDoc localizer None tipe
    )


collectArgs : Can.Type -> ( Can.Type, TList Can.Type )
collectArgs tipe =
    case tipe of
        Can.TLambda a rest ->
            let
                ( b, cs ) =
                    collectArgs rest
            in
            ( a, b :: cs )

        _ ->
            ( tipe, [] )
