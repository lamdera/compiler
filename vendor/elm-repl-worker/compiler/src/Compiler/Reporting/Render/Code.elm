module Compiler.Reporting.Render.Code exposing
    ( Next(..)
    , Source
    , nextLineStartsWithCloseCurly
    , nextLineStartsWithKeyword
    , toPair
    , toSnippet
    , toSource
    , whatIsNext
    )

import Compiler.Parse.Primitives as P
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe
import Extra.Type.Set as Set
import Extra.Type.String as SE


type Source
    = Source (TList ( Int, String ))


toSource : String -> Source
toSource source =
    Source <| MList.indexedFrom 1 <| String.lines source



-- CODE FORMATTING


toSnippet : Source -> A.Region -> Maybe A.Region -> ( D.Doc, D.Doc ) -> D.Doc
toSnippet source region highlight ( preHint, postHint ) =
    D.vcat
        [ preHint
        , D.fromChars ""
        , render source region highlight
        , postHint
        ]


toPair : Source -> A.Region -> A.Region -> ( D.Doc, D.Doc ) -> ( D.Doc, D.Doc, D.Doc ) -> D.Doc
toPair source r1 r2 ( oneStart, oneEnd ) ( twoStart, twoMiddle, twoEnd ) =
    case renderPair source r1 r2 of
        OneLine codeDocs ->
            D.vcat
                [ oneStart
                , D.fromChars ""
                , codeDocs
                , oneEnd
                ]

        TwoChunks code1 code2 ->
            D.vcat
                [ twoStart
                , D.fromChars ""
                , code1
                , twoMiddle
                , D.fromChars ""
                , code2
                , twoEnd
                ]



-- RENDER SNIPPET


render : Source -> A.Region -> Maybe A.Region -> D.Doc
render (Source sourceLines) ((A.Region (A.Position startLine _) (A.Position endLine _)) as region) maybeSubRegion =
    let
        relevantLines =
            sourceLines
                |> MList.drop (startLine - 1)
                |> MList.take (1 + endLine - startLine)

        width =
            String.length (String.fromInt (Tuple.first (MList.last relevantLines)))

        smallerRegion =
            Maybe.withDefault region maybeSubRegion
    in
    case makeUnderline width endLine smallerRegion of
        Nothing ->
            drawLines True width smallerRegion relevantLines (D.fromChars "")

        Just underline ->
            drawLines False width smallerRegion relevantLines underline


makeUnderline : Int -> Int -> A.Region -> Maybe D.Doc
makeUnderline width realEndLine (A.Region (A.Position start c1) (A.Position end c2)) =
    if start /= end || end < realEndLine then
        Nothing

    else
        let
            spaces =
                String.repeat (c1 + width + 1) " "

            zigzag =
                String.repeat (max 1 (c2 - c1)) "^"
        in
        Just (D.hcat [ D.fromChars spaces, D.redS zigzag ])


drawLines : Bool -> Int -> A.Region -> TList ( Int, String ) -> D.Doc -> D.Doc
drawLines addZigZag width (A.Region (A.Position startLine _) (A.Position endLine _)) sourceLines finalLine =
    D.vcat <|
        MList.map (drawLine addZigZag width startLine endLine) sourceLines
            ++ [ finalLine ]


drawLine : Bool -> Int -> Int -> Int -> ( Int, String ) -> D.Doc
drawLine addZigZag width startLine endLine ( n, line ) =
    addLineNumber addZigZag width startLine endLine n (D.fromChars line)


addLineNumber : Bool -> Int -> Int -> Int -> Int -> D.Doc -> D.Doc
addLineNumber addZigZag width start end n line =
    let
        number =
            String.fromInt n

        lineNumber =
            String.repeat (width - String.length number) " " ++ number ++ "|"

        spacer =
            if addZigZag && start <= n && n <= end then
                D.redS ">"

            else
                D.fromChars " "
    in
    D.hcat [ D.fromChars lineNumber, spacer, line ]



-- RENDER PAIR


type CodePair
    = OneLine D.Doc
    | TwoChunks D.Doc D.Doc


renderPair : Source -> A.Region -> A.Region -> CodePair
renderPair ((Source sourceLines) as source) region1 region2 =
    let
        (A.Region (A.Position startRow1 startCol1) (A.Position endRow1 endCol1)) =
            region1

        (A.Region (A.Position startRow2 startCol2) (A.Position endRow2 endCol2)) =
            region2
    in
    if startRow1 == endRow1 && endRow1 == startRow2 && startRow2 == endRow2 then
        let
            lineNumber =
                String.fromInt startRow1

            spaces1 =
                String.repeat (startCol1 + String.length lineNumber + 1) " "

            zigzag1 =
                String.repeat (endCol1 - startCol1) "^"

            spaces2 =
                String.repeat (startCol2 - endCol1) " "

            zigzag2 =
                String.repeat (endCol2 - startCol2) "^"

            line =
                MList.lookup startRow1 sourceLines
                    |> Maybe.withDefault ""
        in
        OneLine <|
            D.vcat
                [ D.hcat [ D.fromChars lineNumber, D.fromChars "| ", D.fromChars line ]
                , D.hcat
                    [ D.fromChars spaces1
                    , D.redS zigzag1
                    , D.fromChars spaces2
                    , D.redS zigzag2
                    ]
                ]

    else
        TwoChunks
            (render source region1 Nothing)
            (render source region2 Nothing)



-- WHAT IS NEXT?


type Next
    = Keyword String
    | Operator String
    | Close String Char
    | Upper Char String
    | Lower Char String
    | Other (Maybe Char)


whatIsNext : Source -> P.Row -> P.Col -> Next
whatIsNext (Source sourceLines) row col =
    case MList.lookup row sourceLines of
        Nothing ->
            Other Nothing

        Just line ->
            case String.uncons (String.dropLeft (col - 1) line) of
                Nothing ->
                    Other Nothing

                Just ( c, cs ) ->
                    if Var.isUpper c then
                        Upper c (SE.takeWhile Var.isInner cs)

                    else if Var.isLower c then
                        detectKeywords c cs

                    else if Symbol.isBinopChar c then
                        Operator (String.cons c <| SE.takeWhile Symbol.isBinopChar cs)

                    else if c == ')' then
                        Close "parenthesis" ')'

                    else if c == ']' then
                        Close "square bracket" ']'

                    else if c == '}' then
                        Close "curly brace" '}'

                    else
                        Other (Just c)


detectKeywords : Char -> String -> Next
detectKeywords c rest =
    let
        cs =
            SE.takeWhile Var.isInner rest

        name =
            String.cons c cs
    in
    if Set.member name Var.reservedWords then
        Keyword name

    else
        Lower c name


startsWithKeyword : String -> String -> Bool
startsWithKeyword restOfLine keyword =
    String.startsWith keyword restOfLine
        && (case String.uncons <| String.dropLeft (String.length keyword) restOfLine of
                Nothing ->
                    True

                Just ( c, _ ) ->
                    not (Var.isInner c)
           )


nextLineStartsWithKeyword : String -> Source -> P.Row -> Maybe ( P.Row, P.Col )
nextLineStartsWithKeyword keyword (Source sourceLines) row =
    MMaybe.bind (MList.lookup (row + 1) sourceLines) <|
        \line ->
            if startsWithKeyword (SE.dropWhile ((==) ' ') line) keyword then
                Just ( row + 1, 1 + String.length (SE.takeWhile ((==) ' ') line) )

            else
                Nothing


nextLineStartsWithCloseCurly : Source -> P.Row -> Maybe ( P.Row, P.Col )
nextLineStartsWithCloseCurly (Source sourceLines) row =
    MMaybe.bind (MList.lookup (row + 1) sourceLines) <|
        \line ->
            case String.uncons <| SE.dropWhile ((==) ' ') line of
                Just ( '}', _ ) ->
                    Just ( row + 1, 1 + String.length (SE.takeWhile ((==) ' ') line) )

                _ ->
                    Nothing
