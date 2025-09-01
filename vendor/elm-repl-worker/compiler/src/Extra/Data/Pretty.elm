module Extra.Data.Pretty exposing
    ( Doc
    , Renderer
    , Styles
    , align
    , blackS
    , blue
    , cat
    , cyan
    , cyanS
    , dullcyan
    , dullred
    , dullyellow
    , dullyellowS
    , empty
    , fillSep
    , green
    , greenS
    , hang
    , hcat
    , hsep
    , indent
    , pretty
    , red
    , redS
    , renderPretty
    , sep
    , text
    , underline
    , vcat
    , yellow
    )

-- Substitute for Text.PrettyPrint.ANSI.Leijen

import Extra.Type.List exposing (TList)
import Pretty as P
import Pretty.Renderer as PR


type alias Styles t =
    { underline : t
    , red : t
    , green : t
    , cyan : t
    , blue : t
    , black : t
    , yellow : t
    , dullcyan : t
    , dullred : t
    , dullyellow : t
    }


type alias Doc t =
    P.Doc (Styles t -> t)


type alias Renderer t a b =
    PR.Renderer (Styles t -> t) a b


align : P.Doc t -> P.Doc t
align =
    P.align


blackS : String -> Doc t
blackS str =
    P.taggedString str .black


blue : String -> Doc t
blue str =
    P.taggedString str .blue


cat : TList (Doc t) -> Doc t
cat =
    P.group << vcat


cyan : Doc t -> Doc t
cyan =
    P.setTag .cyan


cyanS : String -> Doc t
cyanS str =
    P.taggedString str .cyan


dullcyan : Doc t -> Doc t
dullcyan =
    P.setTag .dullcyan


dullred : Doc t -> Doc t
dullred =
    P.setTag .dullred


dullyellow : Doc t -> Doc t
dullyellow =
    P.setTag .dullyellow


dullyellowS : String -> Doc t
dullyellowS str =
    P.taggedString str .dullyellow


empty : Doc t
empty =
    P.empty


fillSep : TList (Doc t) -> Doc t
fillSep =
    P.softlines


green : Doc t -> Doc t
green =
    P.setTag .green


greenS : String -> Doc t
greenS str =
    P.taggedString str .green


hang : Int -> P.Doc t -> P.Doc t
hang =
    P.hang


hcat : TList (Doc t) -> Doc t
hcat docs =
    case docs of
        [] ->
            empty

        [ doc ] ->
            doc

        doc :: docs_ ->
            P.append doc (hcat docs_)



-- join <+> --> P.words


hsep : TList (Doc t) -> Doc t
hsep =
    P.words


indent : Int -> Doc t -> Doc t
indent n doc =
    P.indent n doc


red : Doc t -> Doc t
red =
    P.setTag .red


redS : String -> Doc t
redS str =
    P.taggedString str .red


sep : TList (P.Doc t) -> P.Doc t
sep =
    P.group << vsep


text : String -> P.Doc t
text =
    P.string


underline : String -> Doc t
underline str =
    P.taggedString str .underline



-- <$> line --> P.line
-- <$$> linebreak --> P.tightline
--
-- vsep = fold (<$>) --> P.lines
-- vcat = fold (<$$>)


vcat : TList (Doc t) -> Doc t
vcat =
    P.join P.tightline


vsep : TList (P.Doc t) -> P.Doc t
vsep =
    P.lines


yellow : Doc t -> Doc t
yellow =
    P.setTag .yellow



-- RENDERING


pretty : Int -> Doc t -> String
pretty n doc =
    P.pretty n doc


renderPretty : Int -> PR.Renderer (Styles t -> t) a b -> Doc t -> b
renderPretty =
    PR.pretty
