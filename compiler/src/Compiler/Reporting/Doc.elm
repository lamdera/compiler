{- MANUALLY FORMATTED -}
module Compiler.Reporting.Doc exposing
  ( Doc
  --, (<+>), (<>)
  , align, cat, empty{-, fill-}, fillSep, hang
  , hcat, hsep, indent, sep, vcat
  , red, redS, cyan, cyanS{-, magenta-}, green, greenS, blue, blackS, yellow
  , dullred, dullcyan, dullyellow, dullyellowS
  --
  , fromChars
  , fromName
  , fromVersion
  , fromPackage
  , fromInt
  --
  --, toAnsi
  , toString
  , toLine
  --
  --, encode
  --
  , stack
  , reflow
  , commaSep
  --
  , toSimpleNote
  , toFancyNote
  , toSimpleHint
  , toFancyHint
  --
  , link
  , fancyLink
  , reflowLink
  , makeLink
  , makeNakedLink
  --
  , args
  --, moreArgs
  , ordinal
  , intToOrdinal
  , cycle
  --
  , d, da
  , fromPath, toClient
  )


import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Elm.Error as Client
import Extra.Data.Pretty as P
import Extra.System.Dir as Dir
import Extra.Type.List as MList exposing (TList)



-- EXPORTS FROM EXTRA.DATA.PRETTY


type alias Doc =
  P.Doc Client.Style


align : Doc -> Doc
align =
  P.align


blackS : String -> Doc
blackS =
  P.blackS


blue : String -> Doc
blue =
  P.blue


cat : TList Doc -> Doc
cat =
  P.cat


cyan : Doc -> Doc
cyan =
  P.cyan


cyanS : String -> Doc
cyanS =
  P.cyanS


dullcyan : Doc -> Doc
dullcyan =
  P.dullcyan


dullred : Doc -> Doc
dullred =
  P.dullred


dullyellow : Doc -> Doc
dullyellow =
  P.dullyellow


dullyellowS : String -> Doc
dullyellowS =
  P.dullyellowS


empty : Doc
empty =
  P.empty


fillSep : TList Doc -> Doc
fillSep =
  P.fillSep


green : Doc -> Doc
green =
  P.green


greenS : String -> Doc
greenS =
  P.greenS


hang : Int -> Doc -> Doc
hang =
  P.hang


hcat : TList Doc -> Doc
hcat =
  P.hcat


da : TList Doc -> Doc
da {- abbreviated alias -} =
  hcat


hsep : TList Doc -> Doc
hsep =
  P.hsep


indent : Int -> Doc -> Doc
indent =
  P.indent


red : Doc -> Doc
red =
  P.red


redS : String -> Doc
redS =
  P.redS


sep : TList Doc -> Doc
sep =
  P.sep


vcat : TList Doc -> Doc
vcat =
  P.vcat


yellow : Doc -> Doc
yellow =
  P.yellow



-- FROM


fromChars : String -> Doc
fromChars =
  P.text


d : String -> Doc
d {- abbreviated alias -} =
  fromChars


fromName : Name.Name -> Doc
fromName name =
  P.text name


fromVersion : V.Version -> Doc
fromVersion vsn =
  P.text (V.toChars vsn)


fromPackage : Pkg.Name -> Doc
fromPackage pkg =
  P.text (Pkg.toChars pkg)


fromPath : Dir.FilePath -> Doc
fromPath path =
  P.text (Dir.toString path)


fromInt : Int -> Doc
fromInt n =
  P.text (String.fromInt n)



-- TO STRING


toString : Doc -> String
toString doc =
  P.pretty 80 doc


toLine : Doc -> String
toLine doc =
  P.pretty (2 ^ 40) doc



-- TO CHUNKS


styles : P.Styles Client.Style
styles =
  { underline = Client.Style False True Nothing
  , red = Client.Style False False (Just Client.RED)
  , green = Client.Style False False (Just Client.GREEN)
  , cyan = Client.Style False False (Just Client.CYAN)
  , blue = Client.Style False False (Just Client.BLUE)
  , black = Client.Style False False (Just Client.BLACK)
  , yellow = Client.Style False False (Just Client.YELLOW)
  , dullcyan = Client.Style True False (Just Client.Cyan)
  , dullred = Client.Style True False (Just Client.Red)
  , dullyellow = Client.Style True False (Just Client.Yellow)
  }


toClient : Doc -> TList Client.Chunk
toClient doc =
  P.renderPretty 80 renderer doc


renderer : P.Renderer Client.Style (TList Client.Chunk) (TList Client.Chunk)
renderer =
  { init = rendererInit
  , tagged = rendererTagged
  , untagged = rendererUntagged
  , newline = rendererNewline
  , outer = rendererOuter
  }


rendererInit : TList Client.Chunk
rendererInit =
  []


rendererTagged : (P.Styles Client.Style -> Client.Style) -> String -> TList Client.Chunk -> TList Client.Chunk
rendererTagged tagger text chunks =
  Client.Styled (tagger styles) text :: chunks


rendererUntagged : String -> TList Client.Chunk -> TList Client.Chunk
rendererUntagged text chunks =
  Client.Unstyled text :: chunks


rendererNewline : TList Client.Chunk -> TList Client.Chunk
rendererNewline chunks =
  Client.Unstyled "\n" :: chunks


rendererOuter : TList Client.Chunk -> TList Client.Chunk
rendererOuter chunks =
  MList.reverse chunks



-- FORMATTING


stack : TList Doc -> Doc
stack docs =
  P.vcat (MList.intersperse (P.text "") docs)


reflow : String -> Doc
reflow paragraph =
  P.fillSep (MList.map P.text (String.words paragraph))


commaSep : Doc -> (a -> Doc) -> TList a -> TList Doc
commaSep conjunction addStyle names =
  case names of
    [name] ->
      [ addStyle name ]

    [name1, name2] ->
      [ addStyle name1, conjunction, addStyle name2 ]

    _ ->
      MList.map (\name -> hcat [ addStyle name, fromChars "," ]) (MList.init names)
      ++
      [ conjunction
      , addStyle (MList.last names)
      ]



-- NOTES


toSimpleNote : String -> Doc
toSimpleNote message =
  toFancyNote (MList.map P.text (String.words message))


toFancyNote : TList Doc -> Doc
toFancyNote chunks =
  P.fillSep (P.hcat [ P.underline "Note", P.text ":" ] :: chunks)



-- HINTS


toSimpleHint : String -> Doc
toSimpleHint message =
  toFancyHint (MList.map P.text (String.words message))


toFancyHint : TList Doc -> Doc
toFancyHint chunks =
  P.fillSep (P.hcat [ P.underline "Hint", P.text ":" ] :: chunks)



-- LINKS


link : String -> String -> String -> String -> Doc
link word before fileName after =
  P.fillSep <|
    P.hcat [ P.underline word, P.text ":" ]
    :: MList.map P.text (String.words before)
    ++ P.text (makeLink fileName)
    :: MList.map P.text (String.words after)


fancyLink : String -> TList Doc -> String -> TList Doc -> Doc
fancyLink word before fileName after =
  P.fillSep <|
    P.hcat [ P.underline word, P.text ":" ] :: before ++ P.text (makeLink fileName) :: after


makeLink : String -> String
makeLink fileName =
  "<https://elm-lang.org/" ++ V.toChars V.compiler ++ "/" ++ fileName ++ ">"


makeNakedLink : String -> String
makeNakedLink fileName =
  "https://elm-lang.org/" ++ V.toChars V.compiler ++ "/" ++ fileName


reflowLink : String -> String -> String -> Doc
reflowLink before fileName after =
  P.fillSep <|
    MList.map P.text (String.words before)
    ++ P.text (makeLink fileName)
    :: MList.map P.text (String.words after)



-- HELPERS


args : Int -> String
args n =
  String.fromInt n ++ (if n == 1 then " argument" else " arguments")


ordinal : Index.ZeroBased -> String
ordinal index =
  intToOrdinal (Index.toHuman index)


intToOrdinal : Int -> String
intToOrdinal number =
  let
    remainder10 =
      modBy 10 number

    remainder100 =
      modBy 100 number

    ending =
      if MList.elem remainder100 [ 11, 12, 13 ] then "th"
      else if remainder10 == 1                  then "st"
      else if remainder10 == 2                  then "nd"
      else if remainder10 == 3                  then "rd"
      else                                           "th"
  in
  String.fromInt number ++ ending


cycle : Int -> Name.Name -> TList Name.Name -> Doc
cycle indent_ name names =
  let
    toLn n = hcat [ cycleLn, dullyellowS n ]
  in
  P.indent indent_ <| P.vcat <|
    cycleTop :: MList.intersperse cycleMid (toLn name :: MList.map toLn names) ++ [ cycleEnd ]


cycleTop : Doc
cycleTop = P.text "┌─────┐"
cycleLn  : Doc
cycleLn  = P.text "│    "
cycleMid : Doc
cycleMid = P.text "│     ↓"
cycleEnd : Doc
cycleEnd = P.text "└─────┘"
