{- MANUALLY FORMATTED -}
module Builder.Elm.Outline exposing
  ( Outline(..)
  , AppOutline(..)
  , PkgOutline(..)
  , Exposed(..)
  , SrcDir(..), bSrcDir
  , read
  , write
  --, encode
  , decoder
  , defaultSummary
  , flattenExposed
  )


import Builder.File as File
import Builder.Reporting.Exit as Exit
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.Constraint as Con
import Compiler.Elm.Licenses as Licenses
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Json.String as Json
import Compiler.Parse.Primitives as P
import Extra.Data.Binary as B
import Extra.System.Dir as Dir exposing (FilePath)
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- PRIVATE IO


type alias IO c d e f g h v =
  IO.IO (Dir.GlobalState c d e f g h) v



-- OUTLINE


type Outline
  = App AppOutline
  | Pkg PkgOutline


type AppOutline =
  AppOutline
    {- app_elm_version -} V.Version
    {- app_source_dirs -} (NE.TList SrcDir)
    {- app_deps_direct -} (Map.Map Pkg.Comparable V.Version)
    {- app_deps_indirect -} (Map.Map Pkg.Comparable V.Version)
    {- app_test_direct -} (Map.Map Pkg.Comparable V.Version)
    {- app_test_indirect -} (Map.Map Pkg.Comparable V.Version)


type PkgOutline =
  PkgOutline
    {- pkg_name -} Pkg.Name
    {- pkg_summary -} Json.TString
    {- pkg_license -} Licenses.License
    {- pkg_version -} V.Version
    {- pkg_exposed -} Exposed
    {- pkg_deps -} (Map.Map Pkg.Comparable Con.Constraint)
    {- pkg_test_deps -} (Map.Map Pkg.Comparable Con.Constraint)
    {- pkg_elm_version -} Con.Constraint


type Exposed
  = ExposedList (TList ModuleName.Raw)
  | ExposedDict (TList (Json.TString, TList ModuleName.Raw))


type SrcDir
  = AbsoluteSrcDir FilePath
  | RelativeSrcDir FilePath



-- DEFAULTS


defaultSummary : Json.TString
defaultSummary =
  Json.fromChars "helpful summary of your project, less than 80 characters"



-- HELPERS


flattenExposed : Exposed -> TList ModuleName.Raw
flattenExposed exposed =
  case exposed of
    ExposedList names ->
      names

    ExposedDict sections ->
      MList.concatMap Tuple.second sections



-- WRITE


write : FilePath -> Outline -> IO c d e f g h ()
write root outline =
  E.write (Dir.addName root "elm.json") (encode outline)



-- JSON ENCODE


encode : Outline -> E.Value
encode outline =
  case outline of
    App (AppOutline elm srcDirs depsDirect depsTrans testDirect testTrans) ->
      E.object
        [ ( "type", E.chars "application" )
        , ( "source-directories", E.list encodeSrcDir (NE.toList srcDirs) )
        , ( "elm-version", V.encode elm )
        , ( "dependencies",
            E.object
              [ ( "direct", encodeDeps V.encode depsDirect )
              , ( "indirect", encodeDeps V.encode depsTrans )
              ] )
        , ( "test-dependencies",
            E.object
              [ ( "direct", encodeDeps V.encode testDirect )
              , ( "indirect", encodeDeps V.encode testTrans )
              ] )
        ]

    Pkg (PkgOutline name summary license version exposed deps tests elm) ->
      E.object
        [ ( "type", E.string (Json.fromChars "package") )
        , ( "name", Pkg.encode name )
        , ( "summary", E.string summary )
        , ( "license", Licenses.encode license )
        , ( "version", V.encode version )
        , ( "exposed-modules", encodeExposed exposed )
        , ( "elm-version", Con.encode elm )
        , ( "dependencies", encodeDeps Con.encode deps )
        , ( "test-dependencies", encodeDeps Con.encode tests )
        ]


encodeExposed : Exposed -> E.Value
encodeExposed exposed =
  case exposed of
    ExposedList modules ->
      E.list encodeModule modules

    ExposedDict chunks ->
      E.object (MList.map (Tuple.mapSecond (E.list encodeModule)) chunks)


encodeModule : ModuleName.Raw -> E.Value
encodeModule name =
  E.name name


encodeDeps : (a -> E.Value) -> Map.Map Pkg.Comparable a -> E.Value
encodeDeps encodeValue deps =
  E.dict (Pkg.toJsonString << Pkg.fromComparable) encodeValue deps


encodeSrcDir : SrcDir -> E.Value
encodeSrcDir srcDir =
  case srcDir of
    AbsoluteSrcDir dir -> E.chars (Dir.toString dir)
    RelativeSrcDir dir -> E.chars (Dir.toString dir)



-- PARSE AND VERIFY


read : FilePath -> IO c d e f g h (Either Exit.Outline Outline)
read root =
  IO.bind (File.readUtf8 (Dir.addName root "elm.json")) <| \bytes ->
  case D.fromByteString decoder bytes of
    Left err ->
      IO.return <| Left (Exit.OutlineHasBadStructure err)

    Right outline ->
      case outline of
        Pkg (PkgOutline pkg _ _ _ _ deps _ _) ->
          IO.return <|
            if not (Map.member (Pkg.toComparable Pkg.core) deps) && pkg /= Pkg.core
            then Left Exit.OutlineNoPkgCore
            else Right outline

        App (AppOutline _ srcDirs direct indirect _ _) ->
          if not (Map.member (Pkg.toComparable Pkg.core) direct) then
            IO.return <| Left Exit.OutlineNoAppCore

          else if not (Map.member (Pkg.toComparable Pkg.json) direct) && not (Map.member (Pkg.toComparable Pkg.json) indirect) then
            IO.return <| Left Exit.OutlineNoAppJson

          else
            IO.bind (MList.filterM IO.pure IO.liftA2 (isSrcDirMissing root) (NE.toList srcDirs)) <| \badDirs ->
            case MList.map toGiven badDirs of
              d::ds ->
                IO.return <| Left (Exit.OutlineHasMissingSrcDirs d ds)

              [] ->
                IO.bind (detectDuplicates root (NE.toList srcDirs)) <| \maybeDups ->
                case maybeDups of
                  Nothing ->
                    IO.return <| Right outline

                  Just (canonicalDir, (dir1,dir2)) ->
                    IO.return <| Left (Exit.OutlineHasDuplicateSrcDirs canonicalDir dir1 dir2)


isSrcDirMissing : FilePath -> SrcDir -> IO c d e f g h Bool
isSrcDirMissing root srcDir =
  IO.fmap not <| Dir.doesDirectoryExist (toAbsolute root srcDir)


toGiven : SrcDir -> FilePath
toGiven srcDir =
  case srcDir of
    AbsoluteSrcDir dir -> dir
    RelativeSrcDir dir -> dir


toAbsolute : FilePath -> SrcDir -> FilePath
toAbsolute root srcDir =
  case srcDir of
    AbsoluteSrcDir dir -> dir
    RelativeSrcDir dir -> Dir.combine root dir


detectDuplicates : FilePath -> TList SrcDir -> IO c d e f g h (Maybe (String, (FilePath, FilePath)))
detectDuplicates root srcDirs =
  IO.bind (MList.traverse IO.pure IO.liftA2 (toPair root) srcDirs) <| \pairs ->
  IO.return <| Map.lookupMin <| Map.mapMaybe isDup <|
    Map.fromListWith OneOrMore.more pairs


toPair : FilePath -> SrcDir -> IO c d e f g h (String, OneOrMore.OneOrMore FilePath)
toPair root srcDir =
  IO.return ( Dir.toString (toAbsolute root srcDir), OneOrMore.one (toGiven srcDir) )


isDup : OneOrMore.OneOrMore FilePath -> Maybe (FilePath, FilePath)
isDup paths =
  case paths of
    OneOrMore.One _    -> Nothing
    OneOrMore.More a b -> Just (OneOrMore.getFirstTwo a b)



-- JSON DECODE


type alias Decoder a =
  D.Decoder Exit.OutlineProblem a


decoder : Decoder Outline
decoder =
  let
    application = Json.fromChars "application"
    package     = Json.fromChars "package"
  in
  D.bind (D.field "type" D.string) <| \tipe ->
  if      tipe == application then D.fmap App <| appDecoder
  else if tipe == package     then D.fmap Pkg <| pkgDecoder
  else                             D.failure Exit.OP_BadType


appDecoder : Decoder AppOutline
appDecoder =
  D.pure AppOutline
    |> D.andMap (D.field "elm-version" versionDecoder)
    |> D.andMap (D.field "source-directories" dirsDecoder)
    |> D.andMap (D.field "dependencies" (D.field "direct" (depsDecoder versionDecoder)))
    |> D.andMap (D.field "dependencies" (D.field "indirect" (depsDecoder versionDecoder)))
    |> D.andMap (D.field "test-dependencies" (D.field "direct" (depsDecoder versionDecoder)))
    |> D.andMap (D.field "test-dependencies" (D.field "indirect" (depsDecoder versionDecoder)))


pkgDecoder : Decoder PkgOutline
pkgDecoder =
  D.pure PkgOutline
    |> D.andMap (D.field "name" nameDecoder)
    |> D.andMap (D.field "summary" summaryDecoder)
    |> D.andMap (D.field "license" (Licenses.decoder Exit.OP_BadLicense))
    |> D.andMap (D.field "version" versionDecoder)
    |> D.andMap (D.field "exposed-modules" exposedDecoder)
    |> D.andMap (D.field "dependencies" (depsDecoder constraintDecoder))
    |> D.andMap (D.field "test-dependencies" (depsDecoder constraintDecoder))
    |> D.andMap (D.field "elm-version" constraintDecoder)



-- JSON DECODE HELPERS


nameDecoder : Decoder Pkg.Name
nameDecoder =
  D.mapError (\(a,b) -> Exit.OP_BadPkgName a b) Pkg.decoder


summaryDecoder : Decoder Json.TString
summaryDecoder =
  D.customString
    (boundParser 80 Exit.OP_BadSummaryTooLong)
    (\_ _ -> Exit.OP_BadSummaryTooLong)


versionDecoder : Decoder V.Version
versionDecoder =
  D.mapError (\(a,b) -> Exit.OP_BadVersion a b) V.decoder


constraintDecoder : Decoder Con.Constraint
constraintDecoder =
  D.mapError Exit.OP_BadConstraint Con.decoder


depsDecoder : Decoder a -> Decoder (Map.Map Pkg.Comparable a)
depsDecoder valueDecoder =
  D.dict (Pkg.keyDecoder Exit.OP_BadDependencyName) valueDecoder


dirsDecoder : Decoder (NE.TList SrcDir)
dirsDecoder =
  D.fmap (NE.fmap (toSrcDir << Dir.fromString << Json.toChars)) <| D.nonEmptyList D.string Exit.OP_NoSrcDirs


toSrcDir : FilePath -> SrcDir
toSrcDir path =
  if Dir.isRelative path
  then RelativeSrcDir path
  else AbsoluteSrcDir path



-- EXPOSED MODULES DECODER


exposedDecoder : Decoder Exposed
exposedDecoder =
  D.oneOf
    [ D.fmap ExposedList <| D.list moduleDecoder
    , D.fmap ExposedDict <| D.pairs headerKeyDecoder (D.list moduleDecoder)
    ]


moduleDecoder : Decoder ModuleName.Raw
moduleDecoder =
  D.mapError (\(a,b) -> Exit.OP_BadModuleName a b) ModuleName.decoder


headerKeyDecoder : D.KeyDecoder Exit.OutlineProblem Json.TString
headerKeyDecoder =
  D.KeyDecoder
    (boundParser 20 Exit.OP_BadModuleHeaderTooLong)
    (\_ _ -> Exit.OP_BadModuleHeaderTooLong)



-- BOUND PARSER


boundParser : Int -> x -> P.Parser x Json.TString
boundParser bound tooLong =
  P.Parser <| \(P.State src pos end indent row col) ->
    let
      len = end - pos
      newCol = col + len
    in
    if len < bound
    then P.Cok (Json.fromPtr src pos end) (P.State src end end indent row newCol)
    else P.Cerr row newCol (\_ _ -> tooLong)



-- BINARY


bSrcDir : B.Binary SrcDir
bSrcDir =
  B.custom "binary encoding of SrcDir was corrupted"
    (\p0 p1 srcDir ->
      case srcDir of
        AbsoluteSrcDir a -> p0 a
        RelativeSrcDir a -> p1 a
    )
    |> B.var1 0 AbsoluteSrcDir B.bPath
    |> B.var1 1 RelativeSrcDir B.bPath
    |> B.finish
