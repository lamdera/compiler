{- MANUALLY FORMATTED -}
module Builder.Deps.Registry exposing
  ( Registry(..)
  , KnownVersions(..)
  , read
  , fetch
  , update
  --, latest
  , getVersions
  , getVersionsE
  )


import Builder.Deps.Website as Website
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Compiler.Parse.Primitives as P
import Extra.Data.Binary as B
import Extra.System.Dir as Dir
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- PRIVATE IO


type alias IO c d e f g h v =
  IO.IO (Dir.GlobalState c d e f g h) v



-- REGISTRY


type Registry =
  Registry
    {- count -} Int
    {- versions -} (Map.Map Pkg.Comparable KnownVersions)


type KnownVersions =
  KnownVersions
    {- newest -} V.Version
    {- previous -} (TList V.Version)



-- READ


read : Stuff.PackageCache -> IO c d e f g h (Maybe Registry)
read cache =
  File.readBinary bRegistry (Stuff.registry cache)



-- FETCH


fetch : Http.Manager -> Stuff.PackageCache -> IO c d e f g h (Either Exit.RegistryProblem Registry)
fetch manager cache =
  post manager "/all-packages" allPkgsDecoder <|
    \versions ->
      let size = Map.foldr addEntry 0 versions in
      let registry = Registry size versions in
      let path = Stuff.registry cache in
      IO.bind (File.writeBinary bRegistry path registry) <| \_ ->
      IO.return registry


addEntry : KnownVersions -> Int -> Int
addEntry (KnownVersions _ vs) count =
  count + 1 + MList.length vs


allPkgsDecoder : D.Decoder () (Map.Map Pkg.Comparable KnownVersions)
allPkgsDecoder =
  let
    keyDecoder =
      Pkg.keyDecoder bail

    versionsDecoder =
      D.list (D.mapError (\_ -> ()) V.decoder)

    toKnownVersions versions =
      case MList.sortBy (\a b -> compare (V.toComparable b) (V.toComparable a)) versions of
        v::vs -> D.return (KnownVersions v vs)
        []   -> D.failure ()
  in
  D.dict keyDecoder (D.andThen toKnownVersions versionsDecoder)



-- UPDATE


update : Http.Manager -> Stuff.PackageCache -> Registry -> IO c d e f g h (Either Exit.RegistryProblem Registry)
update manager cache ((Registry size packages) as oldRegistry) =
  post manager ("/all-packages/since/" ++ String.fromInt size) (D.list newPkgDecoder) <|
    \news ->
      case news of
        [] ->
          IO.return oldRegistry

        _::_ ->
          let
            newSize = size + MList.length news
            newPkgs = MList.foldr addNew packages news
            newRegistry = Registry newSize newPkgs
          in
          IO.bind (File.writeBinary bRegistry (Stuff.registry cache) newRegistry) <| \_ ->
          IO.return newRegistry


addNew : (Pkg.Name, V.Version) -> Map.Map Pkg.Comparable KnownVersions -> Map.Map Pkg.Comparable KnownVersions
addNew (name, version) versions =
  let
    add maybeKnowns =
      case maybeKnowns of
        Just (KnownVersions v vs) ->
          KnownVersions version (v::vs)

        Nothing ->
          KnownVersions version []
  in
  Map.alter (Just << add) (Pkg.toComparable name) versions



-- NEW PACKAGE DECODER


newPkgDecoder : D.Decoder () (Pkg.Name, V.Version)
newPkgDecoder =
  D.customString newPkgParser bail


newPkgParser : P.Parser () (Pkg.Name, V.Version)
newPkgParser =
  P.bind (P.specialize (\_ _ _ -> ()) Pkg.parser) <| \pkg ->
  P.bind (P.word1 0x40 {-@-} bail) <| \_ ->
  P.bind (P.specialize (\_ _ _ -> ()) V.parser) <| \vsn ->
  P.return (pkg, vsn)


bail : row -> col -> ()
bail _ _ =
  ()



-- GET VERSIONS


getVersions : Pkg.Comparable -> Registry -> Maybe KnownVersions
getVersions name (Registry _ versions) =
  Map.lookup name versions


getVersionsE : Pkg.Comparable -> Registry -> Either (TList Pkg.Name) KnownVersions
getVersionsE name (Registry _ versions) =
  case Map.lookup name versions of
    Just kvs -> Right kvs
    Nothing -> Left <| Pkg.nearbyNames (Pkg.fromComparable name) (MList.map Pkg.fromComparable <| Map.keys versions)



-- POST


post : Http.Manager -> String -> D.Decoder x v -> (v -> IO c d e f g h z) -> IO c d e f g h (Either Exit.RegistryProblem z)
post manager path decoder callback =
  let
    url = Website.route path []
  in
  Http.post manager url [] Exit.RP_Http <|
    \body ->
      case D.fromByteString decoder body of
        Right a -> IO.fmap Right <| callback a
        Left _ -> IO.return <| Left <| Exit.RP_Data url body



-- BINARY


bRegistry : B.Binary Registry
bRegistry = B.bin2 Registry (\(Registry a b) -> B.T2 a b)
  B.bWord64 (B.bMap Pkg.bComparable bKnownVersions)


bKnownVersions : B.Binary KnownVersions
bKnownVersions = B.bin2 KnownVersions (\(KnownVersions a b) -> B.T2 a b)
  V.bVersion (B.bTList V.bVersion)
