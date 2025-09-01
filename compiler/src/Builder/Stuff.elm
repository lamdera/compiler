{- MANUALLY FORMATTED -}
module Builder.Stuff exposing
  ( details
  , interfaces
  , objects
  --, prepublishDir
  , elmi
  , elmo
  --, temp
  , findRoot
  --, withRootLock
  --, withRegistryLock
  , PackageCache
  , getPackageCache
  , registry
  , package
  , getReplCache
  , getElmHome
  )


import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Extra.System.Dir as Dir exposing (FileName, FilePath)
import Extra.System.IO as IO


-- PRIVATE IO


type alias IO c d e f g h v =
  IO.IO (Dir.GlobalState c d e f g h) v



-- PATHS


stuff : FilePath -> FilePath
stuff root =
  Dir.addNames root [ "elm-stuff", compilerVersion ]


details : FilePath -> FilePath
details root =
  Dir.addName (stuff root) "d.dat"


interfaces : FilePath -> FilePath
interfaces root =
  Dir.addName (stuff root) "i.dat"


objects : FilePath -> FilePath
objects root =
  Dir.addName (stuff root) "o.dat"


compilerVersion : FileName
compilerVersion =
  V.toChars V.compiler



-- ELMI and ELMO


elmi : FilePath -> ModuleName.Raw -> FilePath
elmi root name =
  toArtifactPath root name "elmi"


elmo : FilePath -> ModuleName.Raw -> FilePath
elmo root name =
  toArtifactPath root name "elmo"


toArtifactPath : FilePath -> ModuleName.Raw -> String -> FilePath
toArtifactPath root name ext =
  Dir.addName (stuff root) (ModuleName.toHyphenName name ++ "." ++ ext)



-- ROOT


findRoot : IO c d e f g h (Maybe FilePath)
findRoot =
  IO.bind Dir.getCurrentDirectory <| \dir ->
  findRootHelp dir


findRootHelp : FilePath -> IO c d e f g h (Maybe FilePath)
findRootHelp dirs =
  IO.bind (Dir.doesFileExist (Dir.addName dirs "elm.json")) <| \exists ->
  if exists
    then IO.return (Just dirs)
    else
      case Dir.splitLastName dirs of
        ( _, "" ) ->
          IO.return Nothing

        ( parent, _ ) ->
          findRootHelp parent



-- PACKAGE CACHES


type PackageCache = PackageCache FilePath


getPackageCache : IO c d e f g h PackageCache
getPackageCache =
  IO.fmap PackageCache <| getCacheDir "packages"


registry : PackageCache -> FilePath
registry (PackageCache dir) =
  Dir.addName dir "registry.dat"


package : PackageCache -> Pkg.Name -> V.Version -> FilePath
package (PackageCache dir) name version =
  Dir.addName (Dir.combine dir (Pkg.toFilePath name)) (V.toChars version)



-- CACHE


getReplCache : IO c d e f g h FilePath
getReplCache =
  getCacheDir "repl"


getCacheDir : FileName -> IO c d e f g h FilePath
getCacheDir projectName =
  IO.bind getElmHome <| \home ->
  let root = Dir.addNames home [ compilerVersion, projectName ] in
  IO.bind (Dir.createDirectoryIfMissing True root) <| \_ ->
  IO.return root


getElmHome : IO c d e f g h FilePath
getElmHome =
  Dir.getAppUserDataDirectory "elm"
