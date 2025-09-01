{- MANUALLY FORMATTED -}
module Terminal.Helpers exposing
  ( {-version
  , elmFile
  ,-} parsePackage
  )


import Compiler.Elm.Package as Pkg
import Compiler.Parse.Primitives as P
import Extra.Type.Either exposing (Either(..))



-- PACKAGE


parsePackage : String -> Maybe Pkg.Name
parsePackage chars =
  case P.fromByteString Pkg.parser Tuple.pair chars of
    Right pkg -> Just pkg
    Left _    -> Nothing
