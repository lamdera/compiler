{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Reporting.Evergreen where

import qualified Reporting.Doc as D


import Lamdera


exposureHint givenName =
  D.reflow $
    "Note: Evergreen migrations need access to all custom type variants. Make sure both `" ++
    givenName ++ "` and `Evergreen.VX." ++ givenName ++ "` are exposed."


exposureHintToDetails nearbyNames givenName noSuggestionDetails yesSuggestionDetails =
  case nearbyNames of
    [] ->
      D.stack
        [ D.reflow noSuggestionDetails
        , exposureHint givenName
        , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
        ]

    suggestions ->
      D.stack
        [ D.reflow yesSuggestionDetails
        , D.indent 4 $ D.vcat $ map D.dullyellow $ map D.fromChars suggestions
        , exposureHint givenName
        , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
        ]



-- Maybe in future if we figure out a way to get the filename context we can do a much more fine-grained error message


-- import qualified Data.Char as Char
-- import qualified Data.List as List
-- import qualified Data.Map as Map
-- import qualified Data.Name as Name
-- import qualified Data.OneOrMore as OneOrMore
-- import qualified Data.Set as Set

-- import qualified AST.Canonical as Can
-- import qualified AST.Source as Src
-- import qualified Data.Index as Index
-- import qualified Elm.ModuleName as ModuleName
-- import qualified Reporting.Annotation as A
-- import Reporting.Doc (Doc, (<+>), (<>))
-- import qualified Reporting.Render.Code as Code
-- import qualified Reporting.Render.Type as RT
-- import qualified Reporting.Report as Report
-- import qualified Reporting.Suggest as Suggest

-- import qualified Lamdera.Reporting.Suggestions


-- explainExposureErrors :: Code.Source -> A.Region -> Maybe Name.Name -> Name.Name -> String -> Locals -> Quals -> Report.Report
-- explainExposureErrors source region maybePrefix name thing locals quals =
--   notFound source region maybePrefix name thing locals quals


-- type Locals = Set.Set Name.Name
-- type Quals = Map.Map Name.Name (Set.Set Name.Name)


-- -- Clone of Reporting.Error.Canonicalize.notFound
-- notFound :: Code.Source -> A.Region -> Maybe Name.Name -> Name.Name -> String -> Locals -> Quals -> Report.Report
-- notFound source region maybePrefix name thing locals quals =
--   let
--     evergreenExposureHint =
--       D.reflow $ "Note: Evergreen migrations need access to all custom type constructors, make sure the `" ++ givenName ++ "` constructor is exposed and not just the `Evergreen.VX." ++ givenName ++ "` one."

--     givenName =
--       maybe Name.toChars toQualString maybePrefix name

--     possibleNames =
--       let
--         addQuals prefix localSet allNames =
--           Set.foldr (\x xs -> toQualString prefix x : xs) allNames localSet
--       in
--       Map.foldrWithKey addQuals (map Name.toChars (Set.toList locals)) quals
--         & Lamdera.Reporting.Suggestions.hideWireSuggestions

--     nearbyNames =
--       take 4 (Suggest.sort givenName id possibleNames)

--     toDetails noSuggestionDetails yesSuggestionDetails =
--       case nearbyNames of
--         [] ->
--           D.stack
--             [ D.reflow noSuggestionDetails
--             , evergreenExposureHint
--             , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
--             ]

--         suggestions ->
--           D.stack
--             [ D.reflow yesSuggestionDetails
--             , D.indent 4 $ D.vcat $ map D.dullyellow $ map D.fromChars suggestions
--             , evergreenExposureHint
--             , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
--             ]

--   in
--   Report.Report "NAMING ERROR" region nearbyNames $
--     Code.toSnippet source region Nothing
--       (
--         D.reflow $
--           "I cannot find a `" ++ givenName ++ "` " ++ thing ++ ":"
--       ,
--         case maybePrefix of
--           Nothing ->
--             toDetails
--               "Is there an `import` or `exposing` missing up top?"
--               "These names seem close though:"

--           Just prefix ->
--             case Map.lookup prefix quals of
--               Nothing ->
--                 toDetails
--                   ("I cannot find a `" ++ Name.toChars prefix ++ "` module. Is there an `import` for it?")
--                   ("I cannot find a `" ++ Name.toChars prefix ++ "` import. These names seem close though:")

--               Just _ ->
--                 toDetails
--                   ("The `" ++ Name.toChars prefix ++ "` module does not expose a `" ++ Name.toChars name ++ "` " ++ thing ++ ".")
--                   ("The `" ++ Name.toChars prefix ++ "` module does not expose a `" ++ Name.toChars name ++ "` " ++ thing ++ ". These names seem close though:")
--       )


-- toQualString :: Name.Name -> Name.Name -> String
-- toQualString prefix name =
--   Name.toChars prefix ++ "." ++ Name.toChars name
