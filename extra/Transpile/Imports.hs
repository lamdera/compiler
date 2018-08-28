module Transpile.Imports where


import qualified Language.Haskell.Exts.Simple.Syntax as Hs

import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Data.Map.Strict

(==>) = (,)

reservedWords = reservedElmWords <> reservedHaskellWords
reservedElmWords = ["if", "then", "else", "case", "of", "let", "in", "type", "module", "where", "import", "exposing", "as", "port"]

reservedHaskellSymbols =
  ["!", "'", "''", "-", "--", "-<", "-<<", "->", "::", ";", "<-", ",", "=", "=>", ">", "?", "#", "*", "@", "[|", "|]", "\\", "_", "`", "{", "}", "{-", "-}", "|", "~"]
reservedHaskellWords =
  [ "as"
  , "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "family"
  , "forall"
  , "foreign"
  , "hiding"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "mdo"
  , "module"
  , "newtype"
  , "of"
  , "proc"
  , "qualified"
  , "rec"
  , "then"
  , "type"
  , "where"
  ]

{-
-- TODO: translate
-- from elm-compiler 0.18
-- tImports :: (t, Map [UppercaseIdentifier] (a, A.ImportMethod)) -> [Hs.ImportDecl]
defaultImports =
  ( []
  , fromList
    [ ([UppercaseIdentifier "Haskelm", UppercaseIdentifier "Core"], ([], ImportMethod Nothing ([], ([], OpenListing (Commented [] () [])))))
    , ([UppercaseIdentifier "Basics"], ([], ImportMethod Nothing ([], ([], OpenListing (Commented [] () [])))))
    , ([UppercaseIdentifier "Debug"] , ([], ImportMethod Nothing ([], ([], ClosedListing))))
    , ([UppercaseIdentifier "List"]  , ([], ImportMethod Nothing ([], ([], ClosedListing))))
    , ( [UppercaseIdentifier "Maybe"]
      , ( []
        , ImportMethod
          Nothing
          ( []
          , ( []
            , ExplicitListing
              ( DetailedListing
                { values    = fromList []
                , operators = fromList []
                , types     = fromList [(UppercaseIdentifier "Maybe", Commented [] ([], OpenListing (Commented [] () [])) [])]
                }
              )
              False
            )
          )
        )
      )
    , ( [UppercaseIdentifier "Platform"]
      , ( []
        , ImportMethod
          Nothing
          ( []
          , ( []
            , ExplicitListing
              ( DetailedListing
                { values    = fromList []
                , operators = fromList []
                , types     = fromList [(UppercaseIdentifier "Program", Commented [] ([], ClosedListing) [])]
                }
              )
              False
            )
          )
        )
      )
    , ( [UppercaseIdentifier "Platform", UppercaseIdentifier "Cmd"]
      , ( []
        , ImportMethod
          (Just ([], ([], UppercaseIdentifier "Cmd")))
          ( []
          , ( []
            , ExplicitListing
              ( DetailedListing
                { values    = fromList []
                , operators = fromList [(SymbolIdentifier "!", Commented [] () [])]
                , types     = fromList [(UppercaseIdentifier "Cmd", Commented [] ([], ClosedListing) [])]
                }
              )
              False
            )
          )
        )
      )
    , ( [UppercaseIdentifier "Platform", UppercaseIdentifier "Sub"]
      , ( []
        , ImportMethod
          (Just ([], ([], UppercaseIdentifier "Sub")))
          ( []
          , ( []
            , ExplicitListing
              ( DetailedListing
                { values    = fromList []
                , operators = fromList []
                , types     = fromList [(UppercaseIdentifier "Sub", Commented [] ([], ClosedListing) [])]
                }
              )
              False
            )
          )
        )
      )
    , ( [UppercaseIdentifier "Result"]
      , ( []
        , ImportMethod
          Nothing
          ( []
          , ( []
            , ExplicitListing
              ( DetailedListing
                { values    = fromList []
                , operators = fromList []
                , types     = fromList [(UppercaseIdentifier "Result", Commented [] ([], OpenListing (Commented [] () [])) [])]
                }
              )
              False
            )
          )
        )
      )
    , ([UppercaseIdentifier "String"], ([], ImportMethod Nothing ([], ([], ClosedListing))))
    , ([UppercaseIdentifier "Tuple"] , ([], ImportMethod Nothing ([], ([], ClosedListing))))
    ]
  )

defaultLanguagePragmas = fmap
  (\p -> Hs.LanguagePragma [Hs.Ident p])
  [ "ConstraintKinds"
  -- , "FlexibleContexts"
  , "TypeFamilies"
  , "TypeOperators"
  , "OverloadedStrings"
  , "OverloadedLabels"
  , "DataKinds"
  , "NoImplicitPrelude"
  , "DeriveAnyClass"
  , "NoMonomorphismRestriction"
  , "GADTs"
  , "StandaloneDeriving"
  , "FlexibleContexts" -- This prevents issues with SuperRecord when it's used kinda anonymously (i.e. passed around a lot between function calls in a single function body)
  ]


mhead (a:_) = Just a
mhead [] = Nothing
-}
