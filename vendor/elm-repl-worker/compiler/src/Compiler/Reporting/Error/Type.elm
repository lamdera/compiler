{- MANUALLY FORMATTED -}
module Compiler.Reporting.Error.Type exposing
  ( Error(..)
  -- expectations
  , Expected(..)
  , Context(..)
  , SubContext(..)
  , MaybeName(..)
  , Category(..)
  , PExpected(..)
  , PContext(..)
  , PCategory(..)
  , typeReplace
  , ptypeReplace
  -- make reports
  , toReport
  )


import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (d, da)
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Reporting.Report as Report
import Compiler.Reporting.Suggest as Suggest
import Compiler.Type.Error as T
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe



-- ERRORS


type Error
  = BadExpr A.Region Category T.Type (Expected T.Type)
  | BadPattern A.Region PCategory T.Type (PExpected T.Type)
  | InfiniteType A.Region Name.Name T.Type



-- EXPRESSION EXPECTATIONS


type Expected tipe
  = NoExpectation tipe
  | FromContext A.Region Context tipe
  | FromAnnotation Name.Name Int SubContext tipe


type Context
  = ListEntry Index.ZeroBased
  | Negate
  | OpLeft Name.Name
  | OpRight Name.Name
  | IfCondition
  | IfBranch Index.ZeroBased
  | CaseBranch Index.ZeroBased
  | CallArity MaybeName Int
  | CallArg MaybeName Index.ZeroBased
  | RecordAccess A.Region (Maybe Name.Name) A.Region Name.Name
  | RecordUpdateKeys Name.Name (Map.Map Name.Name Can.FieldUpdate)
  | RecordUpdateValue Name.Name
  | Destructure


type SubContext
  = TypedIfBranch Index.ZeroBased
  | TypedCaseBranch Index.ZeroBased
  | TypedBody


type MaybeName
  = FuncName Name.Name
  | CtorName Name.Name
  | OpName Name.Name
  | NoName


type Category
  = CList
  | Number
  | CFloat
  | CString
  | CChar
  | If
  | Case
  | CallResult MaybeName
  | Lambda
  | Accessor Name.Name
  | Access Name.Name
  | Record
  | Tuple
  | Unit
  | Shader
  | Effects
  | Local Name.Name
  | Foreign Name.Name



-- PATTERN EXPECTATIONS


type PExpected tipe
  = PNoExpectation tipe
  | PFromContext A.Region PContext tipe


type PContext
  = PTypedArg Name.Name Index.ZeroBased
  | PCaseMatch Index.ZeroBased
  | PCtorArg Name.Name Index.ZeroBased
  | PListEntry Index.ZeroBased
  | PTail


type PCategory
  = PRecord
  | PUnit
  | PTuple
  | PList
  | PCtor Name.Name
  | PInt
  | PStr
  | PChr
  | PBool



-- HELPERS


typeReplace : Expected a -> b -> Expected b
typeReplace expectation tipe =
  case expectation of
    NoExpectation _ ->
      NoExpectation tipe

    FromContext region context _ ->
      FromContext region context tipe

    FromAnnotation name arity context _ ->
      FromAnnotation name arity context tipe


ptypeReplace : PExpected a -> b -> PExpected b
ptypeReplace expectation tipe =
  case expectation of
    PNoExpectation _ ->
      PNoExpectation tipe

    PFromContext region context _ ->
      PFromContext region context tipe



-- TO REPORT


toReport : Code.Source -> L.Localizer -> Error -> Report.Report
toReport source localizer err =
  case err of
    BadExpr region category actualType expected ->
      toExprReport source localizer region category actualType expected

    BadPattern region category tipe expected ->
      toPatternReport source localizer region category tipe expected

    InfiniteType region name overallType ->
      toInfiniteReport source localizer region name overallType



-- TO PATTERN REPORT


toPatternReport : Code.Source -> L.Localizer -> A.Region -> PCategory -> T.Type -> PExpected T.Type -> Report.Report
toPatternReport source localizer patternRegion category tipe expected =
  Report.Report "TYPE MISMATCH" patternRegion <|
  case expected of
    PNoExpectation expectedType ->
      Code.toSnippet source patternRegion Nothing <|
        ( d"This pattern is being used in an unexpected way:"
        , patternTypeComparison localizer tipe expectedType
            (addPatternCategory "It is" category)
            "But it needs to match:"
            []
        )

    PFromContext region context expectedType ->
      Code.toSnippet source region (Just patternRegion) <|
        case context of
          PTypedArg name index ->
            ( D.reflow <|
                "The " ++ D.ordinal index ++ " argument to `" ++ name ++ "` is weird."
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory "The argument is a pattern that matches" category)
                ( "But the type annotation on `" ++ name
                  ++ "` says the " ++ D.ordinal index ++ " argument should be:"
                )
                []
            )

          PCaseMatch index ->
            if index == Index.first then
              (
                D.reflow <|
                  "The 1st pattern in this `case` causing a mismatch:"
              ,
                patternTypeComparison localizer tipe expectedType
                  (addPatternCategory "The first pattern is trying to match" category)
                  "But the expression between `case` and `of` is:"
                  [ D.reflow <|
                      "These can never match! Is the pattern the problem? Or is it the expression?"
                  ]
              )
            else
              ( D.reflow <|
                  "The " ++ D.ordinal index ++ " pattern in this `case` does not match the previous ones."
              , patternTypeComparison localizer tipe expectedType
                  (addPatternCategory ("The " ++ D.ordinal index ++ " pattern is trying to match") category)
                  "But all the previous patterns match:"
                  [ D.link "Note"
                      "A `case` expression can only handle one type of value, so you may want to use"
                      "custom-types"
                      "to handle “mixing” types."
                  ]
              )

          PCtorArg name index ->
            ( D.reflow <|
                "The " ++ D.ordinal index ++ " argument to `" ++ name ++ "` is weird."
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory "It is trying to match" category)
                ( "But `" ++ name ++ "` needs its "
                  ++ D.ordinal index ++ " argument to be:"
                )
                []
            )

          PListEntry index ->
            ( D.reflow <|
                "The " ++ D.ordinal index ++ " pattern in this list does not match all the previous ones:"
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory ("The " ++ D.ordinal index ++ " pattern is trying to match") category)
                "But all the previous patterns in the list are:"
                [ D.link "Hint"
                    ("Everything in a list must be the same type of value. This way, we never"
                    ++ " run into unexpected values partway through a List.map, List.foldl, etc. Read")
                    "custom-types"
                    "to learn how to “mix” types."
                ]
            )

          PTail ->
            ( D.reflow <|
                "The pattern after (::) is causing issues."
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory "The pattern after (::) is trying to match" category)
                "But it needs to match lists like this:"
                []
            )



-- PATTERN HELPERS


patternTypeComparison : L.Localizer -> T.Type -> T.Type -> String -> String -> TList D.Doc -> D.Doc
patternTypeComparison localizer actual expected iAmSeeing insteadOf contextHints =
  let
    (actualDoc, expectedDoc, problems) =
      T.toComparison localizer actual expected
  in
  D.stack <|
    [ D.reflow iAmSeeing
    , D.indent 4 actualDoc
    , D.reflow insteadOf
    , D.indent 4 expectedDoc
    ]
    ++ problemsToHint problems
    ++ contextHints


addPatternCategory : String -> PCategory -> String
addPatternCategory iAmTryingToMatch category =
  iAmTryingToMatch ++
    case category of
      PRecord -> " record values of type:"
      PUnit -> " unit values:"
      PTuple -> " tuples of type:"
      PList -> " lists of type:"
      PCtor name -> " `" ++ name ++ "` values of type:"
      PInt -> " integers:"
      PStr -> " strings:"
      PChr -> " characters:"
      PBool -> " booleans:"



-- EXPR HELPERS


typeComparison : L.Localizer -> T.Type -> T.Type -> String -> String -> TList D.Doc -> D.Doc
typeComparison localizer actual expected iAmSeeing insteadOf contextHints =
  let
    (actualDoc, expectedDoc, problems) =
      T.toComparison localizer actual expected
  in
  D.stack <|
    [ D.reflow iAmSeeing
    , D.indent 4 actualDoc
    , D.reflow insteadOf
    , D.indent 4 expectedDoc
    ]
    ++ contextHints
    ++ problemsToHint problems


loneType : L.Localizer -> T.Type -> T.Type -> D.Doc -> TList D.Doc -> D.Doc
loneType localizer actual expected iAmSeeing furtherDetails =
  let
    (actualDoc, _, problems) =
      T.toComparison localizer actual expected
  in
  D.stack <|
    [ iAmSeeing
    , D.indent 4 actualDoc
    ]
    ++ furtherDetails
    ++ problemsToHint problems


addCategory : String -> Category -> String
addCategory thisIs category =
  case category of
    Local name -> "This `" ++ name ++ "` value is a:"
    Foreign name -> "This `" ++ name ++ "` value is a:"
    Access field -> "The value at ." ++ field ++ " is a:"
    Accessor field -> "This ." ++ field ++ " field access function has type:"
    If -> "This `if` expression produces:"
    Case -> "This `case` expression produces:"
    CList -> thisIs ++ " a list of type:"
    Number -> thisIs ++ " a number of type:"
    CFloat -> thisIs ++ " a float of type:"
    CString -> thisIs ++ " a string of type:"
    CChar -> thisIs ++ " a character of type:"
    Lambda -> thisIs ++ " an anonymous function of type:"
    Record -> thisIs ++ " a record of type:"
    Tuple -> thisIs ++ " a tuple of type:"
    Unit -> thisIs ++ " a unit value:"
    Shader -> thisIs ++ " a GLSL shader of type:"
    Effects -> thisIs ++ " a thing for CORE LIBRARIES ONLY."
    CallResult maybeName ->
      case maybeName of
        NoName -> thisIs ++ ":"
        FuncName name -> "This `" ++ name ++ "` call produces:"
        CtorName name -> "This `" ++ name ++ "` call produces:"
        OpName _ -> thisIs ++ ":"


problemsToHint : TList T.Problem -> TList D.Doc
problemsToHint problems =
  case problems of
    [] ->
      []

    problem :: _ ->
      problemToHint problem


problemToHint : T.Problem -> TList D.Doc
problemToHint problem =
  case problem of
    T.IntFloat ->
      [ D.fancyLink "Note" [d"Read"] "implicit-casts"
          [d"to",d"learn",d"why",d"Elm",d"does",d"not",d"implicitly",d"convert"
          ,d"Ints",d"to",d"Floats.",d"Use",D.greenS "toFloat",d"and"
          ,D.greenS "round",d"to",d"do",d"explicit",d"conversions."
          ]
      ]

    T.StringFromInt ->
      [ D.toFancyHint
          [d"Want",d"to",d"convert",d"an",d"Int",d"into",d"a",d"String?"
          ,d"Use",d"the",D.greenS "String.fromInt",d"function!"
          ]
      ]

    T.StringFromFloat ->
      [ D.toFancyHint
          [d"Want",d"to",d"convert",d"a",d"Float",d"into",d"a",d"String?"
          ,d"Use",d"the",D.greenS "String.fromFloat",d"function!"
          ]
      ]

    T.StringToInt ->
      [ D.toFancyHint
          [d"Want",d"to",d"convert",d"a",d"String",d"into",d"an",d"Int?"
          ,d"Use",d"the",D.greenS "String.toInt",d"function!"
          ]
      ]

    T.StringToFloat ->
      [ D.toFancyHint
          [d"Want",d"to",d"convert",d"a",d"String",d"into",d"a",d"Float?"
          ,d"Use",d"the",D.greenS "String.toFloat",d"function!"
          ]
      ]

    T.AnythingToBool ->
      [ D.toSimpleHint <|
          "Elm does not have “truthiness” such that ints and strings and lists"
          ++ " are automatically converted to booleans. Do that conversion explicitly!"
      ]

    T.AnythingFromMaybe ->
      [ D.toFancyHint
          [d"Use",D.greenS "Maybe.withDefault",d"to",d"handle",d"possible",d"errors."
          ,d"Longer",d"term,",d"it",d"is",d"usually",d"better",d"to",d"write",d"out",d"the"
          ,d"full",d"`case`",d"though!"
          ]
      ]

    T.ArityMismatch x y ->
      [ D.toSimpleHint <|
          if x < y then
            "It looks like it takes too few arguments. I was expecting " ++ String.fromInt (y - x) ++ " more."
          else
            "It looks like it takes too many arguments. I see " ++ String.fromInt (x - y) ++ " extra."
      ]

    T.BadFlexSuper direction super tipe ->
      case tipe of
        T.Lambda _ _ _   -> badFlexSuper direction super tipe
        T.Infinite       -> []
        T.Error          -> []
        T.FlexVar _      -> []
        T.FlexSuper s _  -> badFlexFlexSuper super s
        T.RigidVar y     -> badRigidVar y (toASuperThing super)
        T.RigidSuper s _ -> badRigidSuper s (toASuperThing super)
        T.Type _ _ _     -> badFlexSuper direction super tipe
        T.Record _ _     -> badFlexSuper direction super tipe
        T.Unit           -> badFlexSuper direction super tipe
        T.Tuple _ _ _    -> badFlexSuper direction super tipe
        T.Alias _ _ _ _  -> badFlexSuper direction super tipe

    T.BadRigidVar x tipe ->
      case tipe of
        T.Lambda _ _ _   -> badRigidVar x "a function"
        T.Infinite       -> []
        T.Error          -> []
        T.FlexVar _      -> []
        T.FlexSuper s _  -> badRigidVar x (toASuperThing s)
        T.RigidVar y     -> badDoubleRigid x y
        T.RigidSuper _ y -> badDoubleRigid x y
        T.Type _ n _     -> badRigidVar x ("a `" ++ n ++ "` value")
        T.Record _ _     -> badRigidVar x "a record"
        T.Unit           -> badRigidVar x "a unit value"
        T.Tuple _ _ _    -> badRigidVar x "a tuple"
        T.Alias _ n _ _  -> badRigidVar x ("a `" ++ n ++ "` value")

    T.BadRigidSuper super x tipe ->
      case tipe of
        T.Lambda _ _ _   -> badRigidSuper super "a function"
        T.Infinite       -> []
        T.Error          -> []
        T.FlexVar _      -> []
        T.FlexSuper s _  -> badRigidSuper super (toASuperThing s)
        T.RigidVar y     -> badDoubleRigid x y
        T.RigidSuper _ y -> badDoubleRigid x y
        T.Type _ n _     -> badRigidSuper super ("a `" ++ n ++ "` value")
        T.Record _ _     -> badRigidSuper super "a record"
        T.Unit           -> badRigidSuper super "a unit value"
        T.Tuple _ _ _    -> badRigidSuper super "a tuple"
        T.Alias _ n _ _  -> badRigidSuper super ("a `" ++ n ++ "` value")

    T.FieldsMissing fields ->
      case MList.map (D.green << D.fromName) fields of
        [] ->
          []

        [f1] ->
          [ D.toFancyHint [d"Looks",d"like",d"the",f1,d"field",d"is",d"missing."]
          ]

        fieldDocs ->
          [ D.toFancyHint <|
              [d"Looks",d"like",d"fields"] ++ D.commaSep (d"and") identity fieldDocs ++ [d"are",d"missing."]
          ]

    T.FieldTypo typo possibilities ->
      case Suggest.sort typo identity possibilities of
        [] ->
          []

        nearest::_ ->
          [ D.toFancyHint <|
              [d"Seems",d"like",d"a",d"record",d"field",d"typo.",d"Maybe"
              ,D.dullyellow (D.fromName typo),d"should",d"be"
              ,da[D.green (D.fromName nearest), d"?"]
              ]
          , D.toSimpleHint
              ("Can more type annotations be added? Type annotations always help me give"
              ++ " more specific messages, and I think they could help a lot in this case!")
          ]



-- BAD RIGID HINTS


badRigidVar : Name.Name -> String -> TList D.Doc
badRigidVar name aThing =
  [ D.toSimpleHint <|
      "Your type annotation uses type variable `" ++ name ++
      "` which means ANY type of value can flow through, but your code is saying it specifically wants "
      ++ aThing ++ ". Maybe change your type annotation to"
      ++ " be more specific? Maybe change the code to be more general?"
  , D.reflowLink "Read" "type-annotations" "for more advice!"
  ]


badDoubleRigid : Name.Name -> Name.Name -> TList D.Doc
badDoubleRigid x y =
  [ D.toSimpleHint <|
      "Your type annotation uses `" ++ x ++ "` and `" ++ y ++
      "` as separate type variables. Your code seems to be saying they are the"
      ++ " same though. Maybe they should be the same in your type annotation?"
      ++ " Maybe your code uses them in a weird way?"
  , D.reflowLink "Read" "type-annotations" "for more advice!"
  ]


toASuperThing : T.Super -> String
toASuperThing super =
  case super of
    T.Number     -> "a `number` value"
    T.Comparable -> "a `comparable` value"
    T.CompAppend -> "a `compappend` value"
    T.Appendable -> "an `appendable` value"



-- BAD SUPER HINTS


badFlexSuper : T.Direction -> T.Super -> T.Type -> TList D.Doc
badFlexSuper direction super tipe =
  case super of
    T.Comparable ->
      case tipe of
        T.Record _ _ ->
          [ D.link "Hint"
              ("I do not know how to compare records. I can only compare ints, floats,"
              ++ " chars, strings, lists of comparable values, and tuples of comparable values."
              ++ " Check out") "comparing-records" "for ideas on how to proceed."
          ]

        T.Type _ name _ ->
          [ D.toSimpleHint <|
              "I do not know how to compare `" ++ name ++ "` values. I can only"
              ++ " compare ints, floats, chars, strings, lists of comparable values, and tuples"
              ++ " of comparable values."
          , D.reflowLink
              "Check out" "comparing-custom-types" "for ideas on how to proceed."
          ]

        _ ->
          [ D.toSimpleHint <|
              "I only know how to compare ints, floats, chars, strings, lists of"
              ++ " comparable values, and tuples of comparable values."
          ]

    T.Appendable ->
      [ D.toSimpleHint "I only know how to append strings and lists."
      ]

    T.CompAppend ->
      [ D.toSimpleHint "Only strings and lists are both comparable and appendable."
      ]

    T.Number ->
      let
        otherwise () =
          [ D.toFancyHint [d"Only",D.greenS "Int",d"and",D.greenS "Float",d"values",d"work",d"as",d"numbers."]
          ]
      in
      case tipe of
        T.Type home name _ -> if T.isString home name then
          case direction of
            T.Have ->
              [ D.toFancyHint [d"Try",d"using",D.greenS "String.fromInt",d"to",d"convert",d"it",d"to",d"a",d"string?"]
              ]

            T.Need ->
              [ D.toFancyHint [d"Try",d"using",D.greenS "String.toInt",d"to",d"convert",d"it",d"to",d"an",d"integer?"]
              ]
          else otherwise ()

        _ ->
          otherwise ()


badRigidSuper : T.Super -> String -> TList D.Doc
badRigidSuper super aThing =
  let
    (superType, manyThings) =
      case super of
        T.Number -> ("number", "ints AND floats")
        T.Comparable -> ("comparable", "ints, floats, chars, strings, lists, and tuples")
        T.Appendable -> ("appendable", "strings AND lists")
        T.CompAppend -> ("compappend", "strings AND lists")
  in
  [ D.toSimpleHint <|
      "The `" ++ superType ++ "` in your type annotation is saying that "
      ++ manyThings ++ " can flow through, but your code is saying it specifically wants "
      ++ aThing ++ ". Maybe change your type annotation to"
      ++ " be more specific? Maybe change the code to be more general?"
  , D.reflowLink "Read" "type-annotations" "for more advice!"
  ]


badFlexFlexSuper : T.Super -> T.Super -> TList D.Doc
badFlexFlexSuper s1 s2 =
  let
    likeThis super =
      case super of
        T.Number -> "a number"
        T.Comparable -> "comparable"
        T.CompAppend -> "a compappend"
        T.Appendable -> "appendable"
  in
    [ D.toSimpleHint <|
        "There are no values in Elm that are both "
        ++ likeThis s1 ++ " and " ++ likeThis s2 ++ "."
    ]



-- TO EXPR REPORT


toExprReport : Code.Source -> L.Localizer -> A.Region -> Category -> T.Type -> Expected T.Type -> Report.Report
toExprReport source localizer exprRegion category tipe expected =
  case expected of
    NoExpectation expectedType ->
      Report.Report "TYPE MISMATCH" exprRegion <|
        Code.toSnippet source exprRegion Nothing
          ( d"This expression is being used in an unexpected way:"
          , typeComparison localizer tipe expectedType
              (addCategory "It is" category)
              "But you are trying to use it as:"
              []
          )

    FromAnnotation name _ subContext expectedType ->
      let
        thing =
          case subContext of
            TypedIfBranch index   -> D.ordinal index ++ " branch of this `if` expression:"
            TypedCaseBranch index -> D.ordinal index ++ " branch of this `case` expression:"
            TypedBody             -> "body of the `" ++ name ++ "` definition:"

        itIs =
          case subContext of
            TypedIfBranch index   -> "The " ++ D.ordinal index ++ " branch is"
            TypedCaseBranch index -> "The " ++ D.ordinal index ++ " branch is"
            TypedBody             -> "The body is"
      in
      Report.Report "TYPE MISMATCH" exprRegion <|
        Code.toSnippet source exprRegion Nothing <|
          ( D.reflow ("Something is off with the " ++ thing)
          , typeComparison localizer tipe expectedType
              (addCategory itIs category)
              ("But the type annotation on `" ++ name ++ "` says it should be:")
              []
          )

    FromContext region context expectedType ->
      let
        mismatch (maybeHighlight, problem, (thisIs, insteadOf, furtherDetails)) =
          Report.Report "TYPE MISMATCH" exprRegion <|
            Code.toSnippet source region maybeHighlight
              ( D.reflow problem
              , typeComparison localizer tipe expectedType (addCategory thisIs category) insteadOf furtherDetails
              )

        badType (maybeHighlight, problem, (thisIs, furtherDetails)) =
          Report.Report "TYPE MISMATCH" exprRegion <|
            Code.toSnippet source region maybeHighlight
              ( D.reflow problem
              , loneType localizer tipe expectedType (D.reflow (addCategory thisIs category)) furtherDetails
              )

        custom maybeHighlight docPair =
          Report.Report "TYPE MISMATCH" exprRegion <|
            Code.toSnippet source region maybeHighlight docPair
      in
      case context of
        ListEntry index ->
          let ith = D.ordinal index in
          mismatch
          ( Just exprRegion
          , "The " ++ ith ++ " element of this list does not match all the previous elements:"
          , ("The " ++ ith ++ " element is"
          , "But all the previous elements in the list are:"
          , [ D.link "Hint"
                ("Everything in a list must be the same type of value. This way, we never"
                ++ " run into unexpected values partway through a List.map, List.foldl, etc. Read")
                "custom-types"
                "to learn how to “mix” types."
            ]
          ))

        Negate ->
          badType
          ( Just exprRegion
          , "I do not know how to negate this type of value:"
          , ("It is"
          , [ D.fillSep
                [d"But",d"I",d"only",d"now",d"how",d"to",d"negate"
                ,D.dullyellowS "Int",d"and",D.dullyellowS "Float",d"values."
                ]
            ]
          ))

        OpLeft op ->
          custom (Just exprRegion) <|
            opLeftToDocs localizer category op tipe expectedType

        OpRight op ->
          case opRightToDocs localizer category op tipe expectedType of
            EmphBoth details ->
              custom Nothing details

            EmphRight details ->
              custom (Just exprRegion) details

        IfCondition ->
          badType
          ( Just exprRegion
          , "This `if` condition does not evaluate to a boolean value, True or False."
          , ("It is"
          , [ D.fillSep [d"But",d"I",d"need",d"this",d"`if`",d"condition",d"to",d"be",d"a",D.dullyellowS "Bool",d"value."]
            ]
          ))

        IfBranch index ->
          let ith = D.ordinal index in
          mismatch
          ( Just exprRegion
          , "The " ++ ith ++ " branch of this `if` does not match all the previous branches:"
          , ("The " ++ ith ++ " branch is"
          , "But all the previous branches result in:"
          , [ D.link "Hint"
                ("All branches in an `if` must produce the same type of values. This way, no"
                ++ " matter which branch we take, the result is always a consistent shape. Read")
                "custom-types"
                "to learn how to “mix” types."
            ]
          ))

        CaseBranch index ->
          let ith = D.ordinal index in
          mismatch
          ( Just exprRegion
          , "The " ++ ith ++ " branch of this `case` does not match all the previous branches:"
          , ("The " ++ ith ++ " branch is"
          , "But all the previous branches result in:"
          , [ D.link "Hint"
                ("All branches in a `case` must produce the same type of values. This way, no"
                ++ " matter which branch we take, the result is always a consistent shape. Read")
                "custom-types"
                "to learn how to “mix” types."
            ]
          ))

        CallArity maybeFuncName numGivenArgs ->
          Report.Report "TOO MANY ARGS" exprRegion <|
          Code.toSnippet source region (Just exprRegion) <|
          case countArgs tipe of
            0 ->
              let
                thisValue =
                  case maybeFuncName of
                    NoName        -> "This value"
                    FuncName name -> "The `" ++ name ++ "` value"
                    CtorName name -> "The `" ++ name ++ "` value"
                    OpName op     -> "The (" ++ op ++ ") operator"
              in
              ( D.reflow <| thisValue ++ " is not a function, but it was given " ++ D.args numGivenArgs ++ "."
              , D.reflow <| "Are there any missing commas? Or missing parentheses?"
              )

            n ->
              let
                thisFunction =
                  case maybeFuncName of
                    NoName        -> "This function"
                    FuncName name -> "The `" ++ name ++ "` function"
                    CtorName name -> "The `" ++ name ++ "` constructor"
                    OpName op     -> "The (" ++ op ++ ") operator"
              in
              ( D.reflow <| thisFunction ++ " expects " ++ D.args n ++ ", but it got " ++ String.fromInt numGivenArgs ++ " instead."
              , D.reflow <| "Are there any missing commas? Or missing parentheses?"
              )

        CallArg maybeFuncName index ->
          let
            ith = D.ordinal index

            thisFunction =
              case maybeFuncName of
                NoName        -> "this function"
                FuncName name -> "`" ++ name ++ "`"
                CtorName name -> "`" ++ name ++ "`"
                OpName op     -> "(" ++ op ++ ")"
          in
          mismatch
          ( Just exprRegion
          , "The " ++ ith ++ " argument to " ++ thisFunction ++ " is not what I expect:"
          , ("This argument is"
          , "But " ++ thisFunction ++ " needs the " ++ ith ++ " argument to be:"
          ,
            if Index.toHuman index == 1 then
              []
            else
              [ D.toSimpleHint <|
                  "I always figure out the argument types from left to right. If an argument"
                  ++ " is acceptable, I assume it is “correct” and move on. So the problem may"
                  ++ " actually be in one of the previous arguments!"
              ]
          ))

        RecordAccess recordRegion maybeName fieldRegion field ->
          case T.iteratedDealias tipe of
            T.Record fields ext ->
              custom (Just fieldRegion)
                ( D.reflow <|
                    "This "
                    ++ MMaybe.maybe "" (\n -> "`" ++ n ++ "`") maybeName
                    ++ " record does not have a `" ++ field ++ "` field:"
                , case Suggest.sort field Tuple.first (Map.toList fields) of
                    [] ->
                      D.reflow "In fact, it is a record with NO fields!"

                    f::fs ->
                      D.stack
                        [ D.reflow <|
                            "This is usually a typo. Here are the "
                            ++ MMaybe.maybe "" (\n -> "`" ++ n ++ "`") maybeName
                            ++ " fields that are most similar:"
                        , toNearbyRecord localizer f fs ext
                        , D.fillSep
                            [d"So",d"maybe",D.dullyellow (D.fromName field)
                            ,d"should",d"be",da[D.green (D.fromName (Tuple.first f)), d"?"]
                            ]
                        ]
                )

            _ ->
              badType
              ( Just recordRegion
              , "This is not a record, so it has no fields to access!"
              , ("It is"
              , [ D.fillSep
                    [d"But",d"I",d"need",d"a",d"record",d"with",d"a"
                    ,D.dullyellow (D.fromName field),d"field!"
                    ]
                ]
              ))

        RecordUpdateKeys record expectedFields ->
          case T.iteratedDealias tipe of
            T.Record actualFields ext ->
              case Map.lookupMin (Map.difference expectedFields actualFields) of
                Nothing ->
                  mismatch
                  ( Nothing
                  , "Something is off with this record update:"
                  , ("The `" ++ record ++ "` record is"
                  , "But this update needs it to be compatable with:"
                  , [ D.reflow
                        ("Do you mind creating an <http://sscce.org/> that produces this error message and"
                        ++ " sharing it at <https://github.com/elm/error-message-catalog/issues> so we"
                        ++ " can try to give better advice here?")
                    ]
                  ))

                Just (field, Can.FieldUpdate fieldRegion _) ->
                  let
                    rStr = "`" ++ record ++ "`"
                    fStr = "`" ++ field ++ "`"
                  in
                  custom (Just fieldRegion)
                    ( D.reflow <|
                        "The " ++ rStr ++ " record does not have a " ++ fStr ++ " field:"
                    , case Suggest.sort field Tuple.first (Map.toList actualFields) of
                        [] ->
                          D.reflow <| "In fact, " ++ rStr ++ " is a record with NO fields!"

                        f::fs ->
                          D.stack
                            [ D.reflow <|
                                "This is usually a typo. Here are the " ++ rStr ++ " fields that are most similar:"
                            , toNearbyRecord localizer f fs ext
                            , D.fillSep
                                [d"So",d"maybe",D.dullyellow (D.fromName field)
                                ,d"should",d"be",da[D.green (D.fromName (Tuple.first f)), d"?"]
                                ]
                            ]
                    )

            _ ->
              badType
              ( Just exprRegion
              , "This is not a record, so it has no fields to update!"
              , ("It is"
              , [ D.reflow <| "But I need a record!"
                ]
              ))

        RecordUpdateValue field ->
          mismatch
          ( Just exprRegion
          , "I cannot update the `" ++ field ++ "` field like this:"
          , ("You are trying to update `" ++ field ++ "` to be"
          , "But it should be:"
          , [ D.toSimpleNote
                ("The record update syntax does not allow you to change the type of fields."
                ++ " You can achieve that with record constructors or the record literal syntax.")
            ]
          ))

        Destructure ->
          mismatch
          ( Nothing
          , "This definition is causing issues:"
          , ("You are defining"
          , "But then trying to destructure it as:"
          , []
          ))



-- HELPERS


countArgs : T.Type -> Int
countArgs tipe =
  case tipe of
    T.Lambda _ _ stuff ->
      1 + MList.length stuff

    _ ->
      0



-- FIELD NAME HELPERS


toNearbyRecord : L.Localizer -> (Name.Name, T.Type) -> TList (Name.Name, T.Type) -> T.Extension -> D.Doc
toNearbyRecord localizer f fs ext =
  D.indent 4 <|
    if MList.length fs <= 3 then
      RT.vrecord (MList.map (fieldToDocs localizer) (f::fs)) (extToDoc ext)
    else
      RT.vrecordSnippet (fieldToDocs localizer f) (MList.map (fieldToDocs localizer) (MList.take 3 fs))


fieldToDocs : L.Localizer -> (Name.Name, T.Type) -> (D.Doc, D.Doc)
fieldToDocs localizer (name, tipe) =
  ( D.fromName name
  , T.toDoc localizer RT.None tipe
  )


extToDoc : T.Extension -> Maybe D.Doc
extToDoc ext =
  case ext of
    T.Closed      -> Nothing
    T.FlexOpen  x -> Just (D.fromName x)
    T.RigidOpen x -> Just (D.fromName x)



-- OP LEFT


opLeftToDocs : L.Localizer -> Category -> Name.Name -> T.Type -> T.Type -> (D.Doc, D.Doc)
opLeftToDocs localizer category op tipe expected =
  case op of
    "+" ->
      if isString tipe then badStringAdd
      else if isList tipe then badListAdd localizer category "left" tipe expected
      else badMath localizer category "Addition" "left" "+" tipe expected []

    "*" ->
      if isList tipe then badListMul localizer category "left" tipe expected
      else badMath localizer category "Multiplication" "left" "*" tipe expected []

    "-"  -> badMath localizer category "Subtraction" "left" "-" tipe expected []
    "^"  -> badMath localizer category "Exponentiation" "left" "^" tipe expected []
    "/"  -> badFDiv localizer (d"left") tipe expected
    "//" -> badIDiv localizer (d"left") tipe expected
    "&&" -> badBool localizer (d"&&") (d"left") tipe expected
    "||" -> badBool localizer (d"||") (d"left") tipe expected
    "<"  -> badCompLeft localizer category "<" "left" tipe expected
    ">"  -> badCompLeft localizer category ">" "left" tipe expected
    "<=" -> badCompLeft localizer category "<=" "left" tipe expected
    ">=" -> badCompLeft localizer category ">=" "left" tipe expected

    "++" -> badAppendLeft localizer category tipe expected

    "<|" ->
      ( d"The left side of (<|) needs to be a function so I can pipe arguments to it!"
      , loneType localizer tipe expected
          (D.reflow (addCategory "I am seeing" category))
          [ D.reflow <| "This needs to be some kind of function though!"
          ]
      )

    _ ->
      ( D.reflow <|
          "The left argument of (" ++ op ++ ") is causing problems:"
      , typeComparison localizer tipe expected
          (addCategory "The left argument is" category)
          ("But (" ++ op ++ ") needs the left argument to be:")
          []
      )



-- OP RIGHT


type RightDocs
  = EmphBoth (D.Doc, D.Doc)
  | EmphRight (D.Doc, D.Doc)


opRightToDocs : L.Localizer -> Category -> Name.Name -> T.Type -> T.Type -> RightDocs
opRightToDocs localizer category op tipe expected =
  case op of
    "+" ->
      if isFloat expected && isInt tipe then badCast op FloatInt
      else if isInt expected && isFloat tipe then badCast op IntFloat
      else if isString tipe then EmphRight <| badStringAdd
      else if isList tipe   then EmphRight <| badListAdd localizer category "right" tipe expected
      else EmphRight <| badMath localizer category "Addition" "right" "+" tipe expected []

    "*" ->
      if isFloat expected && isInt tipe then badCast op FloatInt
      else if isInt expected && isFloat tipe then badCast op IntFloat
      else if isList tipe then EmphRight <| badListMul localizer category "right" tipe expected
      else EmphRight <| badMath localizer category "Multiplication" "right" "*" tipe expected []

    "-" ->
      if isFloat expected && isInt tipe then badCast op FloatInt
      else if isInt expected && isFloat tipe then badCast op IntFloat
      else
        EmphRight <| badMath localizer category "Subtraction" "right" "-" tipe expected []

    "^" ->
      if isFloat expected && isInt tipe then badCast op FloatInt
      else if isInt expected && isFloat tipe then badCast op IntFloat
      else
        EmphRight <| badMath localizer category "Exponentiation" "right" "^" tipe expected []

    "/"  -> EmphRight <| badFDiv localizer (d"right") tipe expected
    "//" -> EmphRight <| badIDiv localizer (d"right") tipe expected
    "&&" -> EmphRight <| badBool localizer (d"&&") (d"right") tipe expected
    "||" -> EmphRight <| badBool localizer (d"||") (d"right") tipe expected
    "<"  -> badCompRight localizer "<" tipe expected
    ">"  -> badCompRight localizer ">" tipe expected
    "<=" -> badCompRight localizer "<=" tipe expected
    ">=" -> badCompRight localizer ">=" tipe expected
    "==" -> badEquality localizer "==" tipe expected
    "/=" -> badEquality localizer "/=" tipe expected

    "::" -> badConsRight localizer category tipe expected
    "++" -> badAppendRight localizer category tipe expected

    "<|" ->
      EmphRight
        ( D.reflow <| "I cannot send this through the (<|) pipe:"
        , typeComparison localizer tipe expected
            "The argument is:"
            "But (<|) is piping it to a function that expects:"
            []
        )

    "|>" ->
      case (tipe, expected) of
        (T.Lambda expectedArgType _ _, T.Lambda argType _ _) ->
          EmphRight
            ( D.reflow <| "This function cannot handle the argument sent through the (|>) pipe:"
            , typeComparison localizer argType expectedArgType
                "The argument is:"
                "But (|>) is piping it to a function that expects:"
                []
            )

        _ ->
          EmphRight
            ( D.reflow <| "The right side of (|>) needs to be a function so I can pipe arguments to it!"
            , loneType localizer tipe expected
                (D.reflow (addCategory "But instead of a function, I am seeing" category))
                []
            )

    _ ->
      badOpRightFallback localizer category op tipe expected


badOpRightFallback : L.Localizer -> Category -> Name.Name -> T.Type -> T.Type -> RightDocs
badOpRightFallback localizer category op tipe expected =
  EmphRight
    ( D.reflow <|
        "The right argument of (" ++ op ++ ") is causing problems."
    , typeComparison localizer tipe expected
        (addCategory "The right argument is" category)
        ("But (" ++ op ++ ") needs the right argument to be:")
        [ D.toSimpleHint <|
            "With operators like (" ++ op ++ ") I always check the left"
            ++ " side first. If it seems fine, I assume it is correct and check the right"
            ++ " side. So the problem may be in how the left and right arguments interact!"
        ]
    )


isInt : T.Type -> Bool
isInt tipe =
  case tipe of
    T.Type home name [] ->
      T.isInt home name

    _ ->
      False


isFloat : T.Type -> Bool
isFloat tipe =
  case tipe of
    T.Type home name [] ->
      T.isFloat home name

    _ ->
      False


isString : T.Type -> Bool
isString tipe =
  case tipe of
    T.Type home name [] ->
      T.isString home name

    _ ->
      False


isList : T.Type -> Bool
isList tipe =
  case tipe of
    T.Type home name [_] ->
      T.isList home name

    _ ->
      False



-- BAD CONS


badConsRight : L.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badConsRight localizer category tipe expected =
  let
    otherwise1 () =
      EmphRight
        ( D.reflow "The (::) operator can only add elements onto lists."
        , loneType localizer tipe expected
            (D.reflow (addCategory "The right side is" category))
            [D.fillSep [d"But",d"(::)",d"needs",d"a",D.dullyellowS "List",d"on",d"the",d"right."]
            ]
        )
  in
  case tipe of
    T.Type home1 name1 [actualElement] -> if T.isList home1 name1 then
      let
        otherwise2 () =
          badOpRightFallback localizer category "::" tipe expected
      in
      case expected of
        T.Type home2 name2 [expectedElement] -> if T.isList home2 name2 then
          EmphBoth
            ( D.reflow "I am having trouble with this (::) operator:"
            , typeComparison localizer expectedElement actualElement
                "The left side of (::) is:"
                "But you are trying to put that into a list filled with:"
                ( let
                    otherwise3 () =
                      [ D.reflow
                          "Lists need ALL elements to be the same type though."
                      ]
                  in
                  case expectedElement of
                    T.Type home name [_] -> if T.isList home name then
                      [ D.toSimpleHint
                          ("Are you trying to append two lists? The (++) operator"
                          ++ " appends lists, whereas the (::) operator is only for"
                          ++ " adding ONE element to a list.")
                      ]
                      else otherwise3 ()

                    _ ->
                      otherwise3 ()
                )
            )
          else otherwise2 ()

        _ ->
          otherwise2 ()
      else otherwise1 ()

    _ ->
      otherwise1 ()


-- BAD APPEND


type AppendType
  = ANumber D.Doc D.Doc
  | AString
  | AList
  | AOther


toAppendType : T.Type -> AppendType
toAppendType tipe =
  case tipe of
    T.Type home name _ ->
      if T.isInt    home name then ANumber (d"Int") (d"String.fromInt")
      else if T.isFloat  home name then ANumber (d"Float") (d"String.fromFloat")
      else if T.isString home name then AString
      else if T.isList   home name then AList
      else AOther

    T.FlexSuper T.Number _ -> ANumber (d"number") (d"String.fromInt")

    _ -> AOther


badAppendLeft : L.Localizer -> Category -> T.Type -> T.Type -> (D.Doc, D.Doc)
badAppendLeft localizer category tipe expected =
  case toAppendType tipe of
    ANumber thing stringFromThing ->
      ( D.fillSep
          [d"The",d"(++)",d"operator",d"can",d"append",d"List",d"and",d"String"
          ,d"values,",d"but",d"not",D.dullyellow thing,d"values",d"like",d"this:"
          ]
      , D.fillSep
          [d"Try",d"using",D.green stringFromThing,d"to",d"turn",d"it",d"into",d"a",d"string?"
          ,d"Or",d"put",d"it",d"in",d"[]",d"to",d"make",d"it",d"a",d"list?"
          ,d"Or",d"switch",d"to",d"the",d"(::)",d"operator?"
          ]
      )

    _ ->
      ( D.reflow <|
          "The (++) operator cannot append this type of value:"
      , loneType localizer tipe expected
          (D.reflow (addCategory "I am seeing" category))
          [ D.fillSep
              [d"But",d"the",d"(++)",d"operator",d"is",d"only",d"for",d"appending"
              ,D.dullyellowS "List",d"and",D.dullyellowS "String",d"values."
              ,d"Maybe",d"put",d"this",d"value",d"in",d"[]",d"to",d"make",d"it",d"a",d"list?"
              ]
          ]
      )


badAppendRight : L.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badAppendRight localizer category tipe expected =
  case (toAppendType expected, toAppendType tipe) of
    (AString, ANumber thing stringFromThing) ->
      EmphRight
        ( D.fillSep
            [d"I",d"thought",d"I",d"was",d"appending",D.dullyellowS "String",d"values",d"here,"
            ,d"not",D.dullyellow thing,d"values",d"like",d"this:"
            ]
        , D.fillSep
            [d"Try",d"using",D.green stringFromThing,d"to",d"turn",d"it",d"into",d"a",d"string?"]
        )

    (AList, ANumber thing _) ->
      EmphRight
        ( D.fillSep
            [d"I",d"thought",d"I",d"was",d"appending",D.dullyellowS "List",d"values",d"here,"
            ,d"not",D.dullyellow thing,d"values",d"like",d"this:"
            ]
        , D.reflow "Try putting it in [] to make it a list?"
        )

    (AString, AList) ->
      EmphBoth
        ( D.reflow <|
            "The (++) operator needs the same type of value on both sides:"
        , D.fillSep
            [d"I",d"see",d"a",D.dullyellowS "String",d"on",d"the",d"left",d"and",d"a"
            ,D.dullyellowS "List",d"on",d"the",d"right.",d"Which",d"should",d"it",d"be?"
            ,d"Does",d"the",d"string",d"need",d"[]",d"around",d"it",d"to",d"become",d"a",d"list?"
            ]
        )

    (AList, AString) ->
      EmphBoth
        ( D.reflow <|
            "The (++) operator needs the same type of value on both sides:"
        , D.fillSep
            [d"I",d"see",d"a",D.dullyellowS "List",d"on",d"the",d"left",d"and",d"a"
            ,D.dullyellowS "String",d"on",d"the",d"right.",d"Which",d"should",d"it",d"be?"
            ,d"Does",d"the",d"string",d"need",d"[]",d"around",d"it",d"to",d"become",d"a",d"list?"
            ]
        )

    _ ->
      EmphBoth
        ( D.reflow <|
            "The (++) operator cannot append these two values:"
        , typeComparison localizer expected tipe
            "I already figured out that the left side of (++) is:"
            (addCategory "But this clashes with the right side, which is" category)
            []
        )



-- BAD MATH


type ThisThenThat = FloatInt | IntFloat


badCast : Name.Name -> ThisThenThat -> RightDocs
badCast op thisThenThat =
  EmphBoth
    ( D.reflow <|
        "I need both sides of (" ++ op ++ ") to be the exact same type. Both Int or both Float."
    , let
        anInt = [d"an", D.dullyellowS "Int"]
        aFloat = [d"a", D.dullyellowS "Float"]
        toFloat = D.greenS "toFloat"
        round = D.greenS "round"
      in
      case thisThenThat of
        FloatInt ->
          badCastHelp aFloat anInt round toFloat

        IntFloat ->
          badCastHelp anInt aFloat toFloat round
    )


badCastHelp : TList D.Doc -> TList D.Doc -> D.Doc -> D.Doc -> D.Doc
badCastHelp anInt aFloat toFloat round =
  D.stack
    [ D.fillSep <|
        [d"But",d"I",d"see"]
        ++ anInt
        ++ [d"on",d"the",d"left",d"and"]
        ++ aFloat
        ++ [d"on",d"the",d"right."]
    , D.fillSep
        [d"Use",toFloat,d"on",d"the",d"left",d"(or",round,d"on"
        ,d"the",d"right)",d"to",d"make",d"both",d"sides",d"match!"
        ]
    , D.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
    ]


badStringAdd : (D.Doc, D.Doc)
badStringAdd =
  (
    D.fillSep [d"I",d"cannot",d"do",d"addition",d"with",D.dullyellowS "String",d"values",d"like",d"this",d"one:"]
  ,
    D.stack
      [ D.fillSep
          [d"The",d"(+)",d"operator",d"only",d"works",d"with",D.dullyellowS "Int",d"and",D.dullyellowS "Float",d"values."
          ]
      , D.toFancyHint
          [d"Switch",d"to",d"the",D.greenS "(++)",d"operator",d"to",d"append",d"strings!"
          ]
      ]
  )


badListAdd : L.Localizer -> Category -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badListAdd localizer category direction tipe expected =
  (
    d"I cannot do addition with lists:"
  ,
    loneType localizer tipe expected
      (D.reflow (addCategory ("The " ++ direction ++ " side of (+) is") category))
      [ D.fillSep
          [d"But",d"(+)",d"only",d"works",d"with",D.dullyellowS "Int",d"and",D.dullyellowS "Float",d"values."
          ]
      , D.toFancyHint
          [d"Switch",d"to",d"the",D.greenS "(++)",d"operator",d"to",d"append",d"lists!"
          ]
      ]
  )


badListMul : L.Localizer -> Category -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badListMul localizer category direction tipe expected =
  badMath localizer category "Multiplication" direction "*" tipe expected
    [
      D.toFancyHint
        [ d"Maybe", d"you", d"want"
        , D.greenS "List.repeat"
        , d"to", d"build",d"a",d"list",d"of",d"repeated",d"values?"
        ]
    ]


badMath : L.Localizer -> Category -> String -> String -> String -> T.Type -> T.Type -> TList D.Doc -> (D.Doc, D.Doc)
badMath localizer category operation direction op tipe expected otherHints =
  (
    D.reflow <|
      operation ++ " does not work with this value:"
  ,
    loneType localizer tipe expected
      (D.reflow (addCategory ("The " ++ direction ++ " side of (" ++ op ++ ") is") category))
      ( D.fillSep
          [d"But",da[d"(", D.fromChars op, d")"],d"only",d"works",d"with"
          ,D.dullyellowS "Int",d"and",D.dullyellowS "Float",d"values."
          ]
        :: otherHints
      )
  )


badFDiv : L.Localizer -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
badFDiv localizer direction tipe expected =
  (
    D.reflow <|
      "The (/) operator is specifically for floating-point division:"
  ,
    if isInt tipe then
      D.stack
        [ D.fillSep
            [d"The",direction,d"side",d"of",d"(/)",d"must",d"be",d"a"
            ,da[D.dullyellowS "Float", d","]
            ,d"but",d"I",d"am",d"seeing",d"an",da[D.dullyellowS "Int", d"."]
            ,d"I",d"recommend:"
            ]
        , D.vcat
            [ da[D.greenS "toFloat", d" for explicit conversions     ", D.blackS "(toFloat 5 / 2) == 2.5"]
            , da[D.greenS "(//)   ", d" for integer division         ", D.blackS "(5 // 2)        == 2"]
            ]
        , D.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
        ]

    else
      loneType localizer tipe expected
        (D.fillSep
          [d"The",direction,d"side",d"of",d"(/)",d"must",d"be",d"a"
          ,da[D.dullyellowS "Float", d","],d"but",d"instead",d"I",d"am",d"seeing:"
          ]
        )
        []
  )


badIDiv : L.Localizer -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
badIDiv localizer direction tipe expected =
  (
    D.reflow <|
      "The (//) operator is specifically for integer division:"
  ,
    if isFloat tipe then
      D.stack
        [ D.fillSep
            [d"The",direction,d"side",d"of",d"(//)",d"must",d"be",d"an"
            ,da[D.dullyellowS "Int", d","]
            ,d"but",d"I",d"am",d"seeing",d"a",da[D.dullyellowS "Float", d"."]
            ,d"I",d"recommend",d"doing",d"the",d"conversion",d"explicitly"
            ,d"with",d"one",d"of",d"these",d"functions:"
            ]
        , D.vcat
            [ da[D.greenS "round", d" 3.5     == 4"]
            , da[D.greenS "floor", d" 3.5     == 3"]
            , da[D.greenS "ceiling", d" 3.5   == 4"]
            , da[D.greenS "truncate", d" 3.5  == 3"]
            ]
        , D.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
        ]
    else
      loneType localizer tipe expected
        ( D.fillSep
            [d"The",direction,d"side",d"of",d"(//)",d"must",d"be",d"an"
            ,da[D.dullyellowS "Int", d","],d"but",d"instead",d"I",d"am",d"seeing:"
            ]
        )
        []
  )



-- BAD BOOLS


badBool : L.Localizer -> D.Doc -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
badBool localizer op direction tipe expected =
  (
    D.reflow <|
      "I am struggling with this boolean operation:"
  ,
    loneType localizer tipe expected
      ( D.fillSep
          [d"Both",d"sides",d"of",da[d"(", op, d")"],d"must",d"be"
          ,D.dullyellowS "Bool",d"values,",d"but",d"the",direction,d"side",d"is:"
          ]
      )
      []
  )



-- BAD COMPARISON


badCompLeft : L.Localizer -> Category -> String -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badCompLeft localizer category op direction tipe expected =
  (
    D.reflow <|
      "I cannot do a comparison with this value:"
  ,
    loneType localizer tipe expected
      (D.reflow (addCategory ("The " ++ direction ++ " side of (" ++ op ++ ") is") category))
      [ D.fillSep
          [d"But",da[d"(", D.fromChars op, d")"],d"only",d"works",d"on"
          ,da[D.dullyellowS "Int", d","]
          ,da[D.dullyellowS "Float", d","]
          ,da[D.dullyellowS "Char", d","]
          ,d"and"
          ,D.dullyellowS "String"
          ,d"values.",d"It",d"can",d"work",d"on",d"lists",d"and",d"tuples"
          ,d"of",d"comparable",d"values",d"as",d"well,",d"but",d"it",d"is"
          ,d"usually",d"better",d"to",d"find",d"a",d"different",d"path."
          ]
      ]
  )


badCompRight : L.Localizer -> String -> T.Type -> T.Type -> RightDocs
badCompRight localizer op tipe expected =
  EmphBoth
    (
      D.reflow <|
        "I need both sides of (" ++ op ++ ") to be the same type:"
    ,
      typeComparison localizer expected tipe
        ("The left side of (" ++ op ++ ") is:")
        "But the right side is:"
        [ D.reflow <|
            "I cannot compare different types though! Which side of (" ++ op ++ ") is the problem?"
        ]
    )



-- BAD EQUALITY


badEquality : L.Localizer -> String -> T.Type -> T.Type -> RightDocs
badEquality localizer op tipe expected =
  EmphBoth
    (
      D.reflow <|
        "I need both sides of (" ++ op ++ ") to be the same type:"
    ,
      typeComparison localizer expected tipe
        ("The left side of (" ++ op ++ ") is:")
        "But the right side is:"
        [ if isFloat tipe || isFloat expected then
            D.toSimpleNote <|
              "Equality on floats is not 100% reliable due to the design of IEEE 754. I"
              ++ " recommend a check like (abs (x - y) < 0.0001) instead."
          else
            D.reflow  "Different types can never be equal though! Which side is messed up?"
        ]
    )



-- INFINITE TYPES


toInfiniteReport : Code.Source -> L.Localizer -> A.Region -> Name.Name -> T.Type -> Report.Report
toInfiniteReport source localizer region name overallType =
  Report.Report "INFINITE TYPE" region <|
    Code.toSnippet source region Nothing
      (
        D.reflow <|
          "I am inferring a weird self-referential type for " ++ name ++ ":"
      ,
        D.stack
          [ D.reflow <|
              "Here is my best effort at writing down the type. You will see ∞ for"
              ++ " parts of the type that repeat something already printed out infinitely."
          , D.indent 4 (D.dullyellow (T.toDoc localizer RT.None overallType))
          , D.reflowLink
              "Staring at this type is usually not so helpful, so I recommend reading the hints at"
              "infinite-type"
              "to get unstuck!"
          ]
      )
