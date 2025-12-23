module Compiler.Reporting.Warning exposing
    ( Context(..)
    , Warning(..)
    )



-- ALL POSSIBLE WARNINGS


type Warning
    = UnusedVariable
    | MissingTypeAnnotation


type Context
    = Def
    | Pattern
