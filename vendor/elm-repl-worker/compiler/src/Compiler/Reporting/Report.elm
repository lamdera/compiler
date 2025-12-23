module Compiler.Reporting.Report exposing (Report(..))

import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D



-- BUILD REPORTS


type Report
    = Report
        --{ title : String
        --, region : A.Region
        --, message : D.Doc
        --}
        String
        A.Region
        D.Doc
