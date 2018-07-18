module Wire where

import AST.Source (VarType(..))
import AST.Valid
import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N

modify valid = valid
  { _decls = [ At
               ( Region
                 { _start = Position
                   { _line   = 8
                   , _column = 1
                   }
                 , _end   = Position
                   { _line   = 9
                   , _column = 9
                   }
                 }
               )
               ( Decl
                 ( At
                   ( Region
                     { _start = Position
                       { _line   = 8
                       , _column = 1
                       }
                     , _end   = Position
                       { _line   = 8
                       , _column = 5
                       }
                     }
                   )
                   (N.fromString "asdf")
                 )
                 []
                 ( At
                   ( Region
                     { _start = Position
                       { _line   = 9
                       , _column = 5
                       }
                     , _end   = Position
                       { _line   = 9
                       , _column = 9
                       }
                     }
                   )
                   (Var Value (N.fromString "blah"))
                 )
                 Nothing
               )
             , At
               ( Region
                 { _start = Position
                   { _line   = 4
                   , _column = 1
                   }
                 , _end   = Position
                   { _line   = 5
                   , _column = 6
                   }
                 }
               )
               ( Decl
                 ( At
                   ( Region
                     { _start = Position
                       { _line   = 1
                       , _column = 1
                       }
                     , _end   = Position
                       { _line   = 1
                       , _column = 1
                       }
                     }
                   )
                   (N.fromString "blah")
                 )
                 []
                 ( At
                   ( Region
                     { _start = Position
                       { _line   = 1
                       , _column = 1
                       }
                     , _end   = Position
                       { _line   = 1
                       , _column = 1
                       }
                     }
                   )
                   (Int 2)
                 )
                 Nothing
               )
             ]
  }
