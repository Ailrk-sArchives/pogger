module Exception where

import AST
import Parser

data ListpError
    = NumArgs Integer [SchemeVal]
    | TypeMisMatch String SchemeVal
    | Parser ParseError
    | BadSpecialForm String SchemeVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

