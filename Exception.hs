module Exception where

import AST
import Parser

data ListpError
    = NumArgs Integer [PoggerVal]
    | TypeMisMatch String PoggerVal
    | Parser ParseError
    | BadSpecialForm String PoggerVal
    | NotFunction String String
    | UnboundVar String String
    | Default String
