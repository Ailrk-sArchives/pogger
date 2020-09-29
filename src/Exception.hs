module Exception where

import AST
import Parser.Error

data PoggerError
    = NumArgs Integer [PoggerVal]
    | TypeMisMatch String PoggerVal
    | Parser ParseError
    | BadSpecialForm String PoggerVal
    | NotFunction String String
    | UnboundVar String String
    | Default String
