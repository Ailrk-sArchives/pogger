module Exception where

import           AST
import           Control.Monad.Except
import           Text.Parsec                    ( ParseError )

data PoggerError
    = NumArgs Integer [PoggerVal]
    | TypeMisMatch String PoggerVal
    | ParserError ParseError
    | BadSpecialForm String PoggerVal
    | NotFunction String String
    | UnboundVar String String
    | Default String


showPoggerError :: PoggerError -> String
showPoggerError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; Found " ++ show found
showPoggerError (TypeMisMatch expected found) =
  "Invalid Type: expected " ++ expected ++ ", found " ++ show found
showPoggerError (ParserError parseError) = "Parse error at " ++ show parseError
showPoggerError (BadSpecialForm msg form      ) = msg ++ ": " ++ show form
showPoggerError (NotFunction    msg func      ) = msg ++ ": " ++ show func
showPoggerError (UnboundVar     msg identifier) = msg ++ ": " ++ identifier
showPoggerError (Default str                  ) = str


instance Show PoggerError where
  show = showPoggerError


type ThrowsError = Either PoggerError

-- catch throws error
trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

-- extract value from Right.
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _           = error "Can't extract value from an error"
