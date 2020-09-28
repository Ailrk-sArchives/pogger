module AST where


data SchemeVal = Atom String
               | List [SchemeVal]
               | DottedList [SchemeVal] SchemeVal
               | Integer Integer
               | Float Double
               | Rational Integer Integer
               | Complex Double Double
               | String String
               | Bool Bool
               | Char Char
               deriving (Eq)

showSchemeVal :: SchemeVal -> String
showSchemeVal = undefined

instance  Show SchemeVal where
  show = showSchemeVal
