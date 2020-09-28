module AST where


data PoggerVal = Atom String
               | List [PoggerVal]
               | DottedList [PoggerVal] PoggerVal
               | Integer Integer
               | Float Double
               | Rational Integer Integer
               | Complex Double Double
               | String String
               | Bool Bool
               | Char Char
               deriving (Eq)

showPoggerVal :: PoggerVal -> String
showPoggerVal = undefined

instance  Show PoggerVal where
  show = showPoggerVal
