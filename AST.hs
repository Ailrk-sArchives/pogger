module AST where


data SchemeVal = Atom String
               | List [SchemeVal]
               | DottedList [SchemeVal] SchemeVal
               | Number Double
               | String String
               | Bool Bool
               deriving (Show, Eq)
