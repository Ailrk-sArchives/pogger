{-
  AST for scheme ir. The indentation based layer will be first
  preprocessed to s-expression and be compiled to this ast.
-}
module AST where

import           Prettyprinter

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


instance Pretty PoggerVal where
  pretty (Atom    str  ) = pretty str
  pretty (List    xs   ) = prettyList xs
  pretty (Integer int  ) = pretty int
  pretty (Float   float) = pretty float
  pretty (Rational de d) = pretty de <+> pretty "/" <+> pretty d
  pretty (Complex  r  i) = pretty r <+> pretty "+" <+> pretty i <+> pretty "i"
  pretty (Bool b       ) = if b then pretty "#t" else pretty "#f"
  pretty (Char c       ) = pretty $ "\\#" ++ [c]

instance Show PoggerVal where
  show = show . pretty
