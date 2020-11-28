{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}

{-
  AST for scheme ir. The indentation based layer will be first
  preprocessed to s-expression and be compiled to this ast.
-}
module AST where

import           Prettyprinter

--data PoggerVal                deriving (Eq)

data PoggerVal where
  Atom :: String -> PoggerVal
  List :: [PoggerVal] -> PoggerVal
  DottedList :: [PoggerVal] -> PoggerVal -> PoggerVal
  String :: String -> PoggerVal
  Bool :: Bool -> PoggerVal
  Char :: Char -> PoggerVal
  Number :: PoggerNum -> PoggerVal
  deriving stock Eq


data PoggerNum where
  Integer :: Integer -> PoggerNum
  Real :: Double -> PoggerNum
  Rational :: Integer -> Integer -> PoggerNum
  Complex :: Double  -> Double -> PoggerNum
  deriving stock (Eq, Show)

instance Pretty PoggerVal where
  pretty (Atom    str  )          = pretty str
  pretty (List    xs   )          = prettyList xs
  pretty (Number (Integer int))   = pretty int
  pretty (Number (Real float))    = pretty float
  pretty (Number (Rational de d)) = pretty (show de <> "/" <> show d)
  pretty (Number (Complex  r  i)) = pretty (show r <> "+" <> show i <> "i")
  pretty (Bool b       )          = if b then pretty "#t" else pretty "#f"
  pretty (Char c       )          = pretty $ "\\#" ++ [c]
  pretty (String s)               = pretty s

instance Show PoggerVal where
  show = show . pretty
