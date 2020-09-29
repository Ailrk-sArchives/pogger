module Parser.Error where

data ParseError
  = UnknownSymbol String
  | BlockMismatch String
  deriving Show
