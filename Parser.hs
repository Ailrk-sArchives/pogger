module Parser where


import           Numeric
import           Data.Char                      ( digitToInt )
import           Text.Parsec
import           Text.Parsec.Expr              as Ex
import           Text.Parsec.String             ( Parser )
import           Text.Parsec.Token             as Tok
import           AST
import           Debug.Trace


-- | Non alpha numeric characters
schemeSymbol :: Parser Char
schemeSymbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- | R5RS string internal escape: \" \n \r \t \\
schemeString :: Parser SchemeVal
schemeString = do
  char '"'
  x <- many escapedChar
  char '"'
  return $ String x
 where
  escapedChar =
    escape '"'
      <|> escape '\\'
      <|> escape '\n'
      <|> escape '\n'
      <|> escape '\r'
      <|> escape '\t'
      <|> noneOf "\""
  escape :: Char -> Parser Char
  escape c = try $ do
    char '\\'
    char c

schemeAtom :: Parser SchemeVal
schemeAtom = do
  h  <- letter <|> schemeSymbol
  hs <- many $ letter <|> digit <|> schemeSymbol
  let atom = h : hs
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

{- | conform R5S5 bases.
Support both the integer and floating point form of
hexdecimal (#x), binary (#d), oct (#o).
-}
schemeNumber :: Parser SchemeVal
schemeNumber = Number <$> (try dec <|> try hex <|> try oct <|> try bin)
 where
  sign = (string "+" >> return "") <|> string "-" <|> return ""
  dot  = string "."
  getPart :: (Eq a, Num a) => ReadS a -> String -> String -> a
  getPart rs sign | sign == "-" = negate . pick
                  | otherwise   = pick
    where pick = fst . head . rs
  prefixedFormatToDecimal
    :: String -> ReadS Double -> Parser String -> Parser Double
  prefixedFormatToDecimal pstr rs num = try double <|> try int
   where
    prefix = string pstr
    base = case pstr of
             "#x" -> 16
             "#o" -> 8
             "#b" -> 2
    int    = prefix >> sign >>= \s -> getPart rs s <$> num
    double = prefix >> sign >>= \s -> do
      integral   <- show . floor . getPart rs s <$> num >> dot
      fractional <- show . floor . getPart rs s <$> num
      let
          full :: Double
          full = read (integral <> fractional)
      return undefined
  hex = prefixedFormatToDecimal "#x" readHex (many $ digit <|> upper)
  oct = prefixedFormatToDecimal "#o" readOct (many $ oneOf "01234567")
  bin = prefixedFormatToDecimal "#b" readBin (many $ oneOf "01")
    where readBin = readInt 2 (`elem` "01") digitToInt
  dec = try double <|> try int
   where
    int    = read <$> sign <> num
    double = read <$> sign <> num <> dot <> num
    num    = many1 digit

schemeExpr :: Parser SchemeVal
schemeExpr = schemeAtom <|> schemeNumber <|> schemeString

readExpr :: String -> String
readExpr input = case parse schemeExpr "lisp" input of
  Left  err -> "No match " ++ show input
  Right val -> show val

