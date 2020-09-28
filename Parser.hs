module Parser where


import           Numeric
import           Data.Char                      ( digitToInt )
import           Data.Maybe
import qualified Data.Map                      as M
import           Text.Parsec
import           Text.Parsec.Expr              as Ex
import           Text.Parsec.String             ( Parser )
import           AST
import           Debug.Trace

data ParseError
  = UnknownSymbol String
  | BlockMismatch String
  deriving Show

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

-- | Atom are alphanumeric followed alphanumerics, symbols and digits.
schemeAtom :: Parser SchemeVal
schemeAtom = do
  h  <- letter <|> schemeSymbol
  hs <- many $ letter <|> digit <|> schemeSymbol
  let atom = h : hs
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

{- | conform R5S5 number tower.
Support both the integer and floating point form of
hexdecimal (#x), binary (#d), oct (#o).
-}
schemeFloat :: Parser SchemeVal
schemeFloat =
  Float <$> (try decFloat <|> try hexFloat <|> try octFloat <|> try binFloat)

schemeInteger :: Parser SchemeVal
schemeInteger =
  Integer
    <$> (try decInteger <|> try hexInteger <|> try octInteger <|> try binInteger
        )

schemeComplex :: Parser SchemeVal
schemeComplex = sign >>= \s -> do
  real <- (try decFloat <|> (fromIntegral <$> decInteger))
    >>= \r -> return $ if s == "-" then negate r else r
  char '+'
  img <- try decFloat <|> try (fromIntegral <$> decInteger)
  char 'i'
  return $ Complex real img

schemeRational :: Parser SchemeVal
schemeRational = sign >>= \s -> do
  denominator <- decInteger >>= \d -> return $ if s == "-" then negate d else d
  char '/'
  dvisor <- decInteger
  return $ Rational denominator dvisor

hexInteger = prefixedFormatToInteger "#x" readHex (many $ digit <|> upper)
octInteger = prefixedFormatToInteger "#o" readOct (many $ oneOf "01234567")
binInteger = prefixedFormatToInteger "#b" readBin (many $ oneOf "01")
  where readBin = readInt 2 (`elem` "01") digitToInt

hexFloat = prefixedFormatToFloat "#x" readHex (many $ digit <|> upper)
octFloat = prefixedFormatToFloat "#o" readOct (many $ oneOf "01234567")
binFloat = prefixedFormatToFloat "#b" readBin (many $ oneOf "01")
  where readBin = readInt 2 (`elem` "01") digitToInt

decFloat :: Parser Double
decFloat = let num = many1 digit in read <$> sign <> num <> dot <> num

decInteger :: Parser Integer
decInteger = let num = many1 digit in read <$> sign <> num

prefixedFormatToFloat
  :: String -> ReadS Double -> Parser String -> Parser Double
prefixedFormatToFloat pstr rs num = try double <|> try int
 where
  prefix = string pstr
  base   = case pstr of
    "#x" -> 16
    "#o" -> 8
    "#b" -> 2
  int    = prefix >> sign >>= \s -> getPart rs s <$> num
  double = prefix >> sign >>= \s -> do
    integral <- num
    dot
    fractional <- num
    let combined = (integral <> fractional)
    let d        = base ^ length fractional
    let r        = getPart rs s combined
    return $ (r / d)

prefixedFormatToInteger
  :: String -> ReadS Integer -> Parser String -> Parser Integer
prefixedFormatToInteger pstr rs num = prefix >> sign >>= \s ->
  getPart rs s <$> num
  where prefix = string pstr

getPart :: (Eq a, Num a) => ReadS a -> String -> String -> a
getPart rs sign | sign == "-" = negate . pick
                | otherwise   = pick
  where pick = fst . head . rs

sign :: Parser String
sign = (string "+" >> return "") <|> string "-" <|> return ""

dot :: Parser String
dot = string "."

{- | Parse scheme characeter
\#<char> or \#<char name>
Characeters in \# are self-evaluating, no need to quote them.
A character must end with a space.
-}
schemeChar :: Parser SchemeVal
schemeChar = do
  char '\\' >> char '#'
  n <- do
    let ss = string <$> M.keys charNames
    try (lkCharNames <$> (choice ss)) <|> anyChar
  many1 space
  return . Char $ n
 where
  lkCharNames = fromJust . flip M.lookup charNames
  charNames   = M.fromList
    [ ("space"    , ' ')
    , ("newline"  , '\n')
    , ("tab"      , '\t')
    , ("linefeed" , ' ')
    , ("page"     , ' ')
    , ("backspace", ' ')
    ]

-- | Parse scheme list
schemeList :: Parser SchemeVal
schemeList = List <$> sepBy schemeExpr spaces

-- | Parse dotted scheme list
schemeDottedList :: Parser SchemeVal
schemeDottedList = do
  xs <- endBy schemeExpr spaces
  x  <- char '.' >> spaces >> schemeExpr
  return $ DottedList xs x

schemeQuoted :: Parser SchemeVal
schemeQuoted = do
  char '\''
  x <- schemeExpr
  return $ List [Atom "quote", x]

-- | `(a b ,c) => ((quote a b) c)
-- TODO unimplemented
schemeQuasiQuoted :: Parser SchemeVal
schemeQuasiQuoted = do
  char '`'
  x <- schemeExpr
  return $ List [Atom "quote", x]

schemeNumeric :: Parser SchemeVal
schemeNumeric =
  try schemeFloat
    <|> try schemeComplex
    <|> try schemeRational
    <|> try schemeInteger

-- | Parse scheme expression
schemeExpr :: Parser SchemeVal
schemeExpr = schemeAtom <|> schemeNumeric <|> schemeQuoted <|> do
  char '('
  x <- try schemeList <|> schemeDottedList
  char ')'
  return x

readExpr :: String -> String
readExpr input = case parse schemeExpr "poggerScheme" input of
  Left  err -> "No match " ++ show input
  Right val -> show val
