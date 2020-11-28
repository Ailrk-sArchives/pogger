{-# LANGUAGE FlexibleContexts #-}
{-
   Pogger is a s-expression based language, transform the surface syntax to
   s-expression is the first step of the compilation.

  Regular scheme macro expander can be used after we have the s-expression
  format.
-}
module Parser.Sexp where



import           AST
import           Control.Monad.Except
import           Data.Char            (digitToInt)
import qualified Data.Map             as M
import           Data.Maybe
import           Exception
import           Numeric
import           Text.Parsec
import           Text.Parsec.String   (Parser)

-- | Non alpha numeric characters
poggerSymbol :: Parser Char
poggerSymbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- | R5RS string internal escape: \" \n \r \t \\
poggerString :: Parser PoggerVal
poggerString = do
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
poggerAtom :: Parser PoggerVal
poggerAtom = do
  h  <- letter <|> poggerSymbol
  hs <- many $ letter <|> digit <|> poggerSymbol
  let atom = h : hs
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

{- | conform R5S5 number tower.
Support both the integer and floating point form of
hexdecimal (#x), binary (#d), oct (#o).
-}
poggerReal :: Parser PoggerVal
poggerReal =
  Number . Real <$> (try decFloat <|> try hexFloat <|> try octFloat <|> try binFloat)

poggerInteger :: Parser PoggerVal
poggerInteger =
  Number . Integer
    <$> (try decInteger <|> try hexInteger <|> try octInteger <|> try binInteger
        )

poggerComplex :: Parser PoggerVal
poggerComplex = sign >>= \s -> do
  real <- (try decFloat <|> (fromIntegral <$> decInteger)) >>= \r ->
    return $ if s == "-" then negate r else r
  char '+'
  img <- try decFloat <|> try (fromIntegral <$> decInteger)
  char 'i'
  return . Number $ Complex real img

poggerRational :: Parser PoggerVal
poggerRational =  do
  s <- sign
  denominator <- decInteger >>= \d -> return $ if s == "-" then negate d else d
  char '/'
  dvisor <- decInteger
  return . Number $ Rational denominator dvisor

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

hex :: Parser String
hex = many $ digit <|> upper

oct :: Parser String
oct = many $ oneOf "01234567"

bin :: Parser String
bin = many $ oneOf "01"

hexInteger = prefixedFormatToInteger "#x" readHex hex
octInteger = prefixedFormatToInteger "#o" readOct oct
binInteger = prefixedFormatToInteger "#b" readBin bin

hexFloat = prefixedFormatToFloat "#x" readHex hex
octFloat = prefixedFormatToFloat "#o" readOct oct
binFloat = prefixedFormatToFloat "#b" readBin bin

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
prefixedFormatToInteger pstr rs num = prefix *> sign >>= \s ->
  getPart rs s <$> num
  where prefix = string pstr

getPart :: (Eq a, Num a) => ReadS a -> String -> String -> a
getPart rs sign | sign == "-" = negate . pick
                | otherwise   = pick
  where pick = fst . head . rs

sign :: Parser String
sign = (string "+" >> return "") <|> string "-" <|> pure ""

dot :: Parser String
dot = string "."

{- | Parse pogger characeter
\#<char> or \#<char name>
Characeters in \# are self-evaluating, no need to quote them.
A character must end with a space.
-}
poggerChar :: Parser PoggerVal
poggerChar = do
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

-- | Parse pogger list
poggerList :: Parser PoggerVal
poggerList = List <$> sepBy poggerExpr spaces

-- | Parse dotted pogger list
poggerDottedList :: Parser PoggerVal
poggerDottedList = do
  xs <- endBy poggerExpr spaces
  x  <- char '.' *> spaces *> poggerExpr
  return $ DottedList xs x

poggerQuoted :: Parser PoggerVal
poggerQuoted = do
  char '\''
  x <- poggerExpr
  return $ List [Atom "quote", x]

-- | `(a b ,c) => ((quote a b) c)
-- TODO unimplemented
poggerQuasiQuoted :: Parser PoggerVal
poggerQuasiQuoted = do
  char '`'
  x <- poggerExpr
  return $ List [Atom "quote", x]

poggerNumeric :: Parser PoggerVal
poggerNumeric =
  try poggerReal
    <|> try poggerComplex
    <|> try poggerRational
    <|> try poggerInteger

-- | Top level pogger expression parser
poggerExpr :: Parser PoggerVal
poggerExpr = poggerAtom <|> poggerNumeric <|> poggerQuoted <|> do
  char '('
  spaces
  x <- try poggerList <|> poggerDottedList
  spaces
  char ')'
  return x

-- | Parser string to pogger values.
readExpr :: (MonadError PoggerError m) => String -> m PoggerVal
readExpr input = case parse poggerExpr "poggerScheme" input of
  Left  err -> throwError $ ParserError err
  Right val -> return val
