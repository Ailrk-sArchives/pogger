{-
    Convert the surface level syntax into s-expression based scheme syntax.

    Surface syntax use indentation to replace unnecessary parenthesis, and
    using curly bracket and square bracket to wrap infix annotation.
-}

module Parser.Surface where

import           Text.Parsec.String
import           Text.Parsec             hiding ( State )
import           Text.Parsec.Language           ( emptyDef )
import qualified Text.Parsec.Token             as Tok
import           Control.Monad.Identity

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
 where
  rn    = ["quote"]
  style = emptyDef { Tok.commentLine = "--", Tok.reservedNames = rn }

-- Holds the current indentation level.
type IndentLevelStack = [Int]

-- Customized parser.
type IndentParser = Parsec String IndentLevelStack

-- TODO expose an api for customized transformation.

runSurfaceTransform :: String -> Either ParseError String
runSurfaceTransform = runParser transformSurface [0] "<surface tranform>"

-- | Top level surface syntax transformation.
transformSurface :: IndentParser String
transformSurface = do
  try transformIndentation <|> try transformInfix <|> try transformBracket

-- == define different transformation from surface syntax to s-expression.

{- | Transform indentation to bracket.
Leading white space or tabs of each line will be treated as an indentation,
and each layer of indentation will be transformed to a s-expression bracket.

Indentation must be multiple of 2.

The algorithm:
  Before parsing anything a 0 indicate 0 indentation will be pushed
  into the stack.

  For each line, the indentation (# of spaces) will be compared with the
  previous line. If they are the same, nothing happen.
  If the indentation is larger, the number will be pushed into the stack.
  and a @ ( @ token will be inserted into the stream.

  If the indentation is less than the previous one, it must existed somewhere
  in the stack already. pop until there, and for each item get pop off insert
  a @ ) @

Before the tranformation
@
(define (fact n)
  (if (<= n 1)
    (* n (fact (- n 1)))))
@

After the transformation
@
define (fact n)
  if (<= n 1)
    * n (fact (- n 1))
@
-}
transformIndentation :: IndentParser String
transformIndentation = do
  stack <- getState
  char '\n'
  ws <- many space
  let indentLevel  = length ws
      prevStackTop = head stack
  if indentLevel > prevStackTop
    then do   -- push new indent level
      modifyState (indentLevel :)
      return undefined
    else do   -- pop til the same indent level
      modifyState $ dropWhile (/= indentLevel)
      return undefined
  return undefined
  where continue = undefined


{- | Convert prefix notation into infix notation.
Before infix transformation @ a op b op c op ... @
After infix transformation @ op a b c ... @

If the first element in the expression is not an atom, then expression
is a list of expressions. Then the infix rule will recursively
propogate to next levels of expressions.

For instance the first element of @ {} @ in the following expression is not
an atom:
@
data (Maybe a)
  {
    Just : { a -> Maybe a }
    Nothing : Nothing
  }
@

So the infix rule propagate to each expressions it contains.
This will be equivalent to
@
(data (Maybe a)
  ({Just : { a -> Maybe a}}
   {Nothing : Nothing}))
@

This rule is very useful to write definitions without too much noises.

Infix transformation only happen after bracket transformation.
-}
transformInfix :: IndentParser String
transformInfix = do
  undefined


{- | Tranform different bracket types to uniform s-expression bracket.
There are two types of brackets in pogger. @ {} @ and @ [] @ are infix
bracket, and @ () @ is s-expression bracket.

The job of bracket transformation is to convert @ {} @  and @ [] @ int @ () @,and convert the fixation of operator based on the bracket type.
-}
transformBracket :: IndentParser String
transformBracket = undefined


-- | parse infix parenthesis
infixParens :: Parser a -> Parser a
infixParens p = do
  c <- infixL
  let infixR = char $ case c of
        '{' -> '}'
        '[' -> ']'
  n <- p
  infixR
  return n
  where infixL = (char '{') <|> char '['
