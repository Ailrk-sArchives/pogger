{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
    Convert the surface level syntax into s-expression based scheme syntax.

    Surface syntax use indentation to replace unnecessary parenthesis, and
    using curly bracket and square bracket to wrap infix annotation.
-}

module Parser.Surface where

import           Text.Parsec             hiding ( State )
import           Text.Parsec.Language           ( emptyDef )
import qualified Text.Parsec.Token             as Tok
import           Control.Monad.Identity
import           Data.Sequence

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
 where
  rn    = ["quote"]
  style = emptyDef { Tok.commentLine = "--", Tok.reservedNames = rn }

-- Holds the current indentation level.
type IndentLevelStack = Seq Int

-- Customized parser.
type IndentParser = Parsec String IndentLevelStack


-- == define different transformation from surface syntax to s-expression.

{- | Transform indentation to bracket.
Leading white space or tabs of each line will be treated as an indentation,
and each layer of indentation will be transformed to a s-expression bracket.

The algorithm:
  Before parsing anything a 0 indicate 0 indentation will be pushed
  into the stack.

  For each line, the indentation (# of spaces) will be compared with the
  previous line. If they are the same, nothing happen.
  If the indentation

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
transformIndentation = undefined


{- | Convert prefix notation into infix notation.
Before infix transformation @ a op b op c op ... @
After infix transformation @ op a b c ... @

Infix transformation only happen after bracket transformation.
-}
transformInfix :: IndentParser String
transformInfix = undefined


{- | Tranform different bracket types to uniform s-expression bracket.
There are two types of brackets in pogger. @ {} @ and @ [] @ are infix
bracket, and @ () @ is s-expression bracket.

The job of bracket transformation is to convert @ {} @  and @ [] @ int @ () @,and convert the fixation of operator based on the bracket type.
-}
transformBracket :: IndentParser String
transformBracket = undefined

