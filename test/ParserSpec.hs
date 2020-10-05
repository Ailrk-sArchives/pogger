module ParserSpec where

import           Test.Hspec
import           Parser.Sexp
import           AST
import           Text.Parsec

spec :: Spec
spec = do
  sexp

-- | test surface syntax transformation.
surface :: Spec
surface = do
  it "surface 1" $ True `shouldBe` True

-- | test s-expression parser.
sexp :: Spec
sexp = do
  it "sexpression test 1"
    $ let
        source
          = "(define (foo a)  \
                 \ (if (eq? a 10) \
                 \    (+ a 10/3)    \
                 \    (- a 10+3i)))"
        ast = Right $ List
          [ Atom "define"
          , List [Atom "foo", Atom "a"]
          , List
            [ Atom "if"
            , List [Atom "eq?", Atom "a", Integer 10]
            , List [Atom "+", Atom "a", Rational 10 3]
            , List [Atom "-", Atom "a", Complex 10 3]
            ]
          ]
      in
        parse poggerExpr "scheme1" source `shouldBe` ast
