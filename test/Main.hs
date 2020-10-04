module Main where

import           Test.Hspec
import qualified ParserSpec


main :: IO ()
main = hspec $ do
  ParserSpec.spec
