{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
module Evaluator where

import           AST
import           Control.Monad.Except
import           Data.Complex
import qualified Data.HashMap.Strict  as H
import           Data.Ratio
import           Exception


-- the core evaluator function
eval :: (MonadError PoggerError m) =>  PoggerVal -> m PoggerVal

-- eval data
eval val@(String _)              = return val
eval val@(Number (Integer _))    = return val
eval val@(Number (Real _))       = return val
eval val@(Number (Rational _ _)) = return val
eval val@(Number (Complex _ _))  = return val
eval val@(Bool _)                = return val
eval val@(Char _)                = return val

-- eval quotes
eval (List [Atom "quote", val])  = return val
-- eval (List (Atom func : args))  = mapM eval args >>= apply


-- apply :: String -> [PoggerVal] -> PoggerVal
-- apply func args = maybe (Bool False) ($ args) $ lookup func primitives


primitives :: H.HashMap String ([PoggerVal] -> PoggerVal)
primitives = H.fromList []


numericBinop :: Num a => (a -> a -> a) -> [PoggerVal] -> PoggerVal
numericBinop op  params = Number $ foldl1 op $ unpackNum <$> params

unpackNum :: PoggerVal -> PoggerNum
unpackNum (Number n) = n
unpackNum _          = error "not number"


-- existential
-- data Number = forall a. Num a => N a

-- pogger number
-- implements the conversion between different number types.
instance Num PoggerNum where
  -- multipliation
  (Complex a b) + (Complex c d) =
    let r = (a :+ b) +  (c :+ d)
     in Complex (realPart r) (imagPart r)

  (Complex a b) + (Rational d de) = (Complex (a + (fromIntegral d / fromIntegral de)) b)
  (Rational d de) + (Complex a b) = (Complex (a + (fromIntegral d / fromIntegral de)) b)
  (Complex a b) + (Integer n) = (Complex (a + fromInteger n) b)
  (Integer n) + (Complex a b)  = (Complex (a + fromInteger n) b)

  (Complex a b) + (Real n) = (Complex (a + n) b)
  (Real n) + (Complex a b)  = (Complex (a + n) b)

  (Rational a b) + (Rational c d) = Rational (a + c) (b + d)

  (Rational a b) + (Integer n) = Rational (a + n) b
  (Integer n) + (Rational a b) = Rational (a + n) b

  (Rational a b) + (Real n) = (Real ((fromIntegral a + n) / fromIntegral b))
  (Real n) + (Rational a b) = (Real ((fromIntegral a + n) / fromIntegral b))

  (Integer a) + (Integer b) = Integer (a + b)
  (Integer a) + (Real b) = Real (fromIntegral a + b)
  (Real b) + (Integer a) = Real (fromIntegral a + b)

  (Real a) + (Real b) = Real (a + b)


  -- multipliation
  (Complex a b) * (Complex c d) =
    let r = (a :+ b) * (c :+ d)
     in Complex (realPart r) (imagPart r)

  (Complex a b) * (Rational d de) = (Complex (a * fromIntegral d / fromIntegral de) b)
  (Rational d de) * (Complex a b) = (Complex (a * fromIntegral d / fromIntegral de) b)

  (Complex a b) * (Integer n) = Complex (a * fromIntegral n) b
  (Integer n) * (Complex a b) = Complex (a * fromIntegral n) b

  (Complex a b) * (Real n) = Complex (a * n) b
  (Real n) * (Complex a b) = Complex (a * n) b

  (Integer a) * (Integer b) = Integer (a * b)
  (Integer a) * (Real b) = Real (fromIntegral a * b)
  (Real b) * (Integer a)  = Real (fromIntegral a * b)

  (Real a) * (Real b) = Real (a * b)

  -- absolute
  abs (Complex a b)  = Complex (abs a) (abs b)
  abs (Integer a)    = Integer (abs a)
  abs (Rational a b) = Rational (abs a) (abs b)
  abs (Real a)       = Real (abs a)

  -- negate
  negate (Complex a b) =
    let r = negate (a :+ b)
     in Complex (realPart r) (imagPart r)

  negate (Integer a) = Integer (negate a)
  negate (Rational a b) = Rational (negate a) (negate b)
  negate (Real a) = Real (negate a)

  -- fromInteger
  fromInteger n = Integer n

  -- signum
  signum (Integer n) | n > 0 = Integer 1
    | n == 0 = Integer 0
    | n < 0 = Integer (-1)

  signum (Real n) | n > 0 = Real 1
    | n == 0 = Real 0
    | otherwise = Real (-1)

  signum (Rational a b) | (a > 0 && b > 0) || (a < 0 && b < 0) = Rational 1 b
    | (a == 0) = Rational 0 b
    | otherwise = Rational (-1) b

  -- Not used TODO
  signum (Complex _ _) = Complex 1 1


instance Fractional PoggerNum where

  -- TODO not full
  (Real a) / (Real b)             = Real (a / b)
  (Integer a) / (Integer b)       = Real (fromIntegral a / fromIntegral b)
  (Rational a b) / (Rational c d) = Rational (a * c) (b * d)

  fromRational v = Rational (numerator v) (denominator v)
