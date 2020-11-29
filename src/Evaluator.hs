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
eval :: PoggerVal -> ThrowsError PoggerVal

-- eval data
eval val@(String _)              = return val
eval val@(Number (Integer _))    = return val
eval val@(Number (Real _))       = return val
eval val@(Number (Rational _ _)) = return val
eval val@(Number (Complex _ _))  = return val
eval val@(Bool _)                = return val
eval val@(Char _)                = return val
eval (List [Atom "quote", val])  = return val
eval (List (Atom func : args))   = mapM eval args >>= apply func
eval other = throwError $ BadSpecialForm "Unrecognized form" other

apply :: String -> [PoggerVal] -> ThrowsError PoggerVal
apply func args = maybe (throwError $ NotFunction "Undefined: " func) ($ args) (H.lookup func primitives)

primitives :: H.HashMap String ([PoggerVal] -> ThrowsError PoggerVal)
primitives = H.fromList [ ("+", numericBinop (+))
                        , ("-", numericBinop (-))
                        , ("*", numericBinop (*))
                        , ("/", numericBinop (/))
                        ]

numericBinop :: (PoggerNum -> PoggerNum -> PoggerNum) -> [PoggerVal] -> ThrowsError PoggerVal
numericBinop _ []      = throwError $ NumArgs 2 []
numericBinop _ val@[_] = throwError $ NumArgs 2 val
numericBinop op params = mapM unpackNum params >>= return .Number . foldl1 op

unpackNum :: PoggerVal -> ThrowsError PoggerNum
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum other      = throwError $ TypeMisMatch "number" other


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
  signum (Integer n) = Integer (signum n)

  signum (Real n) = Real (signum n)

  signum (Rational a b) | (a > 0 && b > 0) || (a < 0 && b < 0) = Rational 1 b
    | (a == 0) = Rational 0 b
    | otherwise = Rational (-1) b

  signum (Complex a b) = Complex (signum a) (signum b)


instance Fractional PoggerNum where

  (Rational a b) / (Rational c d) = Rational (a * c) (b * d)

  (Complex a b) / (Complex c d) =
    let v = (a :+ b) / (c :+ d)
     in Complex (realPart v) (imagPart v)

  (Complex a b) / (Real n) = Complex (a / n) b
  (Real n) / (Complex a b)  =
    let v1 = (n :+ 0)
        v2 = (a :+ b)
        v3 = v1 / v2
     in (Complex (realPart v3) (imagPart v3))

  (Complex a b) / (Integer n) = Complex (a / fromIntegral n) b
  (Integer n) / (Complex a b)  =
    let v1 = (fromIntegral n :+ 0)
        v2 = (a :+ b)
        v3 = v1 / v2
     in (Complex (realPart v3) (imagPart v3))

  (Complex a b) / (Rational c d) = (Complex (a / (fromIntegral c / fromIntegral d)) b)

  (Rational c d) / complex =
    let v1 = Real (fromIntegral c / fromIntegral d)
     in v1 / complex

  (Integer a) / (Integer b) = Rational a b
  (Integer a) / (Real b) = Real (fromIntegral a / b)
  (Real b) / (Integer a) = Real (fromIntegral a / b)

  (Rational c d) / (Integer a) = Rational c (d * a)
  (Integer a) / (Rational c d) = Rational (a * c) d

  (Real a) / (Real b)             = Real (a / b)

  (Real a) / (Rational c d) = Real (a / (fromIntegral c / fromIntegral d))
  (Rational c d) / (Real a) = Real ((fromIntegral c / fromIntegral d) / a)


  fromRational v = Rational (numerator v) (denominator v)
