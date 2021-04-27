{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Num where

import AST
import Control.Monad.Except
import qualified Data.Complex as C

-- --------------------------------------------------------------------------
-- Pogger number
-- This module implements some conversion between pogger numbers.
-- ophan instance. Why not? Nobody will define another Num for PoggerNum
-- Somewhere else.
-- --------------------------------------------------------------------------

-- | convert a PoggerNum to Complex
poggerNumToComplex :: PoggerNum -> C.Complex Double
poggerNumToComplex (Integer n) = (realToFrac n) C.:+ 0.0
poggerNumToComplex (Real r) = r C.:+ 0.0
poggerNumToComplex (Rational a b) = (realToFrac a / realToFrac b) C.:+ 0.0
poggerNumToComplex (Complex a b) = a C.:+ b

poggerNumToDouble :: PoggerNum -> Double
poggerNumToDouble (Integer n) = fromIntegral n
poggerNumToDouble (Real r) = r
poggerNumToDouble (Rational a b) = fromIntegral a / fromIntegral b
poggerNumToDouble (Complex _ _) = error "Can't downgrade complex to real"

-- | Convert complex to pogger complex.
toPoggerComplex :: C.Complex Double -> PoggerNum
toPoggerComplex c = Complex (C.realPart c) (C.imagPart c)

-- | Generic binary operator that handles coersion rules.
poggerBinop :: (forall a. Num a => a -> a -> a) -> PoggerNum -> PoggerNum -> PoggerNum
poggerBinop op (Integer m) (Integer n) = Integer (m `op` n)
poggerBinop op (Rational m n) (Rational m' n') = Rational (m `op` m') (n `op` n')
poggerBinop op (Real r) (Real r') = Real (r `op` r')
poggerBinop op (Complex m n) (Complex m' n') = Complex (m `op` m') (n `op` n')
poggerBinop op (Rational a b) (Integer n) = simplify $ Rational (a `op` (n * b)) b
poggerBinop op i@(Integer _) r@(Rational _ _) = r `op` i
poggerBinop op m@(Complex _ _) n = toPoggerComplex (poggerNumToComplex m `op` poggerNumToComplex n)
poggerBinop op m n@(Complex _ _) = m `op` n
poggerBinop op m@(Real _) n = (Real . C.realPart) (poggerNumToComplex m `op` poggerNumToComplex n)
poggerBinop op m n@(Real _) = m `op` n
{-# INLINE poggerBinop #-}

-- | absolute value of pogger num
-- >>> abs (Integer 2)
-- Integer 2
poggerUnary :: (forall a. Num a => a -> a) -> PoggerNum -> PoggerNum
poggerUnary op = \case
  Integer m -> Integer (op m)
  Rational m n -> Rational (op m) (op n)
  Real m -> Real (op m)
  c@(Complex _ _) ->
    let a = op (poggerNumToComplex c)
     in Complex (C.realPart a) (C.imagPart a)

instance Num PoggerNum where
  (+) = poggerBinop (+)
  {-# INLINE (+) #-}

  (*) = poggerBinop (*)
  {-# INLINE (*) #-}

  abs = poggerUnary abs
  {-# INLINE abs #-}

  negate = poggerUnary negate
  {-# INLINE negate #-}

  fromInteger = Integer
  {-# INLINE fromInteger #-}

  signum = poggerUnary signum
  {-# INLINE signum #-}

-- | division on pogger number
poggerNumDivide :: PoggerNum -> PoggerNum -> PoggerNum
poggerNumDivide (Real r) (Real r') = Real (r / r')
poggerNumDivide c@(Complex _ _) c'@(Complex _ _) =
  toPoggerComplex (poggerNumToComplex c / poggerNumToComplex c')
poggerNumDivide (Integer m) (Integer n) = (Integer . floor) ((fromIntegral m) / (fromIntegral n))
poggerNumDivide (Rational m n) (Rational m' n') = simplify (Rational (m * n') (n * m'))

-- >>> (abs . negate) (Rational 2 3 / Rational 5 3)
-- Rational 2 5
-- >>> Rational 2 3 / Rational 2 6
-- Integer 2
instance Fractional PoggerNum where
  (/) = poggerNumDivide
  {-# INLINE (/) #-}

  fromRational = Real . fromRational
  {-# INLINE fromRational #-}

-- | simply rational terms.
simplify :: PoggerNum -> PoggerNum
simplify (Rational a b) =
  let v = gcd a b
      r@(Rational m n) = Rational (a `div` v) (b `div` v)
   in if n == 1
        then Integer m
        else Rational m n
simplify _ = error "trying to simply non-rational"
{-# INLINE simplify #-}

-- | ord for pogger number
instance Ord PoggerNum where
  m <= n = poggerNumToDouble m <= poggerNumToDouble n
  {-# INLINE (<=) #-}

instance Real PoggerNum where
  toRational (Integer n) = fromIntegral n
  toRational (Real r) = toRational r
  toRational (Rational a b) = fromIntegral a / fromIntegral b
  toRational (Complex a _) = toRational a
  {-# INLINE toRational #-}

instance Enum PoggerNum where
  toEnum n = Integer (fromIntegral n)
  {-# INLINE toEnum #-}

  fromEnum (Integer n) = fromIntegral n
  fromEnum _ = (-1)
  {-# INLINE fromEnum #-}

-- | pogger
instance Integral PoggerNum where
  toInteger (Integer n) = n
  toInteger (Real r) = floor r
  toInteger (Rational a b) = floor (fromIntegral a / fromIntegral b)
  toInteger (Complex a _) = floor a
  {-# INLINE toInteger #-}

  quotRem (Integer a) (Integer b) = let (a', b') = (quotRem a b) in (Integer a', Integer b')
  quotRem a b = error ("no division algorithm defined for" ++ show a ++ show b)

-- | Safe wrappers
safeQuotRem :: PoggerNum -> PoggerNum -> ThrowsError (PoggerNum, PoggerNum)
safeQuotRem a@(Integer _) b@(Integer _) = return (quotRem a b)
safeQuotRem a _ = throwError (TypeMisMatch "can't mod on non integer values" (Number a))

safeMod :: PoggerNum -> PoggerNum -> ThrowsError PoggerNum
safeMod a b = safeQuotRem a b >>= return . snd

safeDiv :: PoggerNum -> PoggerNum -> ThrowsError PoggerNum
safeDiv a b = safeQuotRem a b >>= return . fst
