{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Num where

import AST
import qualified Data.Complex as C

-- --------------------------------------------------------------------------
-- Pogger number
-- This module implements some conversion between pogger numbers.
-- --------------------------------------------------------------------------

-- | convert a PoggerNum to Complex
poggerNumToComplex :: PoggerNum -> C.Complex Double
poggerNumToComplex (Integer n) = (realToFrac n) C.:+ 0.0
poggerNumToComplex (Real r) = r C.:+ 0.0
poggerNumToComplex (Rational a b) = (realToFrac a / realToFrac b) C.:+ 0.0
poggerNumToComplex (Complex a b) = a C.:+ b

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

-- instance Fractional PoggerNum where
--   {-# INLINE (/) #-}

--   {-# INLINE fromRational #-}

-- simply rational terms.
simplify :: PoggerNum -> PoggerNum
simplify (Rational a b) =
  let v = gcd a b
   in Rational (a `div` v) (b `div` v)
simplify _ = error "trying to simply non-rational"
{-# INLINE simplify #-}

-- ord for pogger num

-- instance Ord PoggerNum where
--   (<=) = poggerBinop (<=)
--   {-# INLINE (<=) #-}
