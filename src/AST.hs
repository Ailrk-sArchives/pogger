{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- Definition of all top level data types.

module AST where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Complex
import           Data.IORef
import           Data.List
import           Data.Ratio
import           Prettyprinter
import           Text.Parsec          (ParseError)

--data PoggerVal                deriving (Eq)

data PoggerVal where
  Atom :: String -> PoggerVal
  List :: [PoggerVal] -> PoggerVal
  DottedList :: [PoggerVal] -> PoggerVal -> PoggerVal
  String :: String -> PoggerVal
  Bool :: Bool -> PoggerVal
  Char :: Char -> PoggerVal
  Number :: PoggerNum -> PoggerVal
  FnPrimtive ::  PoggerPrimitiveFn -> PoggerVal
  Fn :: PoggerFunc -> PoggerVal
  deriving stock Eq

-- | wrapper of primitive functions.
-- We need this to derive Eq for PoggerVal
newtype PoggerPrimitiveFn = PoggerPrimitiveFn
  { unPrimitiveFn :: ([PoggerVal] -> ThrowsError PoggerVal) }

instance Eq PoggerPrimitiveFn where
  _ == _ = False

data PoggerFunc =
  PoggerFunc { params  :: [String]     -- function parameters
             , varargs :: (Maybe String)  -- varadic funtion
             , body    :: [PoggerVal]     -- function body expression
             , closure :: Env             -- closure
             }
             deriving Eq

-- R5S5 allows number to be coerced, but in haskell
-- a numberic type can only be applied with itself.
-- To implement a scheme number system we need a coercible
-- number type.
data PoggerNum where
  Integer :: Integer -> PoggerNum
  Real :: Double -> PoggerNum
  Rational :: Integer -> Integer -> PoggerNum
  Complex :: Double  -> Double -> PoggerNum
  deriving stock (Eq, Show, Read)

instance Pretty PoggerVal where
  pretty (Atom    str  )          = pretty str
  pretty (List    xs   )          = mconcat $
    [pretty "'("] <> (intersperse (pretty " ") $ pretty <$> xs)
                  <> [pretty ")"]
  pretty (DottedList [x] y) = pretty "'("
                            <> pretty x
                            <> pretty " . "
                            <> pretty y
                            <> pretty ")"
  pretty (DottedList xs y) = mconcat $ l (pretty <$> xs)
    where
      l xs = [pretty "'(" ]
          <> xs
          <> [pretty " . " , pretty y , pretty ")"]

  pretty (Number (Integer int))   = pretty int
  pretty (Number (Real float))    = pretty float
  pretty (Number (Rational de d)) = pretty
    (case d of
       1 -> show de
       _ -> show de <> "/" <> show d)
  pretty (Number (Complex  r  i)) = pretty (show r <> "+" <> show i <> "i")
  pretty (Bool b       )          = if b then pretty "#t" else pretty "#f"
  pretty (Char c       )          = pretty $ "\\#" ++ [c]
  pretty (String s)               = pretty s
  pretty (FnPrimtive _)         = pretty "<primitives>"
  pretty (Fn (PoggerFunc {..}))   = pretty $ "(lambda ("
                                          ++ unwords (show <$> params)
                                          ++ (case varargs of
                                                Nothing  -> ""
                                                Just arg -> " . " ++ arg)
                                          ++ ") ...)"


instance Show PoggerVal where
  show = show . pretty


-- PoggerError can be handled as exceptions --
data PoggerError
    = NumArgs !Integer ![PoggerVal]
    | TypeMisMatch !String !PoggerVal
    | ParserError !ParseError
    | BadSpecialForm !String !PoggerVal
    | NotFunction !String !String
    | UnboundVar !String !String
    | Default !String

type ThrowsError = Either PoggerError

-- | Environment.
type Env = IORef [(String, IORef PoggerVal)]
type IOThrowsError = ExceptT PoggerError IO


-- The main monad transformer stack --
type Pogger' = ReaderT Env IOThrowsError

newtype Pogger a = Pogger { unPogger :: Pogger' a }
  deriving newtype
    ( Functor
    , Applicative
    , MonadReader Env
    , MonadError PoggerError
    , MonadIO
    )

deriving instance Monad Pogger

-- | lift helper
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left e)  = throwError e
liftThrows (Right a) = return a

toPogger :: ThrowsError a -> Pogger a
toPogger  = Pogger . lift . liftThrows

toPogger' :: IOThrowsError a -> Pogger a
toPogger' = Pogger . lift


-- pogger number --
-- implements the conversion between different number types.
-- Note: all expressions that return a rational number needs to
--       be simplified.
instance Num PoggerNum where
  -- multipliation
  Complex a b + Complex c d =
    let r = (a :+ b) +  (c :+ d)
     in Complex (realPart r) (imagPart r)

  (Complex a b) + (Rational d de) =
    (Complex (a + (fromIntegral d / fromIntegral de)) b)

  (Rational d de) + (Complex a b) =
    (Complex (a + (fromIntegral d / fromIntegral de)) b)

  (Complex a b) + (Integer n) = (Complex (a + fromInteger n) b)
  (Integer n) + (Complex a b)  = (Complex (a + fromInteger n) b)

  (Complex a b) + (Real n) = (Complex (a + n) b)
  (Real n) + (Complex a b)  = (Complex (a + n) b)

  (Rational a b) + (Rational c d) = simplifyR $ Rational (a + c) (b + d)

  Rational a b + Integer n = simplifyR $ Rational (a + b * n) b
  Integer n + Rational a b = Rational a b + Integer n

  (Rational a b) + (Real n) =
    Real ((fromIntegral a + n) / fromIntegral b)

  (Real n) + (Rational a b) =
    Real ((fromIntegral a + n) / fromIntegral b)

  (Integer a) + (Integer b) = Integer (a + b)
  (Integer a) + (Real b) = Real (fromIntegral a + b)
  (Real b) + (Integer a) = Real (fromIntegral a + b)

  (Real a) + (Real b) = Real (a + b)
  {-# INLINE (+) #-}


  -- multipliation
  (Complex a b) * (Complex c d) =
    let r = (a :+ b) * (c :+ d)
     in Complex (realPart r) (imagPart r)

  (Complex a b) * (Rational d de) =
    Complex (a * fromIntegral d / fromIntegral de) b

  (Rational d de) * (Complex a b) =
    Complex (a * fromIntegral d / fromIntegral de) b

  (Complex a b) * (Integer n) = Complex (a * fromIntegral n) b
  (Integer n) * (Complex a b) = Complex (a * fromIntegral n) b

  (Complex a b) * (Real n) = Complex (a * n) b
  (Real n) * (Complex a b) = Complex (a * n) b

  (Integer a) * (Integer b) = Integer (a * b)
  (Integer a) * (Real b) = Real (fromIntegral a * b)
  (Real b) * (Integer a)  = Real (fromIntegral a * b)

  (Real a) * (Real b) = Real (a * b)
  {-# INLINE (*) #-}

  -- absolute
  abs (Complex a b)  = Complex (abs a) (abs b)
  abs (Integer a)    = Integer (abs a)
  abs (Rational a b) = Rational (abs a) (abs b)
  abs (Real a)       = Real (abs a)
  {-# INLINE abs #-}

  -- negate
  negate (Complex a b) =
    let r = negate (a :+ b)
     in Complex (realPart r) (imagPart r)

  negate (Integer a) = Integer (negate a)
  negate (Rational a b) = Rational (negate a) (negate b)
  negate (Real a) = Real (negate a)
  {-# INLINE negate #-}

  -- fromInteger
  fromInteger n = Integer n
  {-# INLINE fromInteger #-}

  -- signum
  signum (Integer n) = Integer (signum n)

  signum (Real n) = Real (signum n)

  signum (Rational a b) | (a > 0 && b > 0) || (a < 0 && b < 0) = Rational 1 b
    | (a == 0) = Rational 0 b
    | otherwise = Rational (-1) b

  signum (Complex a b) = Complex (signum a) (signum b)
  {-# INLINE signum #-}



instance Fractional PoggerNum where

  (Rational a b) / (Rational c d) = simplifyR $ Rational (a * c) (b * d)
  (Rational c d) / (Integer a) = simplifyR $ Rational c (d * a)
  (Integer a) / (Rational c d) = simplifyR $ Rational (a * c) d

  (Real a) / (Rational c d) =
    Real (a / (fromIntegral c / fromIntegral d))

  (Rational c d) / (Real a) =
    Real ((fromIntegral c / fromIntegral d) / a)

  (Real a) / (Real b)             = Real (a / b)

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

  (Integer a) / (Integer b) = simplifyR $ Rational a b
  (Integer a) / (Real b) = Real (fromIntegral a / b)
  (Real b) / (Integer a) = Real (fromIntegral a / b)
  {-# INLINE (/) #-}

  fromRational v = simplifyR $ Rational (numerator v) (denominator v)
  {-# INLINE fromRational #-}

-- simply rational terms.
simplifyR :: PoggerNum -> PoggerNum
simplifyR (Rational a b) = let v = gcd a b
                                   in Rational (a `div` v) (b `div` v)
simplifyR _ = error "trying to simply non-rational"
{-# INLINE simplifyR #-}


-- ord for pogger num

instance Ord PoggerNum where
  Complex a b <= Complex c d = (a, b) <= (c, d)
  Complex a _ <= Integer n = a <= fromIntegral n

  Real n <= Complex a _ = n <= a
  Complex a _ <= Real n = a <= n

  Integer n <= Complex a _ = fromIntegral n <= a
  Complex a _ <= Real n = a <= n

  Complex a _ <= Rational c d = a <= fromIntegral c / fromIntegral d
  Rational c d <= Complex a _ = fromIntegral c / fromIntegral d <= a


  Integer a <= Integer b = a <= b

  Integer a <= Real b = fromIntegral a <= b
  Real b <= Integer a = b <= fromIntegral a

  Integer a <= Rational b c =
    fromIntegral a <= fromIntegral b / fromIntegral c
  Rational b c <= Integer a =
    fromIntegral b / fromIntegral c <= fromIntegral a

  Real a <= Real b = a <= b

  Real a <= Rational b c = a <= fromIntegral b / fromIntegral c
  Rational b c <= Real a = fromIntegral b / fromIntegral c <= a

  Rational a b <= Rational c d = fromIntegral a / fromIntegral b <= fromIntegral c / fromIntegral d
  {-# INLINE (<=) #-}
