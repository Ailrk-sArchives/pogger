{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- Definition of all top level data types.
module AST where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.List
import Text.Parsec (ParseError)

--data PoggerVal                deriving (Eq)

data PoggerVal where
  Atom :: String -> PoggerVal
  List :: [PoggerVal] -> PoggerVal
  DottedList :: [PoggerVal] -> PoggerVal -> PoggerVal
  String :: String -> PoggerVal
  Bool :: Bool -> PoggerVal
  Char :: Char -> PoggerVal
  Number :: PoggerNum -> PoggerVal
  Fn :: PoggerFunc -> PoggerVal
  deriving stock (Eq)

data PoggerFunc
  = PoggerFunc
      { params :: [String], -- function parameters
        varargs :: Maybe String, -- varadic funtion
        body :: [PoggerVal], -- function body expression
        closure :: Env -- closure
      }
  | PoggerPrimitiveFn -- wrapper for builtin functions.
      {unPrimitiveFn :: [PoggerVal] -> ThrowsError PoggerVal}

instance Eq PoggerFunc where
  _ == _ = False

-- R5S5 allows number to be coerced, but in haskell
-- a numberic type can only be applied with itself.
-- To implement a scheme number system we need a coercible
-- number type.
data PoggerNum where
  Integer :: Integer -> PoggerNum
  Real :: Double -> PoggerNum
  Rational :: Integer -> Integer -> PoggerNum
  Complex :: Double -> Double -> PoggerNum
  deriving stock (Eq, Show, Read)

-- | handled as exceptions
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

newtype Pogger a = Pogger {unPogger :: Pogger' a}
  deriving newtype
    ( Functor,
      Applicative,
      MonadReader Env,
      MonadError PoggerError,
      MonadIO
    )

deriving instance Monad Pogger

-- | unpack a value and make it throwable.
class Throwable (a :: *) where
  unpack :: PoggerVal -> ThrowsError a

instance Throwable PoggerNum where
  unpack (Number n) = return n
  unpack (List [n]) = unpack n
  unpack (String n) =
    let parsed = reads n
     in if null parsed
          then throwError $ TypeMisMatch "number" $ String n
          else return $ fst $ head parsed
  unpack other = throwError $ TypeMisMatch "number" other
  {-# INLINE unpack #-}

instance Throwable String where
  unpack (String s) = return s
  unpack (Number n) = return . show $ n
  unpack (Bool s) = return . show $ s
  unpack other = throwError (TypeMisMatch "string" other)
  {-# INLINE unpack #-}

instance Throwable Bool where
  unpack (Bool b) = return b
  unpack other = throwError (TypeMisMatch "boolean" other)
  {-# INLINE unpack #-}

-- | lift helper
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left e) = throwError e
liftThrows (Right a) = return a

toPoggerE :: ThrowsError a -> Pogger a
toPoggerE = Pogger . lift . liftThrows

toPogger_ :: IOThrowsError a -> Pogger a
toPogger_ = Pogger . lift

