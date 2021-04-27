{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- Definition of all top level data types.

module AST where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.List
import Prettyprinter
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

instance Pretty PoggerVal where
  pretty (Atom str) = pretty str
  pretty (List xs) =
    mconcat $
      [pretty "'("] <> intersperse (pretty " ") (pretty <$> xs)
        <> [pretty ")"]
  pretty (DottedList [x] y) =
    pretty "'("
      <> pretty x
      <> pretty " . "
      <> pretty y
      <> pretty ")"
  pretty (DottedList xs y) = mconcat $ l (pretty <$> xs)
    where
      l as =
        [pretty "'("]
          <> as
          <> [pretty " . ", pretty y, pretty ")"]
  pretty (Number (Integer int)) = pretty int
  pretty (Number (Real float)) = pretty float
  pretty (Number (Rational de d)) =
    pretty
      ( case d of
          1 -> show de
          _ -> show de <> "/" <> show d
      )
  pretty (Number (Complex r i)) = pretty (show r <> "+" <> show i <> "i")
  pretty (Bool b) = if b then pretty "#t" else pretty "#f"
  pretty (Char c) = pretty $ "\\#" ++ [c]
  pretty (String s) = pretty s
  pretty (Fn PoggerPrimitiveFn {..}) = pretty "<primitives>"
  pretty (Fn PoggerFunc {..}) =
    pretty $
      "(lambda ("
        ++ unwords (show <$> params)
        ++ ( case varargs of
               Nothing -> ""
               Just arg -> " . " ++ arg
           )
        ++ ") ...)"

instance Show PoggerVal where
  show = show . pretty

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

-- | lift helper
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left e) = throwError e
liftThrows (Right a) = return a

toPoggerE :: ThrowsError a -> Pogger a
toPoggerE = Pogger . lift . liftThrows

toPogger_ :: IOThrowsError a -> Pogger a
toPogger_ = Pogger . lift
