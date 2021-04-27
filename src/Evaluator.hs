{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Evaluator where

import AST
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.HashMap.Strict as H
import Data.Maybe (isNothing)
import Env
import Num

-- the core evaluator function
eval :: PoggerVal -> Pogger PoggerVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Char _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, seq, alt]) =
  do
    b <- eval pred
    case b of
      Bool False -> eval alt
      _ -> eval seq
eval (List [Atom "set!", Atom var, form]) = do
  value <- eval form
  env <- ask
  toPogger_ $ setVar env var value
eval (List [Atom "define", Atom var, form]) = do
  value <- eval form
  env <- ask
  toPogger_ $ defineVar env var value

-- note, the order matter, otherwise keywords can be interpreted
-- as functions.
-- eval (List (Atom func : args))   =
--   traverse eval args >>= apply func

eval other = throwError $ BadSpecialForm "Unrecognized form" other

-- | apply a function to paramters.
apply :: PoggerFunc -> [PoggerVal] -> Pogger PoggerVal
apply (PoggerPrimitiveFn fn) args = toPoggerE $ fn args
apply (PoggerFunc {..}) args
  | length params /= length args = throwError (NumArgs (toInteger . length $ params) args)
  | otherwise = liftIO substitute >>= bindVarArgs varargs >>= evalBody
  where
    substitute = bindVars closure (zip params args)
    evalBody = undefined
    bindVarArgs arg env = maybe (return env) _ arg
{-# INLINE apply #-}

-- --------------------------------------------------------------------------
-- environment
-- --------------------------------------------------------------------------

primitives :: H.HashMap String ([PoggerVal] -> Pogger PoggerVal)
primitives =
  (H.fromList . mconcat)
    [ to
        ( [ ("+", BinOp (+)),
            ("-", BinOp (-)),
            ("*", BinOp (*)),
            ("/", BinOp (/))
          ] ::
            [(String, Operator PoggerNum PoggerNum)]
        ),
      to
        ( [ ("=", BinOp (==)),
            ("/=", BinOp (/=)),
            (">", BinOp (>)),
            ("<", BinOp (<)),
            (">=", BinOp (>=)),
            ("<=", BinOp (<=))
          ] ::
            [(String, Operator PoggerNum Bool)]
        ),
      to
        ( [ ("string=?", BinOp (==)),
            ("string>?", BinOp (>)),
            ("string<?", BinOp (<)),
            ("string>=?", BinOp (>=)),
            ("string<=?", BinOp (<=))
          ] ::
            [(String, Operator String Bool)]
        ),
      to
        ( [ ("and", BinOp (&&)),
            ("or", BinOp (||))
          ] ::
            [(String, Operator Bool Bool)]
        ),
      to
        ( [ ("mod", BinOp (safeMod)),
            ("quotient", BinOp (safeDiv)),
            ("rem", BinOp (safeMod))
          ] ::
            [(String, Operator PoggerNum (ThrowsError PoggerNum))]
        ),
      to
        [ ("cons", cons),
          ("cdr", cdr),
          ("car", car),
          ("eq?", eqv),
          ("print", print')
        ]
    ]
  where
    to xs = [(a, toPoggerPrim b) | (a, b) <- xs]

-- --------------------------------------------------------------------------
-- pogger primitive
-- --------------------------------------------------------------------------

data Operator a ret where
  BinOp :: forall a b. (a -> a -> b) -> Operator a b
  UnOP :: forall a b. (a -> b) -> Operator a b

-- | convert arbtrary haskell value into pogger primitive function.
class ToPoggerPrim a where
  toPoggerPrim :: a -> [PoggerVal] -> Pogger PoggerVal

-- | base case.
instance ToPoggerPrim ([PoggerVal] -> Pogger PoggerVal) where
  toPoggerPrim n = n
  {-# INLINE toPoggerPrim #-}

-- | Bool function
instance Throwable a => ToPoggerPrim (Operator a Bool) where
  toPoggerPrim (BinOp _) args | length args /= 2 = throwError (NumArgs 2 [])
  toPoggerPrim (BinOp op) args = do
    vals <- (toPoggerE . sequence) (unpack <$> args)
    return . Bool $ head vals `op` (vals !! 1)
  {-# INLINE toPoggerPrim #-}

-- | operators on Pogger Num
instance ToPoggerPrim (Operator PoggerNum PoggerNum) where
  toPoggerPrim (BinOp _) args | length args /= 2 = throwError (NumArgs 2 [])
  toPoggerPrim (BinOp op) args =
    toPoggerE
      (traverse unpack args >>= return . Number . foldl1 op)
  toPoggerPrim (UnOP _) [] = throwError (NumArgs 1 [])
  toPoggerPrim (UnOP op) [a] = toPoggerE (unpack a >>= \b -> (return . Number) (op b))
  {-# INLINE toPoggerPrim #-}

instance ToPoggerPrim (Operator PoggerNum (ThrowsError PoggerNum)) where
  toPoggerPrim (BinOp _) args | length args /= 2 = throwError (NumArgs 2 [])
  toPoggerPrim (BinOp op) args = do
    vals <- toPoggerE (traverse unpack args)
    Number <$> toPoggerE (foldl1 (liftJoin2 op) (return <$> vals))
    where
      liftJoin2 f ma mb = join (liftM2 f ma mb)
  {-# INLINE toPoggerPrim #-}

-- --------------------------------------------------------------------------
-- list operations.
-- --------------------------------------------------------------------------

cons :: [PoggerVal] -> Pogger PoggerVal
cons [a, List []] = return $ List [a]
cons [a, List xs] = return $ List (a : xs)
cons [a, b] = return $ DottedList [a] b
cons others = throwError $ NumArgs 2 others
{-# INLINE cons #-}

car :: [PoggerVal] -> Pogger PoggerVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [others] = throwError $ TypeMisMatch "pair" others
car others = throwError $ NumArgs 1 others
{-# INLINE car #-}

cdr :: [PoggerVal] -> Pogger PoggerVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [others] = throwError $ TypeMisMatch "pair" others
cdr others = throwError $ NumArgs 1 others
{-# INLINE cdr #-}

-- | strong equality
eqv :: [PoggerVal] -> Pogger PoggerVal
eqv = \case
  [Bool a, Bool b] -> equal a b
  [Number a, Number b] -> equal a b
  [String a, String b] -> equal a b
  [Atom a, Atom b] -> equal a b
  [DottedList xs x, DottedList ys y] -> eqv [List $ xs ++ [x], List $ ys ++ [y]]
  [List xs, List ys] -> equal xs ys
  [_, _] -> return (Bool False)
  other -> throwError $ NumArgs 2 other
  where
    equal :: Eq a => a -> a -> Pogger PoggerVal
    equal = \a b -> return $ Bool $ a == b
{-# INLINE eqv #-}

-- | weak equality.
equal :: [PoggerVal] -> Pogger PoggerVal
equal = undefined
{-# INLINE equal #-}

-- TODO cannot find the variable name right now.
print' :: [PoggerVal] -> Pogger PoggerVal
print' [Atom var] = ask >>= \env -> toPogger_ $ getVar env var
{-# INLINE print' #-}
