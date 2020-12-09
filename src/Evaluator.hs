{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

module Evaluator where

import           AST
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.HashMap.Strict  as H
import           Env


-- the core evaluator function
eval :: PoggerVal -> Pogger PoggerVal
eval val@(String _)              = return val
eval val@(Number (Integer _))    = return val
eval val@(Number (Real _))       = return val
eval val@(Number (Rational _ _)) = return val
eval val@(Number (Complex _ _))  = return val
eval val@(Bool _)                = return val
eval val@(Char _)                = return val
eval (List [Atom "quote", val])  = return val
eval (List [Atom "if", pred, seq, alt]) =
                     do b <- eval pred
                        case b of
                          Bool False -> eval alt
                          _          -> eval seq
eval (List [Atom "set!", Atom var, form]) = do
  value <- eval form
  env <- ask
  toPogger' $ setVar env var value

eval (List [Atom "define", Atom var, form]) = do
  value <- eval form
  env <- ask
  toPogger' $ defineVar env var value

-- note, the order matter, otherwise keywords can be interpreted
-- as functions.
-- eval (List (Atom func : args))   =
--   traverse eval args >>= apply func

eval other = throwError $ BadSpecialForm "Unrecognized form" other
{-# INLINE eval #-}

-- | apply a function to paramters.
apply :: PoggerVal -> [PoggerVal] -> Pogger PoggerVal
apply (FnPrimtive fn) args = toPogger $ (unPrimitiveFn fn) args
apply (Fn (PoggerFunc{..})) args
  | length params /= length args && varargs == Nothing =
      throwError $ NumArgs (toInteger . length $ params) args
  | otherwise = (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    evalBody env = undefined
    bindVarArgs arg env = maybe (return env) _ arg

-- | environment
primitives :: H.HashMap String ([PoggerVal] -> Pogger PoggerVal)
primitives = H.fromList
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop (/))

  , ("mod", partialNumericBinop poggerMod)
  , ("quotient", partialNumericBinop poggerQuotient)
  , ("remainder", partialNumericBinop poggerRemainder)

  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("<=", numBoolBinop (<=))
  , (">=", numBoolBinop (>=))
  , ("/=", numBoolBinop (/=))

  , ("and", boolBoolBinop (&&))
  , ("or", boolBoolBinop (||))

  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))

  , ("cons", cons)
  , ("cdr", cdr)
  , ("car", car)

  , ("eq?", eqv)

  , ("print", print')
  ]


-- unpack a pogger value to a, if failed throws an error.
type Unpacker a = PoggerVal -> ThrowsError a

unpackNum :: Unpacker PoggerNum
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
         then throwError $ TypeMisMatch "number"  $ String n
         else return $ fst $ parsed !! 0
unpackNum other      = throwError $ TypeMisMatch "number" other
{-# INLINE unpackNum #-}


unpackString :: Unpacker String
unpackString (String s) = return s
unpackString (Number n) = return . show $ n
unpackString (Bool s)   = return . show $ s
unpackString other      = throwError $ TypeMisMatch "string" other
{-# INLINE unpackString #-}

unpackBool :: Unpacker Bool
unpackBool (Bool b) = return b
unpackBool other    = throwError $ TypeMisMatch "boolean" other
{-# INLINE unpackBool #-}


-- | fold a binary operator over parameters
numericBinop :: (PoggerNum -> PoggerNum -> PoggerNum)
             -> [PoggerVal]
             -> Pogger PoggerVal
numericBinop _ []      = throwError $ NumArgs 2 []
numericBinop _ val@[_] = throwError $ NumArgs 2 val
numericBinop op params = toPogger $ traverse unpackNum params >>= return . Number . foldl1 op
{-# INLINE numericBinop #-}

-- | numericBinop but the operator but can throws an error.
partialNumericBinop :: (PoggerNum -> PoggerNum -> ThrowsError PoggerNum)
                    -> [PoggerVal]
                    -> Pogger PoggerVal
partialNumericBinop _ []      = throwError $ NumArgs 2 []
partialNumericBinop _ val@[_] = throwError $ NumArgs 2 val
partialNumericBinop op params = do
  pvals <- toPogger $ traverse unpackNum params
  Number <$> (toPogger $ foldl1 (liftJoin2 op) (pure <$> pvals))
  where
    liftJoin2 f ma mb = join (liftM2 f ma mb)
{-# INLINE partialNumericBinop #-}

-- | boolean op factory.
-- The purpose of boolean binary operation is to
-- check if two paramters satisfy certain predicates.
mkBoolBinop :: Unpacker a
            -> (a -> a -> Bool)
            -> [PoggerVal]
            -> Pogger PoggerVal
mkBoolBinop unpacker op args =
  if length args /= 2
     then throwError $ NumArgs 2 args
     else do
       vals <- toPogger . sequence $ unpacker <$> args
       return . Bool $ (vals !! 0) `op` (vals !! 1)

numBoolBinop = mkBoolBinop unpackNum
strBoolBinop = mkBoolBinop unpackString
boolBoolBinop = mkBoolBinop unpackBool


-- factory function for mod and it's varaints.
mkPoggerPartialIntBinop :: (Integer -> Integer -> Integer)
                        -> PoggerNum
                        -> PoggerNum
                        -> ThrowsError PoggerNum
mkPoggerPartialIntBinop op (Integer a) (Integer b) = return $ Integer (a `op` b)
mkPoggerPartialIntBinop _ (Integer _) b  =
  throwError . TypeMisMatch "number" $ Number b
mkPoggerPartialIntBinop _ a _  = throwError . TypeMisMatch "integer" $ Number a

poggerMod = mkPoggerPartialIntBinop mod
{-# INLINE poggerMod #-}
poggerQuotient = mkPoggerPartialIntBinop quot
{-# INLINE poggerQuotient #-}
poggerRemainder = mkPoggerPartialIntBinop rem
{-# INLINE poggerRemainder #-}

-- | list operations.

cons :: [PoggerVal] -> Pogger PoggerVal
cons [a, List []] = return $ List [a]
cons [a, List xs] = return $ List (a : xs)
cons [a, b]       = return $ DottedList [a] b
cons others       = throwError $ NumArgs 2 others
{-# INLINE cons #-}

car :: [PoggerVal] -> Pogger PoggerVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [others]             = throwError $ TypeMisMatch "pair" others
car others               = throwError $ NumArgs 1 others
{-# INLINE car #-}

cdr :: [PoggerVal] -> Pogger PoggerVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return $ x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [others]              = throwError $ TypeMisMatch "pair" others
cdr others                = throwError $ NumArgs 1 others
{-# INLINE cdr #-}

-- | strong equality
eqv :: [PoggerVal] -> Pogger PoggerVal
eqv [(Bool a), (Bool b)]     =  return . Bool $ a == b
eqv [(Number a), (Number b)] =  return . Bool $ a == b
eqv [(String a), (String b)] =  return . Bool $ a == b
eqv [(Atom a), (Atom b)]     =  return . Bool $ a == b
eqv [(DottedList xs x), (DottedList ys y)]     =
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List xs), (List ys)]     =  return . Bool $ xs == ys
eqv [_, _] = return . Bool $ False
eqv other = throwError $ NumArgs 2 other
{-# INLINE eqv #-}

-- | weak equality.
equal :: [PoggerVal] -> Pogger PoggerVal
equal = undefined
{-# INLINE equal #-}

-- TODO cannot find the variable name right now.
print' :: [PoggerVal] -> Pogger PoggerVal
print' [Atom var] = ask >>= \env -> toPogger' $ getVar env var
