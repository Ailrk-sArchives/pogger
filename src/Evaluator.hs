{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
module Evaluator where

import           AST
import           Control.Monad
import           Control.Monad.Except
import qualified Data.HashMap.Strict  as H
import           Exception


-- the core evaluator function
eval :: PoggerVal -> ThrowsError PoggerVal
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

-- note, the order matter, otherwise keywords can be interpreted
-- as functions.
eval (List (Atom func : args))   = traverse eval args >>= apply func

eval other = throwError $ BadSpecialForm "Unrecognized form" other


-- | apply a function to paramters.
apply :: String -> [PoggerVal] -> ThrowsError PoggerVal
apply func args = maybe (throwError $ NotFunction "Undefined: " func) ($ args) (H.lookup func primitives)

-- | environment
primitives :: H.HashMap String ([PoggerVal] -> ThrowsError PoggerVal)
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
  ]


-- unpack a pogger value to a, if failed throws an error.
type Unpacker a = PoggerVal -> ThrowsError a

unpackNum :: Unpacker PoggerNum
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
         then throwError
         $ TypeMisMatch "number"  $ String n
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
             -> ThrowsError PoggerVal
numericBinop _ []      = throwError $ NumArgs 2 []
numericBinop _ val@[_] = throwError $ NumArgs 2 val
numericBinop op params = traverse unpackNum params >>= return . Number . foldl1 op
{-# INLINE numericBinop #-}

-- | numericBinop but the operator but can throws an error.
partialNumericBinop :: (PoggerNum -> PoggerNum -> ThrowsError PoggerNum)
                    -> [PoggerVal]
                    -> ThrowsError PoggerVal
partialNumericBinop _ []      = throwError $ NumArgs 2 []
partialNumericBinop _ val@[_] = throwError $ NumArgs 2 val
partialNumericBinop op params = do
  pvals <- traverse unpackNum params
  a <- foldl1 (liftJoin2 op) (pure <$> pvals)
  return . Number $ a
  where
    liftJoin2 f ma mb = join (liftM2 f ma mb)
{-# INLINE partialNumericBinop #-}

-- | boolean op factory.
-- The purpose of boolean binary operation is to
-- check if two paramters satisfy certain predicates.
mkBoolBinop :: Unpacker a
            -> (a -> a -> Bool)
            -> [PoggerVal]
            -> ThrowsError PoggerVal
mkBoolBinop unpacker op args =
  if length args /= 2
     then throwError $ NumArgs 2 args
     else do
       vals <- sequence $ unpacker <$> args
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

cons :: [PoggerVal] -> ThrowsError PoggerVal
cons [a, List []] = return $ List [a]
cons [a, List xs] = return $ List (a : xs)
cons [a, b]       = return $ DottedList [a] b
cons others       = throwError $ NumArgs 2 others

car :: [PoggerVal] -> ThrowsError PoggerVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [others]             = throwError $ TypeMisMatch "pair" others
car others               = throwError $ NumArgs 1 others

cdr :: [PoggerVal] -> ThrowsError PoggerVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return $ x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [others]              = throwError $ TypeMisMatch "pair" others
cdr others                = throwError $ NumArgs 1 others

eqv :: [PoggerVal] -> ThrowsError PoggerVal
eqv = undefined

equal :: [PoggerVal] -> ThrowsError PoggerVal
equal = undefined
