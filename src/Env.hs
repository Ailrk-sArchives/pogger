module Env where

import           Data.IORef

import           AST
import           Control.Monad.Except
import           Exception


type Env = IORef [(String, IORef PoggerVal)]


emptyEnv :: IO Env
emptyEnv = newIORef []

type IOThrowsError = ExceptT PoggerError IO

-- | lift helper
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left e)  = throwError e
liftThrows (Right a) = return a

-- | run the Exxception and trap errors.
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

-- | check if a given variable is bound in the environment.
isBound :: Env -> String -> IO Bool
isBound ref var = readIORef ref >>= return . maybe False (const True) . lookup var

-- | get variable from environent.
getVar :: Env -> String -> IOThrowsError PoggerVal
getVar ref var = (liftIO $ readIORef ref) >>=
  maybe (throwError $ UnboundVar "Unbounded variable" var)
        (liftIO . readIORef)
        . lookup var

-- | set a variable to some value.
setVar :: Env -> String -> PoggerVal -> IOThrowsError PoggerVal
setVar ref var value = (liftIO $ readIORef ref) >>=
   maybe (throwError $ UnboundVar "Unbounded variable" var)
         (liftIO . (flip writeIORef value))
         . lookup var >> return value

-- | set var without return the value.
setVar_ :: Env -> String -> PoggerVal -> IOThrowsError ()
setVar_ ref var value = return () <* setVar ref var value


-- | bring a new binding into the environment
defineVar :: Env -> String -> PoggerVal -> IOThrowsError PoggerVal
defineVar = undefined
