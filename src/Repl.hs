{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Repl where


import           AST
import           Control.Monad.Except
import           Control.Monad.Reader
import           Env
import           Evaluator
import           Exception
import           Parser.Sexp
import           Prettyprinter
import           Text.Parsec

-- | Parser string to pogger values.
readExpr :: (MonadError PoggerError m) => String -> m PoggerVal
readExpr input = case parse poggerExpr "poggerScheme" input of
  Left  err -> throwError $ ParserError err
  Right val -> return val

until' :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until' pred prompt action = do
  val <- prompt
  if pred val
     then return ()
     else action val >> until' pred prompt action

-- | repl entrance.
repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
  val <- return . fmap (show . pretty) $ readExpr line >>= eval
  v <- (runExceptT . runReaderT (unPogger val)) =<< emptyEnv
  putStrLn . extractValue . trapError $ v
  repl
