{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Repl where


import           AST
import           Control.Monad.Except
import           Evaluator
import           Exception
import           Parser.Sexp
import           Text.Parsec

-- | Parser string to pogger values.
readExpr :: (MonadError PoggerError m) => String -> m PoggerVal
readExpr input = case parse poggerExpr "poggerScheme" input of
  Left  err -> throwError $ ParserError err
  Right val -> return val

-- | repl entrance.
repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
  val <- return $ fmap show $ readExpr line >>= eval
  putStrLn . extractValue . trapError $ val
  repl
