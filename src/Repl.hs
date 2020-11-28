{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Repl where


import           Evaluator
import           Exception
import           Parser.Sexp

import           Control.Monad.Except

type Repl =  ExceptT PoggerError IO


repl :: Repl ()
repl = do
  line <- liftIO getLine
  val <- readExpr $ line
  -- liftIO . putStrLn . show . eval $ val
  return ()

startRepl :: IO ()
startRepl = runExceptT repl >> startRepl
