{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Repl where


import           Evaluator
import           Exception
import           Parser.Sexp
import           Prettyprinter

import           Control.Monad.Except

type Repl =  ExceptT PoggerError IO


repl :: Repl ()
repl = do
  liftIO $ putStr "> "
  line <- liftIO getLine
  val <- readExpr $ line
  liftIO . putStrLn . show . pretty . extractValue . eval $ val
  return ()

startRepl :: IO ()
startRepl = runExceptT repl >> startRepl
