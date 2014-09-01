module Main where

import Emit
import Types

import Control.Monad.Trans

import System.IO
import System.Environment

import Parse

import qualified LLVM.General.AST as AST

import Control.Monad.Error (catchError, ErrorT, runErrorT)

repl :: AST.Module -> ErrorT String IO ()
repl mod = do
  input <- liftIO $ putStr "Arrow >> " >> hFlush stdout >> getLine
  if input == "exit"
    then return ()
    else do parsed <- parseArrow input
            newMod <- liftIO $ codegen mod [parsed]
            repl newMod
  `catchError` (\e -> do liftIO $ putStrLn e
                         repl mod)

emptyModule :: AST.Module
emptyModule = AST.defaultModule { AST.moduleName = "Arrow" }

main :: IO ()
main = do
  runErrorT (repl emptyModule)
  return ()

