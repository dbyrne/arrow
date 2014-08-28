module Main where

import Emit

import Control.Monad.Trans

import System.IO
import System.Environment

import qualified LLVM.General.AST as AST

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = Right [(BinOp Add (BinOp Add (Integer 1) (Integer 4)) (Integer 4)),
                   (BinOp Add (Integer 5) (Integer 6))]
  case res of
    Left err -> putStrLn err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

repl :: AST.Module -> IO ()
repl mod = do
  input <- putStr "Arrow >> " >> hFlush stdout >> getLine
  modn <- liftIO $ process mod input
  case modn of
    Just modn -> repl modn
    Nothing -> repl mod

emptyModule :: AST.Module
emptyModule = AST.defaultModule { AST.moduleName = "Arrow" }

main :: IO ()
main = do
  repl emptyModule

