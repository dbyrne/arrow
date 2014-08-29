module Main where

import Emit
import Types

import Control.Monad.Trans

import System.IO
import System.Environment

import qualified LLVM.General.AST as AST

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = Right [
        (Defn "+" ["x", "y"] (BinOp Add (Id "x") (Id "y"))),
        (Compound "+" [(Compound "+" [Integer 5, Integer 1]), Integer 10])
        ]
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

