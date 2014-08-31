module Main where

import Emit
import Types

import Control.Monad.Trans

import System.IO
import System.Environment

import Parse

import qualified LLVM.General.AST as AST

import Control.Monad.Error (catchError, ErrorT, runErrorT)

--process :: AST.Module -> Expr -> IO (Maybe AST.Module)
--process modo source = do
  --let res = Right [
  --      (If (BinOp LessThan (Integer 2) (Integer 2)) (Integer 1) (Integer 2))
        --(Defn "+" ["x", "y"] (BinOp Add (Id "x") (Id "y"))),
        --(If (BinOp LessThan (Integer 1) (Integer 2))
        -- (Compound "+" [(Compound "+" [Integer 5, Integer 1]), Integer 10])
        -- (Integer 99))
 --       ]
 -- case res of
--    Left err -> putStrLn err >> return Nothing
 --   Right ex -> do
 --     ast <- codegen modo ex
 --     return $ Just ast

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

