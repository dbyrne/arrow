module Main where

import Core
import Eval
import Parse
import Types
import System.IO
import Control.Monad.State (evalStateT, liftIO, StateT)
import Control.Monad.Error (catchError, runErrorT)

main ::IO ()
main = do runErrorT (evalStateT repl stdEnv)
          return ()

repl :: StateT Env Exception ()
repl = do liftIO $ putStr "Arrow >> "
	  liftIO $ hFlush stdout
	  x <- liftIO $ getLine
	  if x == "exit"
            then return ()
            else do evaled <- parseArrow x >>= eval
                    liftIO . putStrLn . show $ evaled
                    repl
          `catchError` (\e -> do liftIO $ putStrLn e
                                 repl)






