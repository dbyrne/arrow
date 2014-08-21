module Arrow (evalString) where

import ArrowEval
import ArrowRead
import ArrowTypes
import Control.Monad (liftM)
import Control.Monad.Error (catchError)
import System.IO (hFlush, stdout)

evalString :: String -> String
evalString expr = catchEx $ liftM show $ readArrow expr >>= evalArrow

catchEx :: ThrowsEx String -> String
catchEx action = case catchError action (return . show) of
  (Left ex) -> show ex
  (Right val) -> val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

getInput :: String -> IO String
getInput prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "exit") (getInput "Arrow >> ") (putStrLn . evalString)

