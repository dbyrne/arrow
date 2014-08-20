module Arrow (evalString) where

import ArrowEval
import ArrowRead
import ArrowTypes
import Control.Monad (liftM)
import Control.Monad.Error (catchError)

evalString :: String -> String
evalString expr = catchEx $ liftM show $ readArrow expr >>= evalArrow

catchEx :: ThrowsEx String -> String
catchEx action = case catchError action (return . show) of
  (Left ex) -> show ex
  (Right val) -> val
