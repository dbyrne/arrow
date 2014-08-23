
module Core where

import Types
import Eval
import qualified Data.Map as Map
import Control.Monad.State
import qualified Data.Vector as V

math :: (Integer -> Integer -> Integer) -> Result
math f  = do (List args) <- getSymbol "..."
             binOp f args

binOp :: (Integer -> Integer -> Integer) -> [Expr] -> Result
binOp op args = do return $ foldl1 (binOp' op) args
                     where binOp' op (Integer i) (Integer j) = Integer (i `op` j)

eq :: Result
eq = do (List args) <- getSymbol "..."
        return $ foldl1 (\(Integer a) (Integer b) -> Integer(if a == b then 1 else 0)) args

setArgs = ["symbol", "value"]
set :: Result
set = do [(Symbol s), e] <- getSymbols setArgs
         evalE <- eval e
         updateSymbolInParent s evalE
         return evalE

ifArgs = ["condition", "expr1", "expr2"]
arrowIf :: Result
arrowIf = do [condExpr, expr1, expr2] <- getSymbols ifArgs
             evalCond <- eval condExpr
             if (0 `notEqual` evalCond)
               then eval expr1
               else eval expr2
    where notEqual val1 (Integer val2) = val1 /= val2

fnArgs = ["args", "..."]
fn :: Result
fn = do [(Vector args), (List body)] <- getSymbols fnArgs
        let newFn = do evalBody <- mapM eval body
                       return $ last evalBody
        return $ Fn newFn (map (\(Symbol arg) -> arg) (V.toList args))

stdEnv :: Env
stdEnv = Env (Map.fromList [ ("+",  Fn (math (+)) ["..."])
                           , ("-",  Fn (math (-)) ["..."])
                           , ("*",  Fn (math (*)) ["..."])
			   , ("/",  Fn (math div) ["..."])
                           , ("eq", Fn eq ["..."])
			   , ("set", Special set setArgs)
                           , ("if",  Special arrowIf ifArgs)
                           , ("fn",  Special fn fnArgs )
                           ]) Nothing

getSymbol :: String -> Result
getSymbol sym = eval $ (Symbol sym)

getSymbols :: [String] -> ListResult
getSymbols syms = mapM getSymbol syms
