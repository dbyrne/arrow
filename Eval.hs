{-# LANGUAGE FlexibleContexts #-}

module Eval ( eval
            , updateSymbolInParent
            ) where

import Types
import Data.Map as Map
import Control.Monad.State (get, modify, MonadState)
import Control.Monad.Error (throwError)

eval :: Expr -> Result
eval (Integer n) = return (Integer n)
eval (Fn f args) = return (Fn f args)
eval (Special f args) = return (Special f args)
eval (Symbol s) = do env <- get
                     lookupSymbol env
    where lookupSymbol (Env symTable parentEnv) =
              if s `member` symTable == True
              then return (symTable ! s)
              else case parentEnv of
                     Nothing -> throwError ("Symbol " ++ s ++ " is unbound.")
                     (Just parent) -> lookupSymbol parent
eval (QuotedList x) = return (List x)
eval (List (x:xs)) =  eval x >>= apply
  where apply (Special f expectedArgs) = apply' expectedArgs xs f
        apply (Fn f expectedArgs) = do args <- mapM eval xs
                                       apply' expectedArgs args f
        apply _ = throwError "invalid function call"

apply' :: FuncSig -> [Expr] -> Result -> Result
apply' expectedArgs args f = do modify pushEnv
                                applyArgsToEnv expectedArgs args
                                result <- f
                                modify popEnv
                                return result
 
-- env funcs
applyArgsToEnv :: FuncSig -> [Expr] -> IOResult
applyArgsToEnv ("...":_) args = do updateSymbol "..." (List args)
applyArgsToEnv (earg:expectedArgs) (arg:args) = do updateSymbol earg arg
                                                   applyArgsToEnv expectedArgs args
applyArgsToEnv [] _ = return ()
applyArgsToEnv _ _ = throwError "Wrong arity"

updateSymbol :: MonadState Env m => String -> Expr -> m ()
updateSymbol s evalE = modify (\(Env symTable parentEnv) -> (Env (Map.insert s evalE symTable)) parentEnv)

updateSymbolInParent :: MonadState Env m => String -> Expr -> m ()
updateSymbolInParent s evalE = modify (\(Env symTable parentEnv) -> (Env symTable (updatedEnv parentEnv)))
    where updatedEnv (Just (Env symTable env)) = (Just (Env (Map.insert s evalE symTable) env))

pushEnv :: Env -> Env
pushEnv env = Env empty (Just env)

popEnv :: Env -> Env
popEnv env@(Env _ Nothing) = env
popEnv (Env _ (Just parentEnv)) = parentEnv
