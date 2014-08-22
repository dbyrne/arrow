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
eval (Symbol s) = do context <- get
                     lookupSymbol context
    where lookupSymbol (Env symTable parentEnv) =
              if s `member` symTable == True
              then return (symTable ! s)
              else case parentEnv of
                     Nothing -> throwError ("Symbol " ++ s ++ " is unbound.")
                     (Just parent) -> lookupSymbol parent

eval (List (x:xs)) = do fn <- eval x
                        apply fn
	where apply (Special f expectedArgs) = apply' expectedArgs xs f
	      apply (Fn f expectedArgs) = do args <- mapM eval xs
                                             apply' expectedArgs args f
              apply' expectedArgs args f = do modify pushContext
                                              applyArgsToContext expectedArgs args
                                              result <- f
                                              modify popContext
                                              return result
              applyArgsToContext ("...":_) args = do updateSymbol "..." (List args)
              applyArgsToContext (earg:expectedArgs) (arg:args) = do updateSymbol earg arg
                                                                     applyArgsToContext expectedArgs args
              applyArgsToContext [] _ = return ()

-- Helper context functions
updateSymbol :: MonadState Env m => String -> Expr -> m ()
updateSymbol s evalE = modify (\(Env symTable parentEnv) -> (Env (Map.insert s evalE symTable)) parentEnv)

updateSymbolInParent :: MonadState Env m => String -> Expr -> m ()
updateSymbolInParent s evalE = modify (\(Env symTable parentEnv) -> (Env symTable (updatedEnv parentEnv)))
    where updatedEnv (Just (Env symTable env)) = (Just (Env (Map.insert s evalE symTable) env))

pushContext :: Env -> Env
pushContext env = Env empty (Just env)

popContext :: Env -> Env
popContext env@(Env _ Nothing) = env
popContext (Env _ (Just parentEnv)) = parentEnv
