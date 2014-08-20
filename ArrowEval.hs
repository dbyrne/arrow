module ArrowEval (evalArrow) where

import ArrowTypes
import Control.Monad.Error (throwError)

evalArrow :: ArrowVal -> ThrowsEx ArrowVal
evalArrow val@(String _) = return val
evalArrow val@(Number _) = return val
evalArrow val@(Bool _) = return val
evalArrow (List [Identifier "quote", val]) = return val
evalArrow (List [Identifier "if", predicate, thenClause, elseClause]) = do
  result <- evalArrow predicate
  case result of
    Bool False -> evalArrow elseClause
    _          -> evalArrow thenClause
evalArrow (List (Identifier func : args)) = mapM evalArrow args >>= apply func
evalArrow badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [ArrowVal] -> ThrowsEx ArrowVal
apply func args = maybe (throwError $ NotFunction "Missing function" func)
                  ($ args)
                  (lookup func primitives)

first :: [ArrowVal] -> ThrowsEx ArrowVal
first [List (x : xs)] = return x
first badArgList      = throwError $ NumArgs 1 badArgList

rest :: [ArrowVal] -> ThrowsEx ArrowVal
rest [List (x : xs)] = return $ List xs
rest badArgList      = throwError $ NumArgs 1 badArgList

cons :: [ArrowVal] -> ThrowsEx ArrowVal
cons [x, List xs] = return $ List $ x : xs
cons badArgList   = throwError $ NumArgs 2 badArgList

eqv :: [ArrowVal] -> ThrowsEx ArrowVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Identifier arg1), (Identifier arg2)] = return $ Bool $ arg1 == arg2
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
          Left err -> False
          Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

primitives = numOps ++ boolOps ++ listOps

listOps :: [(String, [ArrowVal] -> ThrowsEx ArrowVal)]
listOps = [ ("first", first)
          , ("rest", rest)
          , ("cons", cons)
          ]

numOps :: [(String, [ArrowVal] -> ThrowsEx ArrowVal)]
numOps = [ ("+", numOp (+))
         , ("-", numOp (-))
         , ("*", numOp (*))
         , ("/", numOp div)
         , ("mod", numOp mod)
         , ("quot", numOp quot)
         , ("rem", numOp rem)
         ]

numOp :: (Integer -> Integer -> Integer) -> [ArrowVal] -> ThrowsEx ArrowVal
numOp op [] = throwError $ NumArgs 2 []
numOp op [singleVal] = throwError $ NumArgs 2 [singleVal]
numOp op params = mapM unpackNum params >>= return . Number . foldl1 op

boolOps :: [(String, [ArrowVal] -> ThrowsEx ArrowVal)]
boolOps = [ ("eq?", eqv)
          , ("==", numBoolOp (==))
          , ("<", numBoolOp (<))
          , (">", numBoolOp (>))
          , ("/=", numBoolOp (/=))
          , (">=", numBoolOp (>=))
          , ("<=", numBoolOp (<=))
          , ("&&", boolBoolOp (&&))
          , ("||", boolBoolOp (||))
          , ("string=?", strBoolOp (==))
          , ("string<?", strBoolOp (<))
          , ("string>?", strBoolOp (>))
          , ("string<=?", strBoolOp (<=))
          , ("string>=?", strBoolOp (>=))
          ]

boolOp :: (ArrowVal -> ThrowsEx a) -> (a -> a -> Bool) -> [ArrowVal] -> ThrowsEx ArrowVal
boolOp unpacker op args = if length args /= 2 
                          then throwError $ NumArgs 2 args
                          else do left <- unpacker $ args !! 0
                                  right <- unpacker $ args !! 1
                                  return $ Bool $ left `op` right

numBoolOp  = boolOp unpackNum
strBoolOp  = boolOp unpackStr
boolBoolOp = boolOp unpackBool

unpackNum :: ArrowVal -> ThrowsEx Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: ArrowVal -> ThrowsEx String
unpackStr (String s) = return s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: ArrowVal -> ThrowsEx Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool
