{-# LANGUAGE FlexibleContexts #-}

module Parse (parseArrow) where

import Types
import Control.Monad.Error (MonadError, throwError)
import Text.ParserCombinators.Parsec
import qualified Data.Vector as V

validSym = oneOf "!#$%&|*+-/:<=>?@^_~"

parseInteger :: Parser Expr
parseInteger = do sign <- option "" (string "-")
		  number <- many1 digit
		  return $ Integer (read (sign++number))

parseId :: Parser Expr
parseId = do f <- lower <|> validSym
             r <- many (letter <|> validSym <|> digit)
             return $ Id (f:r)

parseList :: Parser Expr
parseList = do char '('
               skipMany space
	       x <- parseExpr' `sepEndBy` (many1 space)
	       char ')'
               return $ List x

parseExpr' :: Parser Expr
parseExpr' =  (try parseInteger)
          <|> (try parseId)
          <|> (try parseList)

parseExpr :: Parser Expr
parseExpr = do skipMany space
	       x <- parseExpr'
	       skipMany space ; eof
	       return x

parseArrow :: MonadError String m => String -> m Expr
parseArrow source = case (parse parseExpr "" source) of
		 Right x -> return x
		 Left e -> throwError $ show e
