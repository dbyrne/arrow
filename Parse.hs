module Parse (parseArrow) where

import Types
import Control.Monad.Error (throwError)
import Text.ParserCombinators.Parsec

validSym = oneOf "!#$%&|*+-/:<=>?@^_~"

parseInteger :: Parser Expr
parseInteger = do sign <- option "" (string "-")
		  number <- many1 digit
		  return $ Integer (read (sign++number))

parseSymbol :: Parser Expr
parseSymbol = do f <- lower <|> validSym
		 r <- many (letter <|> validSym <|> digit)
		 return $ Symbol (f:r)

parseList :: Parser Expr
parseList = do char '(' ; skipMany space
	       x <- parseExpr' `sepEndBy` (many1 space)
	       char ')'
	       return $ List x
               
parseExpr' :: Parser Expr
parseExpr' = (try parseInteger) <|> (try parseSymbol) <|> (try parseList)

parseExpr :: Parser Expr
parseExpr = do skipMany space
	       x <- parseExpr'
	       skipMany space ; eof
	       return x

parseArrow :: String -> Result
parseArrow source = case (parse parseExpr "" source) of
		 Right x -> return x
		 Left e -> throwError $ show e
