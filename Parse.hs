module Parse (parseArrow) where

import Types
import Control.Monad.Error (throwError)
import Text.ParserCombinators.Parsec
import qualified Data.Vector as V

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
parseList = do char '('
               skipMany space
	       x <- parseExpr' `sepEndBy` (many1 space)
	       char ')'
	       return $ List x

parseQuotedList :: Parser Expr
parseQuotedList = do char '\''
                     List x <- parseList
                     return $ QuotedList x

parseVector :: Parser Expr
parseVector = do char '['
                 skipMany space
                 x <- parseExpr' `sepEndBy` (many1 space)
                 char ']'
                 return $ Vector (V.fromList x)
               
parseExpr' :: Parser Expr
parseExpr' =  (try parseInteger)
          <|> (try parseSymbol)
          <|> (try parseList)
          <|> (try parseQuotedList)
          <|> (try parseVector)

parseExpr :: Parser Expr
parseExpr = do skipMany space
	       x <- parseExpr'
	       skipMany space ; eof
	       return x

parseArrow :: String -> Result
parseArrow source = case (parse parseExpr "" source) of
		 Right x -> return x
		 Left e -> throwError $ show e
