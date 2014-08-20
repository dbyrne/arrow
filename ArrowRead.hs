module ArrowRead (readArrow) where

import ArrowTypes
import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Text.ParserCombinators.Parsec hiding (spaces)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseArrow :: Parser ArrowVal
parseArrow = parseIdentifier
             <|> parseString
             <|> parseNumber
             <|> parseQuoted
             <|> parseList

readArrow :: String -> ThrowsEx ArrowVal
readArrow input = case parse parseArrow "arrow" input of
  Left err -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser ArrowVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseIdentifier :: Parser ArrowVal
parseIdentifier = do 
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let identifier = first:rest
  return $ case identifier of 
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Identifier identifier

parseNumber :: Parser ArrowVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser ArrowVal
parseList = do
  char '('
  x <- sepBy parseArrow spaces
  char ')'
  return $ List x

parseQuoted :: Parser ArrowVal
parseQuoted = do
  char '\''
  x <- parseArrow
  return $ List [Identifier "quote", x]


