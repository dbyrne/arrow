module ArrowTypes ( ArrowVal(..)
                  , ArrowEx(..)
                  , ThrowsEx
                  ) where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error (Error(..))

-- Expression Types
data ArrowVal = Identifier String
              | List [ArrowVal]
              | DottedList [ArrowVal] ArrowVal
              | Number Integer
              | String String
              | Bool Bool

showVal :: ArrowVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Identifier name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"

instance Show ArrowVal where show = showVal

-- Exception Types
type ThrowsEx = Either ArrowEx
                             
data ArrowEx = NumArgs Integer [ArrowVal]
             | TypeMismatch String ArrowVal
             | Parser ParseError
             | BadSpecialForm String ArrowVal
             | NotFunction String String
             | UnboundVar String String
             | Default String

showError :: ArrowEx -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
 
instance Show ArrowEx where show = showError

instance Error ArrowEx where
     noMsg = Default "An error has occurred"
     strMsg = Default

-- Helper Functions
unwordsList :: [ArrowVal] -> String
unwordsList = unwords . map showVal






