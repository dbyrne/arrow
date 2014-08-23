module Types ( Expr (..)
             , Env (..)
             , Exception
             , FuncSig
             , IOResult
             , Result
             , ListResult
             ) where

import Data.Map (Map)
import qualified Data.Vector as V
import Control.Monad.State
import Control.Monad.Error

data Expr = Integer Integer
          | Symbol String
          | Fn Func FuncSig
          | Special Func FuncSig
          | List [Expr]
          | QuotedList [Expr]
          | Vector (V.Vector Expr)

type FuncSig = [String]
type Func = Result

type SymbolTable = Map String Expr
data Env = Env SymbolTable (Maybe Env)

type Exception = ErrorT String IO

type Res a = StateT Env Exception a
type Result = Res Expr
type IOResult = Res ()
type ListResult = Res [Expr]

instance Show Expr where
	show (Integer x) = show x
	show (Symbol x) = x
	show (Fn _ _) = "<function>"
	show (Special _ _) = "<special-form>"
	show (List x) = "(" ++ unwords (map show x) ++ ")"


