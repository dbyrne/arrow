{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Applicative (Applicative)
import Control.Monad.State (MonadState, State)
import Data.Map (Map)
import Data.Word (Word)
import LLVM.General.AST (Module, Name, Operand)
import LLVM.General.AST.Instruction (Instruction, Named, Terminator)

data Expr = Integer Integer
          | Symbol String
          | Id String
          | Func String
          | Defn String [String] Expr
          | BinOp Op Expr Expr
          | If Expr Expr Expr
          | List [Expr]

data Op = Add
        | LessThan

type SymbolTable = [(String, Operand)]

type Names = Map String Int

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

data CodegenState = CodegenState {
    currentBlock :: Name
  , blocks       :: Map Name BlockState
  , symtab       :: SymbolTable
  , blockCount   :: Int
  , count        :: Word
  , names        :: Names
  } deriving Show

data BlockState = BlockState {
    idx   :: Int
  , stack :: [Named Instruction]
  , term  :: Maybe (Named Terminator)
  } deriving Show

newtype LLVM a = LLVM {unLLVM :: State Module a}
  deriving (Functor, Applicative, Monad, MonadState Module)

