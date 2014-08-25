{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen () where

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import Data.Map as Map
import Data.Word
import Control.Monad.State
import Control.Applicative

double :: Type
double = FloatingPointType 64 IEEE

type Names = Map.Map String Int

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState { currentBlock :: Name
                                 , blocks       :: Map.Map Name BlockState
                                 , symtab       :: SymbolTable
                                 , blockCount   :: Int
                                 , count        :: Word
                                 , names        :: Names
                                 } deriving Show
                                            
data BlockState = BlockState { idx   :: Int
                             , stack :: [Named Instruction]
                             , term  :: Maybe (Named Terminator)
                             } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
                  deriving (Functor, Applicative, Monad, MonadState CodegenState )
                           
newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }
