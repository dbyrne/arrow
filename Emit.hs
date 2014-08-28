{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.ExecutionEngine as EE
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import LLVM.General.AST.Type

import Data.Function
import Data.List
import Data.Word
import Data.Int
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import JIT

import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

data Expr = Integer Integer
          | BinOp Op Expr Expr

data Op = Add

type SymbolTable = [(String, AST.Operand)]

type Names = Map.Map String Int

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

data CodegenState
  = CodegenState {
    currentBlock :: AST.Name                     -- Name of the active block to append to
  , blocks       :: Map.Map AST.Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [G.BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (AST.Name, BlockState) -> G.BasicBlock
makeBlock (l, (BlockState _ s t)) = G.BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (AST.Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

addBlock :: String -> Codegen AST.Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (AST.Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (AST.Name qname)

ret :: AST.Operand -> Codegen (I.Named I.Terminator)
ret val = terminator $ I.Do $ I.Ret (Just val) []

terminator :: I.Named I.Terminator -> Codegen (I.Named I.Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

setBlock :: AST.Name -> Codegen AST.Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [I.Named I.Instruction]            -- Stack of instructions
  , term  :: Maybe (I.Named I.Terminator)       -- Block terminator
  } deriving Show
             
add :: AST.Operand -> AST.Operand -> Codegen AST.Operand
add a b = instr $ AST.Add False False a b []

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: I.Instruction -> Codegen (AST.Operand)
instr ins = do
  n <- fresh
  let ref = (AST.UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref I.:= ins] } )
  return $ AST.LocalReference i32 ref

cgen :: Expr -> Codegen AST.Operand
cgen (Integer n) = return $ AST.ConstantOperand $ C.Int 32 n

cgen (BinOp op a b) = do
  case op of
    Add -> do
      ca <- cgen a
      cb <- cgen b
      add ca cb

exPutChar :: AST.Operand
exPutChar = AST.ConstantOperand $ C.GlobalReference (FunctionType i32 [i32] False) (AST.Name "main")

namedTerm = I.Do $ I.Ret (Just (AST.LocalReference double (AST.UnName 0))) []

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

define ::  Type -> String -> [(Type, AST.Name)] -> [G.BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  AST.GlobalDefinition $ G.functionDefaults {
    G.name        = AST.Name label
  , G.parameters  = ([G.Parameter ty nm [] | (ty, nm) <- argtys], False)
  , G.returnType  = retty
  , G.basicBlocks = body
  }

addDefn :: AST.Definition -> LLVM ()
addDefn d = do
  defs <- gets AST.moduleDefinitions
  modify $ \s -> s { AST.moduleDefinitions = defs ++ [d] }

codegenTop :: Expr -> LLVM ()
codegenTop exp = do
  define i32 "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

codegen :: AST.Module -> [Expr] -> IO AST.Module
codegen mod fns = do
  res <- runJIT oldast
  case res  of
    Right newast -> return newast
    Left err     -> putStrLn err >> return oldast
  where
    modn    = mapM codegenTop fns
    oldast  = runLLVM mod modn



