{-# LANGUAGE OverloadedStrings #-}


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
import Types

import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.IntegerPredicate as IP


one = AST.ConstantOperand $ C.Int 32 1
zero = AST.ConstantOperand $ C.Int 32 0
false = zero
true = one

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [G.BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (AST.Name, BlockState) -> G.BasicBlock
makeBlock (l, (BlockState _ s t)) = G.BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

getBlock :: Codegen AST.Name
getBlock = gets currentBlock

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

add :: AST.Operand -> AST.Operand -> Codegen AST.Operand
add a b = instr $ AST.Add False False a b []

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- icmp IP.ULT a b
  return test
  --uitofp i32 test

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

toArgs :: [AST.Operand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: AST.Operand -> [AST.Operand] -> Codegen AST.Operand
call fn args = instr $ I.Call False CC.C [] (Right fn) (toArgs args) [] []

externf :: AST.Name -> AST.Operand
externf = AST.ConstantOperand . C.GlobalReference (FunctionType i32 [i32, i32] False)

toSig :: [Expr] -> [(AST.Type, AST.Name)]
toSig ((Id x):xs) = (i32, AST.Name x):(toSig xs)
toSig [] = []

alloca :: Type -> Codegen AST.Operand
alloca ty = instr $ I.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> Codegen AST.Operand
store ptr val = instr $ I.Store False ptr val Nothing 0 []

load :: AST.Operand -> Codegen AST.Operand
load ptr = instr $ I.Load False ptr Nothing 0 []

assign :: String -> AST.Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen AST.Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

codegenTop :: Expr -> LLVM ()
codegenTop (List [Id "defn", Id name, List args, body]) = do
  define i32 name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \(Id a) -> do
        var <- alloca i32
        store var (AST.LocalReference (FunctionType i32 [i32, i32] False) (AST.Name a))
        assign a var
      cgen body >>= ret

--codegenTop (Extern name args) = do
--  external double name fnargs
--  where fnargs = toSig args

codegenTop exp = do
  define i32 "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret



cgen :: Expr -> Codegen AST.Operand
cgen (Integer x) = return $ AST.ConstantOperand $ C.Int 32 x

cgen (Id x) = getvar x >>= load

cgen (List x) = do
  (f:args) <- mapM cgen x
  -- call (externf (AST.Name f)) args
  call f args
  
cgen (BinOp op a b) = do
  case op of
    LessThan -> do
      ca <- cgen a
      cb <- cgen b
      lt ca cb
    Add -> do
      ca <- cgen a
      cb <- cgen b
      add ca cb

cgen (If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  cond <- cgen cond
  test <- icmp IP.EQ false cond
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  setBlock ifthen
  trval <- cgen tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  setBlock ifelse
  flval <- cgen fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  setBlock ifexit
  phi i32 [(trval, ifthen), (flval, ifelse)]

phi :: Type -> [(AST.Operand, AST.Name)] -> Codegen AST.Operand
phi ty incoming = instr $ I.Phi ty incoming []

icmp :: IP.IntegerPredicate -> AST.Operand -> AST.Operand -> Codegen AST.Operand
icmp cond a b = instr $ I.ICmp cond a b []

  -- Control Flow
br :: AST.Name -> Codegen (I.Named I.Terminator)
br val = terminator $ I.Do $ I.Br val []

cbr :: AST.Operand -> AST.Name -> AST.Name -> Codegen (I.Named I.Terminator)
cbr cond tr fl = terminator $ I.Do $ I.CondBr cond tr fl []


exPutChar :: AST.Operand
exPutChar = AST.ConstantOperand $ C.GlobalReference (FunctionType i32 [i32] False) (AST.Name "main")

namedTerm = I.Do $ I.Ret (Just (AST.LocalReference i32 (AST.UnName 0))) []

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

external :: Type -> String -> [(Type, AST.Name)] -> LLVM ()
external retty label argtys = addDefn $
  AST.GlobalDefinition $ G.functionDefaults {
    G.name        = AST.Name label
  , G.parameters  = ([G.Parameter ty nm [] | (ty, nm) <- argtys], False)
  , G.returnType  = retty
  , G.basicBlocks = []
  }

addDefn :: AST.Definition -> LLVM ()
addDefn d = do
  defs <- gets AST.moduleDefinitions
  modify $ \s -> s { AST.moduleDefinitions = defs ++ [d] }

codegen :: AST.Module -> [Expr] -> IO AST.Module
codegen mod fns = do
  res <- runJIT oldast
  case res  of
    Right newast -> return newast
    Left err     -> putStrLn err >> return oldast
  where
    modn    = mapM codegenTop fns
    oldast  = runLLVM mod modn



