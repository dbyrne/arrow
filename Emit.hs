{-# LANGUAGE OverloadedStrings #-}

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

import Data.Word
import Data.Int
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map as Map

import JIT

import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

cgenDave :: Integer -> AST.Operand
cgenDave n = AST.ConstantOperand $ C.Int 32 n

exPutChar :: AST.Operand
exPutChar = AST.ConstantOperand $ C.GlobalReference (FunctionType i32 [i32] False) (AST.Name "main")

namedTerm = I.Do $ I.Ret (Just (AST.LocalReference double (AST.UnName 0))) []

initModule :: AST.Module
initModule = AST.Module "Arrow" Nothing Nothing [
  AST.GlobalDefinition $ G.functionDefaults {
              G.returnType = i32,
              G.name = AST.Name "main",
              G.basicBlocks = [
                G.BasicBlock (AST.UnName 0) [
                   (AST.UnName 1) I.:= I.Add False False (cgenDave 5) (cgenDave 3) [],
                   --(AST.UnName 2) I.:= I.Call False CC.C [] (Right (exPutChar)) [] [] [],
                   (AST.UnName 2) I.:= I.Add False False (AST.LocalReference i32 (AST.UnName 1)) (AST.LocalReference i32 (AST.UnName 1)) []
                   ] ( 
                   I.Do $ I.Ret (Just (AST.LocalReference i32 (AST.UnName 2))) []
                   )
                ]
             }
  ]
  
--  AST.GlobalDefinition AST.functionDefaults {
--     G.returnType = A.T.double,
--     G.name = AST.Name "ArrowAdd",
--     G.basicBlocks = [G.BasicBlock (AST.UnName 0) i t] } ]

codegenDave :: AST.Module -> IO AST.Module
codegenDave mod = do
  res <- runJIT mod
  case res  of
    Right newast -> return newast
    Left err     -> putStrLn err >> return mod

main :: IO ()
main = do
  y <- codegenDave initModule
  return ()

