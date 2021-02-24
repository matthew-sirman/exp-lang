module Passes.BasicBlockTransforms where

import qualified IR
import Builtin
-- import Data.Sequence as Seq

flattenBinaryOperations :: [IR.Instruction] -> [IR.Instruction]
flattenBinaryOperations (i0@(IR.Call t0 (IR.Builtin b) lhs):i1@(IR.Call t1 (IR.Variable t0') rhs):is)
    | t0 == t0' = replace b t1 lhs rhs : flattenBinaryOperations is
    | otherwise = i0 : flattenBinaryOperations (i1:is)
    where
        replace :: Builtin -> IR.VarID -> IR.Value -> IR.Value -> IR.Instruction
        replace (BinOp Add) = IR.Add
        replace (BinOp Sub) = IR.Sub
        replace (BinOp Mul) = IR.Mul
        replace (BinOp Div) = IR.Div

        replace (CmpOp EQ_) = IR.EQ
        replace (CmpOp LT_) = IR.LT
        replace (CmpOp GT_) = IR.GT
        replace (CmpOp LE_) = IR.LE
        replace (CmpOp GE_) = IR.GE

flattenBinaryOperations [] = []
flattenBinaryOperations (i:is) = i : flattenBinaryOperations is

constantFoldingPass :: [IR.Instruction] -> [IR.Instruction]
constantFoldingPass = id

