module Passes (
    applyLinearITransform
  , applyInstructionTransform
  , applyBasicTransform
  , applyFunctionTransform
  , module Passes.BasicBlockTransforms
) where

import qualified Data.Map as M
import Data.Sequence as Seq
import Data.Foldable (toList)
import qualified IR

import Passes.BasicBlockTransforms

applyLinearITransform :: ([IR.Instruction] -> [IR.Instruction]) -> IR.BasicBlock -> IR.BasicBlock
applyLinearITransform transform bb = applyInstructionTransform transform' bb
    where
        transform' = Seq.fromList . transform . toList

applyInstructionTransform :: (Seq IR.Instruction -> Seq IR.Instruction) -> IR.BasicBlock -> IR.BasicBlock
applyInstructionTransform transform (IR.BasicBlock lab is) = IR.BasicBlock lab (transform is)

applyBasicTransform :: (IR.BasicBlock -> IR.BasicBlock) -> IR.Program -> IR.Program
applyBasicTransform transform = applyFunctionTransform ftrans
    where
        ftrans :: IR.Function -> IR.Function
        ftrans (IR.Function name bs) = IR.Function name (transform <$> bs)

applyFunctionTransform :: (IR.Function -> IR.Function) -> IR.Program -> IR.Program
applyFunctionTransform transform (IR.Program fs) = IR.Program (M.map transform fs)

