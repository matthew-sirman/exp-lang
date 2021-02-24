{-# LANGUAGE ScopedTypeVariables #-}

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

applyLinearITransform :: ([IR.Instruction a] -> [IR.Instruction b]) 
                      -> IR.BasicBlock a -> IR.BasicBlock b
applyLinearITransform transform bb = applyInstructionTransform transform' bb
    where
        transform' = Seq.fromList . transform . toList

applyInstructionTransform :: (Seq (IR.Instruction a) -> Seq (IR.Instruction b)) 
                          -> IR.BasicBlock a -> IR.BasicBlock b
applyInstructionTransform transform (IR.BasicBlock lab is) = IR.BasicBlock lab (transform is)

applyBasicTransform :: forall a b. (IR.BasicBlock a -> IR.BasicBlock b) 
                    -> IR.Program a -> IR.Program b
applyBasicTransform transform = applyFunctionTransform ftrans
    where
        ftrans :: IR.Function a -> IR.Function b
        ftrans (IR.Function name bs) = IR.Function name (transform <$> bs)

applyFunctionTransform :: (IR.Function a -> IR.Function b) 
                       -> IR.Program a -> IR.Program b
applyFunctionTransform transform (IR.Program fs) = IR.Program (M.map transform fs)

