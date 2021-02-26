module IR.BasicBlock (
    BasicBlock(..)
  , Labelled(..)
  , mkBasicBlock
  , blockIPush
  , blockIPushFront
) where

import Data.Sequence as Seq
import IR.Instruction

showLabel :: Label -> String
showLabel l = l

-- Class to indicate that a type is able to provide a label
class Labelled a where
    getLabel :: a -> Label

-- Basic Block record type
-- A basic block is defined here as a label followed by a sequence
-- of instructions. The instructions should not have any control flow
-- i.e., we can construct a directed graph of basic blocks which
-- determines the control flow of the program, knowing if we pass through
-- block A on an execution path, every instruction in A will be executed.
data BasicBlock r = BasicBlock
    { label :: Label
    , iList :: Seq (Instruction r)
    }

instance Labelled (BasicBlock a) where
    getLabel = label

-- Show instance for pretting printing a basic block
instance Show r => Show (BasicBlock r) where
    show (BasicBlock lab is) = showLabel lab ++ ":\n" ++ concatMap (\i -> "    " ++ show i ++ "\n") is

-- Functor proof for basic block
instance Functor BasicBlock where
    fmap f bb@(BasicBlock _ is) = bb { iList = (f <$>) <$> is }

-- Helper constructor for creating a new basic block - give it
-- a label, but start with no instructions
mkBasicBlock :: Label -> BasicBlock r
mkBasicBlock lab = BasicBlock lab Seq.empty

-- Helper function for pushing an instruction onto the top of a basic
-- block (less common)
-- Instructions are represented as sequences, so pushing is O(1)
blockIPushFront :: Instruction r -> BasicBlock r -> BasicBlock r
blockIPushFront i blk = blk { iList = i :<| iList blk }

-- Helper function for pushing an instruction onto the end of a basic
-- block
-- Instructions are represented as sequences, so pushing to the end
-- is also O(1)
blockIPush :: Instruction r -> BasicBlock r -> BasicBlock r
blockIPush i blk = blk { iList = iList blk :|> i }

