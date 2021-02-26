{-# LANGUAGE ScopedTypeVariables #-}

module IR.Function (
    FuncID
  , Function(..)
  , mkFunc
  , pushBlock
) where

import Data.Sequence as Seq
import IR.Instruction
import IR.BasicBlock

showFunc :: FuncID -> String
showFunc f = f

-- Function record type
-- A function is a defined as a name (so it is callable) followed
-- by a sequence of basic blocks. Functions have (at the moment!)
-- exactly one argument value, and a return value.
-- Basic blocks are also represented as a sequence, so pushing
-- to the end is O(1)
data Function r = Function
    { fid :: FuncID
    , blocks :: Seq (BasicBlock r)
    }

-- Show instance for pretty printing functions
instance Show r => Show (Function r) where
    show (Function f bs) = showFunc f ++ "(" ++ show (Argument :: Value r) ++ ") {\n" ++ concatMap show bs ++ "}\n\n"

-- Functor proof for functions
instance Functor Function where
    fmap f (Function name bs) = Function name ((f <$>) <$> bs)

-- Helper function for making an empty IR function. The
-- basic block sequence is again empty
mkFunc :: FuncID -> Function r
mkFunc nm = Function nm Seq.empty

-- Helper function for adding a basic block to the end of a function
pushBlock :: BasicBlock r -> Function r -> Function r
pushBlock blk (Function name bs) = Function name (bs :|> blk)

