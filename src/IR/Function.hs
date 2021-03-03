{-# LANGUAGE ScopedTypeVariables #-}

module IR.Function (
    FuncID
  , Function(..)
  , mkFunc
  , pushBlock
) where

import Data.Sequence as Seq
import Data.List as L (intersperse)
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
    , args :: Int
    , blocks :: Seq (BasicBlock r)
    }

-- Show instance for pretty printing functions
instance Show r => Show (Function r) where
    show (Function f as bs) = showFunc f ++ "(" ++ argList ++ ") {\n" ++ concatMap show bs ++ "}\n\n"
        where
            argList :: String
            argList = concat $ L.intersperse ", " $ map (show . (Argument :: Int -> Value r)) [0..(as-1)]

-- Functor proof for functions
instance Functor Function where
    fmap f (Function name as bs) = Function name as ((f <$>) <$> bs)

-- Helper function for making an empty IR function. The
-- basic block sequence is again empty
mkFunc :: FuncID -> Int -> Function r
mkFunc nm as = Function nm as Seq.empty

-- Helper function for adding a basic block to the end of a function
pushBlock :: BasicBlock r -> Function r -> Function r
pushBlock blk (Function name as bs) = Function name as (bs :|> blk)

